source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(pbapply)

############# carve channels into dem to concentrate flow -------------
useFlowSink=T

dem=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeDem.tif")
streamline=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/nhdFlowline_simplify.gpkg")
streamline=st_transform(streamline,crs=st_crs(dem))

streamCells=extract(dem,vect(streamline),cells=T)

dem[streamCells$cell]=dem[streamCells$cell]-10

writeRaster(dem,"~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeDem_carve.tif",overwrite=T)


#use flowSink polygons to define 'the optional map of actual depressions or sinkholes in the landscape that are large enough to slow and store surface runoff from a storm event.'
#All cells that are not NULL and not zero indicate depressions. Water will flow into but not out of depressions. 
if(useFlowSink){
  flowSink=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowSink.gpkg")
  
  flowSinkRaster=dem*0
  
  addSinkCells=extract(flowSinkRaster,vect(flowSink),cells=T)
  flowSinkRaster[addSinkCells$cell]=1
  
  demSinkCells=extract(dem,vect(flowSink),cells=T)
  dem[demSinkCells$cell]=dem[demSinkCells$cell]-100
  
}


############# calculate flowdir, uaa from r.watershed---------------

InitGrass_byRaster(dem,"dem")

if(useFlowSink){
  write_RAST(flowSinkRaster,"flowSink")
  stringexecGRASS("r.watershed -s --overwrite --verbose elevation=dem depression=flowSink accumulation=uaa_cells_sink drainage=flowDir_sink")
  flowDir=read_RAST("flowDir_sink")
  uaa_cells=read_RAST("uaa_cells_sink")
} else {
  stringexecGRASS("r.watershed -s --overwrite --verbose elevation=dem accumulation=uaa_cells drainage=flowDir")
  flowDir=read_RAST("flowDir")
  uaa_cells=read_RAST("uaa_cells")
}


cellsPerKm=res(uaa_cells)[1]*res(uaa_cells)[2] / 1000^2
uaa_km=uaa_cells * cellsPerKm

if(useFlowSink){
  
  writeRaster(flowDir,"~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif",overwrite=T)
  writeRaster(uaa_km,"~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif",overwrite=T)
  
}else{
  
  writeRaster(flowDir,"~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve.tif",overwrite=T)
  writeRaster(uaa_km,"~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km.tif",overwrite=T)
  
}


###################### create stream points -------------

allStreams=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/nhdFlowline_simplify.gpkg")
#everything above little wood confluence:
scArea=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/silverCreekWatershedExtent.gpkg")
scStreams=st_intersection(st_zm(allStreams),scArea)



scStreamPoints=extract(rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif"),
                       vect(scStreams),
                       touches=T,xy=T)

scStreamPoints=scStreamPoints[,c("uaa_cells_sink","x","y")]
names(scStreamPoints)[1]="uaa"
scStreamPoints=st_as_sf(scStreamPoints,coords = c("x","y"),crs=st_crs(scStreams))

######################limit points to those w/ >=,1 km uaa
#scStreamPoints=scStreamPoints[scStreamPoints$uaa>=.1,]

#add elevation and slope attributes

scStreamPoints$elevation=extract(rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeDem.tif"),vect(scStreamPoints))$Layer_1
scStreamPoints$pointid=1:nrow(scStreamPoints)

slopeDistance=50

scStreamPointsNearby=st_join(scStreamPoints,scStreamPoints[,c("elevation")],join=st_is_within_distance,dist=slopeDistance)

spread=function(x){
  return(max(x)-min(x))
}

scStreamPoints_elevrange=aggregate(scStreamPointsNearby[,c("pointid","elevation.y")],list(pointid=scStreamPointsNearby$pointid),FUN=spread)
scStreamPoints_elevrange$slope=scStreamPoints_elevrange$elevation.y/(2*slopeDistance)

scStreamPoints=merge(scStreamPoints,data.frame(scStreamPoints_elevrange[,c("pointid","slope")]))



dbExecute(conn(),"TRUNCATE TABLE streampoints RESTART IDENTITY;")
st_write(scStreamPoints,conn(),"streampoints")



######## calculate watersheds for each location in db, write to watersheds table
uaaThreshold=.5
snapDistance=100

uaa=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif")                        
flowdir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif")
#scNet=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/scNet.gpkg")
allLocations=st_read(dsn=conn(),query="SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
        FROM locations LEFT JOIN data ON locations.locationid = data.locationid;")

#Silver Creek only
scIDs=st_intersection(allLocations,st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/silverCreekWatershedExtent.gpkg"))$locationid

#whole area
useLocations=allLocations


#test:
#getNearestStream_rast(location=useLocations[1,],uaaRast=uaa,maxDistance=1000,uaaThreshold = uaaThreshold)

##########-----------snap to streams ----------
scStreamPoints=st_read(conn(),query="SELECT * FROM streampoints;")
useLocations$snapped=F

for(i in 1:nrow(useLocations)){
  thisLocation=useLocations[i,]
  if(thisLocation$metric %in% c("flow","Water Temperature", "Dissolved Oxygen", "predicted daily flow")){ #try to snap to stream
    if(thisLocation$locationid %in% scIDs){ #in sliver creek watershed, snap to streampoints
      thisPoint=snapPointsToPoints(thisLocation,scStreamPoints,maxSnapDistance = snapDistance)
    } else { # no streampoints outside of SC watershed, snap to raster instead
      thisPoint=getNearestStream_rast(thisLocation,uaa,snapDistance,uaaThreshold = uaaThreshold,distanceIncriment = 1) #use raster method here.  could use snapPointsToLines vector method instead
    }
    if(!is.null(thisPoint)){ #update useLocations entry with this geometry
      st_geometry(useLocations[i,]) = st_geometry(thisPoint)
      useLocations[i,"snapped"]=T
    }
  }
}

############# ------------- calculate & write watersheds (slow)-----------------
dbExecute(conn(),"TRUNCATE TABLE watersheds RESTART IDENTITY;")


#InitGrass_byRaster(baseRaster=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif"),grassRasterName = "flowDir")

#execGRASS("g.list",parameters=list(type="rast"))
#pointLocation=st_read(conn(),query="SELECT * FROM LOCATIONS LIMIT 1;")

#test case for erroneous tiny watershed
#useLocations=useLocations[useLocations$locationid==55,]

#useLocations=useLocations[useLocations$locationid==137,]

useLocationsList=split(useLocations,useLocations$locationid)

wshedsInfo=pbsapply(useLocationsList,calcWshed)  #calcWshed() writes to db


######## refresh locationattributes

dbExecute(conn(),"REFRESH MATERIALIZED VIEW locationattributes;")
#dbExecute(conn(), "DROP MATERIALIZED VIEW locationattributes")


# dbExecute(conn(), "CREATE MATERIALIZED VIEW locationattributes AS SELECT ROW_NUMBER() OVER(), met.locationid, met.name, met.locationgeometry, met.metrics, wsh.wshedareakm, wsh.watershedgeometry FROM
#                       (SELECT DISTINCT locations.name, locations.locationid, locations.geometry AS locationgeometry, STRING_AGG(DISTINCT(data.metric), ',') AS metrics FROM locations LEFT JOIN data ON locations.locationid = data.locationid GROUP BY locations.locationid) met
#                      LEFT JOIN 
#                       (SELECT locations.locationid, watersheds.geometry AS watershedgeometry, ST_AREA(watersheds.geometry)/1000000 AS wshedareakm FROM locations LEFT JOIN watersheds ON locations.locationid = watersheds.outflowlocationid) wsh
#                       ON met.locationid = wsh.locationid;")


