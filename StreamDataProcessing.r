source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(parallel)
library(pbapply)
dem=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeDem.tif")
streamline=st_zm(st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/nhdFlowline_simplify.gpkg"))
scWshed=st_zm(st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/silverCreekWatershedExtent.gpkg"))
useFlowSink=T


streamline=st_transform(streamline,crs=st_crs(dem))

############# carve channels into dem to concentrate flow -------------
streamCells=extract(dem,vect(streamline),cells=T)
dem[streamCells$cell]=dem[streamCells$cell]-10

#use flowSink polygons to define 'the optional map of actual depressions or sinkholes in the landscape that are large enough to slow and store surface runoff from a storm event.'
#All cells that are not NULL and not zero indicate depressions. Water will flow into but not out of depressions. 
if(useFlowSink){
  flowSink=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowSink.gpkg")
  flowSink=st_zm(flowSink)
  flowSinkRaster=dem*0
  
  addSinkCells=extract(flowSinkRaster,vect(flowSink),cells=T)
  flowSinkRaster[addSinkCells$cell]=1
  
  demSinkCells=extract(dem,vect(flowSink),cells=T)
  dem[demSinkCells$cell]=dem[demSinkCells$cell]-100
  
}
writeRaster(dem,"~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeDem_carve.tif",overwrite=T)



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



######## segment streamlines within sc watershed--------

#lots of io time to do this in grass, but it works...
scStreamline=st_intersection(streamline,scWshed)
write_VECT(vect(scStreamline),"streamline",flags="overwrite")
execGRASS("v.split", input="streamline",output="streamSegs",length=50,units="meters",flags="overwrite")
streamSegs=st_zm(st_as_sf(read_VECT("streamSegs")))

#drop extra info from nhd, keep one id in cas it is useful in the future:
streamSegs$segid=1:nrow(streamSegs)
streamSegs=streamSegs[,c("segid","NHDPlusID")]
names(streamSegs)[2]="nhdplusid"

###################### streamSegs characteristics
streamSegs$length=st_length(streamSegs)

uaa=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif")
#uaa assignment is tricky due to imperfect alignment between raster uaa values and stream segs.
#median does ok for most, bus for some segs a majority of the overlapped cells 'miss' the channel (and hterefore have low uaa values...)
#max avoids this^ issue, but assigns mainstem values to tributairys at junctions
# use second highest value
secondHighest=function(x){
  x=x[order(x,decreasing=T)]
  return(x[2])
}

streamSegs$uaa=extract(uaa, vect(streamSegs),fun=secondHighest)[,2]

streamSegs$slope=(extract(dem, vect(streamSegs),fun=max)[,2]-extract(dem, vect(streamSegs),fun=min)[,2])/as.numeric(streamSegs$length)

streamSegs$elevation=extract(dem, vect(streamSegs),fun=min)[,2]
head(streamSegs)

#write to db
dbExecute(conn(), "DROP MATERIALIZED VIEW usds;")
dbExecute(conn(),"TRUNCATE TABLE streamsegments RESTART IDENTITY;")
st_write(streamSegs,conn(),"streamsegments")

#identify upstream-downstream relationships

dbExecute(conn(), "CREATE MATERIALIZED VIEW usds AS SELECT DISTINCT ON (us.segid) us.segid AS us_segid, us.uaa AS us_uaa, ds.segid AS ds_segid, ds.uaa AS ds_uaa FROM streamsegments us 
          LEFT JOIN streamsegments ds ON ST_INTERSECTS(ds.geometry, us.geometry) AND ds.segid != us.segid AND ds.uaa > us.uaa 
           ORDER BY us.segid, ds.uaa DESC;")



# 
# flowDir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif")
# #crs(flowDir)
# write_RAST(flowDir,"flowDir")
# 
# shortSegs=streamSegs[1:4,]
# 
# getSegWatershed=function(thisSegGeom,thisSegUaa){
#   
#   allSegCells=extract(uaa, vect(thisSeg),list=F,cells=T,xy=T)
#   wshedSegCell=allSegCells[allSegCells$uaa_cells_sink==thisSeg$uaa,]
#   execGRASS("r.water.outlet", input="flowDir",output="thisSegWshedRast",coordinates=c(wshedSegCell$x,wshedSegCell$y),flags="overwrite")
#   execGRASS("r.to.vect",flags=c("s","overwrite","quiet"),parameters=list(input="thisSegWshedRast",output="thisSegWshedVect",type="area"))
#   # execGRASS("v.db.addcolumn",map="thisSegWshedVect", columns="segID INT")
#   # execGRASS("v.db.update", map="thisSegWshedVect", column="segID",value=as.character(thisSegID))
#   # execGRASS("v.dissolve",input="thisSegWshedVect",column="segID",output="singleSegWshed",flags=c("overwrite","quiet"))
#   
#   thisSegWshed=st_as_sf(read_VECT("thisSegWshedVect"))
#   #stupid, but necessary to stick diagonally joined areas together
#   thisSegWshed=st_buffer(thisSegWshed,dist=1)
#   #thisSegWshed=st_boundary(thisSegWshed)
#   thisSegWshed=st_make_valid(st_union(thisSegWshed))
#   return(st_as_sf(thisSegWshed))
# }
# 
# 
# #sw=getSegWatershed(shortSegs$geometry[1],shortSegs$uaa[1])
# segWatersheds=pbmapply(FUN=getSegWatershed,thisSegGeom=st_geometry(streamSegs),thisSegUaa=streamSegs$uaa)
# segWatersheds=st_as_sfc(do.call(rbind,segWatersheds))
# streamSegs$wshedgeom=segWatersheds






######## calculate watersheds for each location in db, write to watersheds table ----------
uaaThreshold=.5
snapDistance=100


flowdir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif")
#scNet=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/scNet.gpkg")
allLocations=st_read(dsn=conn(),query="SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
        FROM locations LEFT JOIN data ON locations.locationid = data.locationid;")

#Silver Creek only
scIDs=st_intersection(allLocations,scWshed)$locationid

#whole area
useLocations=allLocations


#test:
#getNearestStream_rast(location=useLocations[1,],uaaRast=uaa,maxDistance=1000,uaaThreshold = uaaThreshold)

##########-----------snap to streams ----------

scStreamPoints=st_line_sample(st_cast(st_zm(scStreamline),"LINESTRING"),density=.5)
scStreamPoints=st_cast(scStreamPoints,"POINT")

useLocations$snapped=F

for(i in 1:nrow(useLocations)){
  thisLocation=useLocations[i,]
  thisLocationMetrics=useLocations$metric[useLocations$locationid == thisLocation$locationid]
  if( any(thisLocationMetrics %in% c("flow","Water Temperature", "Dissolved Oxygen", "predicted daily flow"))){ #try to snap to stream
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

