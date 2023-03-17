#process for identifying watershed areas associated with each location
source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

uaaThreshold=1
snapDistance=100

uaa=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif")                        
flowdir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif")
scNet=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/scNet.gpkg")
allLocations=st_read(dsn=conn(),query="SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
        FROM locations LEFT JOIN data ON locations.locationid = data.locationid;")

#Silver Creek only
#scLocations=st_intersection(allLocations,st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/silverCreekWatershedExtent.gpkg"))

#whole area
scLocations=allLocations
  #test:
#getNearestStream_rast(location=scLocations[1,],uaaRast=uaa,maxDistance=1000,uaaThreshold = uaaThreshold)

scLocations$snapped=F
for(i in 1:nrow(scLocations)){
  thisLocation=scLocations[i,]
  if(thisLocation$metric %in% c("flow","Water Temperature", "Dissolved Oxygen", "predicted daily flow")){ #try to snap to stream
    thisPoint=getNearestStream_rast(thisLocation,uaa,snapDistance,uaaThreshold = uaaThreshold,distanceIncriment = 1) #use raster method here.  could use snapPointsToLines vector method instead
    if(!is.null(thisPoint)){ #update scLocations entry with this geometry
      st_geometry(scLocations[i,]) = st_geometry(thisPoint)
      scLocations[i,"snapped"]=T
    }
  }
}


#dbSendQuery(conn(),"TRUNCATE TABLE watersheds RESTART IDENTITY;")


InitGrass_byRaster(baseRaster=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif"),grassRasterName = "flowDir")

#execGRASS("g.list",parameters=list(type="rast"))
#pointLocation=st_read(conn(),query="SELECT * FROM LOCATIONS LIMIT 1;")

#test case for erronious tiny watershed
#scLocations=scLocations[scLocations$locationid==55,]

#scLocations=scLocations[scLocations$locationid==137,]

scLocationsList=split(scLocations,scLocations$locationid)

wshedsInfo=sapply(scLocationsList,calcWshed)


#dbSendQuery(conn(),"DELETE FROM watersheds WHERE outflowlocationid = 60;")

