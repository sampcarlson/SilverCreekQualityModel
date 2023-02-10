#process for identifying watershed areas associated with each location
source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

uaaThreshold=100000
snapDistance=50

uaa=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/WholeUAA_ScCarve.tif")                        
flowdir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeFlowDir_scCarve.tif")   
allLocations=st_read(dsn=conn(),query="SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
        FROM locations LEFT JOIN data ON locations.locationid = data.locationid;")



  #test:
getNearestStream_rast(location=allLocations[1,],uaaRast=uaa,maxDistance=1000,uaaThreshold = uaaThreshold)

allLocations$snapped=F
for(i in 1:nrow(allLocations)){
  thisLocation=allLocations[i,]
  if(thisLocation$metric %in% c("flow","Water Temperature", "Dissolved Oxygen", "predicted daily flow")){ #try to snap to stream
    thisPoint=getNearestStream_rast(thisLocation,uaa,snapDistance,uaaThreshold = uaaThreshold,distanceIncriment = 1)
    if(!is.null(thisPoint)){ #update allLocations entry with this geometry
      st_geometry(allLocations[i,]) = st_geometry(thisPoint)
      allLocations[i,"snapped"]=T
    }
  }
}
