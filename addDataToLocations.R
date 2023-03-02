source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

allLocations=st_read(dsn=conn(),query="SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
        FROM locations LEFT JOIN data ON locations.locationid = data.locationid;")

allLocations$onSC=F
allLocations$onSC[allLocations$sourcenote=="source_TemperatureLocations.gpkg"]=T
allLocations$onSC[allLocations$sourcenote=="source_DOLocations.gpkg"]=T
allLocations$onSC[allLocations$sourcenote=="source_SWLocations.gpkg"]=T

uaa=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/WholeUAA_ScCarve.tif")                        
flowdir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeFlowDir_scCarve.tif")   
scNet=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/SilverCreekNet_revised.gpkg")

scLocations=snapPointsToLines(allLocations[allLocations$onSC,],scNet, maxSnapDistance = 50)
allLocations=rbind(scLocations,allLocations[!allLocations$onSC,])


allLocations$uaa=extract(uaa,vect(allLocations))[,2]

dbAppendTable(conn(),"locationinfo",data.frame(locationid=allLocations$locationid,metric="uaa",value=allLocations$uaa))
