source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

allLocations=st_read(dsn=conn(),query="SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
        FROM locations LEFT JOIN data ON locations.locationid = data.locationid;")

allLocations$onSC=F
allLocations$onSC[allLocations$sourcenote=="source_TemperatureLocations.gpkg"]=T
allLocations$onSC[allLocations$sourcenote=="source_DOLocations.gpkg"]=T
allLocations$onSC[allLocations$sourcenote=="source_SWLocations.gpkg"]=T

uaa=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif")                        
flowdir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/flowDir_carve_sink.tif")   
scNet=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/SilverCreekNet_revised.gpkg")

scLocations=snapPointsToLines(allLocations[allLocations$onSC,],scNet, maxSnapDistance = 50)
allLocations=rbind(scLocations,allLocations[!allLocations$onSC,])


allLocations$uaa=extract(uaa,vect(allLocations))[,2]

#dbAppendTable(conn(),"locationinfo",data.frame(locationid=allLocations$locationid,metric="uaa",value=allLocations$uaa))


###################### create points along all flowlines, add uaa and elevation, write to db
#this process fails to create some points for unknown reason...

allStreams=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/nhdFlowline_simplify.gpkg")
scArea=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/silverCreekWatershedExtent.gpkg")
scStreams=st_intersection(st_zm(allStreams),scArea)



scStreamPoints=extract(rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/uaa_km_sink.tif"),
               vect(scStreams),
               touches=T,xy=T)




scStreamPoints=scStreamPoints[,c("uaa_cells_sink","x","y")]
names(scStreamPoints)[1]="uaa"
scStreamPoints=st_as_sf(scStreamPoints,coords = c("x","y"),crs=st_crs(scStreams))

######################limit points to those w/ >=,1 km uaa
scStreamPoints=scStreamPoints[scStreamPoints$uaa>=.1,]


dbSendQuery(conn(),"TRUNCATE TABLE streampoints")
st_write(scStreamPoints,conn(),"streampoints")
