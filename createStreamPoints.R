source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

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
scStreamPoints=scStreamPoints[scStreamPoints$uaa>=.1,]


dbSendQuery(conn(),"TRUNCATE TABLE streampoints")
st_write(scStreamPoints,conn(),"streampoints")
