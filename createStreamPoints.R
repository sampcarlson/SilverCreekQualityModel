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

#add elevation

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



dbSendQuery(conn(),"TRUNCATE TABLE streampoints")
st_write(scStreamPoints,conn(),"streampoints")
