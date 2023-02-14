library(rgrass)
library(terra)
library(sf)

InitGrass_byRaster(baseRaster=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeFlowDir_scCarve.tif"),grassRasterName = "flowDir")

#execGRASS("g.list",parameters=list(type="rast"))
#pointLocation=st_read(conn(),query="SELECT * FROM LOCATIONS LIMIT 1;")

calcWshed=function(pointLocation,flowDir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeFlowDir_scCarve.tif")){
  if(!("sf" %in% class(pointLocation))){
    stop("function requires sf point object")
  }
  
  if(!(pointLocation$locationid %in% dbGetQuery(conn(),"SELECT outflowlocationid FROM watersheds;"))){
    
    addRasterIfAbsent(flowDir,"flowDir")
    
    print(paste("Determining watershed for point",pointLocation$name,"with geometry",paste(st_coordinates(pointLocation),collapse=", ")))
    
    #vectors must be SpatVector to be written to grass
    #write_VECT(vect(pointLocation),"wshedPoint",flags=c("overwrite"))
    
    if(!crs(flowDir,proj=T)==st_crs(pointLocation,parameters=T)$proj4string){
      #could automatically transform point to raster crs
      stop("point crs does not match raster crs")
    }
    
    execGRASS("r.water.outlet",flags="overwrite", parameters=list(input="flowDir",output="thisWatershedRast",coordinates=c(st_coordinates(pointLocation)[1,"X"],st_coordinates(pointLocation)[1,"Y"])))
    execGRASS("r.to.vect",flags=c("s","overwrite"),parameters=list(input="thisWatershedRast",output="thisWatershedVect",type="area"))
    
    w=st_as_sf(read_VECT("thisWatershedVect"))
    if(nrow(w)>1){
      w$area=st_area(w)
      w=w[order(w$area,decreasing = T)]
      w=w[1,]
    }
    
    
    w$outflowlocationid=pointLocation$locationid
    watersheds=w[,c('outflowlocationid','geometry')]
    st_write(watersheds,conn(),append = T)
  }
  
}

allLocations=st_read(conn(),query="SELECT * FROM LOCATIONS;")
allLocations=split(allLocations,allLocations$locationid)

sapply(allLocations,calcWshed)

