source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

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