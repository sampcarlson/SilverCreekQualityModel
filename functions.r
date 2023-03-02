library(RPostgreSQL)
library(RColorBrewer)
library(rgrass)
library(sf)
library(terra)

getColor=function(reps){
  allColors=brewer.pal(12,"Set3")
  reps=reps %% length(allColors)
  return(allColors[reps])
  
}

conn=function(){    #checks for db connection object ('scdb') in global env, creates if necessary, returns and places in global enc
  
  scdbConnect=function(){
    scdb=dbConnect(RPostgres::Postgres(),
                   host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                   port="25060",
                   dbname="silvercreekdb" ,
                   user="dbread",
                   password="dbread")
    return(scdb)
  }
  
  if(!exists("scdb",where=globalenv())){      #scdb does not exist - create it
    scdb=scdbConnect()
    assign("scdb",scdb,envir=globalenv())     #assign to global environment for future use
  } else {                                    #scdb exists, get it and test
    scdb=get("scdb",envir=globalenv())
    if(!dbIsValid(scdb)){                     #scdb not valid, recreate
      scdb=scdbConnect()
      assign("scdb",scdb,envir=globalenv())
    }
  }
  return(get("scdb",envir=globalenv()))
}


listDataSeries=function(){
  #allSeries=dbGetQuery(conn(),"SELECT DISTINCT locationid, metricid FROM data;")
  allSeries=dbGetQuery(conn(),"SELECT DISTINCT metricid, locationid, MIN(datetime) ,MAX(datetime), COUNT(datetime) FROM data GROUP BY metricid, locationid;")
  allSeries=merge(allSeries, dbGetQuery(conn(),"SELECT metricid, name FROM metrics;"))
  allSeries$seriesid=1:nrow(allSeries)
  allSeries=allSeries[,c("seriesid","locationid","metricid","name","count","min","max")]
  names(allSeries)[4]="metric"
  names(allSeries)[6]="startdate"
  names(allSeries)[7]="enddate"
  return(allSeries)
}  

getSeriesData=function(seriesids,dataSeriesList=dataSeries){
  thisSeries=dataSeriesList[dataSeriesList$seriesid %in% seriesids,]
  seriesData=dbGetQuery(conn(),paste0("SELECT * FROM data WHERE 
                             locationid IN ('",paste0(thisSeries$locationid,collapse="', '"),"' ) 
                             AND metricid  IN ('",paste0(thisSeries$metricid,collapse="', '"),"' ) 
                             ORDER BY datetime;")
  )
  seriesData=merge(thisSeries,seriesData,by=c("locationid","metricid","metric"))
  return(seriesData)
}


makePlot=function(plotData){
  
  plotData=plotData[complete.cases(plotData[,c("metric","value","datetime")]),]
  
  xmin=min(plotData$datetime)
  xmax=max(plotData$datetime)
  metrics=unique(plotData$metric)
  #set up plot area:
  firstLine=plotData[plotData$metric==metrics[1],]
  firstLine=firstLine[firstLine$locationid==firstLine$locationid[1],]
  
  leftMargin=4*length(metrics)
  bottomMargin=3+length(metrics)
  par(mar=c(bottomMargin,leftMargin,2,0))
  
  plot(firstLine$value~firstLine$datetime,type="n",xlim=c(xmin,xmax),yaxt="n",ylab="",xlab="")
  
  metricLty=1:length(metrics)
  
  axisLine=0
  
  for(metric in metrics){
    thisPlotData=plotData[plotData$metric==metric,]
    
    
    par(new=T)
    plot(firstLine$value~firstLine$datetime,type="n",xlim=c(xmin,xmax),ylim=c(min(thisPlotData$value),max(thisPlotData$value)),axes=F,ylab="",xlab="")
    
    rep=1
    for(location in unique(thisPlotData$locationid)){
      thisLineData=thisPlotData[thisPlotData$locationid==location,]
      thisLineData=thisLineData[order(thisLineData$datetime),]
      lines(thisLineData$value~thisLineData$datetime,col=getColor(rep),lty=metricLty[metrics==metric],lwd=2)
      rep=rep+1
    }
    
    axis(side=2,line=axisLine)
    mtext(text=metric, side=2, line=axisLine+2,font=2)
    axisLine=axisLine+4
    
  }
  par(xpd=T)
  par(new=T)
  par(mar=c(.1,2,2,2))
  plot.new()
  
  legend(x="bottom",legend=rev(metrics),lty=rev(metricLty),lwd=2,ncol=2,bty="n")
  
  par(xpd=par()$xpd)
  par(mar=par()$mar)
  par(oma=par()$oma)
}


getInWatershed=function(watershedID=NULL,outflowLocationID=NULL,metricNames=NULL,returnData=F){
  metricString=""
  if(!is.null(metricNames)){
    metricString = paste0(" AND metric IN ('",paste(metricNames,collapse="', '"),"')")
  }
  
  if(!is.null(outflowLocationID)){
    watershedid=dbGetQuery(conn(),paste0("SELECT watershedid FROM watersheds WHERE outflowlocationid = '",outflowLocationID,"';"))$watershedid
  }
  
  locations=dbGetQuery(conn(),paste0("SELECT DISTINCT locations.locationid, name, geometry, sourcenote, sitenote, source_site_id, metric, metricid 
                                     FROM locations LEFT JOIN data ON locations.locationid = data.locationid 
                                     WHERE ST_WITHIN(locations.geometry, (SELECT geom FROM watersheds WHERE watershedid = '",watershedid,"'))",metricString,";"))
  
  
  
  if(returnData){
    locationData=dbGetQuery(conn(),paste0("SELECT * FROM data WHERE data.locationid IN ('",paste(locations$locationid,collapse="', '"),"')",metricString,";"))
    locations=merge(locationData,locations)
  }
  
  return(locations)
}


calcMeanResByLocation=function(locationID,residenceColName=NULL,summary=T){
  wholeStream=dbGetQuery(conn(),paste0("SELECT * FROM scstreampoints WHERE 
                                       ST_WITHIN(scstreampoints.geometry,
                                       (SELECT geometry FROM watersheds WHERE outflowlocationid = '",locationID,"'));"))
  if(!is.null(residenceColName)){
    wholeStream$residence=wholeStream[,residenceColName]
  } else {
    wholeStream$residence=1
  }
  
  wholeStream$flow=wholeStream$uaa
  
  #flow-weighted mean residence time = sum (for all segments s){ (residenceTime_s * Qs) / max(Qs)}.  
  #note that max(Qs) is just outflow Q
  maxFlow=max(wholeStream$flow)
  meanRes=0
  for(i in 1:nrow(wholeStream)){
    meanRes=meanRes+wholeStream$residence[i]*wholeStream$flow[i]/maxFlow
  }
  return(meanRes)
}

snapPointsToLines=function(points_to_snap,target_lines,maxSnapDistance=100){
  #First define points along lines to snap to...
  #force(target_lines)
  snapPoints=st_line_sample(st_cast(st_zm(target_lines),"LINESTRING"),density=1)#'density' snap points per meter
  
  #snap array pts to pts along network lines:
  #st_snap to the line directly doesn't work, st_snap to all sampled points also fails.  
  
  for(i in 1:nrow(points_to_snap)){ #iterate through points
    near=st_nearest_points(points_to_snap[i,],snapPoints)  #find line to all snapPoints
    thisLocation=st_geometry( 
      st_cast(near[which.min(st_length(near))],"POINT")[2] #find geometry of nearest snap point. [2] returns 'to' point of st_nearest line
    )  
    
    if(as.numeric(st_distance(points_to_snap[i,],thisLocation))<maxSnapDistance){
      print(paste("Point",points_to_snap$name[i],"snapped to target line at distance of", round(st_distance(points_to_snap[i,],thisLocation),1), "meters"))
      st_geometry( points_to_snap[i,] ) = st_geometry( thisLocation )
    }  else{
      print(paste("Point",points_to_snap$name[i],"not snapped due to excessive distance of", round(st_distance(points_to_snap[i,],thisLocation),1), "meters"))
    }
  }
  return(points_to_snap)
}


getNearestStream_rast=function(location,uaaRast,maxDistance,uaaThreshold,distanceIncriment=NULL){
  if(is.null(distanceIncriment)){
    distanceIncriment=as.numeric(mean(res(uaaRast)))
  }
  getStr_worker=function(location,uaaRast,distance){
    locationArea=st_buffer(location,dist=distance)
    areaInfo=extract(uaaRast,vect(locationArea),cells=T)
    names(areaInfo)=c("ID","uaa","cell")
    return(areaInfo)
  }
  done=F
  distance=distanceIncriment*1
  maxCoords=NULL
  while(done==F){
    areaInfo=getStr_worker(location,uaaRast,distance)
    #print(paste("distance:",distance))
    #print(paste("Max UAA:",round(max(areaInfo$uaa))))
    if(max(areaInfo$uaa)>uaaThreshold){ #found a pixel w/ sufficient uaa
      maxCell=areaInfo[which.max(areaInfo$uaa),]$cell
      maxCoords=xyFromCell(uaa,maxCell)
      done=T
    }
    if(distance >= maxDistance){
      done=T
    }
    distance = distance + distanceIncriment
  }
  #create point geometry, transform from raster crs to location CRS
  if(!is.null(maxCoords)){
    print(maxCoords)
    #maxCoords=as.matrixmaxCoords)
    #print(maxCoords)
    thisPoint=st_point(maxCoords)
    
    thisPoint=st_sfc(thisPoint,crs=st_crs(uaaRast))
    thisPoint=st_transform(thisPoint,crs=st_crs(location))
    maxCoords=thisPoint
  }
  return(maxCoords)
}



grassTableToDF=function(grassDF){
  return (read.table(text=grassDF,header=T,sep="|",stringsAsFactors = F))
}

addRasterIfAbsent=function(addRaster,grassRasterName){
  if(!grassRasterName %in% stringexecGRASS("g.list type=raster",intern = T)) {
    write_RAST(addRaster,grassRasterName)
    print(paste0("raster '", grassRasterName, "' added to GRASS"))
  }
}

#initialize a re-useable workspace based on a raster.  requires terra, rgrass
InitGrass_byRaster=function(baseRaster,grassRasterName,grassPath){
  
  ##these are necessary to fully trash a pervious grass session
  unlink_.gislock()
  remove_GISRC()
  
  
  
  #initialize grass.  SG does not do what I want it to do
  initGRASS(override=TRUE,mapset="PERMANENT",remove_GISRC = T, SG=baseRaster)
  #note that has no info (yet) about projection, extent, or resolution
  
  #Use the projection of the raster - save projection definition in a format GRASS likes
  p=crs(baseRaster,proj=T)
  #set projection from RGDAL's interpetation of raster proj4string
  execGRASS("g.proj",flags="c",parameters = list(proj4=p))
  
  #actually write the raster to GRASS
  write_RAST(baseRaster,vname=grassRasterName)
  
  #set the resolution
  execGRASS("g.region",raster=grassRasterName)
  
  #see the region as lat-long or xy
  execGRASS("g.region", flags='l')
  execGRASS("g.region", flags='p')
  
  stringexecGRASS("g.gisenv")
  stringexecGRASS("g.proj -p")
}

calcWshed=function(pointLocation,flowDir=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/wholeFlowDir_scCarve.tif"),streamVect=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/SilverCreekNet_revised.gpkg")){
  if(!("sf" %in% class(pointLocation))){
    stop("function requires sf point object")
  }
  
  if(!(pointLocation$locationid %in% dbGetQuery(conn(),"SELECT outflowlocationid FROM watersheds;")$outflowlocationid)){
    
    addRasterIfAbsent(flowDir,"flowDir")
    
    pointLocation=pointLocation[1,]
    
    print(paste("Determining watershed for point",pointLocation$name,"with geometry",paste(st_coordinates(pointLocation),collapse=", ")))
    
    #vectors must be SpatVector to be written to grass
    #write_VECT(vect(pointLocation),"wshedPoint",flags=c("overwrite"))
    
    if(!crs(flowDir,proj=T)==st_crs(pointLocation,parameters=T)$proj4string){
      #could automatically transform point to raster crs
      stop("point crs does not match raster crs")
    }
    
    execGRASS("r.water.outlet",flags=c("overwrite","quiet"), parameters=list(input="flowDir",output="thisWatershedRast",coordinates=c(st_coordinates(pointLocation)[1,"X"],st_coordinates(pointLocation)[1,"Y"])))
    execGRASS("r.to.vect",flags=c("s","overwrite","quiet"),parameters=list(input="thisWatershedRast",output="thisWatershedVect",type="area"))
    
    w=st_as_sf(read_VECT("thisWatershedVect"))
    w$area=st_area(w$geometry)
    if(nrow(w)>1){
      w=w[order(w$area,decreasing = T),]
      w=w[1,]
    }
    
    
    print(st_area(w$geometry))
    if( as.numeric(st_area(w))<1000*res(flowDir)[1]*res(flowDir)[2] ) { #tiny watershed, try again
      
      if(!is.null(streamVect)){  #snap to stream vector
        newCoords=snapPointsToLines(pointLocation,streamVect,maxSnapDistance = 100)
      } else {
        uaaRast=rast("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/WholeUAA_ScCarve.tif")
        thisUAA=extract(uaaRast,vect(pointLocation))[1,2]
        newCoords=getNearestStream_rast(location=pointLocation,uaaRast=uaaRast,maxDistance=res(flowDir)[1]*10,uaaThreshold=thisUAA+1000)
      }
      
      execGRASS("r.water.outlet",flags=c("overwrite","quiet"), parameters=list(input="flowDir",output="thisWatershedRast",coordinates=c(st_coordinates(newCoords)[1,"X"],st_coordinates(newCoords)[1,"Y"])))
      execGRASS("r.to.vect",flags=c("s","overwrite","quiet"),parameters=list(input="thisWatershedRast",output="thisWatershedVect",type="area"))
      
      w=st_as_sf(read_VECT("thisWatershedVect"))
      w$area=st_area(w$geometry)
      if(nrow(w)>1){
        w=w[order(w$area,decreasing = T),]
        w=w[1,]
      } 
      print(st_area(w$geometry))
      
      
      
      
    }
    
    w$outflowlocationid=pointLocation$locationid
    watersheds=w[,c('outflowlocationid','geometry')]
    st_write(watersheds,conn(),append = T)
    #return(w)
  }
}



