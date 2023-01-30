library(RPostgreSQL)
library(RColorBrewer)
library(sf)

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


