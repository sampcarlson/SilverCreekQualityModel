library(RPostgreSQL)



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


plotDataSeries=function(seriesData){  #currently limited to 1 metric
  if(length(unique(seriesData$metricid))>1){
    print(paste0("Plotting only ",seriesData$metric[seriesData$metricid==unique(seriesData$metricid)[1]][1],", ignoring all other metrics"))
  }
  seriesData=seriesData[seriesData$metricid == unique(seriesData$metricid)[1],]
  plot(seriesData$value~seriesData$datetime,type="n")
  colorScheme=data.frame(seriesid=unique(seriesData$seriesid),color=sample(colors(),length(unique(seriesData$seriesid))))
  for(i in 1:length(unique(seriesData$seriesid))){}
  
}
