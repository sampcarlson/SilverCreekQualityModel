source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(pbapply)

flowModel=readRDS(file="~/Dropbox/SilverCreek/flowModel.rds")

dbGetQuery(conn(),"SELECT * FROM locations WHERE locations.name ILIKE '%sportsman%';")
flowIndexLocationID=144  ###### everything is relative to flow at sportsmans

# allFlowData=getFlowIndexData(flowIndexLocationID = flowIndexLocationID, upstreamOfLocationID = 147)
# 
# flow=getFlowByDate(startDate="2000-07-1")
# 

getTemperatureModelParameters=function(locationID,date=as.Date("2021-07-01"),flowModel=readRDS(file="~/Dropbox/SilverCreek/flowModel.rds"),airTempReferenceLocation=180){
  
  #print(paste("l:",locationID,"d:",date))
  
  date=as.Date(date)
  indexFlow=dbGetQuery(conn(),paste0("SELECT AVG(value) AS flow FROM data WHERE locationid = '",flowIndexLocationID,
                                     "' AND datetime::date = '",date,"' AND metric = 'flow';"))$flow
  
  uaa=dbGetQuery(conn(),paste0("SELECT wshedareakm AS uaa FROM locationattributes WHERE locationid = '",locationID,"';"))$uaa
  
  flow=predict(flowModel,newdata=data.frame(indexFlow=indexFlow,uaa=uaa))*indexFlow
  
  
  resid=calcMeanResidence(indexFlow=indexFlow,outflowLocationID = locationID,useResidenceFunction = F)
  
  #air temperature within 50 km
  # airTemp=dbGetQuery(conn(),paste0("SELECT AVG(value) FROM data WHERE data.metric = 'air temperature' AND data.datetime BETWEEN '",date-5,"' AND '",date+1,"'
  #                           AND data.locationid IN 
  #                          (SELECT locationid FROM locations WHERE ST_DWITHIN(locations.geometry, 
  #                          (SELECT locations.geometry FROM locations WHERE locations.locationid = '",locationID,"'),
  #                          50000))")
  # )$avg
  # 
  # ^this is unnecessary as there is only one reliable station nearby (at Picabo)
  
  airTemp=dbGetQuery(conn(),paste0("SELECT AVG(value) FROM data WHERE data.metric = 'air temperature' AND data.datetime BETWEEN '",date-5,"' AND '",date+1,"'
                            AND data.locationid = '",airTempReferenceLocation,"';") )$avg
  
  
  
  return(data.frame(locationid=locationID,date=date,flow=flow,residence=resid,airTemp=airTemp))
}


tempModelBaseLocationID=147
#aggregate by day - no need for multiple models per day
temperatureData=dbGetQuery(conn(),paste0("SELECT AVG(data.value) AS temperature, data.datetime::date AS date, data.locationid FROM data LEFT JOIN locations ON data.locationid = locations.locationid 
           WHERE metric = 'Water Temperature' AND ST_WITHIN(locations.geometry, (SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid = '",tempModelBaseLocationID,"'))
                                         GROUP BY data.datetime::date, data.locationid;")
)

system.time(
  getTemperatureModelParameters(3)
)


temperatureParamDF=do.call(rbind,pbmapply(getTemperatureModelParameters,locationID=temperatureData$locationid,date=temperatureData$date,SIMPLIFY = F))
temperatureParamDF=merge(temperatureParamDF,temperatureData)

#temperatureParamDF$airTempScalar=1-exp(-temperatureParamDF$residence/temperatureParamDF$flow)

write.csv(temperatureParamDF,file=paste0("~/temperatureParamaterSet_",Sys.Date(),".csv"))

m=glm(temperatureParamDF$temperature~temperatureParamDF$airTemp+temperatureParamDF$flow+temperatureParamDF$residence)
summary(m)

summary(nls(temperature~I( airTemp*1-exp( exp_coef*(-residence/flow) ) ),data=temperatureParamDF,start=list(exp_coef=0)))
