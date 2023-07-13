#predict temperatures across silver creek
source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

####  temperature and flow prediction process:

temperatureModel=readRDS("temperatureModel.rds")
summary(temperatureModel)

airTempLocation=180
streamSegs=st_read(conn(),query="SELECT segid, geometry FROM streamsegments;")
indexFlow=10
forecastDate="2012-07-24"

getAirTempDistribution=function(forecastDate){
  forecastDate=as.Date(forecastDate)
  forecastDOY=as.numeric(format.Date(forecastDate, "%j"))
  
  forecastAirTemps=dbGetQuery(conn(),paste0("SELECT AVG(value) AS daymean FROM data WHERE locationid = '",airTempReferenceLocation,
                                            "' AND metric = 'air temperature' AND EXTRACT(doy FROM datetime) IN ('",paste(seq(from=forecastDOY-3,to=forecastDOY+3),collapse="', '"),
                                            "') GROUP BY datetime::date;"))$daymean
  
  return(forecastAirTemps)
}

predictSegTemperatures=function(indexFlow, meanAirTemp_F, forecastDate, streamSegs, tempModel=temperatureModel,airTempReferenceLocation=airTempLocation){
  
  meanAirTemp = 5/9*(meanAirTemp_F - 32)
  
  forecastDate=as.Date(forecastDate)
  forecastDOY=as.numeric(format.Date(forecastDate, "%j"))
  
  segTemperatures=dbGetQuery(conn(),paste0("SELECT meanresidencetoseg AS residence, flow, segid FROM residences WHERE indexflow = '",indexFlow,"';"))
  
  segTemperatures=merge(streamSegs, segTemperatures, by="segid")
  segTemperatures$maxSunElevation=maxSunAngleFun(forecastDOY)
  segTemperatures$meanAirTemp=meanAirTemp
  
  segTemperatures$temperature_F=  ( predict(tempModel,newdata=segTemperatures) * (9/5) ) + 32 
  return(segTemperatures)
  
}


segTemperatures=predictSegTemperatures(indexFlow=50,meanAirTemp_F = 70, forecastDate = "2021-07-05",streamSegs=streamSegs,tempModel = temperatureModel)
mean(segTemperatures$temperature_F,na.rm=T)

hist(segTemperatures$temperature_F)
#hist(segTemperatures$flow)
#hist(segTemperatures$residence)
sum(segTemperatures$temperature_F>70,na.rm=T)

segTemperatures[segTemperatures$segid %in% c(6:10,370),]

topSegs=segTemperatures[segTemperatures$segid %in% c(6:10),]
sum(topSegs$flow*49.53170/max(topSegs$flow))

r=0
m=max(topSegs$flow)
for(i in 1:nrow(topSegs)){
  r=r+(topSegs$flow[i]*49.53170)
}
r/m
