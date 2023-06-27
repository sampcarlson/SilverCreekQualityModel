source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(DBI)
library(RPostgreSQL)
library(pbapply)

flowModel=readRDS(file="~/Dropbox/SilverCreek/flowModel.rds")

dbGetQuery(conn(),"SELECT * FROM locations WHERE locations.name ILIKE '%sportsman%';")
flowIndexLocationID=144  ###### everything is relative to flow at sportsmans

# allFlowData=getFlowIndexData(flowIndexLocationID = flowIndexLocationID, upstreamOfLocationID = 147)
# 
# flow=getFlowByDate(startDate="2000-07-1")
# 

#allTempData=dbGetQuery(conn(),"SELECT * FROM data WHERE metric = 'Water Temperature' AND qcstatus='true';")

#hist(allTempData$value)

#dev.new()
#plot(allTempData$value[allTempData$qcstatus]~allTempData$datetime[allTempData$qcstatus])
#points(allTempData$value[!allTempData$qcstatus]~allTempData$datetime[!allTempData$qcstatus],col="red")



getTemperatureModelParameters=function(locationID,date,flowModel=readRDS(file="~/Dropbox/SilverCreek/flowModel.rds"),airTempReferenceLocation=180,airTempPeriodDays=1){
  
  #print(paste("l:",locationID,"d:",date))
  
  date=as.Date(date)
  
  indexFlow=dbGetQuery(conn(),paste0("SELECT AVG(value) AS flow FROM data WHERE locationid = '",flowIndexLocationID,
                                     "' AND datetime::date = '",date,"' AND metric = 'flow';"))$flow
  
  uaa=dbGetQuery(conn(),paste0("SELECT wshedareakm AS uaa FROM locationattributes WHERE locationid = '",locationID,"';"))$uaa
  
  flow=predict(flowModel,newdata=data.frame(indexFlow=indexFlow,uaa=uaa))*indexFlow
  
  
  resid=calcMeanResidence(ID=locationID,indexFlow=indexFlow,isSegID=F,useResidenceFunction = F)
  
  #air temperature within 50 km
  # airTemp=dbGetQuery(conn(),paste0("SELECT AVG(value) FROM data WHERE data.metric = 'air temperature' AND data.datetime BETWEEN '",date-5,"' AND '",date+1,"'
  #                           AND data.locationid IN 
  #                          (SELECT locationid FROM locations WHERE ST_DWITHIN(locations.geometry, 
  #                          (SELECT locations.geometry FROM locations WHERE locations.locationid = '",locationID,"'),
  #                          50000))")
  # )$avg
  # 
  # ^this is unnecessary as there is only one reliable station nearby (at Picabo)
  #get 5 day window past air temp at picabo
  airTemps=dbGetQuery(conn(),paste0("SELECT MAX(value), AVG(value) FROM data WHERE data.metric = 'air temperature' AND data.datetime::date > '",date-airTempPeriodDays,"' AND data.datetime::date <='",date,"'
                            AND data.locationid = '",airTempReferenceLocation,"';") )
  fToC=function(f){
    c = 5/9*(f - 32)
    return(c)
  }
  
  maxAirTemp=fToC(airTemps$max)
  meanAirTemp=fToC(airTemps$avg)
  
  return(data.frame(locationid=locationID,date=date,flow=flow,residence=resid,meanAirTemp=meanAirTemp,maxAirTemp=maxAirTemp))
}


tempModelBaseLocationID=148
dbGetQuery(conn(), "SELECT locationid, name, metrics, wshedareakm FROM locationattributes WHERE locationid = '148';")
#aggregate by day - no need for multiple models per day


temperatureData=dbGetQuery(conn(),paste0("SELECT AVG(data.value) AS meantemp, MAX(data.value) as maxTemp, data.datetime::date AS date, data.locationid FROM data LEFT JOIN locations ON data.locationid = locations.locationid 
           WHERE data.qcstatus='true' AND metric = 'Water Temperature' AND ST_WITHIN(locations.geometry, (SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid = '",tempModelBaseLocationID,"'))
                                         GROUP BY data.datetime::date, data.locationid;")
)

# exclude well location - though it may be interesting for comparison...
dbGetQuery(conn(),"SELECT * FROM locations WHERE locationid = 63;")
temperatureData=temperatureData[!temperatureData$locationid == 63,]

#time per location*date
system.time(
  getTemperatureModelParameters(3,"2021-07-01")
)

inSeason=function(date){
  month=as.numeric(format.Date(date,"%m"))
  return(month>=6 & month <=9)
}

temperatureData$inSeason=inSeason(temperatureData$date)
temperatureData=temperatureData[temperatureData$inSeason,]

#temperatureParamDF=do.call(rbind,pbmapply(getTemperatureModelParameters,locationID=temperatureData$locationid,date=temperatureData$date,SIMPLIFY = F))
#temperatureParamDF=merge(temperatureParamDF,temperatureData)
#temperatureParamDF$doy=as.numeric(format.Date(temperatureParamDF$date,"%j"))

#write.csv(temperatureParamDF,file=paste0(getwd(),"/temperatureParamaterSet_",Sys.Date(),".csv"))
temperatureParamDF=read.csv(paste0(getwd(),"/temperatureParamaterSet_2023-06-15.csv"))

modelLocations=st_read(conn(),query=paste0("SELECT locationid, name, wshedareakm, metrics, locationgeometry FROM locationattributes WHERE locationid IN ('",
                                           paste(unique(temperatureParamDF$locationid), collapse="', '"),"');"))
temperatureParamDF=merge(temperatureParamDF,data.frame(locationid=modelLocations$locationid,wshedArea=modelLocations$wshedareakm))
temperatureParamDF$date=as.Date(temperatureParamDF$date)

maxSunAngleFun=function(doy){
 43+23.45*sin((2*pi/360)*(360/365)*(doy-81))
}

temperatureParamDF$maxSunElevation=maxSunAngleFun(temperatureParamDF$doy)

#exclude 3 sites immediately downstream of ponds:
dbGetQuery(conn(),"SELECT * FROM locations WHERE locationid IN ('8', '43', '37');")
temperatureParamDF=temperatureParamDF[!temperatureParamDF$locationid %in% c(8,43,37),]

plot(temperatureParamDF$meantemp~temperatureParamDF$date)

plot(temperatureParamDF$meantemp~temperatureParamDF$doy)
plot(temperatureParamDF$maxtemp~temperatureParamDF$doy)
plot(temperatureParamDF$meantemp~temperatureParamDF$maxSunElevation)

plot(temperatureParamDF$meantemp~temperatureParamDF$meanAirTemp)
plot(temperatureParamDF$meantemp[temperatureParamDF$date==as.Date("2020-07-15")]~temperatureParamDF$residence[temperatureParamDF$date==as.Date("2020-07-15")])

hist(temperatureParamDF$meantemp/temperatureParamDF$meanAirTemp)
plot(temperatureParamDF$meantemp/temperatureParamDF$meanAirTemp~temperatureParamDF$flow)
plot(temperatureParamDF$meantemp/temperatureParamDF$meanAirTemp~temperatureParamDF$residence)



# pdf(file="allPlot.pdf",width=18,height=14)
# plot(temperatureParamDF[,c("meantemp","maxtemp","flow","residence","meanAirTemp","maxAirTemp","doy","maxSunElevation")])
# dev.off()


m=lm(temperatureParamDF$meantemp~temperatureParamDF$meanAirTemp+temperatureParamDF$maxAirTemp+temperatureParamDF$flow*temperatureParamDF$residence+
       temperatureParamDF$doy+temperatureParamDF$wshedArea+temperatureParamDF$maxSunElevation)
summary(m)
sigma(m)

# m=nls(meantemp~I(intercept + meanAirTemp*(1-exp( -exp_coef*residence/flow )) ),data=temperatureParamDF,start=list(intercept=10,exp_coef=0.08),control=list(maxiter=500))
# summary(m)

m=nls(meantemp~I(intercept+springTemp*(exp(-expCoef*residence/flow )) + atmCoef*meanAirTemp*(1-exp(-expCoef*residence/flow )) ),data=temperatureParamDF,start=list(intercept=0,springTemp=10,atmCoef=1,expCoef=.008),control=list(maxiter=500, minFactor=1e-6))
summary(m)

m=nls(meantemp~I(intercept+sunCoef*maxSunElevation+residCoef*residence+springTemp*(exp(-expCoef*residence/flow )) + atmCoef*meanAirTemp*(1-exp(-expCoef*residence/flow )) ),
      data=temperatureParamDF,
      start=list(intercept=0,springTemp=10,atmCoef=1,expCoef=.008,sunCoef=.1,residCoef=.1),
      control=list(maxiter=500, minFactor=1e-6))
summary(m)

# m=nls(meantemp~I(springTemp + atmCoef*meanAirTemp*(1-exp(-expCoef*residence/flow )) ),data=temperatureParamDF,start=list(springTemp=10,atmCoef=1,expCoef=.008),control=list(maxiter=500, minFactor=1e-6))
# summary(m)
# 
# m=nls(meantemp~I(springTemp + atmCoef*(meanAirTemp-springTemp)*(1-exp(-expCoef*wshedArea/flow )) ),data=temperatureParamDF,start=list(springTemp=10,atmCoef=1,expCoef=.008),control=list(maxiter=500, minFactor=1e-6))
# summary(m)

# m=nls(meantemp~I(springTemp*(exp(-expCoef*residence/flow )) + atmCoef*meanAirTemp*(1-exp(-expCoef*residence/flow )) ),data=temperatureParamDF,start=list(springTemp=10,atmCoef=1,expCoef=.008),control=list(maxiter=500, minFactor=1e-6))
# summary(m)
# 
# m=nls(meantemp~I(intercept+springTemp*(exp(-expCoef*residence/flow )) + atmCoef*meanAirTemp*(1-exp(-expCoef*residence/flow )) + residCoef*wshedArea),data=temperatureParamDF,start=list(intercept=0,springTemp=10,atmCoef=1,expCoef=.1,residCoef=0),control=list(maxiter=500, minFactor=1e-6))
# summary(m)

saveRDS(m, "temperatureModel.rds")


temperatureParamDF$predictedTemp=predict(m,newdata = temperatureParamDF)
temperatureParamDF$resid=temperatureParamDF$predictedTemp-temperatureParamDF$meantemp

plot(temperatureParamDF$predictedTemp~temperatureParamDF$meantemp)
abline(a=0,b=1,col="blue")
highlightid=29
points(temperatureParamDF$predictedTemp[temperatureParamDF$locationid==highlightid]~temperatureParamDF$meantemp[temperatureParamDF$locationid==highlightid],pch="*",col="red")

summary(lm(temperatureParamDF$predictedTemp~temperatureParamDF$meantemp))



modelLocations$residence=0
modelLocations$meanError=0
modelLocations$meanAbsError=0

for( i in 1:nrow(modelLocations)){
  l=modelLocations$locationid[i]
  thisData=temperatureParamDF[temperatureParamDF$locationid==l,]
  modelLocations$residence[i]=mean(thisData$residence)
  modelLocations$meanError[i]=mean(thisData$resid,na.rm=T)
  modelLocations$meanAbsError[i]=mean(abs(thisData$resid),na.rm=T)
}

st_write(modelLocations,"~/Dropbox/SilverCreek/SilverCreekSpatial/tempModelPerformance.gpkg",append=F)

plot(modelLocations$meanError~modelLocations$residence)

plot(modelLocations$meanError~modelLocations$wshedareakm)


plot(1-exp(-.016*temperatureParamDF$residence/temperatureParamDF$flow)~temperatureParamDF$wshedArea)
