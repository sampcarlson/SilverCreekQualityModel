source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(DBI)
library(RPostgreSQL)
library(pbapply)

flowModel=getFlowModel()

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



getTemperatureModelDataset=function(locationID,date,flowIndexLocationID=144,airTempReferenceLocation=180,airTempPeriodDays=1,preFabList=preFabData){
  
  preFabList=buildPreFabDataList()
  
  list2env(preFabList,envir = globalenv())
  flowModel=getFlowModel()
  allFlowData=getAllFlowData()
  locationAttributeTable=getLocationAttributeTable()
  airTempData=getAirTempData()
  residences=getResidences()
  
  #print(paste("l:",locationID,"d:",date))
  date=as.Date(date)
  
  #indexFlow=dbGetQuery(conn(),paste0("SELECT AVG(value) AS flow FROM data WHERE locationid = '",flowIndexLocationID,
  #                                   "' AND datetime::date = '",date,"' AND metric = 'flow';"))$flow
  indexFlow=allFlowData[locationid == flowIndexLocationID][datetime == date]$flow
  
  #uaa=dbGetQuery(conn(),paste0("SELECT wshedareakm AS uaa FROM locationattributes WHERE locationid = '",locationID,"';"))$uaa
  uaa=locationAttributeTable[locationid == locationID]$wshedareakm
  tribCount=locationAttributeTable[locationid == locationID]$tribCount
  
  
  flow=pmax(0,predict(flowModel,newdata=data.frame(indexFlow=indexFlow,uaa=uaa,tribCount=tribCount))*indexFlow)
  
  
  resid=calcMeanResidence(ID=locationID,indexFlow=indexFlow,isSegID=F,useResidenceFunction = F)
  specificResidence=getSpecificResidence_location(scLocation = locationID, indexFlow = indexFlow)
  
  
  #air temperature within 50 km
  # airTemp=dbGetQuery(conn(),paste0("SELECT AVG(value) FROM data WHERE data.metric = 'air temperature' AND data.datetime BETWEEN '",date-5,"' AND '",date+1,"'
  #                           AND data.locationid IN 
  #                          (SELECT locationid FROM locations WHERE ST_DWITHIN(locations.geometry, 
  #                          (SELECT locations.geometry FROM locations WHERE locations.locationid = '",locationID,"'),
  #                          50000))")
  # )$avg
  # 
  # ^this is unnecessary as there is only one reliable station nearby (at Picabo)
  #get n day window past air temp at picabo
  
  
  airTemps=airTempData[(airTempData$datetime>date-airTempPeriodDays & airTempData$datetime <= date ),]
  
  # airTemps=dbGetQuery(conn(),paste0("SELECT MAX(value), AVG(value) FROM data WHERE data.metric = 'air temperature' AND data.datetime::date > '",date-airTempPeriodDays,"' AND data.datetime::date <='",date,"'
  #                           AND data.locationid = '",airTempReferenceLocation,"';") )
  
  
  fToC=function(f){
    c = 5/9*(f - 32)
    return(c)
  }
  
  maxAirTemp=fToC(airTemps$daymax)
  meanAirTemp=fToC(airTemps$daymean)
  
  return(data.table(locationid=locationID,date=date,flow=flow,tribCount=tribCount,sR=specificResidence,residence=resid,meanAirTemp=meanAirTemp,maxAirTemp=maxAirTemp,indexFlow=indexFlow))
}


tempModelBaseLocationID=147
dbGetQuery(conn(), "SELECT locationid, name, metrics, wshedareakm FROM locationattributes WHERE locationid = '147';")
#aggregate by day - no need for multiple models per day


temperatureData=dbGetQuery(conn(),paste0("SELECT AVG(data.value) AS meantemp, MAX(data.value) as maxTemp, data.datetime::date AS date, data.locationid FROM data LEFT JOIN locations ON data.locationid = locations.locationid 
           WHERE data.qcstatus='true' AND metric = 'Water Temperature' AND ST_WITHIN(locations.geometry, (SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid = '",tempModelBaseLocationID,"'))
                                         GROUP BY data.datetime::date, data.locationid;")
)

# exclude well location - though it may be interesting for comparison...
dbGetQuery(conn(),"SELECT * FROM locations WHERE locationid = 63;")
temperatureData=temperatureData[!temperatureData$locationid == 63,]


#time per location*date
#must run once before parallel to initialize preFabList
system.time(
  getTemperatureModelDataset(2,"2021-07-01",preFabList=preFabData)
)

inSeason=function(date){
  month=as.numeric(format.Date(date,"%m"))
  return(month>=6 & month <=9)
}

temperatureData$inSeason=inSeason(temperatureData$date)
temperatureData=temperatureData[temperatureData$inSeason,]


#single threadprocess:
#tempParamList = pbmapply(getTemperatureModelDataset,locationID=temperatureData$locationid,date=temperatureData$date,MoreArgs = list(preFabList=preFabData), SIMPLIFY = F)

#multi thread (forked) process:
cores=max(1,parallel::detectCores()-4)

tempParamList= mcmapply(getTemperatureModelDataset,locationID=temperatureData$locationid,date=temperatureData$date,
                        MoreArgs = list(preFabList=preFabData),SIMPLIFY = F,mc.cores = cores )
temperatureParamDF=do.call(rbind,tempParamList)
rm(tempParamList)

temperatureParamDF=merge(temperatureParamDF,temperatureData)
temperatureParamDF$doy=as.numeric(format.Date(temperatureParamDF$date,"%j"))

write.csv(temperatureParamDF,file=paste0(getwd(),"/temperatureParamaterSet_",Sys.Date(),".csv"))




deltaT=function(tempModel){
  
  plot(fitted(m)~I(fitted(m)+resid(m)),xlab="observed",ylab="predicted",main=formula(m),cex.main=0.6)
  abline(a=0,b=1,col="blue")
  print(paste("r sq:",round(summary(lm(fitted(m)~I(fitted(m)+resid(m))))$r.squared,2) ))
  
  predictSegTemperatures=function(indexFlow, maxAirTemp_F, forecastDate, streamSegs=streamSegs, tempModel=temperatureModel,airTempReferenceLocation=airTempLocation){
    
    maxAirTemp = 5/9*(maxAirTemp_F - 32)
    meanAirTemp=maxAirTemp-10
    forecastDate=as.Date(forecastDate)
    forecastDOY=as.numeric(format.Date(forecastDate, "%j"))
    
    segTemperatures=dbGetQuery(conn(),paste0("SELECT specificresidencetoseg AS sr, meanresidencetoseg AS residence, flow, segid FROM residences WHERE indexflow = '",indexFlow,"';"))
    names(segTemperatures)[names(segTemperatures)=="sr"] = "sR"
    
    segTemperatures=merge(streamSegs, segTemperatures, by="segid")
    segTemperatures$maxSunElevation=maxSunAngleFun(forecastDOY)
    segTemperatures$maxAirTemp=maxAirTemp
    segTemperatures$meanAirTemp=meanAirTemp
    segTemperatures$indexFlow=indexFlow
    #  print(names(segTemperatures))
    
    segTemperatures$temperature_F=( predict(tempModel,newdata=segTemperatures) * (9/5) ) + 32 
    
    return(segTemperatures)
    
  }
  
  
  t80=predictSegTemperatures(indexFlow=80,maxAirTemp_F=90,forecastDate=as.Date("2021-08-18"),streamSegs = st_read(conn(),query="SELECT segid, geometry FROM streamsegments;"), tempModel = tempModel)
  t100=predictSegTemperatures(indexFlow=100,maxAirTemp_F=90,forecastDate=as.Date("2021-08-18"),streamSegs = st_read(conn(),query="SELECT segid, geometry FROM streamsegments;"), tempModel = tempModel)
  
  return(mean(t80$temperature_F,na.rm=T) - mean(t100$temperature_F,na.rm=T))
  
}

###################   fit models ------------------


# temperatureParamDF=read.csv(paste0(getwd(),"/temperatureParamaterSet_2023-08-10.csv"))


modelLocations=st_read(conn(),query=paste0("SELECT locationid, name, wshedareakm, metrics, locationgeometry FROM locationattributes WHERE locationid IN ('",
                                           paste(unique(temperatureParamDF$locationid), collapse="', '"),"');"))
temperatureParamDF=merge(temperatureParamDF,data.frame(locationid=modelLocations$locationid,wshedArea=modelLocations$wshedareakm),by="locationid")
temperatureParamDF$date=as.Date(temperatureParamDF$date)

temperatureParamDF$maxSunElevation=maxSunAngleFun(temperatureParamDF$doy)

#exclude 3 sites immediately downstream of ponds:
dbGetQuery(conn(),"SELECT * FROM locations WHERE locationid IN ('8', '43', '37');")
temperatureParamDF=temperatureParamDF[!temperatureParamDF$locationid %in% c(8,43,37),]

#exclude temperature from an irrigation ditch:
dbGetQuery(conn(),"SELECT * FROM locations WHERE locationid IN ('3');")
temperatureParamDF=temperatureParamDF[!temperatureParamDF$locationid %in% c(3),]


plot(temperatureParamDF$meantemp~temperatureParamDF$date)

plot(temperatureParamDF$meantemp~temperatureParamDF$doy)
plot(temperatureParamDF$maxtemp~temperatureParamDF$doy)
plot(temperatureParamDF$meantemp~temperatureParamDF$maxSunElevation)

plot(temperatureParamDF$meantemp~temperatureParamDF$maxAirTemp)
plot(temperatureParamDF$meantemp[temperatureParamDF$date==as.Date("2020-07-15")]~temperatureParamDF$sR[temperatureParamDF$date==as.Date("2020-07-15")])

plot(temperatureParamDF$maxtemp[temperatureParamDF$locationid==1]~temperatureParamDF$flow[temperatureParamDF$locationid==1])

hist(temperatureParamDF$maxtemp/temperatureParamDF$maxAirTemp)
plot(temperatureParamDF$maxtemp/temperatureParamDF$maxAirTemp~temperatureParamDF$maxAirTemp)

plot(temperatureParamDF$maxtemp/temperatureParamDF$maxAirTemp~temperatureParamDF$flow)
plot(temperatureParamDF$maxtemp/temperatureParamDF$maxAirTemp~temperatureParamDF$residence)
plot(temperatureParamDF$maxtemp/temperatureParamDF$maxAirTemp~temperatureParamDF$sR)





# pdf(file="allPlot.pdf",width=18,height=14)
# plot(temperatureParamDF[,c("meantemp","maxtemp","flow","residence","meanAirTemp","maxAirTemp","doy","maxSunElevation")])
# dev.off()


m=lm(maxtemp~(maxSunElevation+maxAirTemp+flow)^2 + meanAirTemp*sR ,data=temperatureParamDF)
summary(m)#.56
deltaT(m) #1.69


m=nls(meantemp~I(intercept+sunCoef*maxSunElevation+flowCoef*flow+springTemp*(exp(-expCoef*sR )) + atmCoef*meanAirTemp*(1-exp(-expCoef*sR )) ),
      data=temperatureParamDF,
      start=list(intercept=0,springTemp=10,atmCoef=1,expCoef=.008,sunCoef=.1,flowCoef=.1),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.77
deltaT(m) #.48


m=nls(meantemp~I(intercept+(sunInt+maxSunElevation*sunCoef)*meanAirTemp + atmCoef*meanAirTemp*(1-exp(-expCoef*(sR) )) ),
      data=temperatureParamDF,
      start=list(intercept=0,atmCoef=1,expCoef=.008,sunCoef=.1,sunInt=1),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.75
deltaT(m) #.77


m=nls(maxtemp~I(intercept + (sunInt+maxSunElevation*sunCoef)*meanAirTemp  + atmCoef*meanAirTemp*(1-exp(-expCoef*(sR) )) + springTemp*(exp(-expCoef*(sR) )) ),
      data=temperatureParamDF,
      #weights=temperatureParamDF$maxtemp,
      start=list(intercept=0,atmCoef=1,expCoef=.008,sunCoef=.1, springTemp=20,sunInt=1),
      control=list(maxiter=2000, minFactor=1e-6)
)
summary(m) #.8
deltaT(m) #.50


m=nls(maxtemp~I(  atmCoef*meanAirTemp*(1-exp(-expCoef*(sR) )) + springTemp*(exp(-expCoef*(sR) )) ),
      data=temperatureParamDF,
      #weights=temperatureParamDF$maxtemp,
      start=list(atmCoef=1,expCoef=.008, springTemp=20),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.63
deltaT(m) #1.19



m=nls(maxtemp~I( ic+ maxSunElevation*sunExp + atmCoef*meanAirTemp*(1-exp(-expCoef*(sR) )) + springTemp*(exp(-expCoef*(sR) )) ),
      data=temperatureParamDF,
      #weights=temperatureParamDF$maxtemp,
      start=list(atmCoef=1,expCoef=.008,springTemp=10,sunExp=1,ic=0),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.79
deltaT(m) #.48


m=nls(meantemp~I(intercept+sCoef*maxSunElevation+atCoef*(maxAirTemp-meanAirTemp)+atmCoef*maxSunElevation*meanAirTemp*(1-exp(-expCoef*(sR^flowEx) )) ),
      data=temperatureParamDF,
      #weights=temperatureParamDF$meantemp-5,
      start=list(intercept=0,atmCoef=1,expCoef=.008,flowEx=1,atCoef=1,sCoef=1),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.77
deltaT(m) #.82

m=nls(meantemp~I(intercept+rCoef*residence + atmCoef*maxSunElevation*meanAirTemp*(1-exp(-expCoef*(sR^flowEx) )) ),
      data=temperatureParamDF,
      start=list(intercept=0,atmCoef=1,expCoef=.008,flowEx=1,rCoef=1),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.81
deltaT(m) #.44






m=nls(meantemp~I(intercept+sunCoef*maxSunElevation + atmCoef*meanAirTemp*(1-exp(-expCoef*(sR^flowEx) )) ),
      data=temperatureParamDF,
      start=list(intercept=0,atmCoef=1,expCoef=.008,sunCoef=.1,flowEx=1),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.75
deltaT(m) #.83




m=nls(meantemp~I(intercept+maxSunElevation*atmCoef*meanAirTemp*(1-exp(-expCoef*(sR^flowEx) )) ),
      data=temperatureParamDF,
      weights=temperatureParamDF$meantemp,
      start=list(intercept=0,atmCoef=1,expCoef=.008,flowEx=1),
      control=list(maxiter=500, minFactor=1e-6)
)
summary(m) #.76
deltaT(m) #.82

saveRDS(m, "temperatureModel.rds")


temperatureParamDF$predictedTemp=predict(m,newdata = temperatureParamDF)
temperatureParamDF$residual=temperatureParamDF$predictedTemp-temperatureParamDF$meantemp

plot(temperatureParamDF$predictedTemp~temperatureParamDF$meantemp)
abline(a=0,b=1,col="blue")
highlightid=1
points(temperatureParamDF$predictedTemp[temperatureParamDF$locationid==highlightid]~temperatureParamDF$meantemp[temperatureParamDF$locationid==highlightid],pch="*",col="red")

summary(lm(temperatureParamDF$predictedTemp~temperatureParamDF$meantemp))



modelLocations$residence=0
modelLocations$meanError=0
modelLocations$meanAbsError=0
modelLocations$sR=0

for( i in 1:nrow(modelLocations)){
  l=modelLocations$locationid[i]
  thisData=temperatureParamDF[temperatureParamDF$locationid==l,]
  modelLocations$residence[i]=mean(thisData$residence)
  modelLocations$sR[i]=mean(thisData$sR)
  modelLocations$meanError[i]=mean(thisData$residual,na.rm=T)
  modelLocations$meanAbsError[i]=mean(abs(thisData$residual),na.rm=T)
}

st_write(modelLocations,"~/Dropbox/SilverCreek/SilverCreekSpatial/tempModelPerformance.gpkg",append=F)

plot(modelLocations$meanError~modelLocations$sR)

plot(modelLocations$meanError~modelLocations$wshedareakm)


#plot(1-exp(-.0175*temperatureParamDF$residence/temperatureParamDF$flow)~temperatureParamDF$wshedArea)
