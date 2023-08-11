source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

library(MuMIn)

dbGetQuery(conn(),"SELECT * FROM locations WHERE locations.name ILIKE '%sportsman%';")
flowIndexLocationID=144  ###### everything is relative to flow at sportsmans

#upstream locations:  144 is sportsmans, 147 is ragsdale just above sc-lw confluence, 148 is station 10 further down the little wood (richfield) 

scFlowData=getFlowIndexData(flowIndexLocationID = flowIndexLocationID, upstreamOfLocationID = 147)

flow=data.frame(getFlowByDate(startDate="2000-07-1",endDate="2023-10-1",flowData=scFlowData))
#flow=getFlowByDate(startDate="2021-07-1",endDate="2021-10-1")

inSeason=function(date){
  month=as.numeric(format.Date(date,"%m"))
  return(month>=6 & month <=9)
}
flow=flow[inSeason(flow$date),]

siteDayCount=aggregate(date~locationid, scFlowData,FUN=length)
names(siteDayCount)[2]="obsDays"

#n per day:
dayN=aggregate(locationid~date, data=scFlowData, FUN=length)
names(dayN)[names(dayN)=="locationid"]="locationCount"
sum(dayN$locationCount>=8)

######## limit flow to days with >= 8 flow sites
bestDataDays=dayN$date[dayN$locationCount>=8]
flow=flow[flow$date %in% bestDataDays,]

plot(flow$tribCount~flow$uaa)

plot(flow$flowIndex~flow$indexFlow)
plot(flow$flowIndex~flow$uaa)
plot(flow$flowIndex~flow$tribCount)
plot(flow$flowIndex[flow$indexFlow<50]~flow$uaa[flow$indexFlow<50])
plot(flow$flowIndex[flow$indexFlow>200]~flow$uaa[flow$indexFlow>200])

plot(flow$flowIndex[flow$tribCount==2]~flow$uaa[flow$tribCount==2])

# flowModel=lm(flowIndex~tribCount+uaa,data=flow)
# summary(flowModel) #r2: .807
# 
# flowModel=lm(flowIndex~poly(tribCount,2)+uaa,data=flow)
# summary(flowModel) # .816
# 
# flowModel=lm(flowIndex~tribCount + poly(uaa,2),data=flow)
# summary(flowModel) #.815
# 
# flowModel=lm(flowIndex~tribCount+uaa*indexFlow,data=flow,na.action = "na.fail")
# summary(flowModel)  #.83
# 
# flowModel=lm(flowIndex~tribCount*indexFlow+uaa,data=flow)
# summary(flowModel) #.813
# 
# flowModel=lm(flowIndex~tribCount*indexFlow+uaa*indexFlow,data=flow)
# summary(flowModel) #.817
# 
# flowModel=lm(flowIndex~poly(tribCount,2)*poly(uaa,2)*poly(indexFlow,2),data=flow,na.action = "na.fail")
# summary(flowModel) #.825
# 
# 
# 
# flowModel=lm(flowIndex~poly(uaa,2)*indexFlow,data=flow,na.action = "na.fail")
# summary(flowModel)  #.813
# #monster model
# flowModel=lm(flowIndex~tribCount*indexFlow+uaa*indexFlow+I(uaa^2)*indexFlow,data=flow,na.action = "na.fail")
# summary(flowModel)

flowModel=lm(flowIndex~tribCount*indexFlow+poly(uaa,2)*indexFlow,data=flow,na.action = "na.fail")
summary(flowModel)  #.824
# 
# flowModel=lm(flowIndex~tribCount*indexFlow+uaa*indexFlow,data=flow,na.action = "na.fail")
# summary(flowModel)  #.821
# 
# flowModel=lm(flowIndex~tribCount+poly(uaa,2)*indexFlow,data=flow,na.action = "na.fail")
# summary(flowModel)  #.823

# flowModel=lm(flowIndex~tribCount*indexFlow+poly(uaa,2),data=flow,na.action = "na.fail")
# summary(flowModel)  #.824


dredge(flowModel)

##notes:
#index flow is only useful when it interacts w/ other terms.



# flowModel=lm(flowIndex~tribCount*indexFlow+uaa*indexFlow,data=flow,na.action = "na.fail")
# summary(flowModel) #.815


#extreme long single trib case:
predict(flowModel,data.frame(uaa=15,indexFlow=20,tribCount=1))

#headwater residence time study:
headDF=data.frame(segid=6:10,tribCount=1,uaa=c(1.995, 1.997, 2.02, 2.03, 2.05),indexFlow=150)
predict(flowModel,headDF)


plot(flow$flowIndex~flow$uaa)
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=20,tribCount=10))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=50,tribCount=10))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=100,tribCount=10))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=300,tribCount=10))~c(0:250))


plot(flow$flowIndex~flow$uaa)
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=20,tribCount=1))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=20,tribCount=10))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=20,tribCount=35))~c(0:250))

lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=150,tribCount=1))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=150,tribCount=10))~c(0:250))
lines(predict(flowModel,data.frame(uaa=0:250,indexFlow=150,tribCount=35))~c(0:250))



saveRDS(flowModel,file="~/Dropbox/SilverCreek/flowModel.rds")



######## create residence db table
flowModel=readRDS("~/Dropbox/SilverCreek/flowModel.rds")
summary(flowModel)

baseStreamSeg=dbGetQuery(conn(),"SELECT segid FROM streamsegments ORDER BY uaa DESC LIMIT 1;")$segid


#distribute flow and residence from index flow


dbGetQuery(conn(),"SELECT MIN(value), MAX(value) FROM data WHERE locationid = '144' AND metric = 'flow';")

resTimeList=as.list(seq(from=1,to=300,by=1))
names(resTimeList)=resTimeList

#singleThread
#resTimeList=lapply(resTimeList,calcMeanResidenceForFlow,baseStreamSeg=baseStreamSeg)

cores=max(1,parallel::detectCores()-4)
resTimeList = mclapply(resTimeList,calcSpecificResidence,baseStreamSeg=baseStreamSeg,mc.cores = cores )

resTimeDF=do.call(rbind, resTimeList)
names(resTimeDF)=tolower(names(resTimeDF))

dbExecute(conn(),"TRUNCATE TABLE residences;")
dbWriteTable(conn(),"residences",resTimeDF,append=T)
#dbExecute(conn(),"CREATE INDEX flowindex ON residences (indexflow);")




#examine model performance:
#broken - need to deal with trib count dimension
# 
# sampleModel=function(sampleStartDate,sampleEndDate=NULL,compareModel=flowModel,resid=T,minN=20,highlightLocationID=0,flowData=scFlowData){
#   sampleStartDate=as.Date(sampleStartDate)
#   if(is.null(sampleEndDate)){
#     sampleEndDate=sampleStartDate+7     # fit 1 week at a time
#   }
#   sampleEndDate=as.Date(sampleEndDate)
#   
#   #sample flow needs flowIndex, indexFlow values
#   sampleFlow=getFlowByDate(sampleStartDate,sampleEndDate,flowData=flowData)
#   
#   if(nrow(sampleFlow)>minN){
#     m=lm(flowIndex~poly(uaa,2)*indexFlow,data=sampleFlow)
#     print(summary(m))
#     plot(sampleFlow$flowIndex~sampleFlow$uaa,main=paste("flow model for",sampleStartDate,"to",sampleEndDate,", indexFlow=",round(mean(sampleFlow$indexFlow))))
#     if(highlightLocationID %in% sampleFlow$locationid){
#       points(sampleFlow$flowIndex[sampleFlow$locationid==highlightLocationID]~sampleFlow$uaa[sampleFlow$locationid==highlightLocationID],pch="*",cex=3)
#     }
#     varDF=data.frame(uaa=0:2050,indexFlow=sampleFlow$indexFlow[1])
#     
#     lines(predict(m,varDF)~varDF$uaa,lty=2)
#     if(!is.null(compareModel)){
#       lines(predict(compareModel,varDF)~varDF$uaa) 
#       legend(x="topleft",legend=c("model fitted to this data","general model"),lty=c(2,1))
#     }
#     if(resid){
#       resids=sampleFlow
#       resids$pred=predict(compareModel,sampleFlow)
#       resids$error=resids$pred-resids$flowIndex
#       resids$pctError=100*abs(resids$error)/(resids$flowIndex+0.01)
#       resids$errorPtSize=1+((1+resids$pctError)^(1/4))
#       return(resids)
#     }
#     else(
#       return(m)
#     )
#   }
# }
# 
# 
# #plot whole season, examine effect of indexFlow
# sampleModel("2021-06-01","2021-09-30")
# lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=50))~c(0:2050),col="red")
# lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=100))~c(0:2050),col="orange")
# lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=200))~c(0:2050),col="blue")
# 
# 
# sampleModel("2021-9-15")
# 
# sampleDates=seq.Date(from=as.Date("2000-01-01"),to=as.Date("2022-11-01"),by="week")
# sampleDates=sampleDates[inSeason(sampleDates)]
# 
# modelPerformance=do.call(rbind,lapply(sampleDates,FUN=sampleModel))
# head(modelPerformance)
# 
# plot(modelPerformance$error~modelPerformance$uaa)
# plot(modelPerformance$pctError~modelPerformance$uaa)
# 
# 
# modelPerfSummary=aggregate(cbind(errorPtSize,flowIndex,pred,error,pctError)~locationid, data=modelPerformance, FUN=mean)
# 
# ###make spatial points layer
# modelLocations=st_read(conn(),query=paste0("SELECT * FROM locationattributes WHERE locationid IN ('",
#                                            paste(unique(modelPerformance$locationid),collapse="', '"),
#                                            "');"))
# #somehow looses watershed geometry in merge, but keeps point geom
# modelLocations=merge(modelLocations,modelPerfSummary,by="locationid")
# modelLocations=merge(modelLocations,siteDayCount,by="locationid")
# st_write(modelLocations,dsn="~/Dropbox/SilverCreek/SilverCreekSpatial/modelPerformance.gpkg",delete_dsn=T)
# 
# 
# 
# mp=sampleModel("2021-07-12")
# 
# 
# 
# 
# 
# ######## summary info for flow meeting w/ tnc
# flowDates=scFlowData[,c("locationid","date")]
# head(flowDates)
# plot(flowDates$locationid~flowDates$date,xlim=c(as.Date("2014-01-01"),as.Date("2022-12-24")),xlab = "Date",ylab="location ID")
# 
# flowLocations=modelLocations[,c("locationid","name","wshedareakm","flowIndex","obsDays")]
# 
# flowLocations$watershed_area_km2=round(flowLocations$wshedareakm,2)
# flowLocations$average_percent_of_sportsmans_flow=100*round(flowLocations$flowIndex,2)
# 


