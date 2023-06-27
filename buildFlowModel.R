source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

dbGetQuery(conn(),"SELECT * FROM locations WHERE locations.name ILIKE '%sportsman%';")
flowIndexLocationID=144  ###### everything is relative to flow at sportsmans

#upstream locations:  144 is sportsmans, 147 is ragsdale just above sc-lw confluence, 148 is station 10 further down the little wood (richfield) 

allFlowData=getFlowIndexData(flowIndexLocationID = flowIndexLocationID, upstreamOfLocationID = 147)

#n per day:
dayN=aggregate(locationid~date, data=allFlowData, FUN=length)
sum(dayN$locationid>=8)

siteDayCount=aggregate(date~locationid, allFlowData,FUN=length)
names(siteDayCount)[2]="obsDays"


flow=getFlowByDate(startDate="2000-07-1",endDate="2023-10-1")
#flow=getFlowByDate(startDate="2021-07-1",endDate="2021-10-1")

inSeason=function(date){
  month=as.numeric(format.Date(date,"%m"))
  return(month>=6 & month <=9)
}

flow=flow[inSeason(flow$date),]

plot(flow$flowIndex~flow$indexFlow)
plot(flow$flowIndex~flow$uaa)


flowModel=lm(flowIndex~poly(uaa,2)*indexFlow,data=flow)
summary(flowModel)
sigma(flowModel)

# model fit limited to best data days:
bestDataDays=dayN$date[dayN$locationid>=10]
rich_flow=flow[flow$date %in% bestDataDays,]
rich_flowModel=lm(flowIndex~poly(uaa,2)*indexFlow,data=rich_flow)
summary(rich_flowModel)
sigma(rich_flowModel)

plot(flow$flowIndex[flow$date %in% bestDataDays]~flow$uaa[flow$date %in% bestDataDays])
lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=100))~c(0:2050))
lines(predict(rich_flowModel,data.frame(uaa=0:2050,indexFlow=100))~c(0:2050),lty=2)



saveRDS(flowModel,file="~/Dropbox/SilverCreek/flowModel.rds")



plot(distributeFlow(indexFlow=100)[,"flow"])


#examine model performance:

sampleModel=function(sampleStartDate,sampleEndDate=NULL,compareModel=flowModel,resid=T,minN=20,highlightLocationID=0){
  sampleStartDate=as.Date(sampleStartDate)
  if(is.null(sampleEndDate)){
    sampleEndDate=sampleStartDate+7     # fit 1 week at a time
  }
  sampleEndDate=as.Date(sampleEndDate)
  sampleFlow=getFlowByDate(sampleStartDate,sampleEndDate)
  if(nrow(sampleFlow)>minN){
    m=lm(flowIndex~poly(uaa,2)*indexFlow,data=sampleFlow)
    print(summary(m))
    plot(sampleFlow$flowIndex~sampleFlow$uaa,main=paste("flow model for",sampleStartDate,"to",sampleEndDate,", indexFlow=",round(mean(sampleFlow$indexFlow))))
    if(highlightLocationID %in% sampleFlow$locationid){
      points(sampleFlow$flowIndex[sampleFlow$locationid==highlightLocationID]~sampleFlow$uaa[sampleFlow$locationid==highlightLocationID],pch="*",cex=3)
    }
    varDF=data.frame(uaa=0:2050,indexFlow=sampleFlow$indexFlow[1])
    
    lines(predict(m,varDF)~varDF$uaa,lty=2)
    if(!is.null(compareModel)){
      lines(predict(compareModel,varDF)~varDF$uaa) 
      legend(x="topleft",legend=c("model fitted to this data","general model"),lty=c(2,1))
    }
    if(resid){
      resids=sampleFlow
      resids$pred=predict(compareModel,sampleFlow)
      resids$error=resids$pred-resids$flowIndex
      resids$pctError=100*abs(resids$error)/(resids$flowIndex+0.01)
      resids$errorPtSize=1+((1+resids$pctError)^(1/4))
      return(resids)
    }
    else(
      return(m)
    )
  }
}


#plot whole season, examine effect of indexFlow
sampleModel("2021-06-01","2021-09-30")
lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=50))~c(0:2050),col="red")
lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=100))~c(0:2050),col="orange")
lines(predict(flowModel,data.frame(uaa=0:2050,indexFlow=200))~c(0:2050),col="blue")


sampleModel("2021-9-15")

sampleDates=seq.Date(from=as.Date("2000-01-01"),to=as.Date("2022-11-01"),by="week")
sampleDates=sampleDates[inSeason(sampleDates)]

modelPerformance=do.call(rbind,lapply(sampleDates,FUN=sampleModel))
head(modelPerformance)

plot(modelPerformance$error~modelPerformance$uaa)
plot(modelPerformance$pctError~modelPerformance$uaa)


modelPerfSummary=aggregate(cbind(errorPtSize,flowIndex,pred,error,pctError)~locationid, data=modelPerformance, FUN=mean)

###make spatial points layer
modelLocations=st_read(conn(),query=paste0("SELECT * FROM locationattributes WHERE locationid IN ('",
                                           paste(unique(modelPerformance$locationid),collapse="', '"),
                                           "');"))
#somehow looses watershed geometry in merge, but keeps point geom
modelLocations=merge(modelLocations,modelPerfSummary,by="locationid")
modelLocations=merge(modelLocations,siteDayCount,by="locationid")
st_write(modelLocations,dsn="~/Dropbox/SilverCreek/SilverCreekSpatial/modelPerformance.gpkg",delete_dsn=T)



mp=sampleModel("2021-07-12")





######## summary info for flow meeting w/ tnc
flowDates=allFlowData[,c("locationid","date")]
head(flowDates)
plot(flowDates$locationid~flowDates$date,xlim=c(as.Date("2014-01-01"),as.Date("2022-12-24")),xlab = "Date",ylab="location ID")

flowLocations=modelLocations[,c("locationid","name","wshedareakm","flowIndex","obsDays")]

flowLocations$watershed_area_km2=round(flowLocations$wshedareakm,2)
flowLocations$average_percent_of_sportsmans_flow=100*round(flowLocations$flowIndex,2)



