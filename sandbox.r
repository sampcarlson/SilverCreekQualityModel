source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(rgl)

##################3write silver creek stream point data to db-----------------
#dbSendQuery(conn(),"TRUNCATE TABLE scstreampoints RESTART IDENTITY;")
scPoints=st_read("~/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/scStreamPoints.gpkg")
st_geometry(scPoints) = "geometry"
st_write(scPoints[,c("uaa","geometry")],conn(),layer="scstreampoints")

#################query stream points in watershed of location
dbGetQuery(conn(),"SELECT * FROM scstreampoints WHERE ST_WITHIN(scstreampoints.geometry,(SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid='55'));")











dbGetQuery(conn(),"SELECT * FROM data LIMIT 10;")

flowid=dbGetQuery(conn(),"SELECT metricid FROM metrics WHERE name = 'flow';")$metricid

flowLocations=dbGetQuery(conn(),paste0("SELECT DISTINCT locationid from DATA where metricid = '",flowid,"';"))

flowLocations=dbGetQuery(conn(),paste0("SELECT * FROM locations WHERE locationid IN ('",
                                       paste0(flowLocations$locationid,collapse="', '"),
                                       "');"))

flowLocations=st_read(dsn=conn(),query=paste0("SELECT * FROM locations WHERE locationid IN ('",
                                              paste0(flowLocations$locationid,collapse="', '"),
                                              "');")
)


st_write(flowLocations,"~/Downloads/flowLocations.gpkg",append=F)



dataSeries=listDataSeries()

seriesData=getSeriesData(dataSeries$seriesid[dataSeries$metric=="flow"])

makePlot(seriesData)



#w148=st_read("~/Downloads/wshed148.gpkg")

#w148$outflowlocationid[1]=148

#watersheds=w148[,c("outflowlocationid","geom")]
#st_write(watersheds,conn(),append=T)

w1=st_read("~/Downloads/wshed1.gpkg")
w1$outflowlocationid[1]=1
watersheds=w1[,c('outflowlocationid','geom')]
st_write(watersheds,conn(),append = T)



#simple flow index
scFlow=getInWatershed(outflowLocationID = 1,metricNames = "flow",returnData=T)
makePlot(scFlow)
scFlow$day=as.Date(scFlow$datetime)
scFlow=scFlow[complete.cases(scFlow[,c("value","day")]),]
scFlowIndex=aggregate(scFlow,by=list(day=scFlow$day),FUN=median,na.rm=T)
makePlot(scFlowIndex)


#simple temp index
scTemp=getInWatershed(outflowLocationID = 148, metricNames = "Water Temperature",returnData=T)
#makePlot(scTemp)
scTemp$day=as.Date(scTemp$datetime)
scTemp=scTemp[complete.cases(scTemp[,c("value","day")]),]
scTempIndex=aggregate(scTemp,by=list(day=scTemp$day),FUN=median,na.rm=T)
makePlot(scTempIndex)

flowAndTemp=merge(scFlowIndex[,c("value","day")],scTempIndex[,c("value","day")],by="day",suffixes=c(".flow",".temp"))

flowAndTemp$doy=as.numeric(format.Date(flowAndTemp$day,"%j"))
flowAndTemp$month=as.numeric(format.Date(flowAndTemp$day,"%m"))
plot(flowAndTemp$value.temp~flowAndTemp$value.flow)
plot3d(flowAndTemp$value.flow,flowAndTemp$value.temp,flowAndTemp$doy,xlab="flow",ylab="temperature",zlab="julian day",type="s",size=.5)

flowAndTemp$monthColor=brewer.pal(6,"YlGnBu")[round(flowAndTemp$month/2)+1]
plot(flowAndTemp$value.temp~flowAndTemp$value.flow,col=flowAndTemp$monthColor,pch=1)


#late summer
scFlowData=getInWatershed(outflowLocationID = 1,metricNames = "flow",returnData=T)
scFlowData=scFlowData[scFlowData$datetime>(as.Date("2020-7-1")) & scFlowData$datetime>(as.Date("2020-8-1")),]

makePlot(scFlowData)
