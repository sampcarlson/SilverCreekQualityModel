source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(rgl)

#################query stream points in watershed of location
dbGetQuery(conn(),"SELECT * FROM scstreampoints WHERE ST_WITHIN(scstreampoints.geometry,(SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid='55'));")





dataSeries=listDataSeries()

seriesData=getSeriesData(dataSeries$seriesid[dataSeries$metric=="flow"])

makePlot(seriesData)


daysOfData=dbGetQuery(conn(),"SELECT locationid, metric, COUNT(DISTINCT(datetime::date)) FROM data GROUP BY locationid, metric;")


allFlow=st_read(conn(),query=paste0("SELECT metric, datetime, data.locationid, name, geometry FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE data.metric = 'flow' GROUP BY metric, datetime, data.locationid, name, geometry"))
flowDays=aggregate(datetime~locationid+metric,data=allFlow,FUN=length)
names(flowDays)[names(flowDays)=="datetime"]="observation_count"
flowDays=merge(flowDays,unique(allFlow[,c("metric","locationid","geometry","name")]))
flowDays=merge(flowDays,daysOfData)
names(flowDays)[names(flowDays)=="count"]="day_count"
flowDays$label=paste0(flowDays$name," (",flowDays$day_count,"days of data)")
st_write(flowDays,"~/Dropbox/SilverCreek/SilverCreekSpatial/dataDays.gpkg","flow")

allTemp=st_read(conn(),query=paste0("SELECT metric, datetime, data.locationid, name, geometry FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE data.metric = 'Water Temperature' GROUP BY metric, datetime, data.locationid, name, geometry"))
tempDays=aggregate(datetime~locationid+metric,data=allTemp,FUN=length)
names(tempDays)[names(tempDays)=="datetime"]="observation_count"
tempDays=merge(tempDays,unique(allTemp[,c("metric","locationid","geometry","name")]))
tempDays=merge(tempDays,daysOfData)
names(tempDays)[names(tempDays)=="count"]="day_count"
tempDays$label=paste0(tempDays$name," (",tempDays$day_count,"days of data)")
st_write(tempDays,"~/Dropbox/SilverCreek/SilverCreekSpatial/dataDays.gpkg","temperature")



allDO=st_read(conn(),query=paste0("SELECT metric, datetime, data.locationid, name, geometry FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE data.metric = 'Dissolved Oxygen' GROUP BY metric, datetime, data.locationid, name, geometry"))
DODays=aggregate(datetime~locationid+metric,data=allDO,FUN=length)
names(DODays)[names(DODays)=="datetime"]="observation_count"
DODays=merge(DODays,unique(allDO[,c("metric","locationid","geometry","name")]))
DODays=merge(DODays,daysOfData)
names(DODays)[names(DODays)=="count"]="day_count"
DODays$label=paste0(DODays$name," (",DODays$day_count,"days of data)")
st_write(DODays,"~/Dropbox/SilverCreek/SilverCreekSpatial/dataDays.gpkg","DO")


