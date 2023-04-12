source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(rgl)

#################query stream points in watershed of location
dbGetQuery(conn(),"SELECT * FROM scstreampoints WHERE ST_WITHIN(scstreampoints.geometry,(SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid='55'));")








dataSeries=listDataSeries()

seriesData=getSeriesData(dataSeries$seriesid[dataSeries$metric=="flow"])

makePlot(seriesData)




