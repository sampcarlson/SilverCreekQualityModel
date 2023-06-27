#predict temperatures across silver creek
source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")
library(data.table)


flowModel=readRDS("~/Dropbox/SilverCreek/flowModel.rds")
summary(flowModel)

temperatureModel=readRDS("temperatureModel.rds")
summary(temperatureModel)


baseStreamSeg=dbGetQuery(conn(),"SELECT segid FROM streamsegments ORDER BY uaa DESC LIMIT 1;")$segid

thisIndexFlow=100


streamSegAttributes=data.table(distributeFlow(thisIndexFlow,baseStreamSeg,isSegID=T))

Rprof(tmp <- tempfile())
streamSegmentAttributes = calcMeanResidence_allSegs(streamSegAttributes)
Rprof()
summaryRprof(tmp)