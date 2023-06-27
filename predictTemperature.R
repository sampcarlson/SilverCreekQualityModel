#predict temperatures across silver creek
source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")



flowModel=readRDS("~/Dropbox/SilverCreek/flowModel.rds")
summary(flowModel)

temperatureModel=readRDS("temperatureModel.rds")
summary(temperatureModel)


baseStreamSeg=dbGetQuery(conn(),"SELECT segid FROM streamsegments ORDER BY uaa DESC LIMIT 1;")$segid

thisIndexFlow=100

#distribute flow and residence from index flow
Rprof(tmp <- tempfile())
streamSegAttributes=data.table(distributeFlow(thisIndexFlow,baseStreamSeg,isSegID=T))
setkey(streamSegAttributes,segid)
streamSegAttributes = calcMeanResidence_allSegs(streamSegAttributes)
Rprof()
summaryRprof(tmp)


