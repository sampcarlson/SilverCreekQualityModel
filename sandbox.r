source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")



dbGetQuery(conn(),"SELECT * FROM data LIMIT 10;")

flowid=dbGetQuery(conn(),"SELECT metricid FROM metrics WHERE name = 'flow';")$metricid

flowLocations=dbGetQuery(conn(),paste0("SELECT DISTINCT locationid from DATA where metricid = '",flowid,"';"))

flowLocations=dbGetQuery(conn(),paste0("SELECT * FROM locations WHERE locationid IN ('",
                                       paste0(flowLocations$locationid,collapse="', '"),
                                       "');"))



dataSeries=listDataSeries()

seriesData=getSeriesData(dataSeries$seriesid[dataSeries$metric=="flow"])

makePlot(seriesData)
