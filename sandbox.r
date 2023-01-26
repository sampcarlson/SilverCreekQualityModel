source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")



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



w148=st_read("~/Downloads/wshed148.gpkg")

w148$outflowlocationid[1]=148

watersheds=w148[,c("outflowlocationid","geom")]
#watersheds$watershedid="DEFAULT"

st_write(watersheds,conn(),append=T)

#dbSendQuery(conn(),"DELETE FROM watersheds")

getInWatershed(watershedID=1)

