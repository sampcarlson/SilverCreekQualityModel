source("~/Documents/R Workspace/SilverCreekQualityModel/functions.r")

dbGetQuery(conn(),"SELECT * FROM locations WHERE locations.name ILIKE '%sportsman%';")
flowIndexLocationID=144  ###### everything is relative to flow at sportsmans

allFlowData=getFlowIndexData()


flow=getFlowByDate(startDate="2021-07-1",endDate="2021-10-1")

plot(flow$flowIndex~flow$indexFlow)
plot(flow$flowIndex~flow$uaa)


m=lm(flowIndex~poly(uaa,2)*indexFlow,data=flow)
summary(m)

lines(predict(m)[order(flow$uaa)]~flow$uaa[order(flow$uaa)])


