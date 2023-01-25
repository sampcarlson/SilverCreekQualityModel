library(terra)
library(readr)

wholeDem=merge(rast("/home/sam/Downloads/USGS_13_n44w114_20130911.tif"),rast("/home/sam/Downloads/USGS_13_n44w115_20190614.tif"),rast("/home/sam/Downloads/USGS_13_n44w116_20190614.tif"))
wholeDem=project(wholeDem,"epsg:26911",method="bilinear")

writeRaster(wholeDem,"/home/sam/Downloads/wholeDem.tif")

system("psql -h silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com -p 25060 -d silvercreekdb -U dbread -f /home/sam/Downloads/addDem.sql")
