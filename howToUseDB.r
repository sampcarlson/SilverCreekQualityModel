library(RPostgreSQL) # main library for interfacing w/ postgres db
library(sf)          #'simple features' library or vector tools.  Most of these tools mimic postgis tools
library(terra)       #raster tools library


#basic query structure - a connection and a sql statement:
dbGetQuery(conn(),"SELECT * FROM DATA LIMIT 10;")
#* is wildcard
# sql tatements *should* end in ';'




#to get spatial data from db:
locations=dbGetQuery(conn(),"SELECT * FROM locations LIMIT 10;")
#^this returns a data frame.  It has coordinate information, but is not actually a spatial type data frame:
str(locations)
#to get a spatial object, use st_read (for vector geometry)
#note that query is a named argument here (st_read is a very generalized function, not just for db output)
locations=st_read(conn(),query="SELECT * FROM locations LIMIT 10;")
str(locations)
