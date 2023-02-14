library(RPostgreSQL) # main library for interfacing w/ postgres db
library(sf)          #'simple features' library or vector tools.  Most of these tools mimic postgis tools
library(terra)       #raster tools library



conn=function(){    #checks for db connection object ('scdb') in global env, creates if necessary, returns and places in global enc
  
  scdbConnect=function(){
    scdb=dbConnect(RPostgres::Postgres(),
                   host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                   port="25060",
                   dbname="silvercreekdb" ,
                   user="dbread",
                   password="dbread")
    return(scdb)
  }
  
  if(!exists("scdb",where=globalenv())){      #scdb does not exist - create it
    scdb=scdbConnect()
    assign("scdb",scdb,envir=globalenv())     #assign to global environment for future use
  } else {                                    #scdb exists, get it and test
    scdb=get("scdb",envir=globalenv())
    if(!dbIsValid(scdb)){                     #scdb not valid, recreate
      scdb=scdbConnect()
      assign("scdb",scdb,envir=globalenv())
    }
  }
  return(get("scdb",envir=globalenv()))
}



dbListTables(conn())

dbListFields(conn(),"data")

#basic query structure - a connection and a sql statement:
dbGetQuery(conn(),"SELECT * FROM data LIMIT 10;")
#* is wildcard
# sql tatements *should* end in ';'
# it is standard to have commands in UPPER and names in lower case

#replace * with column names
dbGetQuery(conn(),"SELECT metric, value FROM data LIMIT 10;")


dbGetQuery(conn(),"SELECT metric, datetime, value FROM data WHERE metric = 'flow' AND locationid = '140' AND datetime > '2021-01-01' AND datetime < '2021-02-01';")


dbGetQuery(conn(),"SELECT data.locationid, data.datetime, data.value, locations.name FROM 
           data LEFT JOIN locations ON data.locationid = locations.locationid 
           LIMIT 10;")


#to get spatial data from db:
locations=dbGetQuery(conn(),"SELECT * FROM locations LIMIT 10;")
#^this returns a data frame.  It has coordinate information, but is not actually a spatial type data frame:
str(locations)
#to get a spatial object, use st_read (for vector geometry)
#note that query is a named argument here (st_read is a very generalized function, not just for db output)
locations=st_read(conn(),query="SELECT * FROM locations LIMIT 10;")
str(locations)

#st means spatial type.  many SF and postgis functions start with 'st_'
st_crs(locations)


plot(st_read(conn(),query="SELECT outflowlocationid, geometry FROM watersheds LIMIT 10;"))

watershedid=4

st_read(conn(),query=paste0("SELECT * FROM scstreampoints 
           WHERE ST_WITHIN(scstreampoints.geometry,(SELECT watersheds.geometry FROM watersheds WHERE watersheds.outflowlocationid='",watershedid,"'));"))



