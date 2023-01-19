library(RPostgreSQL)



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


