sendDatabase <- function(username, password, dbname, host, port, id, tableName, tableCreate, tableInput){
  
  
  dbConnection <- tryCatch( 
    expr = {
      storiesDb <- dbConnect(RMariaDB::MariaDB(), 
                             user=username, 
                             password=password, 
                             dbname=dbname, 
                             host=host, 
                             port=port)
      loginfo(paste0(id, " connected with database. Tables: ", paste(dbListTables(storiesDb), collapse=" "), " tableName=", tableName))
      
      if (!(tableName %in% dbListTables(storiesDb))) {
        rsInsert <- dbSendQuery(storiesDb, tableCreate)
        dbClearResult(rsInsert)
      }
      
      dbWriteTable(storiesDb, value = tableInput, row.names = FALSE, name = tableName, append = TRUE )
      dbDisconnect(storiesDb)
      loginfo(paste0(id, " saved in database"))
      
    },
    error = function(e) {
      logerror(paste0(id, " DATABASE SAVING FAILED! ", e))
    }
  )
  
  
}