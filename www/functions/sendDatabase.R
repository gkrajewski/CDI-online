sendDatabase <- function(username, password, dbname, host, port, id, tableName, tableCreate=NULL, tableInput=NULL, tableQuery=NULL){
  
  
  dbConnection <- tryCatch( 
    expr = {
      storiesDb <- dbConnect(RMariaDB::MariaDB(), 
                             user=username, 
                             password=password, 
                             dbname=dbname, 
                             host=host, 
                             port=port)
      loginfo(paste0(id, " connected with database. tableName: ", tableName))
      
      if (!(tableName %in% dbListTables(storiesDb))) {
        rsInsert <- dbSendQuery(storiesDb, tableCreate)
        dbClearResult(rsInsert)
      }
      
      if (!is.null(tableQuery)) {
        rsInsert <- dbSendQuery(storiesDb, tableQuery)
        dbClearResult(rsInsert)
      }
      
      if (!is.null(tableInput)) {
        dbWriteTable(storiesDb, value = tableInput, row.names = FALSE, name = tableName, append = TRUE )
        loginfo(paste0(id, " saved in database"))
      }
      
      dbDisconnect(storiesDb)
    },
    error = function(e) {
      logerror(paste0(id, " DATABASE SAVING FAILED! ", e))
    }
  )
  
  
}