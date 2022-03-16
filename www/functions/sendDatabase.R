sendDatabase <- function(username, password, dbname, host, port, id, tableName, tableCreate=NULL, tableInput=NULL, transaction=NULL, tableQuery=NULL){
  
  
  dbConnection <- tryCatch( 
    expr = {
      storiesDb <- dbConnect(RMariaDB::MariaDB(), 
                             user=username, 
                             password=password, 
                             dbname=dbname, 
                             host=host, 
                             port=port)

      loginfo(paste0(id, " connected with database. tableName: ", tableName))
      result = ""
      
      if (!(tableName %in% dbListTables(storiesDb))) {
        rsInsert <- dbSendQuery(storiesDb, tableCreate)
        dbClearResult(rsInsert)
      }
      
      if (!is.null(transaction)) {
        print("sending transaction")
        dbBegin(storiesDb)
        for (trans in transaction){
          dbSendStatement(storiesDb, trans)
        }
        dbCommit(storiesDb)
      }
      
      if (!is.null(tableInput)) {
        dbWriteTable(storiesDb, value = tableInput, row.names = FALSE, name = tableName, append = TRUE )
        loginfo(paste0(id, " saved in database"))
      }
      
      if (!is.null(tableQuery)) {
        rsInsert <- dbSendQuery(storiesDb, tableQuery)
        result = dbFetch(rsInsert)
        dbClearResult(rsInsert)
      }
      
      dbDisconnect(storiesDb)
      return(result)
    },
    error = function(e) {
      logerror(paste0(id, " DATABASE SAVING FAILED! Tried to save table: ", 
                      tableName, " to database: ", dbname, " ", e))
    }
  )
  
  
}