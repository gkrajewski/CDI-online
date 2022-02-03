getAdditionalEndMessage <- function(id, urlString, type="database", parameters, txt){
  
  if (type=="database") {
    table = parameters[parameters$parameter=="additionalEndMessageTable", "value"]
    val_col = parameters[parameters$parameter=="additionalEndMessageValueColumn", "value"]
    id_col = parameters[parameters$parameter=="additionalEndMessageIdColumn", "value"]
    db_name = Sys.getenv("DB_NAME")
    
    transaction1 = paste0("select @value:= ", val_col, " from `", db_name, "`.`", table,
                          "` where ", id_col, " is null limit 1")
    transaction2 = paste0("update `", db_name, "`.`", table, "` set id=' ",
                          id, "' where ", val_col, "=@value and ", id_col, " is null")
    query = paste0("select ", val_col, " from `", db_name, "`.`", table, "` where ", 
                   id_col, "='", id, "'")
    transaction = c(transaction1, transaction2)
    
    result = sendDatabase(username=Sys.getenv("DB_USERNAME"),
                     password=Sys.getenv("DB_PASSWORD"),
                     dbname=Sys.getenv("DB_NAME"),
                     host=Sys.getenv("DB_HOST"),
                     port=Sys.getenv("DB_PORT"),
                     id="testing",
                     tableName="vouchers",
                     transaction=transaction,
                     tableQuery=query)
    
    message = paste(txt[txt$text_type=='additionalEndMessageFromDatabaseText', "text"], result)
    
    return(message)
  }
  
}



