#' Import Sql
#'
#' Import Sql function from EMSqiTools package.
#'
#' @param ... Parameters passed to `import_sql`.
#'
#' @return Output from `import_sql`.
#' @export

import_sql <- function(sql_file, server, database, uid, pwd, driver = "SQL Server") {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required. Please install it with install.packages('DBI').")
  }
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("Package 'odbc' is required. Please install it with install.packages('odbc').")
  }
  
  # Read the SQL file
  if (!file.exists(sql_file)) {
    stop(paste("SQL file not found:", sql_file))
  }
  query <- paste(readLines(sql_file), collapse = "\n")
  
  # Connect to the database
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = driver,
    Server = server,
    Database = database,
    UID = uid,
    PWD = pwd,
    Port = 1433
  )
  
  # Run the query
  result <- DBI::dbGetQuery(con, query)
  
  # Disconnect
  DBI::dbDisconnect(con)
  
  message("Query executed successfully. Returned ", nrow(result), " rows.")
  return(result)
}
