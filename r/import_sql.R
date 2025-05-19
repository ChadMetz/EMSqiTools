#' import_sql
#'
#' Run a SQL query from a .sql file against a database server.
#'
#' @param sql_file Path to the .sql file containing the query.
#' @param server Database server name or address.
#' @param database Database name.
#' @param uid User ID (username).
#' @param pwd Password.
#' @param driver ODBC driver name (default: "SQL Server").
#' @return A dataframe with the query result.
#' @examples
#' df <- import_sql("query.sql", server = "my_server", database = "my_db", uid = "username", pwd = "password")
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
