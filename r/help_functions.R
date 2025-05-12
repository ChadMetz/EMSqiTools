#' qi_spc_help
#'
#' Help for the qi_spc() and save_qi_spc() functions.
#'
#' @examples
#' qi_spc_help()
qi_spc_help <- function() {
  cat("\n==== QI SPC HELP ====\n")
  cat("qi_spc(): Create SPC chart\n")
  cat("- date_col: date column name (string)\n")
  cat("- id_col: unique identifier column (string)\n")
  cat("- num_condition: filter for numerator (string)\n")
  cat("- den_condition: filter for denominator (string, default 'TRUE')\n")
  cat("- time_unit: 'week', 'month', or 'quarter'\n")
  cat("- name: chart title\n")
  cat("- annotations: optional data.frame with Date, Label, Side, Y_Pos\n")
  cat("- use_arrow: add arrows on vertical lines\n")
  cat("- plot_width, plot_height: plot display size (default 12x3)\n")
  cat("\nsave_qi_spc(): Save last SPC plot\n")
  cat("- path: folder to save in\n")
  cat("- width, height: image size (default 12x3 inches)\n")
}

#' qi_sql_help
#'
#' Help for the qi_sql() function.
#'
#' @examples
#' qi_sql_help()
qi_sql_help <- function() {
  cat("\n==== QI SQL HELP ====\n")
  cat("qi_sql(): Run SQL query from file\n")
  cat("- sql_file: path to .sql file\n")
  cat("- server: database server name\n")
  cat("- database: database name\n")
  cat("- uid: user ID\n")
  cat("- pwd: password\n")
  cat("- driver: ODBC driver (default 'SQL Server')\n")
}

#' qi_csv_help
#'
#' Help for the qi_csv() function.
#'
#' @examples
#' qi_csv_help()
qi_csv_help <- function() {
  cat("\n==== QI CSV HELP ====\n")
  cat("qi_csv(): Import CSV or Excel file\n")
  cat("- file_path: path to .csv, .xls, or .xlsx file\n")
  cat("Returns a dataframe.\n")
}

#' qi_table_help
#'
#' Help for the qi_table() function.
#'
#' @examples
#' qi_table_help()
qi_table_help <- function() {
  cat("\n==== QI TABLE HELP ====\n")
  cat("qi_table(): Create summary table\n")
  cat("- date_col: date column name (string)\n")
  cat("- id_col: unique identifier column (string)\n")
  cat("- num_condition: filter for numerator (string)\n")
  cat("- den_condition: filter for denominator (string, default 'TRUE')\n")
  cat("- time_unit: 'week', 'month', or 'quarter'\n")
}

#' qi_general_help
#'
#' General help listing all package functions.
#'
#' @examples
#' qi_general_help
#'
#' General help listing all package functions.
qi_general_help <- function() {
  cat("\n==== QI TOOLS PACKAGE HELP ====\n")
  cat("- qi_spc(): SPC chart\n")
  cat("- save_qi_spc(): Save SPC plot\n")
  cat("- qi_sql(): Import SQL data\n")
  cat("- qi_csv(): Import CSV or Excel\n")
  cat("- qi_table(): Summary table\n")
  cat("- check_missing(): Check missing data\n")
  cat("- check_duplicates(): Find duplicate rows\n")
  cat("- check_outliers(): Identify outliers in numeric columns\n")
  cat("- describe_data(): Describe dataset\n")
  cat("- profile_data(): Auto-profile report using DataExplorer\n")
  cat("- qi_spc_help(), qi_sql_help(), qi_csv_help(), qi_table_help(): Function-specific help\n")
}
