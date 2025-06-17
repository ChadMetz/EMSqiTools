#' Qi Help
#'
#' Qi Help function from EMSqiTools package.
#'
#' @param ... Parameters passed to `qi_help`.
#'
#' @return Output from `qi_help`.
#' @export

qi_help <- function() {
  cat("\n==== QI TOOLS PACKAGE HELP ====" ,"\n")
  cat("\nCharting Functions:\n")
  cat("- plot_control_chart(): Proportion (P/U) Chart\n")
  cat("- plot_r_chart(): Range Chart (R Chart)\n")
  cat("- plot_c_chart(): Count Chart (C Chart)\n")
  cat("- plot_imr_chart(): I-MR (Individuals & Moving Range) Chart\n")
  cat("- summary_table(): Generate monthly summary tables\n")
  
  cat("\nData Import:\n")
  cat("- qi_sql(): Run SQL query from file\n")
  cat("- qi_csv(): Import CSV or Excel\n")
  
  cat("\nData Checks and Utilities:\n")
  cat("- check_missing(): Check for missing values\n")
  cat("- check_duplicates(): Find duplicate rows\n")
  cat("- check_outliers(): Identify numeric outliers\n")
  cat("- describe_data(): Summary of variables\n")
  cat("- profile_data(): Generate profile report (via DataExplorer)\n")
  
  cat("\nHelp Wrappers:\n")
  cat("- qi_help(): This help menu\n")
  cat("- qi_sql_help(), qi_csv_help(): Function-specific guides\n")
}
