#' Qi Help
#'
#' EMSqiTools Quick Reference Help Menu
#'
#' Lists key functions for charting, data management, and quality improvement
#' workflows. Run `qi_help()` in the R console to display available functions.
#'
#' @return Prints a categorized list of available functions in EMSqiTools.
#' @export
qi_help <- function() {
  cat("\n==============================================\n")
  cat("         EMSqiTools — Quality Improvement\n")
  cat("==============================================\n")

  cat("\n📈  Charting Functions:\n")
  cat("  • plot_p_chart()          – Proportion (p) Control Chart\n")
  cat("  • plot_c_chart()          – Count (c) Control Chart\n")
  cat("  • plot_x_chart()          – X-Bar (Mean) Control Chart\n")
  cat("  • plot_imr_chart()        – Individuals & Moving Range (I–MR) Chart\n")
  cat("  • plot_histogram()        – Histogram for continuous data\n")
  cat("  • plot_with_nemsis_benchmark() – Control chart with NEMSIS benchmark overlay\n")
  cat("  • table_p_chart(), table_c_chart(), table_x_chart(), table_imr_chart() – Matching summary tables\n")

  cat("\n🧮  Summary & Reporting:\n")
  cat("  • summary_table()         – Generate formatted summary tables\n")
  cat("  • pareto()                – Pareto chart of missing or categorical data\n")
  cat("  • run_measure_by_id()     – Generate charts/tables from YAML-configured measures\n")

  cat("\n🗂️  Data Import & SQL Utilities:\n")
  cat("  • qi_sql()                – Execute SQL query from file\n")
  cat("  • qi_csv()                – Import CSV or Excel file\n")
  cat("  • get_nemsis_benchmark()  – Retrieve NEMSIS benchmark data\n")
  cat("  • get_baseline_measure()  – Retrieve baseline or reference measure\n")

  cat("\n🔍  Data Quality & Validation:\n")
  cat("  • check_missing()         – Identify missing values by column\n")
  cat("  • check_duplicates()      – Detect duplicate rows\n")
  cat("  • check_outliers()        – Flag numeric outliers (IQR method)\n")
  cat("  • describe_data()         – Summary of dataset structure and types\n")
  cat("  • profile_data()          – Data profile report (via DataExplorer)\n")

  cat("\n⚙️  Configuration & Workflow Tools:\n")
  cat("  • load_annotations_yaml() – Load centralized YAML annotations\n")
  cat("  • load_measure_yaml()     – Load measure definitions from YAML\n")
  cat("  • plot_config_defaults()  – Default configuration for Power BI/Quarto charts\n")

  cat("\n💡  Help Wrappers:\n")
  cat("  • qi_help()               – This main help menu\n")
  cat("  • qi_sql_help()           – SQL import & query guide\n")
  cat("  • qi_csv_help()           – CSV import guide\n")
  cat("  • qi_plot_help()          – SPC chart usage examples\n")

  cat("\n==============================================\n")
  cat("Tip: Use ?function_name for detailed documentation.\n")
  cat("Example: ?plot_p_chart\n")
  cat("==============================================\n\n")
}
