#' Read Any Excel or CSV File Robustly
#'
#' Opens Excel (.xlsx, .xls) or CSV (.csv) files using the correct method and
#' returns a clean data frame. Automatically detects the file type, handles
#' character encoding, trims whitespace, and preserves column names.
#'
#' @param path Character. Full path to the file.
#' @param sheet Character or numeric. Sheet name or index for Excel files.
#'   Ignored for CSV files.
#' @param guess_max Integer. Maximum number of rows to use for type guessing.
#'   Passed to \code{readr::read_csv()} for CSVs. Default is 10,000.
#' @param trim_ws Logical. Whether to trim whitespace from character columns.
#'   Default is TRUE.
#' @param na Character vector of strings to interpret as NA values. Default:
#'   \code{c("", "NA", "N/A", "NULL")}.
#' @param verbose Logical. If TRUE, prints diagnostic info (e.g., sheet names,
#'   number of rows). Default FALSE.
#'
#' @return A \code{tibble} (data frame) with cleaned column names and parsed
#'   values. Returns \code{NULL} and a warning if the file cannot be read.
#'
#' @examples
#' \dontrun{
#' df <- read_table("data/Quality_Metrics.xlsx", sheet = "Summary")
#' df2 <- read_table("data/Arrest_BI.csv")
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom readr read_csv
#' @importFrom dplyr mutate_all
#' @importFrom janitor clean_names
#' @importFrom stringr str_trim
#' @export
read_table <- function(
    path,
    sheet = 1,
    guess_max = 10000,
    trim_ws = TRUE,
    na = c("", "NA", "N/A", "NULL"),
    verbose = FALSE
) {
  # --- libraries ---
  library(readr)
  library(readxl)
  library(dplyr)
  library(janitor)
  library(stringr)
  
  # --- validation ---
  if (!file.exists(path)) {
    warning(sprintf("File not found: %s", path))
    return(NULL)
  }
  
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "xlsx", "xls")) {
    warning("Unsupported file extension: ", ext)
    return(NULL)
  }
  
  # --- read file safely ---
  df <- tryCatch({
    if (ext == "csv") {
      if (verbose) message("Reading CSV: ", path)
      readr::read_csv(
        file = path,
        na = na,
        guess_max = guess_max,
        show_col_types = FALSE,
        progress = FALSE
      )
    } else {
      # Excel file
      if (verbose) {
        sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
        if (!is.null(sheets)) message("Excel sheets: ", paste(sheets, collapse = ", "))
      }
      readxl::read_excel(
        path = path,
        sheet = sheet,
        na = na,
        trim_ws = trim_ws
      )
    }
  }, error = function(e) {
    warning(sprintf("Failed to read file '%s': %s", basename(path), e$message))
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  # --- clean names & trim ---
  df <- janitor::clean_names(df)
  if (trim_ws) {
    df <- df %>%
      mutate(across(where(is.character), ~ stringr::str_trim(.x)))
  }
  
  if (verbose) {
    message("Loaded ", nrow(df), " rows and ", ncol(df), " columns.")
  }
  
  return(df)
}
