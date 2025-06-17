#' Import Spreadsheet
#'
#' Import Spreadsheet function from EMSqiTools package.
#'
#' @param ... Parameters passed to `import_spreadsheet`.
#'
#' @return Output from `import_spreadsheet`.
#' @export

import_spreadsheet <- function(file_path, sheet = NULL) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with install.packages('readr').")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Please install it with install.packages('readxl').")
  }
  
  ext <- tools::file_ext(file_path)
  
  if (ext == "csv") {
    message("Reading CSV file...")
    data <- readr::read_csv(file_path, show_col_types = FALSE)
  } else if (ext %in% c("xls", "xlsx")) {
    sheets <- readxl::excel_sheets(file_path)
    
    if (length(sheets) > 1 && is.null(sheet)) {
      cat("Available sheets:\n")
      for (i in seq_along(sheets)) {
        cat(i, ":", sheets[i], "\n")
      }
      selection <- as.integer(readline(prompt = "Enter the number of the sheet to read: "))
      if (is.na(selection) || selection < 1 || selection > length(sheets)) {
        stop("Invalid sheet selection.")
      }
      sheet <- sheets[selection]
    } else if (is.null(sheet)) {
      sheet <- sheets[1]
    }
    
    message(paste("Reading Excel sheet:", sheet))
    data <- readxl::read_excel(file_path, sheet = sheet)
  } else {
    stop("Unsupported file type. Please provide a .csv, .xls, or .xlsx file.")
  }
  
  return(data)
}
