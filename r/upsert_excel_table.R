#' Upsert or Replace a Data Frame into an Excel Sheet as a Proper Table (Safe)
#'
#' Writes a data frame into an Excel workbook as a single Excel Table using
#' \code{openxlsx}. If \code{keys} are provided, performs a row-level upsert
#' against an existing sheet: unions columns, harmonizes classes, and updates or
#' inserts by the key columns. If \code{keys} are \code{NULL} or empty, replaces
#' the sheet with the provided data. The function avoids overlapping filters/tables,
#' scrubs control characters that can corrupt .xlsx XML, and preserves leading zeros
#' in specified text columns.
#'
#' @param df A \code{data.frame} or tibble to write.
#' @param file_name Character. Name of the target .xlsx file (not the full path).
#' @param sheet Character. Desired sheet name. Will be sanitized to a valid
#'   Excel sheet name (max 31 chars; replaces \code{[]:*?/\\} with underscores).
#' @param keys Character vector of column names used to match existing rows for
#'   upsert. If \code{NULL} or length 0, the sheet is replaced instead of upserted.
#' @param folder Character. Folder path where the file lives (or will be created).
#' @param table_style Character. Excel table style name (e.g., \code{"TableStyleMedium9"}).
#' @param force_char_cols Character vector of columns that must be written as text
#'   (useful for IDs like \code{PCR_Number} to preserve leading zeros).
#' @param date_fmt Character date format for \code{Date} columns (default \code{"\%Y-\%m-\%d"}).
#' @param dt_fmt Character datetime format for \code{POSIXt} columns
#'   (default \code{"\%Y-\%m-\%d \%H:\%M:\%S"}).
#'
#' @return (Invisibly) a tibble containing the data written to the sheet.
#'
#' @examples
#' \dontrun{
#' upsert_excel_table(
#'   df = yearly_data_clean,
#'   file_name = "Arrest_All_Years.xlsx",
#'   sheet = "Arrest_All_Years",
#'   keys = c("PCR_Number"),
#'   folder = "C:/.../Quality and Safety - Spreadsheets",
#'   force_char_cols = c("PCR_Number", "Incident Number")
#' )
#'
#' # Replace mode:
#' upsert_excel_table(
#'   df = current_year_data,
#'   file_name = "Arrest_2025.xlsx",
#'   sheet = "Arrest_2025",
#'   keys = NULL
#' )
#' }
#'
#' @importFrom openxlsx loadWorkbook createWorkbook read.xlsx removeWorksheet
#' @importFrom openxlsx addWorksheet writeDataTable freezePane setColWidths saveWorkbook sheets
#' @importFrom dplyr rows_upsert arrange across all_of
#' @importFrom tibble as_tibble
#' @export
upsert_excel_table <- function(
  df,
  file_name,
  sheet,
  keys = NULL,  # if NULL or length 0 -> replace mode
  folder = "C:/Users/cmetz/OneDrive - St. Charles County Ambulance District/Quality and Safety - Spreadsheets",
  table_style = "TableStyleMedium9",
  force_char_cols = character(),
  date_fmt = "%Y-%m-%d",
  dt_fmt   = "%Y-%m-%d %H:%M:%S"
) {
  stopifnot(is.data.frame(df))
  if (!is.null(keys) && length(keys) > 0 && !all(keys %in% names(df))) {
    stop("Key column(s) missing from new data: ",
         paste(setdiff(keys, names(df)), collapse = ", "))
  }

  # --- helpers ---
  to_char_if_temporal <- function(x) {
    if (inherits(x, "POSIXt")) return(format(x, dt_fmt))
    if (inherits(x, "Date"))   return(format(x, date_fmt))
    x
  }
  # Remove XML-hostile control chars (keep \t, \n, \r)
  scrub_for_excel <- function(vec) {
    if (!is.character(vec)) return(vec)
    vec <- iconv(vec, from = "", to = "UTF-8", sub = "")  # drop invalid bytes
    gsub("[\\x00-\\x08\\x0B-\\x0C\\x0E-\\x1F]", "", vec, perl = TRUE)
  }
  sanitize_sheet <- function(x) {
    x <- gsub("[\\[\\]\\*\\:\\?/\\\\]", "_", x)
    substr(x, 1, 31)
  }
  make_table_name <- function(sheet_name) {
    nm <- paste0("T_", gsub("[^A-Za-z0-9_]", "_", sheet_name))
    if (!grepl("^[A-Za-z]", nm)) nm <- paste0("T_", nm)
    substr(nm, 1, 254)
  }

  file_path   <- file.path(folder, file_name)
  sheet_clean <- sanitize_sheet(sheet)

  # Prep new data (dates -> char; force id cols; scrub)
  df_new <- lapply(df, to_char_if_temporal) |>
    as.data.frame(check.names = FALSE, stringsAsFactors = FALSE)
  for (col in intersect(force_char_cols, names(df_new))) df_new[[col]] <- as.character(df_new[[col]])
  for (col in names(df_new)[vapply(df_new, is.character, logical(1))]) {
    df_new[[col]] <- scrub_for_excel(df_new[[col]])
  }

  # Load/create workbook
  wb <- if (file.exists(file_path)) openxlsx::loadWorkbook(file_path) else openxlsx::createWorkbook()
  sheet_exists <- sheet_clean %in% openxlsx::sheets(wb)

  # If upserting, read old & merge; else replace
  if (!is.null(keys) && length(keys) > 0) {
    if (sheet_exists) {
      df_old <- tryCatch(
        openxlsx::read.xlsx(file_path, sheet = sheet_clean, detectDates = FALSE),
        error = function(e) {
          warning("Failed to read existing sheet; starting fresh. Details: ", e$message)
          df_new[0, , drop = FALSE]
        }
      )
      df_old <- lapply(df_old, to_char_if_temporal) |>
        as.data.frame(check.names = FALSE, stringsAsFactors = FALSE)
      for (col in intersect(force_char_cols, names(df_old))) df_old[[col]] <- as.character(df_old[[col]])
      for (col in names(df_old)[vapply(df_old, is.character, logical(1))]) {
        df_old[[col]] <- scrub_for_excel(df_old[[col]])
      }
    } else {
      df_old <- df_new[0, , drop = FALSE]
    }

    # Union columns + harmonize classes
    add_missing_cols <- function(a, cols) {
      missing <- setdiff(cols, names(a)); if (length(missing)) a[missing] <- NA; a
    }
    all_cols <- union(names(df_old), names(df_new))
    df_old <- add_missing_cols(df_old, all_cols)[, all_cols, drop = FALSE]
    df_new <- add_missing_cols(df_new, all_cols)[, all_cols, drop = FALSE]
    for (col in all_cols) {
      if (!identical(class(df_old[[col]]), class(df_new[[col]]))) {
        df_old[[col]] <- as.character(df_old[[col]])
        df_new[[col]] <- as.character(df_new[[col]])
      }
    }

    df_out <- dplyr::rows_upsert(tibble::as_tibble(df_old),
                                 tibble::as_tibble(df_new),
                                 by = keys) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  } else {
    df_out <- tibble::as_tibble(df_new)
  }

  # Clean rewrite to avoid stale table XML
  if (sheet_exists) openxlsx::removeWorksheet(wb, sheet_clean)
  openxlsx::addWorksheet(wb, sheet_clean)

  openxlsx::writeDataTable(
    wb, sheet = sheet_clean, x = df_out,
    tableStyle = table_style,
    tableName  = make_table_name(sheet_clean),
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet = sheet_clean, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_clean, cols = 1:ncol(df_out), widths = "auto")

  openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
  invisible(df_out)
}
