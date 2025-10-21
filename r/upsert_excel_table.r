#' Safely upsert/replace a data frame into an Excel sheet as a Table (no XML corruption)
#'
#' @param df                data.frame or tibble to write
#' @param file_path         full path to the .xlsx file
#' @param sheet             desired sheet name (will be sanitized to Excel rules)
#' @param keys              character vector of column names to match rows for upsert.
#'                          If NULL or length 0, REPLACE mode: the sheet is replaced
#'                          with df (no merge with prior data).
#' @param table_style       Excel table style (e.g. "TableStyleMedium9")
#' @param force_char_cols   columns that must be written as text to preserve leading zeros, etc.
#' @param date_fmt          format for Date columns
#' @param dt_fmt            format for POSIXt columns
#'
#' @return (invisible) tibble written to the sheet
#' @import openxlsx dplyr tibble
upsert_excel_table <- function(
  df,
  file_path,
  sheet,
  keys = NULL,
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
  # Remove XML-hostile control characters that crash Excel (0x00–0x08, 0x0B–0x0C, 0x0E–0x1F)
  scrub_for_excel <- function(vec) {
    if (!is.character(vec)) return(vec)
    gsub("[\x00-\x08\x0B-\x0C\x0E-\x1F]", "", vec, perl = TRUE)
  }
  sanitize_sheet <- function(x) {
    x <- gsub("[\\[\\]\\*\\:\\?/\\\\]", "_", x)
    substr(x, 1, 31)
  }
  # Ensure a valid, workbook-unique-ish table name
  make_table_name <- function(sheet_name) {
    nm <- paste0("T_", gsub("[^A-Za-z0-9_]", "_", sheet_name))
    if (!grepl("^[A-Za-z]", nm)) nm <- paste0("T_", nm)
    substr(nm, 1, 254) # Excel limit 255
  }

  sheet_clean <- sanitize_sheet(sheet)

  # Prepare new data (dates/times -> char; protect ID cols; scrub chars)
  df_new <- lapply(df, to_char_if_temporal) |>
    as.data.frame(check.names = FALSE, stringsAsFactors = FALSE)

  # Force specified cols to character (preserve leading zeros)
  force_cols <- unique(force_char_cols)
  for (col in intersect(force_cols, names(df_new))) {
    df_new[[col]] <- as.character(df_new[[col]])
  }
  # Scrub character columns
  char_cols_new <- names(df_new)[vapply(df_new, is.character, logical(1))]
  for (col in char_cols_new) df_new[[col]] <- scrub_for_excel(df_new[[col]])

  # Load or create workbook
  wb <- if (file.exists(file_path)) openxlsx::loadWorkbook(file_path) else openxlsx::createWorkbook()

  # If keys provided: read old, union, harmonize, upsert. Else replace.
  sheet_exists <- sheet_clean %in% openxlsx::sheets(wb)
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
      for (col in intersect(force_cols, names(df_old))) df_old[[col]] <- as.character(df_old[[col]])
      char_cols_old <- names(df_old)[vapply(df_old, is.character, logical(1))]
      for (col in char_cols_old) df_old[[col]] <- scrub_for_excel(df_old[[col]])
    } else {
      df_old <- df_new[0, , drop = FALSE]
    }

    # Union columns
    add_missing_cols <- function(a, cols) {
      missing <- setdiff(cols, names(a))
      if (length(missing)) a[missing] <- NA
      a
    }
    all_cols <- union(names(df_old), names(df_new))
    df_old <- add_missing_cols(df_old, all_cols)[, all_cols, drop = FALSE]
    df_new <- add_missing_cols(df_new, all_cols)[, all_cols, drop = FALSE]

    # Harmonize classes (if mismatch -> character)
    for (col in all_cols) {
      if (!identical(class(df_old[[col]]), class(df_new[[col]]))) {
        df_old[[col]] <- as.character(df_old[[col]])
        df_new[[col]] <- as.character(df_new[[col]])
      }
    }

    # Upsert
    df_out <- dplyr::rows_upsert(tibble::as_tibble(df_old),
                                 tibble::as_tibble(df_new),
                                 by = keys) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  } else {
    # Replace mode: write df_new as-is
    df_out <- tibble::as_tibble(df_new)
  }

  # Clean (re)write sheet to avoid stale table XML
  if (sheet_exists) openxlsx::removeWorksheet(wb, sheet_clean)
  openxlsx::addWorksheet(wb, sheet_clean)

  # Write exactly once as a proper Table; it adds its own filter
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
