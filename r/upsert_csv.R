#' Upsert Rows from a Data Frame into a CSV by Key Columns (Robust)
#'
#' Performs a row-level upsert of \code{df_new} into a CSV at \code{path} by the
#' specified \code{keys}. The function preserves headers, formats dates/times as
#' text for stability, unions columns, harmonizes classes (fallback to character
#' on conflict), scrubs control characters, and forces specified columns to text
#' (useful for IDs with leading zeros). Optionally adds a run timestamp column.
#'
#' @param df_new A \code{data.frame} or tibble with new/updated rows to upsert.
#' @param path Character. Full file path to the target CSV.
#' @param keys Character vector of column names used to match existing rows for upsert.
#' @param add_run_ts Logical. If \code{TRUE}, add a run timestamp column when missing.
#' @param run_ts_col Character. Name of the run timestamp column to add (default \code{"Query_Run_Timestamp"}).
#' @param run_ts POSIXct. Timestamp value to record when \code{add_run_ts = TRUE}
#'   (default \code{Sys.time()}).
#' @param date_fmt Character date format for \code{Date} columns (default \code{"\%Y-\%m-\%d"}).
#' @param dt_fmt Character datetime format for \code{POSIXt} columns
#'   (default \code{"\%Y-\%m-\%d \%H:\%M:\%S"}).
#' @param force_char_cols Character vector of columns that must be written as text
#'   (e.g., IDs).
#'
#' @return (Invisibly) a tibble containing the upserted CSV contents.
#'
#' @examples
#' \dontrun{
#' upsert_csv(
#'   df_new = yearly_data_clean,
#'   path   = "C:/.../BI - CSV/Arrest-BI-V3.csv",
#'   keys   = c("PCR_Number"),
#'   force_char_cols = c("PCR_Number", "Incident Number")
#' )
#' }
#'
#' @importFrom dplyr rows_upsert arrange across all_of as_tibble
#' @importFrom utils read.csv write.csv
#' @export
upsert_csv <- function(
  df_new,
  path,
  keys,
  add_run_ts = FALSE,
  run_ts_col = "Query_Run_Timestamp",
  run_ts = Sys.time(),
  date_fmt = "%Y-%m-%d",
  dt_fmt   = "%Y-%m-%d %H:%M:%S",
  force_char_cols = character()
) {
  stopifnot(is.data.frame(df_new))
  if (!all(keys %in% names(df_new))) {
    stop("Key column(s) missing from new data: ",
         paste(setdiff(keys, names(df_new)), collapse = ", "))
  }

  # Helpers
  to_char_if_temporal <- function(x) {
    if (inherits(x, "POSIXt")) return(format(x, dt_fmt))
    if (inherits(x, "Date"))   return(format(x, date_fmt))
    x
  }
  scrub_for_csv <- function(vec) {
    if (!is.character(vec)) return(vec)
    vec <- iconv(vec, from = "", to = "UTF-8", sub = "")   # sanitize encoding
    gsub("[\\x00-\\x08\\x0B-\\x0C\\x0E-\\x1F]", "", vec, perl = TRUE)
  }

  # Optional run timestamp
  if (add_run_ts && !run_ts_col %in% names(df_new)) {
    df_new[[run_ts_col]] <- format(run_ts, dt_fmt)
  }

  # Load old (or empty)
  df_old <- if (file.exists(path)) {
    tryCatch(
      utils::read.csv(path, na.strings = c("", "NA"),
                      stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) stop("Failed to read existing CSV: ", e$message)
    )
  } else {
    df_new[0, , drop = FALSE]
  }

  # Union columns
  add_missing_cols <- function(a, cols) {
    missing <- setdiff(cols, names(a)); if (length(missing)) a[missing] <- NA; a
  }
  all_cols <- union(names(df_old), names(df_new))
  df_old <- add_missing_cols(df_old, all_cols)[, all_cols, drop = FALSE]
  df_new <- add_missing_cols(df_new, all_cols)[, all_cols, drop = FALSE]

  # Temporal -> char; scrub; force char cols (incl. keys + run_ts_col)
  df_old <- lapply(df_old, to_char_if_temporal) |> as.data.frame(check.names = FALSE, stringsAsFactors = FALSE)
  df_new <- lapply(df_new, to_char_if_temporal) |> as.data.frame(check.names = FALSE, stringsAsFactors = FALSE)

  force_cols <- unique(c(keys, force_char_cols, run_ts_col))
  for (col in intersect(force_cols, names(df_old))) df_old[[col]] <- as.character(df_old[[col]])
  for (col in intersect(force_cols, names(df_new))) df_new[[col]] <- as.character(df_new[[col]])

  for (col in names(df_old)[vapply(df_old, is.character, logical(1))]) df_old[[col]] <- scrub_for_csv(df_old[[col]])
  for (col in names(df_new)[vapply(df_new, is.character, logical(1))]) df_new[[col]] <- scrub_for_csv(df_new[[col]])

  # Harmonize column classes if they differ (fallback to character)
  for (col in all_cols) {
    if (!identical(class(df_old[[col]]), class(df_new[[col]]))) {
      df_old[[col]] <- as.character(df_old[[col]])
      df_new[[col]] <- as.character(df_new[[col]])
    }
  }

  # Upsert
  df_out <- dplyr::rows_upsert(dplyr::as_tibble(df_old),
                               dplyr::as_tibble(df_new),
                               by = keys)

  # Order by keys and write
  df_out <- dplyr::arrange(df_out, dplyr::across(dplyr::all_of(keys)))
  utils::write.csv(df_out, file = path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(df_out)
}
