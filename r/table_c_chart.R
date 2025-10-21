#' Build a Monthly Count Summary Table for C-Chart Use
#'
#' Aggregates events to periods (e.g., months) and returns a summary table of
#' event counts suitable for a \emph{c-chart}. Rows are optionally filtered by
#' an expression applied to \code{num_col} and de-duplicated by \code{id_col}.
#' Rendering adapts to the current output format:
#' \itemize{
#'   \item HTML: \pkg{gt}
#'   \item Word/PDF: \pkg{flextable}
#'   \item Other/unknown: \pkg{knitr} markdown table
#' }
#'
#' @param df A data frame containing the raw event-level data.
#' @param date_col Character. Name of the date/time column in \code{df}. Values
#'   are parsed via \code{lubridate::parse_date_time()} and then floored to
#'   \code{time_unit}.
#' @param id_col Character. Column name holding a unique ID per case; used to
#'   de-duplicate so each case contributes at most once per period.
#' @param num_col Character. Column to which \code{event_condition} is applied.
#' @param event_condition Character string with an R expression evaluated for
#'   each value of \code{num_col}; use \code{.x} to refer to the value, e.g.:
#'   \code{".x == 1"}, \code{".x %in% c('A','B')"}.
#' @param time_unit Character. Period for aggregation (e.g., \code{"month"},
#'   \code{"week"}, \code{"quarter"}); passed to \code{lubridate::floor_date()}.
#' @param name Character. Title/caption for the table.
#' @param months_to_show Integer. Number of most recent periods to display.
#' @param start_date,end_date Optional bounds (Date or date-like) applied after
#'   parsing.
#' @param theme_font Numeric. Base font size for the rendered table.
#' @param sccad_colors Named list of hex colors used for styling (HTML path).
#'
#' @return A formatted table object depending on the output format detected by
#'   \code{rmarkdown::pandoc_to()}:
#'   \itemize{
#'     \item HTML: a \pkg{gt} table
#'     \item Word/PDF: a \pkg{flextable}
#'     \item Other: a \pkg{knitr} markdown table
#'   }
#'   Invisibly returns that object.
#'
#' @examples
#' \dontrun{
#' table_c_chart(
#'   df = events,
#'   date_col = "IncidentDate",
#'   id_col   = "PCR_Number",
#'   num_col  = "EventCode",
#'   event_condition = ".x %in% c('E1','E2')",
#'   time_unit = "month",
#'   months_to_show = 6,
#'   name = "Medication Error Counts"
#' )
#' }
#'
#' @details
#' The \code{event_condition} string is parsed and evaluated with \code{.x}
#' bound to the values of \code{num_col}. Be sure the expression only relies on
#' \code{.x} and literal constants (e.g., numbers or character vectors).
#'
#' @importFrom dplyr mutate filter distinct group_by summarise select slice_tail n
#' @importFrom lubridate parse_date_time floor_date
#' @importFrom rlang .data as_function parse_expr
#' @importFrom rmarkdown pandoc_to
#' @importFrom gt gt tab_header fmt_number cols_label opt_table_font tab_options px
#' @importFrom flextable flextable fontsize autofit
#' @importFrom knitr kable
#' @export
table_c_chart <- function(
    df,
    date_col,
    id_col,
    num_col,
    event_condition,
    time_unit = "month",
    name = "C Chart Summary Table",
    months_to_show = 6,
    start_date = NULL,
    end_date = NULL,
    theme_font = 10,
    sccad_colors = list(gray = "#E1E1E1")
) {
  library(dplyr)
  library(lubridate)
  library(gt)
  library(rlang)

  output_type <- tryCatch(rmarkdown::pandoc_to(), error = function(e) "unknown")
  is_word <- output_type == "docx"
  is_html <- output_type == "html"
  is_latex <- output_type == "latex"

  # Build a filter function f(.x) from the user-provided expression string
  event_filter <- rlang::as_function(~ eval(rlang::parse_expr(event_condition), envir = list(.x = .x)))

  df <- df %>%
    mutate(
      date = parse_date_time(.data[[date_col]], orders = c("ymd", "mdy", "dmy"))
    ) %>%
    filter(!is.na(date))

  if (!is.null(start_date)) df <- df %>% filter(date >= start_date)
  if (!is.null(end_date))   df <- df %>% filter(date <= end_date)

  # Apply event filter on num_col, then deduplicate by id_col and aggregate
  df_filtered <- df %>%
    filter(event_filter(.data[[num_col]])) %>%
    distinct(.data[[id_col]], .keep_all = TRUE) %>%
    mutate(period = floor_date(date, unit = time_unit))

  summary <- df_filtered %>%
    group_by(period) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Date = format(period, "%b %Y")) %>%
    select(Date, Count) %>%
    slice_tail(n = months_to_show)

  if (is_html) {
    gt_tbl <- gt(summary) %>%
      tab_header(title = name) %>%
      fmt_number(columns = "Count", decimals = 0) %>%
      cols_label(
        Date = "Date",
        Count = "Event Count"
      ) %>%
      opt_table_font(size = theme_font) %>%
      tab_options(
        table_body.hlines.style = "solid",
        table_body.hlines.color = sccad_colors$gray,
        table_body.hlines.width = px(1)
      )

    return(gt_tbl)

  } else if (is_word || is_latex) {
    library(flextable)
    cat(glue::glue("### {name}\n\n"))
    return(
      flextable::flextable(summary) %>%
        flextable::fontsize(size = theme_font, part = "all") %>%
        flextable::autofit()
    )

  } else {
    return(knitr::kable(summary, format = "markdown", caption = name))
  }
}
