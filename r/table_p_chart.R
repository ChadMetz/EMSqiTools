#' Build a Monthly Proportion Summary Table (HTML/Word/PDF/Markdown)
#'
#' Aggregates a dataset to monthly (or other) periods and produces a summary
#' table of \emph{Numerator}, \emph{Denominator}, and \emph{Proportion}
#' (Numerator/Denominator). Optionally joins a national benchmark series and
#' renders the last \code{months_to_show} periods using:
#' \itemize{
#'   \item HTML: \pkg{gt}
#'   \item Word/PDF: \pkg{flextable}
#'   \item Other/unknown: \pkg{knitr} markdown table
#' }
#'
#' The function supports optional de-duplication by an ID column at the
#' period level (distinct by \code{date} + \code{id_col}) so that multiple
#' rows for the same case do not inflate counts.
#'
#' @param df A data frame containing the raw event-level data.
#' @param date_col Character. Name of the date/time column in \code{df}.
#'   Accepted formats are parsed by \code{lubridate::parse_date_time()}.
#' @param id_col Character or \code{NULL}. Optional unique ID column used to
#'   de-duplicate counts per period (distinct by period + ID). If \code{NULL},
#'   counts are simple row counts per period.
#' @param num_col Character. Column used to flag numerator events.
#' @param num_value Vector of values in \code{num_col} that count toward the
#'   numerator (e.g., \code{1} or c("Yes","Y")).
#' @param den_col Character. Column used to filter the denominator population.
#' @param den_value Vector or \code{NULL}. If provided, only rows where
#'   \code{den_col} is in \code{den_value} are included in the denominator.
#'   If \code{NULL}, all rows are eligible for the denominator (subject to
#'   other filters).
#' @param time_unit Character. Aggregation unit passed to
#'   \code{lubridate::floor_date()} (e.g., \code{"month"}, \code{"week"},
#'   \code{"quarter"}).
#' @param name Character. Title/caption for the table.
#' @param months_to_show Integer. Number of most recent periods to display.
#' @param start_date,end_date Optional \code{Date} (or date-like) bounds applied
#'   after parsing/rounding to \code{time_unit}.
#' @param theme_font Numeric. Base font size for the rendered table.
#' @param benchmark_df Optional data frame with columns:
#'   \itemize{
#'     \item \code{date}: \code{Date}/POSIXct to align with the aggregated periods
#'     \item \code{rate}: numeric proportion (0–1) for the benchmark
#'   }
#'   These are aggregated (mean) to the same \code{time_unit} and joined as
#'   column \code{"National Proportion"}.
#' @param annotations Reserved for compatibility (not used in this helper).
#' @param return_table Logical. If \code{TRUE}, returns the underlying data
#'   frame for the rendered table (last \code{months_to_show} periods) instead
#'   of a formatted object.
#' @param sccad_colors Named list of hex colors used for styling (HTML path).
#'
#' @return If \code{return_table = TRUE}, a data frame with columns
#'   \code{Date}, \code{Denominator}, \code{Numerator}, \code{Proportion}
#'   (and optionally \code{National Proportion}). Otherwise, a \pkg{gt}
#'   table (HTML), a \pkg{flextable} (Word/PDF), or a \pkg{knitr} markdown
#'   table depending on the current output format detected by
#'   \code{rmarkdown::pandoc_to()}.
#'
#' @examples
#' \dontrun{
#' tbl <- table_p_chart(
#'   df = my_data,
#'   date_col = "CallDate",
#'   id_col   = "PCR_Number",
#'   num_col  = "Aspirin_Admin",
#'   num_value = 1,
#'   den_col   = "Incident_Internal",
#'   den_value = 1,
#'   time_unit = "month",
#'   months_to_show = 6,
#'   name = "Aspirin on Arrival"
#' )
#' }
#'
#' @importFrom dplyr mutate filter group_by summarise left_join distinct n arrange slice_tail select
#' @importFrom dplyr across all_of
#' @importFrom lubridate parse_date_time floor_date
#' @importFrom rlang .data
#' @importFrom rmarkdown pandoc_to
#' @importFrom gt gt tab_header tab_style cell_text cells_title fmt_percent fmt_number cols_label opt_table_font tab_options px
#' @importFrom flextable flextable fontsize autofit
#' @importFrom knitr kable
#' @importFrom glue glue
#' @export
table_p_chart <- function(df,
                          date_col,
                          id_col = NULL,
                          num_col,
                          num_value,
                          den_col,
                          den_value = NULL,
                          time_unit = "month",
                          name = "Summary Table",
                          months_to_show = 6,
                          start_date = NULL,
                          end_date = NULL,
                          theme_font = 8,
                          benchmark_df = NULL,
                          annotations = NULL,
                          return_table = FALSE,
                          sccad_colors = list(
                            red = "#EB0029",
                            dark_blue = "#001689",
                            light_blue = "#4A5AC7",
                            light_red = "#F25C68",
                            gray = "#E1E1E1",
                            white = "#FFFFFF"
                          )) {
  library(dplyr)
  library(lubridate)
  library(rlang)

  output_type <- tryCatch(rmarkdown::pandoc_to(), error = function(e) "unknown")
  is_word <- output_type == "docx"
  is_html <- output_type == "html"
  is_latex <- output_type == "latex"

  df <- df %>%
    mutate(
      date_parsed = parse_date_time(.data[[date_col]], orders = c("ymd", "mdy", "dmy")),
      date = floor_date(date_parsed, unit = time_unit)
    ) %>%
    filter(!is.na(date))

  if (!is.null(start_date)) df <- df %>% filter(date >= as.Date(start_date))
  if (!is.null(end_date))   df <- df %>% filter(date <= as.Date(end_date))
  if (!is.null(den_value))  df <- df %>% filter(.data[[den_col]] %in% den_value)

  if (!is.null(id_col)) {
    den_df <- df %>%
      filter(!is.na(.data[[id_col]])) %>%
      distinct(date, .data[[id_col]]) %>%
      group_by(date) %>%
      summarise(Denominator = n(), .groups = "drop")

    num_df <- df %>%
      filter(.data[[num_col]] %in% num_value) %>%
      filter(!is.na(.data[[id_col]])) %>%
      distinct(date, .data[[id_col]]) %>%
      group_by(date) %>%
      summarise(Numerator = n(), .groups = "drop")
  } else {
    den_df <- df %>%
      group_by(date) %>%
      summarise(Denominator = n(), .groups = "drop")

    num_df <- df %>%
      filter(.data[[num_col]] %in% num_value) %>%
      group_by(date) %>%
      summarise(Numerator = n(), .groups = "drop")
  }

  df <- den_df %>%
    left_join(num_df, by = "date") %>%
    mutate(
      Numerator = ifelse(is.na(Numerator), 0, Numerator),
      Proportion = Numerator / Denominator
    )

  has_benchmark <- !is.null(benchmark_df)

  if (has_benchmark) {
    benchmark_df <- benchmark_df %>%
      mutate(date = floor_date(date, unit = time_unit)) %>%
      group_by(date) %>%
      summarise(`National Proportion` = mean(rate, na.rm = TRUE), .groups = "drop")

    df <- df %>% left_join(benchmark_df, by = "date")
  }

  df <- df %>%
    mutate(
      Date_fmt = date,
      Date = format(date, "%b %Y")
    )

  if (has_benchmark) {
    df <- df %>% select(Date_fmt, Date, Denominator, Numerator, Proportion, `National Proportion`)
  } else {
    df <- df %>% select(Date_fmt, Date, Denominator, Numerator, Proportion)
  }

  if (!is.null(end_date)) df <- df %>% filter(Date_fmt <= as.Date(end_date))

  df_filtered <- df %>%
    arrange(Date_fmt) %>%
    slice_tail(n = months_to_show) %>%
    select(-Date_fmt)

  if (return_table) return(df_filtered)

  if (is_html) {
    library(gt)
    gt_tbl <- gt::gt(df_filtered) %>%
      gt::tab_header(title = name) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::fmt_percent(columns = "Proportion", decimals = 1) %>%
      gt::fmt_number(columns = c("Denominator", "Numerator"), decimals = 0)

    if (has_benchmark) {
      gt_tbl <- gt_tbl %>%
        gt::fmt_percent(columns = "National Proportion", decimals = 1) %>%
        gt::cols_label(
          Date = "Date",
          Denominator = "Denominator",
          Numerator = "Numerator",
          Proportion = "Proportion",
          `National Proportion` = "National Proportion"
        )
    } else {
      gt_tbl <- gt_tbl %>%
        gt::cols_label(
          Date = "Date",
          Denominator = "Denominator",
          Numerator = "Numerator",
          Proportion = "Proportion"
        )
    }

    return(
      gt_tbl %>%
        gt::opt_table_font(size = theme_font) %>%
        gt::tab_options(
          table.border.top.width = gt::px(0),
          table_body.hlines.style = "solid",
          table_body.hlines.color = sccad_colors$gray,
          table_body.hlines.width = gt::px(1)
        )
    )

  } else if (is_word || is_latex) {
    library(flextable)
    df_flex <- df_filtered %>%
      mutate(Proportion = sprintf("%.1f%%", Proportion * 100))

    if (has_benchmark) {
      df_flex <- df_flex %>%
        mutate(`National Proportion` = sprintf("%.1f%%", `National Proportion` * 100))
    }

    cat(glue::glue("### {name}\n\n"))

    return(
      flextable::flextable(df_flex) %>%
        flextable::fontsize(size = theme_font, part = "all") %>%
        flextable::autofit()
    )

  } else {
    df_md <- df_filtered %>%
      mutate(Proportion = sprintf("%.1f%%", Proportion * 100))

    if (has_benchmark) {
      df_md <- df_md %>%
        mutate(`National Proportion` = sprintf("%.1f%%", `National Proportion` * 100))
    }

    return(knitr::kable(df_md, format = "markdown", caption = name))
  }
}
