#' Build an Individuals & Moving Range (IMR) Summary Table
#'
#' Aggregates a continuous metric (assumed in **seconds**) to periods
#' (e.g., months) and summarizes:
#' \itemize{
#'   \item \strong{Mean} of individual values
#'   \item \strong{SD} (standard deviation) of individual values
#'   \item \strong{MR Avg}: average \emph{moving range}, defined as the mean of
#'         \eqn{|X_t - X_{t-1}|} within each period
#'   \item \strong{N}: number of observations in the period
#' }
#'
#' Values are formatted as \code{"m:ss"} for durations under one hour or
#' \code{"h:mm:ss"} for longer durations. Optionally de-duplicates by
#' (\code{period}, \code{id_col}) so each case contributes at most once per period.
#' The renderer adapts to the knitting target: HTML (\pkg{gt}), Word/PDF
#' (\pkg{flextable}), or a markdown table fallback.
#'
#' @param df A data frame of observations.
#' @param date_col Character. Column name of a date/time field to define order
#'   and periods. Parsed via \code{lubridate::parse_date_time()} then floored by
#'   \code{time_unit}.
#' @param value_col Character. Name of the numeric column containing the measured
#'   values, in \emph{seconds}.
#' @param id_col Character or \code{NULL}. Optional unique ID used to de-duplicate
#'   within each period (\code{distinct(period, id_col)}).
#' @param time_unit Character. Period for aggregation (e.g., \code{"month"},
#'   \code{"week"}, \code{"quarter"}); passed to \code{lubridate::floor_date()}.
#' @param name Character. Title/caption for the table.
#' @param months_to_show Integer. Number of most recent periods to display.
#' @param start_date,end_date Optional date bounds applied after parsing.
#' @param theme_font Numeric. Base font size for the rendered table.
#' @param sccad_colors Named list (HTML path) with \code{gray} used for table rules.
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
#' @details
#' Moving Range (\code{MR}) is computed on the sequence of individual values
#' within each period after ordering by \code{date_col}. The first MR in each
#' period is \code{NA} (due to lag) and is excluded from the mean via
#' \code{na.rm = TRUE}.
#'
#' @examples
#' \dontrun{
#' table_imr_chart(
#'   df = my_df,
#'   date_col  = "ArrivalTime",
#'   value_col = "DoorToCT_Sec",  # seconds
#'   id_col    = "PCR_Number",
#'   time_unit = "month",
#'   months_to_show = 6,
#'   name = "Door→CT IMR Summary"
#' )
#' }
#'
#' @importFrom dplyr mutate arrange filter distinct group_by summarise ungroup select slice_tail
#' @importFrom lubridate parse_date_time floor_date
#' @importFrom rlang .data
#' @importFrom rmarkdown pandoc_to
#' @importFrom gt gt tab_header md cols_label opt_table_font tab_options px
#' @importFrom flextable flextable fontsize autofit
#' @importFrom knitr kable
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @export
# ------------------------------------------------------------------------------
# table_imr_chart()
# Individuals & Moving Range (IMR) summary table
#
# - Expects value_col in SECONDS.
# - Formats Mean/SD/MR Avg as m:ss or h:mm:ss.
# - Optional ID-based deduplication per period.
# - Outputs gt (HTML), flextable (DOCX/LaTeX), or markdown fallback.
# ------------------------------------------------------------------------------

table_imr_chart <- function(
    df,
    date_col,
    value_col,            # assumed to be in SECONDS
    id_col = NULL,        # if provided, dedup per period & id
    time_unit = "month",
    name = "Individuals & MR Summary Table",
    months_to_show = 6,
    start_date = NULL,
    end_date = NULL,
    theme_font = 10,
    sccad_colors = list(gray = "#E1E1E1")
) {
  # --- libraries ---
  library(dplyr)
  library(lubridate)
  library(gt)
  library(rlang)
  
  # --- helper: format seconds as m:ss or h:mm:ss (coerce to integers for sprintf) ---
  secs_to_hms <- function(x) {
    x <- as.numeric(round(x))
    h <- as.integer(x %/% 3600)
    m <- as.integer((x %% 3600) %/% 60)
    s <- as.integer(x %% 60)
    
    ifelse(
      is.na(x), NA_character_,
      ifelse(
        h > 0L,
        sprintf("%d:%02d:%02d", h, m, s),
        sprintf("%d:%02d", m, s)
      )
    )
  }
  
  # --- detect output type for table renderer selection ---
  output_type <- tryCatch(rmarkdown::pandoc_to(), error = function(e) "unknown")
  is_word  <- identical(output_type, "docx")
  is_html  <- identical(output_type, "html")
  is_latex <- identical(output_type, "latex")
  
  # --- normalize columns and prep ---
  df <- df %>%
    mutate(
      date_var = parse_date_time(.data[[date_col]], orders = c("ymd", "mdy", "dmy")),
      period   = floor_date(date_var, unit = time_unit),
      value_s  = .data[[value_col]]  # keep in SECONDS for averaging
    ) %>%
    arrange(date_var)
  
  # --- optional date window filters ---
  if (!is.null(start_date)) df <- df %>% filter(date_var >= start_date)
  if (!is.null(end_date))   df <- df %>% filter(date_var <= end_date)
  
  # --- optional dedup: by (period, id_col) to match your recent pattern ---
  if (!is.null(id_col)) {
    df <- df %>% distinct(period, .data[[id_col]], .keep_all = TRUE)
  }
  
  # --- compute moving range on individuals (absolute diff between successive values) ---
  df <- df %>%
    filter(!is.na(value_s)) %>%
    group_by(period) %>%             # ensure MR computed within each period sequence if desired
    arrange(date_var, .by_group = TRUE) %>%
    mutate(MR_s = abs(value_s - lag(value_s))) %>%
    ungroup()
  
  # --- summarise by period ---
  summary <- df %>%
    group_by(period) %>%
    summarise(
      Mean_s   = mean(value_s, na.rm = TRUE),
      SD_s     = sd(value_s, na.rm = TRUE),
      MR_Avg_s = mean(MR_s, na.rm = TRUE),
      N        = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      Period = format(period, "%b %Y"),
      Mean   = secs_to_hms(Mean_s),
      SD     = secs_to_hms(SD_s),
      MR_Avg = secs_to_hms(MR_Avg_s)
    ) %>%
    select(Period, Mean, SD, MR_Avg, N) %>%
    slice_tail(n = months_to_show)
  
  # --- if nothing to show, return a tiny placeholder table so the report keeps rendering ---
  if (nrow(summary) == 0) {
    summary <- tibble::tibble(
      Period = character(),
      Mean   = character(),
      SD     = character(),
      MR_Avg = character(),
      N      = integer()
    )
  }
  
  # --- render by output type ---
  if (is_html) {
    return(
      gt(summary) %>%
        tab_header(title = md(paste0("**", name, "**"))) %>%
        cols_label(
          Period = "Date",
          Mean   = "Mean (mm:ss)",
          SD     = "SD (mm:ss)",
          MR_Avg = "MR Avg (mm:ss)",
          N      = "Count"
        ) %>%
        opt_table_font(size = theme_font) %>%
        tab_options(
          table_body.hlines.style = "solid",
          table_body.hlines.color = sccad_colors$gray,
          table_body.hlines.width = px(1)
        )
    )
  } else if (is_word || is_latex) {
    # For Word/LaTeX we print a section header then return a flextable
    library(flextable)
    cat(glue::glue("### {name}\n\n"))
    return(
      flextable::flextable(summary) %>%
        flextable::fontsize(size = theme_font, part = "all") %>%
        flextable::autofit()
    )
  } else {
    # generic fallback
    return(knitr::kable(summary, format = "markdown", caption = name))
  }
}
