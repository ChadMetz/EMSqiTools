#' Build a Monthly X-Bar Summary Table (Mean & SD in MM:SS)
#'
#' Aggregates a continuous metric to periods (e.g., months) and produces a
#' summary table containing the per-period count \code{N}, mean, and standard
#' deviation. Values in \code{value_col} are assumed to be **seconds** and are
#' rendered as \code{"MM:SS"} for display. Optionally filters the data by
#' \code{den_col == den_value} before aggregation.
#'
#' Output adapts to the current rendering target:
#' \itemize{
#'   \item HTML: \pkg{gt}
#'   \item Word/PDF: \pkg{flextable}
#'   \item Other/unknown: \pkg{knitr} markdown table
#' }
#'
#' @param df A data frame containing the raw observations.
#' @param date_col Character. Name of the date/time column in \code{df}. Values
#'   are parsed via \code{lubridate::parse_date_time()} then floored to
#'   \code{time_unit}.
#' @param value_col Character. Name of the numeric column (in **seconds**) whose
#'   mean (X̄) and standard deviation will be summarized per period.
#' @param den_col Character or \code{NULL}. Optional column used to filter the
#'   analysis population.
#' @param den_value Optional value; if both \code{den_col} and \code{den_value}
#'   are provided, the data are filtered to \code{den_col == den_value}.
#' @param time_unit Character. Period for aggregation (e.g., \code{"month"},
#'   \code{"week"}, \code{"quarter"}); passed to \code{lubridate::floor_date()}.
#' @param name Character. Title/caption for the table.
#' @param months_to_show Integer. Number of most recent periods to display.
#' @param start_date,end_date Optional date bounds (character or \code{Date})
#'   applied after parsing/rounding to \code{time_unit}.
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
#'   If there is no data in the selected period, returns a small message table
#'   (HTML) or prints a message (Word/PDF/other) and returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' table_x_chart(
#'   df = my_data,
#'   date_col  = "OnSceneTime",
#'   value_col = "SceneToCT_Sec",   # seconds
#'   den_col   = "Population",
#'   den_value = "Stroke",
#'   time_unit = "month",
#'   months_to_show = 6,
#'   name = "Scene→CT Mean & SD"
#' )
#' }
#'
#' @details
#' The function coerces \code{value_col} to numeric and computes per-period
#' mean and SD with \code{na.rm = TRUE}. Displayed mean/SD are formatted as
#' \code{"MM:SS"} via an integer-safe helper. Period labels use \code{\%b \%Y}.
#'
#' @importFrom dplyr mutate filter group_by summarise n select slice_tail everything
#' @importFrom dplyr across
#' @importFrom lubridate parse_date_time floor_date
#' @importFrom rlang .data
#' @importFrom rmarkdown pandoc_to
#' @importFrom gt gt tab_header cols_label cols_align cols_width opt_table_font tab_options px pct
#' @importFrom flextable flextable fontsize autofit
#' @importFrom knitr kable
#' @importFrom glue glue
#' @export
table_x_chart <- function(
    df,
    date_col,
    value_col,
    den_col = NULL,
    den_value = NULL,
    time_unit = "month",
    name = "X-Bar Summary Table",
    months_to_show = 6,
    start_date = NULL,
    end_date = NULL,
    theme_font = 10,
    sccad_colors = list(
      red = "#EB0029",
      dark_blue = "#001689",
      light_blue = "#4A5AC7",
      light_red = "#F25C68",
      gray = "#E1E1E1",
      white = "#FFFFFF"
    )
) {
  library(dplyr)
  library(lubridate)
  library(rlang)

  # integer-safe seconds -> "MM:SS"
  sec_to_mmss <- function(x) {
    s <- as.integer(round(x))
    ifelse(is.na(s), NA_character_, sprintf("%d:%02d", s %/% 60L, s %% 60L))
  }

  # Output target
  output_type <- tryCatch(rmarkdown::pandoc_to(), error = function(e) "unknown")
  is_word <- output_type == "docx"
  is_html <- output_type == "html"
  is_latex <- output_type == "latex"

  # Coerce bounds if character
  if (!is.null(start_date) && !inherits(start_date, "Date")) start_date <- as.Date(start_date)
  if (!is.null(end_date)   && !inherits(end_date, "Date"))   end_date   <- as.Date(end_date)

  df <- df %>%
    mutate(
      !!date_col := parse_date_time(.data[[date_col]], orders = c("ymd", "mdy", "dmy")),
      date = floor_date(as.Date(.data[[date_col]]), unit = time_unit),   # ensure Date, not POSIXct
      !!value_col := as.numeric(.data[[value_col]])                      # ensure numeric
    ) %>%
    filter(!is.na(date))

  if (!is.null(start_date)) df <- df %>% filter(date >= start_date)
  if (!is.null(end_date))   df <- df %>% filter(date <= end_date)

  # Optional filter using den_col/den_value
  if (!is.null(den_col) && !is.null(den_value)) {
    df <- df %>% filter(.data[[den_col]] == den_value)
  }

  summary <- df %>%
    group_by(date) %>%
    summarise(
      Mean_sec = mean(.data[[value_col]], na.rm = TRUE),
      SD_sec   = sd(.data[[value_col]],   na.rm = TRUE),
      N        = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      Date = format(date, "%b %Y"),
      Mean = sec_to_mmss(Mean_sec),   # <-- integer-safe
      SD   = sec_to_mmss(SD_sec)      # <-- integer-safe
    ) %>%
    slice_tail(n = months_to_show) %>%
    select(Date, N, Mean, SD)

  if (nrow(summary) == 0) {
    if (is_html) {
      return(gt::gt(data.frame(Message = "No data available for the selected period.")) %>%
               gt::tab_header(title = md(paste0("**", name, "**"))))
    } else {
      cat(glue::glue("### {name}\n\nNo data available for the selected period.\n"))
      return(invisible(NULL))
    }
  }

  if (is_html) {
    library(gt)
    return(
      gt::gt(summary) %>%
        gt::tab_header(title = name) %>%
        gt::cols_label(
          Date = "Date",
          N    = "Count",
          Mean = "X̄ (min:sec)",
          SD   = "SD (min:sec)"
        ) %>%
        gt::cols_align(align = "center", columns = dplyr::everything()) %>%
        gt::cols_width(
          Date ~ px(100),
          N    ~ px(100),
          Mean ~ px(100),
          SD   ~ px(100)
        ) %>%
        gt::opt_table_font(size = theme_font) %>%
        gt::tab_options(
          table.width = gt::pct(100),
          column_labels.text_align = "center",
          table_body.hlines.style = "solid",
          table_body.hlines.color = sccad_colors$gray,
          table_body.hlines.width = gt::px(1)
        )
    )
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
