#' Summary Table
#'
#' Summary Table function from EMSqiTools package.
#'
#' @param ... Parameters passed to `summary_table`.
#'
#' @return Output from `summary_table`.
#' @export

summary_table <- function(df, date_col, numerator_col = NULL, denominator_col = NULL,
                          numerator_value = NULL, plot_type = c("proportion", "count", "average"),
                          time_col = NULL, start_date, end_date) {
  library(dplyr)
  library(gt)
  library(lubridate)
  library(scales)
  
  plot_type <- match.arg(plot_type)
  
  df <- df %>%
    mutate(Date = as.Date(parse_date_time(.data[[date_col]], orders = "ymd"))) %>%
    filter(Date >= start_date, Date <= end_date) %>%
    mutate(Year = year(Date), Month = month(Date, label = TRUE))
  
  if (plot_type == "average") {
    if (is.null(time_col)) stop("'time_col' must be provided for plot_type = 'average'")
    
    summary_df <- df %>%
      group_by(Year, Month) %>%
      summarise(
        n = sum(!is.na(.data[[time_col]])),
        `Average Time In Minutes` = round(mean(.data[[time_col]], na.rm = TRUE), 1),
        .groups = "drop"
      )
  } else if (plot_type == "proportion" || plot_type == "count") {
    summary_df <- df %>%
      group_by(Year, Month) %>%
      summarise(
        Numerator = sum(.data[[numerator_col]] == numerator_value, na.rm = TRUE),
        Denominator = sum(!is.na(.data[[denominator_col]]), na.rm = TRUE),
        Proportion = percent(ifelse(Denominator > 0, Numerator / Denominator, NA), accuracy = 1),
        .groups = "drop"
      )
  }
  
  summary_df %>%
    arrange(Year, Month) %>%
    gt() %>%
    opt_row_striping() %>%
    tab_options(table.align = "center", table.font.size = px(12))
}
