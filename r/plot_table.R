#' plot_table
#'
#' Create a summary table with numerator, denominator, percent, and formatted percent.
#'
#' @param df Dataframe.
#' @param date_col Name of the date column (string).
#' @param id_col Unique identifier column (string).
#' @param num_condition Filter condition for numerator (string, R expression).
#' @param den_condition Filter condition for denominator (string, R expression, default 'TRUE').
#' @param time_unit Time unit for aggregation ('week', 'month', 'quarter').
#' 
#' @return A summary dataframe with: period, numerator, denominator, percent, and formatted percent.
#'
#' @examples
#' table <- plot_table(df, "Incident Date", "pcr", "c02 < 45", "TRUE", "week")

plot_table <- function(df,
                       date_col,
                       id_col,
                       num_condition,
                       den_condition = "TRUE",
                       time_unit = c("week", "month", "quarter")) {
  library(dplyr)
  library(lubridate)
  library(rlang)
  
  time_unit <- match.arg(time_unit)
  
  if (!(date_col %in% names(df))) {
    stop(paste("Column", date_col, "not found in dataframe."))
  }
  
  # Prepare period
  df <- df %>%
    mutate(date_var = as.Date(.data[[date_col]]),
           period = floor_date(date_var, unit = time_unit))
  
  # Calculate Denominator
  den_df <- df %>%
    filter(!!parse_expr(den_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Denominator = n(), .groups = "drop")
  
  # Calculate Numerator
  num_df <- df %>%
    filter(!!parse_expr(num_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Numerator = n(), .groups = "drop")
  
  # Merge and summarize
  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(
      Numerator = coalesce(Numerator, 0),
      Percent = ifelse(Denominator > 0, (Numerator / Denominator) * 100, NA_real_),
      Percent_Formatted = ifelse(!is.na(Percent), paste0(round(Percent, 2), "%"), NA)
    ) %>%
    arrange(period)
  
  return(summary)
}
