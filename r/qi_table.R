#' qi_table
#'
#' Create a summary table with numerator, denominator, percent, and formatted percent.
#'
#' @param df Dataframe.
#' @param date_col Name of the date column (string).
#' @param id_col Unique identifier column (string).
#' @param num_condition Filter condition for numerator (string, R expression).
#' @param den_condition Filter condition for denominator (default 'TRUE').
#' @param time_unit Time unit for aggregation ('week', 'month', 'quarter').
#' @return A summary dataframe.
#' @examples
#' table <- qi_table(df, "Incident Date", "pcr", "c02 < 45", "TRUE", "week")
qi_table <- function(df, date_col, id_col, num_condition, den_condition = "TRUE",
                     time_unit = c("week", "month", "quarter")) {
  time_unit <- match.arg(time_unit)
  
  df <- df %>%
    mutate(date_var = as.Date(.data[[date_col]]),
           period = floor_date(date_var, time_unit))
  
  # Denominator
  den_df <- df %>%
    filter(!! rlang::parse_expr(den_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Denominator = n(), .groups = "drop")
  
  # Numerator
  num_df <- df %>%
    filter(!! rlang::parse_expr(num_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Numerator = n(), .groups = "drop")
  
  # Merge and calculate percent
  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator),
           Percent = ifelse(Denominator > 0, Numerator / Denominator * 100, NA),
           Percent_Formatted = paste0(round(Percent, 2), "%")) %>%
    arrange(period)
  
  return(summary)
}
