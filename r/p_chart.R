#' P Chart (Proportion Chart)
#'
#' Monitors the proportion of successful outcomes in a sample over time.
#'
#' @param df A data frame.
#' @param date_col A string name of the date column.
#' @param id_col A string name of the unique identifier column.
#' @param numerator_condition Logical condition as string (e.g. "Vitals_Taken == 1").
#' @param denominator_condition Logical condition as string (e.g. "TRUE").
#' @param annotations Optional data frame of event annotations.
#' @return A ggplot2 object representing the P Chart.
#' @export
#'
#' @examples
#' p_chart(df, date_col = "Week", id_col = "PCR_Number",
#'         numerator_condition = "Vitals_Taken == 1", denominator_condition = "TRUE")
p_chart <- function(df, date_col, id_col, numerator_condition, denominator_condition, annotations = NULL) {
  df <- df %>%
    dplyr::mutate(
      Year = lubridate::year(.data[[date_col]]),
      Week_of_Year = lubridate::isoweek(.data[[date_col]]),
      Numerator = ifelse(eval(parse(text = numerator_condition)), 1, 0),
      Eligible = ifelse(eval(parse(text = denominator_condition)), 1, 0)
    )

  summary_df <- df %>%
    dplyr::group_by(Year, Week_of_Year) %>%
    dplyr::summarise(
      Total = sum(Eligible, na.rm = TRUE),
      Success = sum(Numerator, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      p = ifelse(Total > 0, Success / Total, NA),
      Week = as.Date(paste0(Year, '-01-01')) + lubridate::weeks(Week_of_Year - 1),
      CL = mean(p, na.rm = TRUE),
      UCL = pmin(CL + 3 * sqrt((CL * (1 - CL)) / Total), 1),
      LCL = pmax(CL - 3 * sqrt((CL * (1 - CL)) / Total), 0)
    )

  ggplot2::ggplot(summary_df, ggplot2::aes(x = Week, y = p)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = CL), color = 'blue') +
    ggplot2::geom_line(ggplot2::aes(y = UCL), linetype = 'dotted', color = 'red') +
    ggplot2::geom_line(ggplot2::aes(y = LCL), linetype = 'dotted', color = 'red') +
    ggplot2::labs(title = 'P Chart', x = 'Week', y = 'Proportion') +
    ggplot2::theme_minimal()
}
