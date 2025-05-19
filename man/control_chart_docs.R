#' X-Bar Chart (Mean Chart)
#' 
#' Plots subgroup means over time to monitor process centering.
#'
#' @param df A data frame containing the data.
#' @param subgroup_col The name of the column indicating subgroup membership.
#' @param value_col The name of the column containing numeric values.
#' @return A ggplot2 object representing the X-Bar chart.
#' @export
#'
#' @examples
#' df <- data.frame(group = rep(1:10, each = 5), val = rnorm(50))
#' plot_xbar_chart(df, subgroup_col = "group", value_col = "val")
plot_xbar_chart <- function(df, subgroup_col, value_col) {
  df <- df %>% group_by(.data[[subgroup_col]]) %>%
    summarise(mean_val = mean(.data[[value_col]], na.rm = TRUE),
              n = n()) %>%
    mutate(CL = mean(mean_val, na.rm = TRUE),
           UCL = CL + 3 * sd(mean_val, na.rm = TRUE),
           LCL = CL - 3 * sd(mean_val, na.rm = TRUE))

  ggplot(df, aes(x = .data[[subgroup_col]], y = mean_val)) +
    geom_line() + geom_point() +
    geom_hline(aes(yintercept = CL), color = 'blue') +
    geom_hline(aes(yintercept = UCL), linetype = "dotted", color = "red") +
    geom_hline(aes(yintercept = LCL), linetype = "dotted", color = "red") +
    labs(title = "X-Bar Chart", x = "Subgroup", y = "Mean Value") +
    theme_minimal()
}

#' R Chart (Range Chart)
#'
#' Plots the range within subgroups over time to monitor process variability.
#'
#' @param df A data frame containing the data.
#' @param subgroup_col The name of the column indicating subgroup membership.
#' @param value_col The name of the column containing numeric values.
#' @return A ggplot2 object representing the R chart.
#' @export
#'
#' @examples
#' df <- data.frame(group = rep(1:10, each = 5), val = rnorm(50))
#' plot_r_chart(df, subgroup_col = "group", value_col = "val")
plot_r_chart <- function(df, subgroup_col, value_col) {
  df <- df %>% group_by(.data[[subgroup_col]]) %>%
    summarise(range_val = max(.data[[value_col]], na.rm = TRUE) -
                            min(.data[[value_col]], na.rm = TRUE)) %>%
    mutate(CL = mean(range_val, na.rm = TRUE),
           UCL = CL + 3 * sd(range_val, na.rm = TRUE),
           LCL = pmax(0, CL - 3 * sd(range_val, na.rm = TRUE)))

  ggplot(df, aes(x = .data[[subgroup_col]], y = range_val)) +
    geom_line() + geom_point() +
    geom_hline(aes(yintercept = CL), color = 'blue') +
    geom_hline(aes(yintercept = UCL), linetype = "dotted", color = "red") +
    geom_hline(aes(yintercept = LCL), linetype = "dotted", color = "red") +
    labs(title = "R Chart", x = "Subgroup", y = "Range") +
    theme_minimal()
}

#' C Chart (Count Chart)
#'
#' Tracks the count of defects per unit when the sample size is constant.
#'
#' @param df A data frame containing the data.
#' @param time_col The name of the column indicating the order or time sequence.
#' @param count_col The name of the column containing count data.
#'
#' @return A ggplot2 object representing the C chart.
#' @export
#'
#' @examples
#' df <- data.frame(week = 1:20, defects = rpois(20, lambda = 3))
#' plot_c_chart(df, time_col = "week", count_col = "defects")
plot_c_chart <- function(df, time_col, count_col) {
  df <- df %>% mutate(CL = mean(.data[[count_col]], na.rm = TRUE),
                      UCL = CL + 3 * sqrt(CL),
                      LCL = pmax(0, CL - 3 * sqrt(CL)))

  ggplot(df, aes(x = .data[[time_col]], y = .data[[count_col]])) +
    geom_line() + geom_point() +
    geom_hline(aes(yintercept = CL), color = 'blue') +
    geom_hline(aes(yintercept = UCL), linetype = "dotted", color = "red") +
    geom_hline(aes(yintercept = LCL), linetype = "dotted", color = "red") +
    labs(title = "C Chart", x = "Time", y = "Count") +
    theme_minimal()
}

#' I-MR Chart (Individuals-Moving Range Chart)
#'
#' Plots both the individual data values and the moving ranges over time.
#'
#' @param df A data frame containing the data.
#' @param value_col The name of the column with individual numeric observations.
#' @param time_col The name of the column indicating time/order.
#'
#' @return A list with two ggplot2 plots: I Chart and MR Chart.
#' @export
#'
#' @examples
#' df <- data.frame(order = 1:20, value = rnorm(20))
#' charts <- imr_chart(df, value_col = "value", time_col = "order")
#' charts$I_Chart
#' charts$MR_Chart
plot_imr_chart <- function(df, value_col, time_col) {
  df <- df %>% arrange(.data[[time_col]])
  df$MR <- c(NA, abs(diff(df[[value_col]])))

  mean_indiv <- mean(df[[value_col]], na.rm = TRUE)
  mr_bar <- mean(df$MR, na.rm = TRUE)

  indiv_plot <- ggplot(df, aes(x = .data[[time_col]], y = .data[[value_col]])) +
    geom_line() + geom_point() +
    geom_hline(yintercept = mean_indiv, color = 'blue') +
    geom_hline(yintercept = mean_indiv + 3 * mr_bar / 1.128, linetype = "dotted", color = "red") +
    geom_hline(yintercept = mean_indiv - 3 * mr_bar / 1.128, linetype = "dotted", color = "red") +
    labs(title = "Individuals Chart", x = "Time", y = "Value") +
    theme_minimal()

  mr_plot <- ggplot(df[-1,], aes(x = .data[[time_col]][-1], y = MR)) +
    geom_line() + geom_point() +
    geom_hline(yintercept = mr_bar, color = 'blue') +
    geom_hline(yintercept = mr_bar + 3 * 0.8525 * mr_bar, linetype = "dotted", color = "red") +
    labs(title = "Moving Range Chart", x = "Time", y = "Moving Range") +
    theme_minimal()

  list(I_Chart = indiv_plot, MR_Chart = mr_plot)
}
