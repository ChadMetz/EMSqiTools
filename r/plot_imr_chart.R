#' Plot Imr Chart
#'
#' Plot Imr Chart function from EMSqiTools package.
#'
#' @param ... Parameters passed to `plot_imr_chart`.
#'
#' @return Output from `plot_imr_chart`.
#' @export

plot_imr_chart <- function(df, date_col, id_col, value_col,
                           den_condition = "TRUE",
                           time_unit = c("week", "month", "quarter"),
                           name = "I-MR Chart", annotations = NULL) {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(grid)
  library(rlang)
  
  time_unit <- match.arg(time_unit)
  
  df <- df %>%
    mutate(date_var = as.Date(.data[[date_col]]),
           period = floor_date(date_var, time_unit)) %>%
    filter(!!parse_expr(den_condition)) %>%
    group_by(period) %>%
    summarise(value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(period)
  
  df$MR <- c(NA, abs(diff(df$value)))
  
  mean_indiv <- mean(df$value, na.rm = TRUE)
  mr_bar <- mean(df$MR, na.rm = TRUE)
  i_UCL <- mean_indiv + 3 * mr_bar / 1.128
  i_LCL <- mean_indiv - 3 * mr_bar / 1.128
  mr_UCL <- mr_bar + 3 * 0.8525 * mr_bar
  
  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Date = as.Date(Date),
        Side = ifelse(is.null(Side), "right", Side),
        hjust_value = ifelse(Side == "left", 1, 0),
        Y_Pos = ifelse(is.na(Y_Pos), max(df$value, na.rm = TRUE) + 0.5, Y_Pos),
        Shape = ifelse(is.null(Shape), 21, Shape),
        x_adjusted = ifelse(Side == "left", Date - days(1), Date + days(1))
      )
  }
  
  i_chart <- ggplot(df, aes(x = period, y = value)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 3, color = "black") +
    geom_hline(yintercept = mean_indiv, color = "#003DA5", linewidth = 1) +
    geom_hline(yintercept = i_UCL, linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_hline(yintercept = i_LCL, linetype = "dotted", color = "#C8102E", linewidth = 1) +
    labs(title = paste(name, "- Individuals"), x = tools::toTitleCase(time_unit), y = "Value") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12, base_family = "Segoe UI") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      panel.grid.minor = element_blank()
    )
  
  if (!is.null(annotations)) {
    i_chart <- i_chart +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = 0, yend = Y_Pos),
                   color = "black", linetype = "dotted", linewidth = 0.8) +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2.5, fill = "black", color = "black", stroke = 1.2) +
      scale_shape_manual(name = "Event Annotations", values = setNames(annotations$Shape, annotations$Label))
  }
  
  mr_chart <- ggplot(df[-1, ], aes(x = period[-1], y = MR)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 3, color = "black") +
    geom_hline(yintercept = mr_bar, color = "#003DA5", linewidth = 1) +
    geom_hline(yintercept = mr_UCL, linetype = "dotted", color = "#C8102E", linewidth = 1) +
    labs(title = paste(name, "- Moving Range"), x = tools::toTitleCase(time_unit), y = "Moving Range") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12, base_family = "Segoe UI") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      panel.grid.minor = element_blank()
    )
  
  assign("last_qi_plot", list(I_Chart = i_chart, MR_Chart = mr_chart), envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", df, envir = .GlobalEnv)
  
  return(list(I_Chart = i_chart, MR_Chart = mr_chart))
}
