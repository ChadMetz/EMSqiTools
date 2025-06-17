#' Plot Xbar Chart with Annotations
#'
#' Plots an X-bar chart (mean chart) for time-series quality improvement analysis.
#' Supports optional annotations and flexible date parsing.
#'
#' @param df A data frame containing the dataset.
#' @param date_col The name of the date column (character).
#' @param id_col The name of the unique ID column (retained for API consistency).
#' @param value_col The name of the numeric column to calculate means from.
#' @param den_condition A string expression to filter the dataset. Defaults to "TRUE".
#' @param time_unit Aggregation level: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param annotations Optional data frame of annotations with columns: Date, Label, Shape, Y (optional), Side (optional).
#'
#' @return A ggplot object with the rendered chart.
#' @export
plot_xbar_chart <- function(df, date_col, id_col, value_col,
                            den_condition = "TRUE",
                            time_unit = c("week", "month", "quarter"),
                            name = "X-Bar Chart", annotations = NULL) {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(grid)
  library(rlang)

  time_unit <- match.arg(time_unit)

  parse_date_flexibly <- function(dates) {
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%d-%m-%Y", "%d/%m/%Y", "%d %b %Y")
    for (fmt in formats) {
      parsed <- as.Date(dates, format = fmt)
      if (all(!is.na(parsed) | is.na(dates))) return(parsed)
    }
    warning("Could not parse all dates. Returning NAs where format failed.")
    return(as.Date(dates))
  }

  df <- df %>%
    mutate(
      raw_date = .data[[date_col]],
      date_var = parse_date_flexibly(raw_date),
      period = floor_date(date_var, time_unit)
    ) %>%
    filter(!!parse_expr(den_condition))

  summary <- df %>%
    group_by(period) %>%
    summarise(mean_val = mean(.data[[value_col]], na.rm = TRUE), n = n(), .groups = "drop") %>%
    mutate(
      CL = mean(mean_val, na.rm = TRUE),
      UCL = CL + 3 * sd(mean_val, na.rm = TRUE),
      LCL = CL - 3 * sd(mean_val, na.rm = TRUE)
    )

  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Label = as.character(Label),
        Date = as.Date(Date),
        Side = if (!"Side" %in% names(.)) "right" else Side,
        Y_Pos = if (!"Y" %in% names(.)) max(summary$UCL, na.rm = TRUE) + 0.5 else Y,
        Shape = if (!"Shape" %in% names(.)) 21 else Shape,
        hjust_value = ifelse(Side == "left", 1, 0),
        x_adjusted = ifelse(Side == "left", Date - days(1), Date + days(1))
      )
  }

  p <- ggplot(summary, aes(x = period, y = mean_val)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 3, color = "black") +
    geom_hline(aes(yintercept = CL), color = "#003DA5", linewidth = 1) +
    geom_hline(aes(yintercept = UCL), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_hline(aes(yintercept = LCL), linetype = "dotted", color = "#C8102E", linewidth = 1)

  if (!is.null(annotations)) {
    p <- p +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = 0, yend = Y_Pos),
                   color = "black", linetype = "dotted", linewidth = 0.8) +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2.5, fill = "black", color = "black", stroke = 1.2) +
      scale_shape_manual(name = "Event Annotations", values = setNames(annotations$Shape, annotations$Label))
  }

  p <- p +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(color = "#1A1A1A"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(x = tools::toTitleCase(time_unit), y = "Mean Value", title = name)

  grid.newpage()
  grid.draw(ggplotGrob(p))

  assign("last_qi_plot", p, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)

  return(p)
}
