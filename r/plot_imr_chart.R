#' Plot I-MR Chart with Annotations
#'
#' Plots an Individuals and Moving Range (I-MR) chart for quality improvement.
#' Supports annotations and cross-platform font consistency.
#'
#' @param df A data frame containing the dataset.
#' @param date_col The name of the date column (character).
#' @param id_col The name of the unique ID column (retained for API consistency).
#' @param value_col The name of the numeric column used for plotting.
#' @param den_condition A string expression to filter the dataset. Defaults to "TRUE".
#' @param time_unit Aggregation level: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param annotations Optional data frame of annotations with columns: Date, Label, Shape, Y (optional), Side (optional).
#'
#' @return A list with two ggplot objects: I_Chart and MR_Chart.
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
        Label = as.character(Label),
        Date = as.Date(Date),
        Side = if (!"Side" %in% names(.)) "right" else Side,
        Y_Pos = if (!"Y" %in% names(.)) max(df$value, na.rm = TRUE) + 0.5 else Y,
        Shape = if (!"Shape" %in% names(.)) 21 else Shape,
        hjust_value = ifelse(Side == "left", 1, 0),
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
    theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  assign("last_qi_plot", list(I_Chart = i_chart, MR_Chart = mr_chart), envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", df, envir = .GlobalEnv)

  return(list(I_Chart = i_chart, MR_Chart = mr_chart))
}
