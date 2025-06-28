#' Plot Individuals and Moving Range (I-MR) Control Chart with Stepwise Limits
#'
#' This function plots an I-MR control chart for continuous data. It applies
#' Western Electric Rule 2 (8+ points on one side of center) to identify shifts
#' and updates control limits accordingly. It returns a faceted ggplot2 object
#' or a summary table.
#'
#' @param df Data frame with your data
#' @param date_col Name of the date column
#' @param value_col Name of the numeric column to chart
#' @param time_unit Aggregation: "week", "month", or "quarter"
#' @param name Title of the chart
#' @param annotations Optional data frame with Date, Label, Shape, Y, Side
#' @param plot_width Not used (for future sizing)
#' @param plot_height Not used
#' @param drop_invalid_dates Drop rows with invalid date formats?
#' @param return_table Return the summary table instead of plotting?
#'
#' @return A ggplot object or summary table
#' @export
plot_imr_chart <- function(
  df,
  date_col,
  value_col,
  time_unit = c("week", "month", "quarter"),
  name = "Individuals & MR Control Chart",
  annotations = NULL,
  plot_width = 12,
  plot_height = 4,
  drop_invalid_dates = FALSE,
  return_table = FALSE
) {
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(grid)
  library(scales)

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
      period = floor_date(date_var, unit = time_unit),
      value = .data[[value_col]]
    ) %>%
    arrange(date_var)

  if (any(is.na(df$date_var))) {
    if (drop_invalid_dates) {
      df <- df %>% filter(!is.na(date_var))
    } else {
      stop("Some dates could not be parsed. Use drop_invalid_dates = TRUE to ignore.")
    }
  }

  df <- df %>%
    mutate(MR = abs(value - lag(value)))

  summary <- df %>%
    select(date_var, period, value, MR) %>%
    filter(!is.na(value))

  ## --- Individuals Chart Step Limits ---
  side <- summary$value > mean(summary$value, na.rm = TRUE)
  rle_obj <- rle(side)
  lens <- rle_obj$lengths
  shift_points <- cumsum(lens)[lens >= 8]
  shift_starts <- shift_points - lens[which(lens >= 8)] + 1
  shift_starts <- sort(unique(c(1, shift_starts)))

  summary$CL_I <- NA
  summary$UCL_I <- NA
  summary$LCL_I <- NA

  for (i in seq_along(shift_starts)) {
    start_idx <- shift_starts[i]
    end_idx <- if (i < length(shift_starts)) shift_starts[i + 1] - 1 else nrow(summary)
    segment <- summary[start_idx:end_idx, ]
    cl <- mean(segment$value, na.rm = TRUE)
    mr_avg <- mean(segment$MR, na.rm = TRUE)
    sigma_est <- mr_avg / 1.128  # d2 for n=2

    summary$CL_I[start_idx:end_idx] <- cl
    summary$UCL_I[start_idx:end_idx] <- cl + 3 * sigma_est
    summary$LCL_I[start_idx:end_idx] <- cl - 3 * sigma_est
  }

  ## --- MR Chart Limits ---
  mr_avg_all <- mean(summary$MR, na.rm = TRUE)
  UCL_MR <- mr_avg_all * 3.267  # D4 for n=2

  summary <- summary %>%
    mutate(CL_MR = mr_avg_all, UCL_MR = UCL_MR)

  if (return_table) return(summary)

  ## --- Plot Individuals + MR ---
  p1 <- ggplot(summary, aes(x = date_var, y = value)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 2.5) +
    geom_line(aes(y = CL_I), color = "#003DA5", linewidth = 1) +
    geom_line(aes(y = UCL_I), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_line(aes(y = LCL_I), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    labs(y = "Value", x = "Date", title = paste(name, "(Individuals)"))

  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Date = as.Date(Date),
        Side = ifelse(!"Side" %in% names(.), "right", Side),
        Y_Pos = ifelse(!"Y" %in% names(.), max(summary$UCL_I, na.rm = TRUE) + 0.05, Y),
        Shape = ifelse(!"Shape" %in% names(.), 21, Shape)
      )

    p1 <- p1 +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = 0, yend = Y_Pos),
                   linetype = "dotted", linewidth = 0.8) +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 3, stroke = 1.1, fill = "black") +
      scale_shape_manual(name = "Event Annotations", values = setNames(annotations$Shape, annotations$Label))
  }

  p2 <- ggplot(summary, aes(x = date_var, y = MR)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 2.5) +
    geom_hline(yintercept = mr_avg_all, color = "#003DA5", linewidth = 1) +
    geom_hline(yintercept = UCL_MR, linetype = "dotted", color = "#C8102E", linewidth = 1) +
    labs(y = "Moving Range", x = "Date", title = paste(name, "(Moving Range)"))

  p1 <- p1 + theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

  p2 <- p2 + theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

  # Combine
  library(patchwork)
  combined <- p1 / p2 + plot_annotation(title = name)

  grid.newpage()
  grid.draw(ggplotGrob(combined))

  assign("last_qi_plot", combined, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)

  return(combined)
}
