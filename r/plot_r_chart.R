#' Plot Proportion Control Chart with Annotations and Shift Detection
#'
#' This function plots a proportion control chart (p-chart) for quality improvement analysis.
#' It supports Western Electric Rule 2 shift detection, segmented control limits, and custom annotations.
#'
#' @param df A data frame containing the dataset.
#' @param date_col The name of the date column (character).
#' @param id_col The name of the unique ID column (character).
#' @param num_condition A string expression to filter the numerator condition.
#' @param den_condition A string expression to filter the denominator condition. Defaults to "TRUE".
#' @param time_unit Time aggregation level: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param annotations Optional data frame of annotations with columns: Date, Label, Shape, Y (optional), Side (optional).
#' @param plot_width Width of the plot in inches (unused).
#' @param plot_height Height of the plot in inches (unused).
#' @param drop_invalid_dates Whether to drop rows with unparseable dates. Defaults to FALSE.
#'
#' @return A summary data frame with proportions and control limits.
#' @export
plot_r_chart <- function(df, date_col, id_col, num_condition, den_condition = "TRUE",
                         time_unit = c("week", "month", "quarter"),
                         name = "QI Control Chart", annotations = NULL,
                         plot_width = 12, plot_height = 3, drop_invalid_dates = FALSE) {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(grid)
  library(rlang)

  time_unit <- match.arg(time_unit)

  if (!(date_col %in% names(df))) {
    stop(paste("Column", date_col, "not found in dataframe."))
  }

  tryCatch({
    parse_expr(num_condition)
    parse_expr(den_condition)
  }, error = function(e) {
    stop("num_condition or den_condition contains invalid R code. Use '&' or '|' instead of commas.")
  })

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
      period = floor_date(date_var, unit = time_unit)
    )

  if (any(is.na(df$date_var))) {
    if (isTRUE(drop_invalid_dates)) {
      warning("Dropping rows with unparseable dates.")
      df <- df %>% filter(!is.na(date_var))
    } else {
      bad_dates <- df %>% filter(is.na(date_var)) %>% distinct(raw_date)
      stop(paste("Some dates could not be parsed. Problematic values include:",
                 paste(head(bad_dates$raw_date, 5), collapse = ", ")))
    }
  }

  den_df <- df %>%
    filter(!!parse_expr(den_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Denominator = n(), .groups = "drop")

  num_df <- df %>%
    filter(!!parse_expr(num_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Numerator = n(), .groups = "drop")

  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(
      Numerator = ifelse(is.na(Numerator), 0, Numerator),
      p = ifelse(Denominator > 0, Numerator / Denominator, NA)
    )

  side <- summary$p > mean(summary$p, na.rm = TRUE)
  rle_obj <- rle(side)
  lens <- rle_obj$lengths
  shift_points <- cumsum(lens)[lens >= 8]
  shift_starts <- shift_points - lens[which(lens >= 8)] + 1
  shift_starts <- sort(unique(c(1, shift_starts)))

  summary$CL_adj <- NA
  summary$UCL_adj <- NA
  summary$LCL_adj <- NA

  for (i in seq_along(shift_starts)) {
    start_idx <- shift_starts[i]
    end_idx <- if (i < length(shift_starts)) shift_starts[i + 1] - 1 else nrow(summary)
    segment <- summary[start_idx:end_idx, ]
    cl <- mean(segment$p, na.rm = TRUE)
    sd <- sqrt((cl * (1 - cl)) / segment$Denominator)

    summary$CL_adj[start_idx:end_idx] <- cl
    summary$UCL_adj[start_idx:end_idx] <- pmin(cl + 2 * sd, 1)
    summary$LCL_adj[start_idx:end_idx] <- pmax(cl - 2 * sd, 0)
  }

  step_df <- summary %>%
    select(period, UCL_adj, LCL_adj) %>%
    mutate(period = period - days(3)) %>%
    bind_rows(
      summary[nrow(summary), ] %>%
        select(period, UCL_adj, LCL_adj) %>%
        mutate(period = max(summary$period))
    )

  cl_labels <- summary %>%
    slice(shift_starts) %>%
    mutate(
      Label = paste0("CL = ", percent(CL_adj, accuracy = 1)),
      X_Adjust = period + 7,
      Y_Pos = CL_adj + 0.02
    )

  shift_lines <- summary %>%
    slice(shift_starts) %>%
    filter(row_number() != 1) %>%
    transmute(Start_Date = period)

  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Label = as.character(Label),
        Date = as.Date(Date),
        Side = if (!"Side" %in% names(.)) "right" else Side,
        Y_Pos = if (!"Y" %in% names(.)) max(summary$UCL_adj, na.rm = TRUE) + 0.05 else Y,
        Shape = if (!"Shape" %in% names(.)) 21 else Shape,
        hjust_value = ifelse(Side == "left", 1, 0),
        x_adjusted = ifelse(Side == "left", Date - days(1), Date + days(1))
      )
  }

  p <- ggplot(summary, aes(x = period, y = p)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 3, color = "black") +
    geom_step(data = step_df, aes(x = period, y = UCL_adj), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_step(data = step_df, aes(x = period, y = LCL_adj), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_line(aes(y = CL_adj), color = "#003DA5", linewidth = 1) +
    geom_text(data = cl_labels, aes(x = X_Adjust - 4, y = Y_Pos + 0.02, label = Label),
              color = "#003DA5", size = 5, hjust = 0, fontface = "bold") +
    geom_segment(data = shift_lines, aes(x = Start_Date, xend = Start_Date, y = 0, yend = 1),
                 color = "#C8102E", linetype = "solid", linewidth = 1)

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
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.1, 1.05)) +
    theme_minimal(base_size = 12, base_family = "Segoe UI") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(color = "#1A1A1A"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(x = tools::toTitleCase(time_unit), y = "Proportion", title = name)

  grid.newpage()
  grid.draw(ggplotGrob(p))

  assign("last_qi_plot", p, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)

  return(summary)
}
