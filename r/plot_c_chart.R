#' Plot Count (c) Control Chart with Annotations and Stepwise Control Limits
#'
#' This function plots a count-based control chart (c-chart) for discrete event counts.
#' It detects shifts using Western Electric Rule 2 and applies segmented control limits.
#'
#' @param df A data frame containing the dataset.
#' @param date_col The name of the date column (character).
#' @param id_col The name of the unique ID column (character).
#' @param event_condition A logical expression or filter for counting events (quoted).
#' @param time_unit Time aggregation level: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param annotations Optional data frame of annotations.
#' @param plot_width Width of the plot (unused).
#' @param plot_height Height of the plot (unused).
#' @param drop_invalid_dates Drop rows with invalid dates?
#' @param return_table Return summary table instead of plot?
#'
#' @return ggplot2 plot or summary table.
#' @export
plot_c_chart <- function(
  df,
  date_col,
  id_col,
  event_condition,
  time_unit = c("week", "month", "quarter"),
  name = "C Control Chart",
  annotations = NULL,
  plot_width = 12,
  plot_height = 3,
  drop_invalid_dates = FALSE,
  return_table = FALSE
) {
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(grid)
  library(rlang)

  time_unit <- match.arg(time_unit)

  event_filter <- rlang::as_function(~ eval(rlang::parse_expr(event_condition), envir = list(.x = .x)))

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
    if (drop_invalid_dates) {
      warning("Dropping rows with unparseable dates.")
      df <- df %>% filter(!is.na(date_var))
    } else {
      stop("Some dates could not be parsed. Use drop_invalid_dates = TRUE to ignore.")
    }
  }

  summary <- df %>%
    filter(event_filter(.data[[id_col]])) %>%
    group_by(period) %>%
    summarise(
      Count = n(),
      .groups = "drop"
    )

  full_range <- df %>%
    distinct(period) %>%
    arrange(period)

  summary <- full_join(full_range, summary, by = "period") %>%
    mutate(Count = replace_na(Count, 0))

  side <- summary$Count > mean(summary$Count, na.rm = TRUE)
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
    cl <- mean(segment$Count, na.rm = TRUE)
    sd <- sqrt(cl)

    summary$CL_adj[start_idx:end_idx] <- cl
    summary$UCL_adj[start_idx:end_idx] <- cl + 2 * sd
    summary$LCL_adj[start_idx:end_idx] <- pmax(cl - 2 * sd, 0)
  }

  if (return_table) return(summary)

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
      Label = paste0("CL = ", round(CL_adj, 1)),
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

  p <- ggplot(summary, aes(x = period, y = Count)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 3, color = "black") +
    geom_step(data = step_df, aes(x = period, y = UCL_adj), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_step(data = step_df, aes(x = period, y = LCL_adj), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_line(aes(y = CL_adj), color = "#003DA5", linewidth = 1) +
    geom_text(data = cl_labels, aes(x = X_Adjust - 4, y = Y_Pos + 0.02, label = Label),
              color = "#003DA5", size = 5, hjust = 0, fontface = "bold") +
    geom_segment(data = shift_lines, aes(x = Start_Date, xend = Start_Date, y = -Inf, yend = Inf),
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
    theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(x = tools::toTitleCase(time_unit), y = "Event Count", title = name)

  grid.newpage()
  grid.draw(ggplotGrob(p))

  assign("last_qi_plot", p, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)

  return(p)
}
