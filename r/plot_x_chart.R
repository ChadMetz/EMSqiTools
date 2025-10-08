#' Plot X-Bar Control Chart with Annotations and Stepwise Control Limits
#'
#' This function plots a control chart for the mean of a continuous variable
#' over time using Western Electric Rule 2 to detect shifts. Segmented control limits
#' are drawn for periods with sustained shifts. Optional annotations can be displayed.
#'
#' @param df A data frame containing the dataset.
#' @param date_col The name of the date column (character).
#' @param id_col The name of the unique ID column (character).
#' @param value_col The name of the numeric column to be summarized (character).
#' @param filter_col Optional column to filter on prior to any calculations.
#' @param filter_value Optional value(s) in `filter_col` to keep (vector-friendly).
#' @param time_unit Time aggregation level: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param annotations Optional data frame of annotations with columns: Date, Label, Shape, Y (optional), Side (optional).
#' @param plot_width Width of the plot in inches (unused).
#' @param plot_height Height of the plot in inches (unused).
#' @param drop_invalid_dates Whether to drop rows with unparseable dates. Defaults to FALSE.
#' @param return_table If TRUE, returns the summary table instead of plotting.
#'
#' @return A ggplot object or a summary table (if return_table = TRUE).
#' @export
plot_x_chart <- function(
  df,
  date_col,
  id_col,
  value_col,
  filter_col = NULL,
  filter_value = NULL,
  time_unit = c("week", "month", "quarter"),
  name = "X-Bar Control Chart",
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
  library(tibble)

  time_unit <- match.arg(time_unit)

  parse_date_flexibly <- function(dates) {
    if (inherits(dates, "Date")) return(dates)
    if (inherits(dates, c("POSIXct","POSIXt"))) return(as.Date(dates))
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%d-%m-%Y", "%d/%m/%Y", "%d %b %Y")
    for (fmt in formats) {
      parsed <- as.Date(dates, format = fmt)
      if (all(!is.na(parsed) | is.na(dates))) return(parsed)
    }
    warning("Could not parse all dates. Returning NAs where format failed.")
    return(as.Date(dates))
  }

  if (is.null(df)) stop("df must be provided.")

  # ---- Optional global filter (aligned with plot_p_chart) ----
  if (!is.null(filter_col) && !is.null(filter_value)) {
    if (!filter_col %in% names(df)) {
      stop(sprintf("filter_col '%s' not found in df.", filter_col))
    }
    df <- df %>% dplyr::filter(.data[[filter_col]] %in% filter_value)
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
      bad_dates <- df %>% filter(is.na(date_var)) %>% distinct(raw_date)
      stop(paste("Some dates could not be parsed. Problematic values include:",
                 paste(head(bad_dates$raw_date, 5), collapse = ", ")))
    }
  }

  # Summarise per period
  summary <- df %>%
    group_by(period) %>%
    summarise(
      x_bar = mean(.data[[value_col]], na.rm = TRUE),
      sd    = sd(.data[[value_col]],   na.rm = TRUE),
      n     = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    ) %>%
    arrange(period)

  # ---- Western Electric Rule 2: 8 points on one side of overall mean ----
  side <- summary$x_bar > mean(summary$x_bar, na.rm = TRUE)
  rle_obj <- rle(side)
  lens <- rle_obj$lengths
  shift_points <- cumsum(lens)[lens >= 8]
  shift_starts <- shift_points - lens[which(lens >= 8)] + 1
  shift_starts <- sort(unique(c(1, shift_starts)))

  summary$CL_adj  <- NA_real_
  summary$UCL_adj <- NA_real_
  summary$LCL_adj <- NA_real_
  segment_labels <- tibble()

  for (i in seq_along(shift_starts)) {
    start_idx <- shift_starts[i]
    end_idx   <- if (i < length(shift_starts)) shift_starts[i + 1] - 1 else nrow(summary)
    segment   <- summary[start_idx:end_idx, ]

    cl <- mean(segment$x_bar, na.rm = TRUE)
    se <- mean(segment$sd / pmax(segment$n, 1)^0.5, na.rm = TRUE)  # pooled-ish SE

    summary$CL_adj[start_idx:end_idx]  <- cl
    summary$UCL_adj[start_idx:end_idx] <- cl + 2 * se
    summary$LCL_adj[start_idx:end_idx] <- cl - 2 * se

    label_row <- segment[ceiling(nrow(segment)/2), ]
    segment_labels <- bind_rows(segment_labels, tibble(
      period = label_row$period,
      CL_adj = cl,
      label  = paste0("CL ", round(cl, 2))
    ))
  }

  if (return_table) {
    return(
      summary %>%
        transmute(
          period,
          Mean = round(x_bar, 2),
          CL   = round(CL_adj, 2),
          UCL  = round(UCL_adj, 2),
          LCL  = round(LCL_adj, 2)
        ) %>%
        as_tibble()
    )
  }

  # Build stepwise segments for CL/UCL/LCL (aligned with p-chart)
  control_segments <- summary %>%
    mutate(next_period = dplyr::lead(period)) %>%
    filter(!is.na(next_period)) %>%
    select(period, next_period, CL_adj, UCL_adj, LCL_adj)

  # ---- Plot (style aligned with plot_p_chart) ----
  p <- ggplot(summary, aes(x = period, y = x_bar)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 1.8, color = "black") +
    geom_segment(data = control_segments,
                 aes(x = period, xend = next_period, y = UCL_adj, yend = UCL_adj),
                 color = "#C8102E", linewidth = 0.7, linetype = "dotted") +
    geom_segment(data = control_segments,
                 aes(x = period, xend = next_period, y = LCL_adj, yend = LCL_adj),
                 color = "#C8102E", linewidth = 0.7, linetype = "dotted") +
    geom_segment(data = control_segments,
                 aes(x = period, xend = next_period, y = CL_adj, yend = CL_adj),
                 color = "#003DA5", linewidth = 0.7) +
    geom_text(data = segment_labels,
              aes(x = period, y = CL_adj, label = label),
              color = "#003DA5", vjust = -1, fontface = "bold", size = 3)

  # ---- Annotations (mirrors p-chart defaults) ----
  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Label = as.character(Label),
        Date  = as.Date(Date),
        Side  = ifelse(!"Side" %in% names(.), "right", Side),
        Y_Pos = ifelse(!"Y"    %in% names(.), max(summary$UCL_adj, na.rm = TRUE) + 0.05 * abs(max(summary$x_bar, na.rm = TRUE)), Y),
        Shape = ifelse(!"Shape"%in% names(.), 21, Shape)
      )

    p <- p +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = -Inf, yend = Y_Pos),
                   linetype = "dotted") +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2, fill = "black") +
      scale_shape_manual(name = "Event Annotations",
                         values = setNames(annotations$Shape, annotations$Label))
  }

  p <- p +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(x = tools::toTitleCase(time_unit), y = "Mean", title = name) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text  = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(face = "bold", hjust = 0.5, size = 14)
    )

  return(p)
}


  return(p)
}
