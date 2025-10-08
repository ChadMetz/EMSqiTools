#' Plot Count (c) Control Chart with Annotations and Stepwise Control Limits
#'
#' Count-based control chart (c-chart) with Western Electric Rule 2 shift detection,
#' segmented control limits, optional global filter, and annotation support.
#'
#' @param df A data frame containing the dataset.
#' @param date_col The name of the date column (character).
#' @param id_col The name of the unique ID column (character).
#' @param event_condition A logical filter indicating rows to count (string/expression).
#' @param filter_col Optional column to filter on prior to any calculations.
#' @param filter_value Optional value(s) in `filter_col` to keep (vector-friendly).
#' @param time_unit Time aggregation level: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param annotations Optional data frame of annotations with `Date`, `Label`, optional `Shape`, `Side`, and `Y`.
#' @param plot_width Width of the plot (unused).
#' @param plot_height Height of the plot (unused).
#' @param drop_invalid_dates Drop rows with invalid dates?
#' @param return_table Return summary table instead of plot?
#'
#' @return ggplot2 plot or formatted summary table.
#' @export
plot_c_chart <- function(
  df,
  date_col,
  id_col,
  event_condition,
  filter_col = NULL,
  filter_value = NULL,
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
  library(tibble)

  time_unit <- match.arg(time_unit)

  # Flexible event condition: allow string or expression
  event_expr <- if (is.character(event_condition)) {
    rlang::parse_expr(event_condition)
  } else {
    rlang::enexpr(event_condition)
  }

  parse_date_flexibly <- function(dates) {
    if (inherits(dates, "Date"))  return(dates)
    if (inherits(dates, c("POSIXct", "POSIXt"))) return(as.Date(dates))
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
    df <- df %>% filter(.data[[filter_col]] %in% filter_value)
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

  # Count events per period (no dedup: counts rows matching event_expr)
  summary <- df %>%
    filter(!!event_expr) %>%
    group_by(period) %>%
    summarise(Count = n(), .groups = "drop")

  # Ensure full period coverage (fill missing with 0)
  full_range <- df %>%
    distinct(period) %>%
    arrange(period)

  summary <- full_range %>%
    left_join(summary, by = "period") %>%
    mutate(Count = tidyr::replace_na(Count, 0)) %>%
    arrange(period)

  # ---- Western Electric Rule 2: 8 points on one side of overall mean ----
  side <- summary$Count > mean(summary$Count, na.rm = TRUE)
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
    cl <- mean(segment$Count, na.rm = TRUE)
    sd <- sqrt(pmax(cl, 0))  # Poisson approx: sd = sqrt(CL)

    summary$CL_adj[start_idx:end_idx]  <- cl
    summary$UCL_adj[start_idx:end_idx] <- pmax(cl + 2 * sd, 0)
    summary$LCL_adj[start_idx:end_idx] <- pmax(cl - 2 * sd, 0)

    label_row <- segment[ceiling(nrow(segment)/2), ]
    segment_labels <- bind_rows(segment_labels, tibble(
      period = label_row$period,
      CL_adj = cl,
      label  = paste0("CL ", round(cl, 1))
    ))
  }

  if (return_table) {
    return(
      summary %>%
        transmute(
          period,
          Count,
          CL = round(CL_adj, 1),
          UCL = round(UCL_adj, 1),
          LCL = round(LCL_adj, 1)
        ) %>%
        as_tibble()
    )
  }

  # Build stepwise segments for CL/UCL/LCL (same approach as p-chart)
  control_segments <- summary %>%
    mutate(next_period = dplyr::lead(period)) %>%
    filter(!is.na(next_period)) %>%
    select(period, next_period, CL_adj, UCL_adj, LCL_adj)

  # ---- Plot (style aligned with plot_p_chart) ----
  p <- ggplot(summary, aes(x = period, y = Count)) +
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
        Y_Pos = ifelse(!"Y"    %in% names(.), max(summary$UCL_adj, na.rm = TRUE) + 0.05 * max(summary$Count, na.rm = TRUE) + 0.5, Y),
        Shape = ifelse(!"Shape"%in% names(.), 21, Shape)
      )

    p <- p +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = 0, yend = Y_Pos),
                   linetype = "dotted") +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2, fill = "black") +
      scale_shape_manual(name = "Event Annotations",
                         values = setNames(annotations$Shape, annotations$Label))
  }

  p <- p +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(x = tools::toTitleCase(time_unit), y = "Event Count", title = name) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text  = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(face = "bold", hjust = 0.5, size = 14)
    )

  return(p)
}
