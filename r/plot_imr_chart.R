#' Plot Individuals and Moving Range (I-MR) Control Chart with Stepwise Limits
#'
#' I-MR chart for continuous data with Western Electric Rule 2 (8+ on one side)
#' and segmented (stepwise) Individuals limits. Optional global filter and annotations.
#'
#' @param df Data frame with your data
#' @param date_col Name of the date column
#' @param value_col Name of the numeric column to chart
#' @param filter_col Optional column to filter on prior to any calculations
#' @param filter_value Optional value(s) in `filter_col` to keep (vector-friendly)
#' @param time_unit Aggregation: "week", "month", or "quarter"
#' @param name Title of the chart
#' @param annotations Optional data frame with Date, Label, Shape, Y, Side
#' @param plot_width Not used
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
  filter_col = NULL,
  filter_value = NULL,
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
  library(scales)
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
      period   = floor_date(date_var, unit = time_unit),
      value    = .data[[value_col]]
    ) %>%
    arrange(date_var)

  if (any(is.na(df$date_var))) {
    if (drop_invalid_dates) {
      df <- df %>% filter(!is.na(date_var))
    } else {
      stop("Some dates could not be parsed. Use drop_invalid_dates = TRUE to ignore.")
    }
  }

  # Moving range of successive observations (n=2)
  df <- df %>%
    mutate(MR = abs(value - dplyr::lag(value)))

  summary <- df %>%
    select(date_var, period, value, MR) %>%
    filter(!is.na(value)) %>%
    arrange(date_var)

  # ---- Individuals stepwise limits via Western Electric Rule 2 ----
  side <- summary$value > mean(summary$value, na.rm = TRUE)
  rle_obj <- rle(side)
  lens <- rle_obj$lengths
  shift_points <- cumsum(lens)[lens >= 8]
  shift_starts <- shift_points - lens[which(lens >= 8)] + 1
  shift_starts <- sort(unique(c(1, shift_starts)))

  summary$CL_I  <- NA_real_
  summary$UCL_I <- NA_real_
  summary$LCL_I <- NA_real_
  segment_labels <- tibble()

  for (i in seq_along(shift_starts)) {
    start_idx <- shift_starts[i]
    end_idx   <- if (i < length(shift_starts)) shift_starts[i + 1] - 1 else nrow(summary)
    segment   <- summary[start_idx:end_idx, ]
    cl       <- mean(segment$value, na.rm = TRUE)
    mr_avg   <- mean(segment$MR, na.rm = TRUE)
    sigma    <- mr_avg / 1.128  # d2 for n=2

    summary$CL_I[start_idx:end_idx]  <- cl
    summary$UCL_I[start_idx:end_idx] <- cl + 3 * sigma
    summary$LCL_I[start_idx:end_idx] <- cl - 3 * sigma

    label_row <- segment[ceiling(nrow(segment)/2), ]
    segment_labels <- bind_rows(segment_labels, tibble(
      date_var = label_row$date_var,
      CL_I = cl,
      label = paste0("CL ", round(cl, 2))
    ))
  }

  # ---- MR chart limits (constant across chart) ----
  mr_avg_all <- mean(summary$MR, na.rm = TRUE)
  UCL_MR     <- mr_avg_all * 3.267  # D4 for n=2
  summary <- summary %>% mutate(CL_MR = mr_avg_all, UCL_MR = UCL_MR)

  if (return_table) {
    return(
      summary %>%
        transmute(
          date = date_var,
          Value = value,
          MR    = MR,
          CL_I  = round(CL_I, 3),
          UCL_I = round(UCL_I, 3),
          LCL_I = round(LCL_I, 3),
          CL_MR = round(CL_MR, 3),
          UCL_MR= round(UCL_MR, 3)
        ) %>%
        as_tibble()
    )
  }

  # Build stepwise segments for Individuals limits (match p/x style)
  control_segments_I <- summary %>%
    mutate(next_date = dplyr::lead(date_var)) %>%
    filter(!is.na(next_date)) %>%
    select(date_var, next_date, CL_I, UCL_I, LCL_I)

  # ----- Individuals plot -----
  p1 <- ggplot(summary, aes(x = date_var, y = value)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 1.8, color = "black") +
    geom_segment(data = control_segments_I,
                 aes(x = date_var, xend = next_date, y = UCL_I, yend = UCL_I),
                 color = "#C8102E", linewidth = 0.7, linetype = "dotted") +
    geom_segment(data = control_segments_I,
                 aes(x = date_var, xend = next_date, y = LCL_I, yend = LCL_I),
                 color = "#C8102E", linewidth = 0.7, linetype = "dotted") +
    geom_segment(data = control_segments_I,
                 aes(x = date_var, xend = next_date, y = CL_I, yend = CL_I),
                 color = "#003DA5", linewidth = 0.7) +
    geom_text(data = segment_labels,
              aes(x = date_var, y = CL_I, label = label),
              color = "#003DA5", vjust = -1, fontface = "bold", size = 3) +
    labs(y = "Value", x = tools::toTitleCase(time_unit), title = paste(name, "(Individuals)")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text  = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(face = "bold", hjust = 0.5, size = 14)
    )

  # ----- Annotations (mirror p/x defaults) -----
  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Label = as.character(Label),
        Date  = as.Date(Date),
        Side  = ifelse(!"Side" %in% names(.), "right", Side),
        Y_Pos = ifelse(!"Y"    %in% names(.), max(summary$UCL_I, na.rm = TRUE) + 0.05 * abs(max(summary$value, na.rm = TRUE)), Y),
        Shape = ifelse(!"Shape"%in% names(.), 21, Shape)
      )

    p1 <- p1 +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = -Inf, yend = Y_Pos),
                   linetype = "dotted") +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2, fill = "black") +
      scale_shape_manual(name = "Event Annotations",
                         values = setNames(annotations$Shape, annotations$Label))
  }

  # ----- Moving Range plot -----
  p2 <- ggplot(summary, aes(x = date_var, y = MR)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 1.8, color = "black") +
    geom_hline(yintercept = mr_avg_all, color = "#003DA5", linewidth = 0.7) +
    geom_hline(yintercept = UCL_MR,     color = "#C8102E", linetype = "dotted", linewidth = 0.7) +
    labs(y = "Moving Range", x = tools::toTitleCase(time_unit), title = paste(name, "(Moving Range)")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text  = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(face = "bold", hjust = 0.5, size = 14)
    )

  # Combine vertically with patchwork
  suppressWarnings({
    if (!"package:patchwork" %in% search()) {
      # don't attach; just use patchwork via :: if needed. Here we rely on + operator if loaded.
    }
  })

  # If patchwork is available, combine; else fall back to list
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- p1 / p2 + patchwork::plot_annotation(title = name)
  } else {
    combined <- list(Individuals = p1, MovingRange = p2)
  }

  return(combined)
}
