#' Plot Proportion Control Chart with Optional NEMSIS Benchmark
#'
#' @param df Data frame with your local data.
#' @param date_col Name of the date column (character).
#' @param id_col Unique identifier column (character).
#' @param num_col Column used for numerator filter.
#' @param num_value Value(s) or function to select numerator.
#' @param den_col Column used for denominator filter.
#' @param den_value Value(s) or function to select denominator.
#' @param time_unit Time unit for aggregation.
#' @param name Chart title.
#' @param annotations Optional event annotations.
#' @param benchmark_df Optional benchmark dataframe (e.g., from `get_nemsis_benchmark()`).
#' @param drop_invalid_dates Drop rows with invalid dates?
#' @param return_table Return summary table instead of plot?
#'
#' @return A ggplot or a summary table.
#' Plot Proportion Control Chart with Optional NEMSIS Benchmark
#' @export
plot_p_chart <- function(
  df, date_col, id_col, num_col, num_value,
  den_col, den_value = NULL,
  time_unit = c("week", "month", "quarter"),
  name = "QI Control Chart",
  annotations = NULL,
  benchmark_df = NULL,
  drop_invalid_dates = FALSE,
  return_table = FALSE
) {
  library(dplyr); library(lubridate); library(ggplot2)
  library(scales); library(grid); library(rlang); library(tibble)

  time_unit <- match.arg(time_unit)

  parse_date_flexibly <- function(dates) {
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%d-%m-%Y", "%d/%m/%Y", "%d %b %Y")
    for (fmt in formats) {
      parsed <- as.Date(dates, format = fmt)
      if (all(!is.na(parsed) | is.na(dates))) return(parsed)
    }
    warning("Could not parse all dates. Returning NAs.")
    return(as.Date(dates))
  }

  if (!is.null(benchmark_df) && is.null(df)) {
    benchmark_df <- benchmark_df %>%
      mutate(period = floor_date(date, unit = time_unit),
             rate = ifelse(is.na(rate), numerator / denominator, rate))

    p <- ggplot(benchmark_df, aes(x = period, y = rate)) +
      geom_line(color = "black", linetype = "longdash", linewidth = 1) +
      geom_point(size = 2) +
      labs(title = name, x = tools::toTitleCase(time_unit), y = "Rate") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
      )
    grid.newpage(); grid.draw(ggplotGrob(p)); return(p)
  }

  if (is.null(df)) stop("df must be provided.")

  df <- df %>%
    mutate(
      raw_date = .data[[date_col]],
      date_var = parse_date_flexibly(raw_date),
      period = floor_date(date_var, unit = time_unit)
    )

  if (any(is.na(df$date_var))) {
    if (drop_invalid_dates) {
      df <- df %>% filter(!is.na(date_var))
    } else {
      stop("Some dates could not be parsed. Use drop_invalid_dates = TRUE to ignore.")
    }
  }

  num_filter <- as_function(~ .x %in% num_value)
  den_filter <- as_function(~ is.null(den_value) || .x %in% den_value)

  den_df <- df %>%
    filter(den_filter(.data[[den_col]])) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Denominator = n(), .groups = "drop")

  num_df <- df %>%
    filter(num_filter(.data[[num_col]])) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Numerator = n(), .groups = "drop")

  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(
      Numerator = ifelse(is.na(Numerator), 0, Numerator),
      p = ifelse(Denominator > 0, Numerator / Denominator, NA)
    )

  # Return formatted summary if requested
  if (return_table) {
    return(
      summary %>%
        arrange(period) %>%
        mutate(
          Proportion = round(p * 100, 1),
          CL = round(mean(p, na.rm = TRUE) * 100, 1)
        ) %>%
        select(period, Numerator, Denominator, Proportion, CL) %>%
        as_tibble()
    )
  }

  side <- summary$p > mean(summary$p, na.rm = TRUE)
  rle_obj <- rle(side)
  lens <- rle_obj$lengths
  shift_points <- cumsum(lens)[lens >= 8]
  shift_starts <- shift_points - lens[which(lens >= 8)] + 1
  shift_starts <- sort(unique(c(1, shift_starts)))

  summary$CL_adj <- NA
  summary$UCL_adj <- NA
  summary$LCL_adj <- NA

  segment_labels <- data.frame()

  for (i in seq_along(shift_starts)) {
    start_idx <- shift_starts[i]
    end_idx <- if (i < length(shift_starts)) shift_starts[i + 1] - 1 else nrow(summary)
    segment <- summary[start_idx:end_idx, ]
    cl <- mean(segment$p, na.rm = TRUE)
    sd <- sqrt((cl * (1 - cl)) / segment$Denominator)
    summary$CL_adj[start_idx:end_idx] <- cl
    summary$UCL_adj[start_idx:end_idx] <- pmin(cl + 2 * sd, 1)
    summary$LCL_adj[start_idx:end_idx] <- pmax(cl - 2 * sd, 0)

    label_row <- segment[ceiling(nrow(segment)/2), ]
    segment_labels <- bind_rows(segment_labels, tibble(
      period = label_row$period,
      CL_adj = cl,
      label = paste0("CL ", round(cl * 100), "%")
    ))
  }

  control_segments <- summary %>%
    mutate(next_period = lead(period)) %>%
    filter(!is.na(next_period)) %>%
    select(period, next_period, CL_adj, UCL_adj, LCL_adj)

  p <- ggplot(summary, aes(x = period, y = p)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 1.5, color = "black") +
    geom_segment(data = control_segments, aes(x = period, xend = next_period, y = UCL_adj, yend = UCL_adj),
                 color = "#C8102E", linewidth = 0.7, linetype = "dotted") +
    geom_segment(data = control_segments, aes(x = period, xend = next_period, y = LCL_adj, yend = LCL_adj),
                 color = "#C8102E", linewidth = 0.7, linetype = "dotted") +
    geom_segment(data = control_segments, aes(x = period, xend = next_period, y = CL_adj, yend = CL_adj),
                 color = "#003DA5", linewidth = 0.7) +
    geom_text(data = segment_labels,
              aes(x = period, y = CL_adj, label = label),
              color = "#003DA5", vjust = -1, fontface = "bold", size = 3)

  if (!is.null(benchmark_df)) {
    benchmark_df <- benchmark_df %>%
      mutate(period = floor_date(date, unit = time_unit),
             rate = ifelse(is.na(rate), numerator / denominator, rate))
    p <- p +
      geom_line(data = benchmark_df, aes(x = period, y = rate), color = "black", linetype = "longdash") +
      geom_text(data = benchmark_df %>% slice_tail(n = 1),
                aes(x = period, y = rate + 0.02, label = "Benchmark"),
                color = "black", hjust = 0, size = 4)
  }

  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Label = as.character(Label),
        Date = as.Date(Date),
        Side = ifelse(!"Side" %in% names(.), "right", Side),
        Y_Pos = ifelse(!"Y" %in% names(.), max(summary$UCL_adj, na.rm = TRUE) + 0.05, Y),
        Shape = ifelse(!"Shape" %in% names(.), 21, Shape)
      )

    p <- p +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = 0, yend = Y_Pos),
                   linetype = "dotted") +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2, fill = "black") +
      scale_shape_manual(name = "Event Annotations", values = setNames(annotations$Shape, annotations$Label))
  }

  p <- p +
    labs(x = tools::toTitleCase(time_unit), y = "Proportion", title = name) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
    )

  grid.newpage()
  grid.draw(ggplotGrob(p))

  assign("last_qi_plot", p, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)

  return(p)
}
