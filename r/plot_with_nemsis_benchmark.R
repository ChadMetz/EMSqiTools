#' Plot With NEMSIS Benchmark (aligned with plot_p_chart)
#'
#' Builds a p-chart from conditions, applies segmented limits (Western Electric Rule 2),
#' and overlays NEMSIS benchmark series.
#'
#' @param data Input data frame.
#' @param date_col Name of the date column (character).
#' @param id_col Name of the unique ID column used for per-period deduplication.
#' @param num_condition Filter condition for numerator (string or expression).
#' @param den_condition Filter condition for denominator (string or expression). Default "TRUE".
#' @param filter_col Optional column to filter on prior to any calculations.
#' @param filter_value Optional value(s) in `filter_col` to keep (vector-friendly).
#' @param time_unit Aggregation unit: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param benchmark_id ID used to retrieve the NEMSIS benchmark.
#' @param benchmark_years Vector of years to include. Defaults to c(2023, 2024, 2025).
#' @param annotations Optional annotation data frame with Date, Label, optional Shape, Side, Y.
#'
#' @return A list with elements: $plot (ggplot) and $summary (tibble).
#' @export
plot_with_nemsis_benchmark <- function(
  data,
  date_col,
  id_col,
  num_condition,
  den_condition = "TRUE",
  filter_col = NULL,
  filter_value = NULL,
  time_unit = c("week", "month", "quarter"),
  name = NULL,
  benchmark_id,
  benchmark_years = c(2023, 2024, 2025),
  annotations = NULL
) {
  library(dplyr); library(lubridate); library(ggplot2)
  library(scales); library(rlang); library(tibble)

  time_unit <- match.arg(time_unit)
  if (is.null(name)) name <- "QI Control Chart (with Benchmark)"

  # ---- helpers ----
  parse_date_flexibly <- function(dates) {
    if (inherits(dates, "Date")) return(dates)
    if (inherits(dates, c("POSIXct","POSIXt"))) return(as.Date(dates))
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%d-%m-%Y", "%d/%m/%Y", "%d %b %Y")
    for (fmt in formats) {
      parsed <- as.Date(dates, format = fmt)
      if (all(!is.na(parsed) | is.na(dates))) return(parsed)
    }
    warning("Could not parse all dates. Returning NAs.")
    return(as.Date(dates))
  }

  to_expr <- function(x) {
    if (is.character(x)) rlang::parse_expr(x) else rlang::enexpr(x)
  }

  if (is.null(data)) stop("`data` must be provided.")

  # ---- optional global filter ----
  if (!is.null(filter_col) && !is.null(filter_value)) {
    if (!filter_col %in% names(data)) {
      stop(sprintf("filter_col '%s' not found in data.", filter_col))
    }
    data <- data %>% dplyr::filter(.data[[filter_col]] %in% filter_value)
  }

  # ---- dates & period ----
  data <- data %>%
    mutate(
      raw_date = .data[[date_col]],
      date_var = parse_date_flexibly(raw_date),
      period   = floor_date(date_var, unit = time_unit)
    )

  if (any(is.na(data$date_var))) {
    stop("Some dates could not be parsed. Consider cleaning dates or adding a drop option upstream.")
  }

  num_expr <- to_expr(num_condition)
  den_expr <- to_expr(den_condition)

  # ---- Build denominator & numerator per period, de-duplicated by id_col ----
  den_df <- data %>%
    filter(!!den_expr) %>%
    distinct(period, .data[[id_col]]) %>%
    count(period, name = "Denominator")

  num_df <- data %>%
    filter(!!num_expr) %>%
    distinct(period, .data[[id_col]]) %>%
    count(period, name = "Numerator")

  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(
      Numerator = coalesce(Numerator, 0L),
      p = ifelse(Denominator > 0, Numerator / Denominator, NA_real_)
    ) %>%
    arrange(period)

  # ---- Western Electric Rule 2 segmentation (8 on one side of overall mean) ----
  side <- summary$p > mean(summary$p, na.rm = TRUE)
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
    cl <- mean(segment$p, na.rm = TRUE)
    sd <- sqrt((cl * (1 - cl)) / segment$Denominator)

    summary$CL_adj[start_idx:end_idx]  <- cl
    summary$UCL_adj[start_idx:end_idx] <- pmin(cl + 2 * sd, 1)
    summary$LCL_adj[start_idx:end_idx] <- pmax(cl - 2 * sd, 0)

    label_row <- segment[ceiling(nrow(segment)/2), ]
    segment_labels <- bind_rows(segment_labels, tibble(
      period = label_row$period,
      CL_adj = cl,
      label  = paste0("CL ", round(cl * 100), "%")
    ))
  }

  # ---- base p-chart plot (aligned styling) ----
  control_segments <- summary %>%
    mutate(next_period = dplyr::lead(period)) %>%
    filter(!is.na(next_period)) %>%
    select(period, next_period, CL_adj, UCL_adj, LCL_adj)

  p <- ggplot(summary, aes(x = period, y = p)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 1.5, color = "black") +
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

  # ---- Overlay NEMSIS benchmark ----
  benchmark_data <- get_nemsis_benchmark(benchmark_id, years = benchmark_years) %>%
    mutate(
      date  = as.Date(date),
      period = floor_date(date, unit = time_unit),
      rate  = ifelse(is.na(rate) & !is.na(numerator) & !is.na(denominator),
                     numerator / denominator, as.numeric(rate))
    ) %>%
    arrange(period)

  if (nrow(benchmark_data) > 0) {
    p <- p +
      geom_line(data = benchmark_data, aes(x = period, y = rate),
                color = "black", linetype = "longdash") +
      geom_text(data = dplyr::slice_tail(benchmark_data, n = 1),
                aes(x = period, y = rate + 0.02, label = "Benchmark"),
                color = "black", hjust = 0, size = 4)
  }

  # ---- Annotations (mirror p-chart defaults) ----
  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Label = as.character(Label),
        Date  = as.Date(Date),
        Side  = ifelse(!"Side" %in% names(.), "right", Side),
        Y_Pos = ifelse(!"Y"    %in% names(.), max(summary$UCL_adj, na.rm = TRUE) + 0.05, Y),
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
    labs(x = tools::toTitleCase(time_unit), y = "Proportion", title = name) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text  = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(face = "bold", hjust = 0.5, size = 14)
    )

  # Nice summary table like plot_p_chart (rounded)
  summary_tbl <- summary %>%
    transmute(
      period, Numerator, Denominator,
      Proportion = round(p * 100, 1),
      CL = round(CL_adj * 100, 1),
      UCL = round(UCL_adj * 100, 1),
      LCL = round(LCL_adj * 100, 1)
    ) %>%
    as_tibble()

  list(plot = p, summary = summary_tbl)
}
