#' Plot With Nemsis Benchmark
#'
#' Adds NEMSIS benchmark overlay to an existing control chart.
#'
#' @param data Input data frame.
#' @param date_col Name of the date column.
#' @param id_col Name of the unique ID column.
#' @param num_condition Filter condition for numerator.
#' @param den_condition Filter condition for denominator. Default is "TRUE".
#' @param time_unit Aggregation unit: "week", "month", or "quarter".
#' @param name Title for the chart.
#' @param benchmark_id ID used to retrieve the NEMSIS benchmark.
#' @param benchmark_years Vector of years to include. Defaults to 2023–2025.
#' @param annotations Optional annotation data frame.
#'
#' @return A list with the updated plot and summary data frame.
#' @export
plot_with_nemsis_benchmark <- function(data,
                                       date_col,
                                       id_col,
                                       num_condition,
                                       den_condition = "TRUE",
                                       time_unit = "month",
                                       name = NULL,
                                       benchmark_id,
                                       benchmark_years = c(2023, 2024, 2025),
                                       annotations = NULL) {
  # Generate base SPC chart
  summary_df <- plot_control_chart(
    df = data,
    date_col = date_col,
    id_col = id_col,
    num_condition = num_condition,
    den_condition = den_condition,
    time_unit = time_unit,
    name = name,
    annotations = annotations
  )

  # Retrieve benchmark data
  benchmark_data <- get_nemsis_benchmark(benchmark_id, years = benchmark_years)

  # Clean benchmark data
  benchmark_data <- benchmark_data %>%
    dplyr::mutate(
      date = as.Date(date),
      rate = as.numeric(rate)
    )

  # Overlay benchmark on the existing plot
  plot_overlay <- get("last_qi_plot", envir = .GlobalEnv) +
    geom_line(data = benchmark_data, aes(x = date, y = rate),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_point(data = benchmark_data, aes(x = date, y = rate),
               color = "red", shape = 1, size = 2) +
    labs(subtitle = paste("Overlay: NEMSIS Benchmark", benchmark_id)) +
    theme_minimal(base_size = 12, base_family = "Arial") +  # Ensure consistent font
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      plot.subtitle = element_text(face = "italic", size = 11, hjust = 0.5),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(color = "#1A1A1A"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  assign("last_qi_plot", plot_overlay, envir = .GlobalEnv)

  return(list(
    plot = plot_overlay,
    summary = summary_df
  ))
}
