#' Plot With Nemsis Benchmark
#'
#' Plot With Nemsis Benchmark function from EMSqiTools package.
#'
#' @param ... Parameters passed to `plot_with_nemsis_benchmark`.
#'
#' @return Output from `plot_with_nemsis_benchmark`.
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
    labs(subtitle = paste("Overlay: NEMSIS Benchmark", benchmark_id))
  
  # Update global plot
  assign("last_qi_plot", plot_overlay, envir = .GlobalEnv)
  
  return(list(
    plot = plot_overlay,
    summary = summary_df
  ))
}
