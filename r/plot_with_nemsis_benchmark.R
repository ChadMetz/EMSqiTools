#' Control Chart with NEMSIS Benchmark Overlay
#'
#' Generates a p-chart using `plot_control_chart()` and overlays national benchmark data from NEMSIS.
#' Useful for comparing your local quality improvement trends against national reference values.
#'
#' @param data A data frame containing your local data.
#' @param date_col Name of the date column (string).
#' @param id_col Unique identifier column (string).
#' @param num_condition Expression string for the numerator (e.g., "field == TRUE").
#' @param den_condition Expression string for the denominator.
#' @param time_unit Time grouping unit: "month", "week", "quarter", etc.
#' @param name Title of the SPC chart.
#' @param benchmark_id Character string. NEMSIS benchmark ID to overlay (e.g., "Seizure-02").
#' @param benchmark_years Integer vector. Calendar years to include for the benchmark overlay.
#' @param annotations Optional. Data frame of annotations passed to `plot_control_chart()`.
#' 
#' @return A list with plot and summary dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_with_nemsis_benchmark(
#'   data = df,
#'   date_col = "IncidentDate",
#'   id_col = "pcr",
#'   num_condition = "intervention == TRUE",
#'   den_condition = "TRUE",
#'   time_unit = "month",
#'   name = "Seizure-02: Intervention Given",
#'   benchmark_id = "Seizure-02",
#'   benchmark_years = 2023:2025
#' )
#' }

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

