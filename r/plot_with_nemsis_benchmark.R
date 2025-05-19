#' Control Chart with NEMSIS Benchmark Overlay
#'
#' Generates a p-chart using `qi_spc()` and overlays national benchmark data from NEMSIS.
#' Useful for comparing your local quality improvement trends against national reference values.
#'
#' @param data A data frame containing your local data.
#' @param date_col Column name (unquoted) for date/time of events.
#' @param id_col Column name (unquoted) for unique encounter IDs.
#' @param num_condition Expression or condition for the numerator.
#' @param den_condition Expression or condition for the denominator.
#' @param time_unit Time grouping unit: "month", "week", "quarter", etc.
#' @param name Title of the SPC chart.
#' @param benchmark_id Character string. NEMSIS benchmark ID to overlay (e.g., "Seizure-02").
#' @param benchmark_years Integer vector. Calendar years to include for the benchmark overlay.
#' @param annotations Optional. Data frame of annotations passed to `qi_spc()`.
#' 
#' @return A list containing the SPC plot with overlay and summary table.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- qi_csv("seizure_cases.csv")
#' spc_overlay <- plot_with_nemsis_benchmark(
#'   data = df,
#'   date_col = IncidentDate,
#'   id_col = pcr,
#'   num_condition = "intervention == TRUE",
#'   den_condition = "TRUE",
#'   time_unit = "month",
#'   name = "Seizure-02: Intervention Given",
#'   benchmark_id = "Seizure-02",
#'   benchmark_years = 2023:2025
#' )
#'
#' spc_overlay$plot
#' }
plot_with_nemsis_benchmark <- function(data,
                                  date_col,
                                  id_col,
                                  num_condition,
                                  den_condition,
                                  time_unit = "month",
                                  name = NULL,
                                  benchmark_id,
                                  benchmark_years = c(2023, 2024, 2025),
                                  annotations = NULL) {
  # Create the SPC chart from qiTools
  summary <- qi_spc(
    data = data,
    date_col = {{ date_col }},
    id_col = {{ id_col }},
    num_condition = num_condition,
    den_condition = den_condition,
    time_unit = time_unit,
    name = name,
    annotations = annotations
  )
  
  # Retrieve national benchmark
  benchmark_data <- get_nemsis_benchmark(benchmark_id, years = benchmark_years)
  
  # Plot overlay
  spc_plot <- summary$plot +
    geom_line(data = benchmark_data, aes(x = date, y = rate),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_point(data = benchmark_data, aes(x = date, y = rate), color = "red", shape = 1) +
    labs(subtitle = paste("Overlay: NEMSIS Benchmark", benchmark_id))

  summary$plot <- spc_plot
  return(summary)
}
