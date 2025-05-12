\name{qi_spc_with_benchmark}
\alias{qi_spc_with_benchmark}
\title{SPC Chart with NEMSIS Benchmark Overlay}
\description{
  Generates an SPC (p-chart) from local EMS data using \code{qi_spc()}, with an overlay of national benchmark data retrieved from the NEMSIS SOAP service.
}
\usage{
qi_spc_with_benchmark(
  data,
  date_col,
  id_col,
  num_condition,
  den_condition,
  time_unit = "month",
  name = NULL,
  benchmark_id,
  benchmark_years = c(2023, 2024, 2025),
  annotations = NULL
)
}
\arguments{
  \item{data}{Local data frame with EMS incident records.}
  \item{date_col}{Column name (unquoted) for date of incident.}
  \item{id_col}{Column name (unquoted) for unique patient care record ID.}
  \item{num_condition}{Logical condition or expression identifying numerator cases.}
  \item{den_condition}{Logical condition or expression for denominator inclusion.}
  \item{time_unit}{Time grouping unit (e.g., "month", "week").}
  \item{name}{Optional. Name/title of the SPC chart.}
  \item{benchmark_id}{Character. NEMSIS benchmark ID to overlay.}
  \item{benchmark_years}{Integer vector. Calendar years to retrieve NEMSIS data for.}
  \item{annotations}{Optional. A data frame of chart annotations.}
}
\value{
  A list containing:
  \item{plot}{A ggplot object with the SPC chart and benchmark overlay.}
  \item{summary}{A data frame of time-based numerator and denominator values.}
}
\examples{
\dontrun{
df <- qi_csv("seizure_cases.csv")
output <- qi_spc_with_benchmark(
  data = df,
  date_col = IncidentDate,
  id_col = pcr,
  num_condition = "intervention == TRUE",
  den_condition = "TRUE",
  time_unit = "month",
  name = "Seizure-02: Intervention Given",
  benchmark_id = "Seizure-02",
  benchmark_years = 2023:2025
)

output$plot
}
}
