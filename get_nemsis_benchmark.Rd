\name{get_nemsis_benchmark}
\alias{get_nemsis_benchmark}
\title{Retrieve NEMSIS National Benchmark Performance Data}
\description{
  Fetches national monthly performance data for a given NEMSIS benchmark ID across specified years.
}
\usage{
get_nemsis_benchmark(benchmark_id, years = c(2023, 2024, 2025))
}
\arguments{
  \item{benchmark_id}{Character string. ID of the NEMSIS benchmark (e.g., "Seizure-02").}
  \item{years}{Integer vector. Calendar years to retrieve data for. Default is 2023–2025.}
}
\value{
  A data.frame with columns:
  \itemize{
    \item \code{year} – Calendar year
    \item \code{month} – Month number (1–12)
    \item \code{numerator} – Number of cases meeting the benchmark
    \item \code{denominator} – Total eligible cases
    \item \code{rate} – Proportion (numerator / denominator)
    \item \code{date} – Date object for the month
    \item \code{rate_percent} – Performance rate as a percentage
  }
}
\examples{
\dontrun{
# Fetch national performance data for Seizure-02 from 2023–2025
national_data <- get_nemsis_benchmark("Seizure-02")

# Plot rate over time
library(ggplot2)
ggplot(national_data, aes(x = date, y = rate)) +
  geom_line(color = "blue") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Seizure-02 National Benchmark", y = "Performance Rate", x = "Date")
}
}
