% Generated by roxygen2: do not edit by hand
% Please edit documentation in r/plot_control_chart.R
\name{plot_control_chart}
\alias{plot_control_chart}
\title{plot_control_chart}
\usage{
plot_control_chart(
  df,
  date_col,
  id_col,
  num_condition,
  den_condition = "TRUE",
  time_unit = c("week", "month", "quarter"),
  name = "QI Control Chart",
  annotations = NULL,
  plot_width = 12,
  plot_height = 3
)
}
\arguments{
\item{df}{Dataframe containing the data.}

\item{date_col}{Name of the date column (string).}

\item{id_col}{Unique identifier column (string).}

\item{num_condition}{Filter condition for numerator (string, R expression).}

\item{den_condition}{Filter condition for denominator (string, R expression, default 'TRUE').}

\item{time_unit}{Time unit for aggregation ('week', 'month', 'quarter').}

\item{name}{Chart title.}

\item{annotations}{Optional dataframe with columns Date, Label, Side, Y_Pos for vertical annotations.}

\item{plot_width}{Width of the plot when displayed (default 12 inches).}

\item{plot_height}{Height of the plot when displayed (default 3 inches).}
}
\value{
A summary dataframe with period, numerator, denominator, proportion, and control limits.
Also displays a control chart.
}
\description{
Create a statistical process control (SPC) chart with annotations and optional control limits.
}
\examples{
annotations <- data.frame(
  Date = as.Date(c("2024-09-01", "2025-02-15")),
  Label = c("Go Live", "Townhall"),
  Side = c("right", "left")
)
plot_control_chart(df, "Incident Date", "pcr", "hr > 100", "TRUE", "week", "High HR", annotations)
}
