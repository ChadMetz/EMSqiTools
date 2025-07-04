% Generated by roxygen2: do not edit by hand
% Please edit documentation in r/plot_p_chart.R
\name{plot_p_chart}
\alias{plot_p_chart}
\title{Plot P Chart}
\usage{
plot_P_chart(
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
\item{...}{Parameters passed to \code{plot_p_chart}.}
}
\value{
Output from \code{plot_p_chart}.
}
\description{
Plot P Chart function from EMSqiTools package.
}
