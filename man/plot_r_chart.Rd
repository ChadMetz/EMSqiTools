% Generated by roxygen2: do not edit by hand
% Please edit documentation in r/control_chart_docs.R
\name{plot_r_chart}
\alias{plot_r_chart}
\title{R Chart (Range Chart)}
\usage{
plot_r_chart(df, subgroup_col, value_col)
}
\arguments{
\item{df}{A data frame containing the data.}

\item{subgroup_col}{The name of the column indicating subgroup membership.}

\item{value_col}{The name of the column containing numeric values.}
}
\value{
A ggplot2 object representing the R chart.
}
\description{
Plots the range within subgroups over time to monitor process variability.
}
\examples{
df <- data.frame(group = rep(1:10, each = 5), val = rnorm(50))
plot_r_chart(df, subgroup_col = "group", value_col = "val")
}
