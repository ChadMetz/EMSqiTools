% Generated by roxygen2: do not edit by hand
% Please edit documentation in r/helpers.R
\name{check_outliers}
\alias{check_outliers}
\title{check_outliers}
\usage{
check_outliers(df)
}
\arguments{
\item{df}{Dataframe.}

\item{...}{Parameters passed to \code{check_outliers}.}
}
\value{
A dataframe with count of outliers per numeric column.

Output from \code{check_outliers}.
}
\description{
Identify numeric columns with values beyond 1.5x IQR.
}
\examples{
check_outliers(df)
Check Outliers

Check Outliers function from EMSqiTools package.

}
