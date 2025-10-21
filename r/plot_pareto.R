#' Plot a Pareto Chart of Missing/Non-Conforming Fields
#'
#' Builds a Pareto chart showing, for a given set of columns, how many records
#' have missing or non-conforming values (\code{NA} or empty string \code{""})
#' and the cumulative percentage across fields. Data are de-duplicated by an ID
#' column before counting, and can be filtered to a date range.
#'
#' @param df A data frame of records.
#' @param date_col Character. Column name with dates used for filtering. Dates
#'   are parsed flexibly from common formats (e.g., \code{"\%Y-\%m-\%d"},
#'   \code{"\%m/\%d/\%Y"}, etc.).
#' @param pareto_cols Character vector of column names to evaluate for missing /
#'   non-conforming values.
#' @param count_col Character. Unique record identifier used to collapse to one
#'   row per record before counting (e.g., \code{PCR_Number}).
#' @param start_date,end_date Optional date bounds (Date or character) applied to
#'   \code{date_col} after parsing.
#' @param name Character. Plot title.
#' @param theme_font Numeric. Base font size for the plot.
#'
#' @details
#' For each column in \code{pareto_cols}, the function counts values that are
#' either \code{NA} or \code{""} (empty string). It then orders fields by
#' descending missing count and overlays the cumulative percentage curve.
#'
#' @return A \pkg{ggplot2} object (Pareto chart).
#'
#' @examples
#' \dontrun{
#' plot_pareto(
#'   df = my_df,
#'   date_col = "CallDate",
#'   pareto_cols = c("BP_Systolic", "Pulse", "Primary_Impression"),
#'   count_col = "PCR_Number",
#'   start_date = "2025-01-01",
#'   end_date   = "2025-06-30",
#'   name = "Missing Clinical Fields (YTD)"
#' )
#' }
#'
#' @importFrom dplyr mutate filter distinct select summarise arrange %>% everything
#' @importFrom dplyr across
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_text labs theme_minimal theme element_text scale_y_continuous
#' @importFrom forcats fct_inorder
#' @export
plot_pareto <- function(
    df,
    date_col,
    pareto_cols,
    count_col,
    start_date = NULL,
    end_date = NULL,
    name = "Pareto Chart (Missing Fields)",
    theme_font = 14
) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(lubridate)

  # Flexible date parsing
  parse_date_flexibly <- function(dates) {
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%d-%m-%Y", "%d/%m/%Y", "%d %b %Y")
    for (fmt in formats) {
      parsed <- as.Date(dates, format = fmt)
      if (all(!is.na(parsed) | is.na(dates))) return(parsed)
    }
    warning("Could not parse all dates. Returning NAs.")
    return(as.Date(dates))
  }

  df <- df %>%
    mutate(date_var = parse_date_flexibly(.data[[date_col]])) %>%
    filter(!is.na(date_var))

  if (!is.null(start_date)) df <- df %>% filter(date_var >= start_date)
  if (!is.null(end_date))   df <- df %>% filter(date_var <= end_date)
  if (nrow(df) == 0) stop("No data in the specified date range.")

  # Collapse to unique records
  df <- df %>% distinct(.data[[count_col]], .keep_all = TRUE)

  # Count missing (non-conforming) values
  pareto_data <- df %>%
    select(all_of(pareto_cols)) %>%
    summarise(across(everything(), ~ sum(is.na(.) | . == "", na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Field", values_to = "Missing") %>%
    arrange(desc(Missing)) %>%
    mutate(
      Field = fct_inorder(factor(Field)),
      CumMissing = cumsum(Missing),
      CumPct = CumMissing / sum(Missing) * 100
    )

  max_y <- max(pareto_data$Missing)

  # Build Pareto chart
  p <- ggplot(pareto_data, aes(x = Field)) +
    geom_col(aes(y = Missing), fill = "#001689", color = "white", width = 0.7) +
    geom_line(aes(y = CumPct * max_y / 100, group = 1), color = "red", linewidth = 1) +
    geom_point(aes(y = CumPct * max_y / 100), color = "red", size = 2) +
    geom_text(
      aes(y = CumPct * max_y / 100, label = paste0(round(CumPct), "%")),
      vjust = 0.0, size = 8, color = "red", fontface = "bold"
    ) +
    scale_y_continuous(name = "Missing Count (non-conforming)") +
    labs(title = name, x = NULL) +
    theme_minimal(base_size = theme_font) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 16),
      axis.title  = element_text(face = "bold", size = 16),
      plot.title  = element_text(face = "bold", hjust = 0.5)
    )

  return(p)
}
