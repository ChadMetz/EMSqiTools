#' Plot a Distribution Histogram (numeric or categorical) with optional date filtering
#'
#' Creates a histogram of a variable, collapsing to unique records by an ID column
#' before plotting (to prevent duplicate-case inflation). If \code{value_col} is
#' character/factor, a categorical bar chart is produced (counts by level) and
#' displayed horizontally. If \code{value_col} is numeric, a standard histogram is
#' produced.
#'
#' The data can be filtered to a date range using \code{date_col}. Dates are parsed
#' flexibly across several common formats.
#'
#' @section Annotations:
#' Annotations are optional and handled differently by type:
#' \itemize{
#'   \item \strong{Numeric histogram:} provide a data frame with columns
#'         \code{X} (numeric position on the x-axis), optional \code{Label} (character),
#'         and optional \code{Y} (numeric height for the label). Vertical
#'         dotted lines are drawn at each \code{X}. If \code{Label} is provided,
#'         it is printed near the top of the panel.
#'   \item \strong{Categorical bar chart:} provide \code{Category} (character; after
#'         truncation to 50 chars), optional \code{Label}, and optional \code{Y}.
#'         The label is placed at the end of each bar.
#' }
#'
#' @param df A data frame.
#' @param date_col Character. Column name of a date-like field used for filtering.
#' @param value_col Character. Column to histogram. Numeric values produce a numeric
#'   histogram; character/factor produces a categorical bar chart.
#' @param count_col Character. ID column used to collapse to unique records
#'   before counting (e.g., PCR Number).
#' @param start_date,end_date Optional date bounds (Date or character) applied to
#'   \code{date_col}.
#' @param time_unit Character. Currently unused (reserved for future faceting);
#'   kept for API consistency. Default \code{c("week","month","quarter")}, only the
#'   first value is matched.
#' @param name Character. Plot title.
#' @param theme_font Numeric. Base font size.
#' @param annotations Optional data frame as described in \emph{Annotations}.
#'
#' @return A \pkg{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' # Numeric example
#' plot_histogram(df = my_df,
#'                date_col = "CallDate",
#'                value_col = "SceneToCT_Sec",
#'                count_col = "PCR_Number",
#'                start_date = "2025-01-01",
#'                end_date   = "2025-06-30",
#'                annotations = data.frame(X = c(900, 1200), Label = c("15 min", "20 min")))
#'
#' # Categorical example
#' plot_histogram(df = my_df,
#'                date_col = "CallDate",
#'                value_col = "Primary_Impression",
#'                count_col = "PCR_Number")
#' }
#'
#' @importFrom dplyr mutate filter distinct %>% across
#' @importFrom ggplot2 ggplot aes geom_histogram geom_bar labs coord_flip theme_minimal
#' @importFrom ggplot2 theme element_text geom_vline annotate scale_y_continuous
#' @importFrom forcats fct_infreq fct_rev
#' @importFrom stringr str_trunc
#' @importFrom lubridate as_date
#' @export
plot_histogram <- function(
    df,
    date_col,
    value_col,
    count_col,
    start_date = NULL,
    end_date = NULL,
    time_unit = c("week", "month", "quarter"),
    name = "Distribution Histogram",
    theme_font = 14,
    annotations = NULL
) {
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(forcats)
  library(stringr)
  library(rlang)

  # keep arg available for future (faceting/binning by period, etc.)
  time_unit <- match.arg(time_unit)

  # Flexible date parser (vectorized)
  parse_date_flexibly <- function(dates) {
    fmts <- c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y", "%d-%m-%Y", "%d/%m/%Y", "%d %b %Y")
    out <- as.Date(dates) # default attempt (handles Date already)
    if (all(!is.na(out) | is.na(dates))) return(out)
    for (fmt in fmts) {
      parsed <- as.Date(dates, format = fmt)
      # accept if every non-NA input parsed non-NA
      if (all(!is.na(parsed) | is.na(dates))) return(parsed)
    }
    warning("Could not parse all dates; unparsed values set to NA.")
    as.Date(dates)
  }

  # Parse/filter by date
  df <- df %>%
    mutate(date_var = parse_date_flexibly(.data[[date_col]])) %>%
    filter(!is.na(date_var))

  if (!is.null(start_date)) df <- df %>% filter(date_var >= as_date(start_date))
  if (!is.null(end_date))   df <- df %>% filter(date_var <= as_date(end_date))
  if (nrow(df) == 0) stop("No data in the specified date range.")

  # Collapse to unique records by ID and value to avoid double-counting
  df <- df %>% distinct(.data[[count_col]], .data[[value_col]], .keep_all = TRUE)

  value_data <- df[[value_col]]

  # Normalize annotations, if any
  has_ann <- !is.null(annotations) && nrow(annotations) > 0

  # CATEGORICAL -------------------------------------------------------------
  if (is.character(value_data) || is.factor(value_data)) {
    df <- df %>%
      mutate(value_clean = as.character(.data[[value_col]])) %>%
      mutate(
        value_trunc = str_trunc(value_clean, 50),
        value_trunc = fct_rev(fct_infreq(value_trunc))
      )

    p <- ggplot(df, aes(x = value_trunc)) +
      geom_bar(
        fill = "#001689",
        color = "white",
        width = 0.7
      ) +
      coord_flip() +
      labs(x = NULL, y = "Unique Count", title = name) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal(base_size = theme_font) +
      theme(
        axis.title.x = element_text(size = theme_font + 6, face = "bold"),
        axis.title.y = element_text(size = theme_font + 6, face = "bold"),
        axis.text.x  = element_text(size = theme_font + 2, hjust = 1, face = "bold"),
        axis.text.y  = element_text(size = theme_font + 2, face = "bold"),
        plot.title   = element_text(face = "bold", hjust = 0.5, size = theme_font + 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )

    if (has_ann) {
      # expect columns: Category (matching truncated values), optional Label, optional Y
      ann <- annotations
      if (!"Category" %in% names(ann)) {
        warning("annotations for categorical plot should include a 'Category' column (matching truncated values). Skipping labels.")
      } else {
        ann$Category <- str_trunc(as.character(ann$Category), 50)
        if (!"Y" %in% names(ann)) ann$Y <- Inf
        if (!"Label" %in% names(ann)) ann$Label <- ""

        p <- p + annotate(
          "text",
          x = ann$Category,
          y = ann$Y,
          label = ann$Label,
          hjust = 0,
          vjust = -0.2,
          size = (theme_font + 0) / 3.5
        )
      }
    }

    return(p)
  }

  # NUMERIC ----------------------------------------------------------------
  # ensure numeric; drop NA for plotting
  df <- df %>% mutate(value_num = suppressWarnings(as.numeric(.data[[value_col]]))) %>%
    filter(!is.na(value_num))

  p <- ggplot(df, aes(x = value_num)) +
    geom_histogram(fill = "#001689", color = "white", bins = 30) +
    labs(x = value_col, y = "Frequency", title = name) +
    theme_minimal(base_size = theme_font) +
    theme(
      axis.title.x = element_text(size = theme_font + 6, face = "bold"),
      axis.title.y = element_text(size = theme_font + 6, face = "bold"),
      axis.text.x  = element_text(size = theme_font + 2),
      axis.text.y  = element_text(size = theme_font + 2),
      plot.title   = element_text(face = "bold", hjust = 0.5, size = theme_font + 2)
    )

  if (has_ann) {
    # expect columns: X (numeric), optional Label, optional Y
    ann <- annotations
    if (!"X" %in% names(ann)) {
      warning("annotations for numeric histogram should include an 'X' column (numeric). Skipping vlines/labels.")
    } else {
      p <- p + geom_vline(data = ann, aes(xintercept = X), linetype = "dotted", linewidth = 0.8)
      if ("Label" %in% names(ann)) {
        # use provided Y or place label near top of panel
        if (!"Y" %in% names(ann)) ann$Y <- Inf
        p <- p + annotate("text", x = ann$X, y = ann$Y, label = ann$Label,
                          vjust = 1.2, size = (theme_font + 0) / 3.5)
      }
    }
  }

  return(p)
}
