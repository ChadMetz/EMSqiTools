#' check_missing
#'
#' Check for missing values in each column.
#' 
#' @param df Dataframe.
#' @return A dataframe with count and percent of missing values per column.
#' @examples check_missing(df)
check_missing <- function(df) {
  result <- sapply(df, function(x) sum(is.na(x)))
  percent <- round(result / nrow(df) * 100, 2)
  data.frame(
    Column = names(result),
    Missing_Count = as.vector(result),
    Missing_Percent = percent
  )
}

#' check_duplicates
#'
#' Find duplicate rows in a dataframe.
#' 
#' @param df Dataframe.
#' @return A dataframe of duplicate rows (or message if none found).
#' @examples check_duplicates(df)
check_duplicates <- function(df) {
  dup_rows <- df[duplicated(df), ]
  if (nrow(dup_rows) == 0) {
    message("No duplicate rows found.")
  }
  return(dup_rows)
}

#' check_outliers
#'
#' Identify numeric columns with values beyond 1.5x IQR.
#' 
#' @param df Dataframe.
#' @return A dataframe with count of outliers per numeric column.
#' @examples check_outliers(df)
check_outliers <- function(df) {
  numeric_cols <- df %>% dplyr::select(where(is.numeric))
  outlier_summary <- lapply(numeric_cols, function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    sum(x < lower | x > upper, na.rm = TRUE)
  })
  data.frame(
    Variable = names(outlier_summary),
    Outlier_Count = as.integer(unlist(outlier_summary))
  )
}

#' describe_data
#'
#' Provide summary statistics for numeric and categorical columns.
#' 
#' @param df Dataframe.
#' @return A list with numeric and categorical summaries.
#' @examples describe_data(df)
describe_data <- function(df) {
  numeric_summary <- df %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::summarise_all(list(
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE)
    ), .groups = "drop")
  
  categorical_summary <- df %>%
    dplyr::select(where(~is.character(.) || is.factor(.))) %>%
    purrr::map(~table(., useNA = "ifany")) %>%
    purrr::map_df(~data.frame(Level = names(.), Count = as.integer(.)), .id = "Variable")
  
  list(
    Numeric_Summary = numeric_summary,
    Categorical_Summary = categorical_summary
  )
}

#' profile_data
#'
#' Generate an interactive data profile report using DataExplorer.
#' 
#' @param df Dataframe.
#' @return An HTML report opened in the browser.
#' @examples profile_data(df)
profile_data <- function(df) {
  if (!requireNamespace("DataExplorer", quietly = TRUE)) {
    stop("Package 'DataExplorer' is required. Please install it with install.packages('DataExplorer').")
  }
  DataExplorer::create_report(df)
}
