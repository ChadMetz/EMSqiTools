#' qi_spc
#'
#' Create a statistical process control (SPC) chart with annotations and optional control limits.
#'
#' @param df Dataframe containing the data.
#' @param date_col Name of the date column (string).
#' @param id_col Unique identifier column (string).
#' @param num_condition Filter condition for numerator (string, R expression).
#' @param den_condition Filter condition for denominator (string, R expression, default 'TRUE').
#' @param time_unit Time unit for aggregation ('week', 'month', 'quarter').
#' @param name Chart title.
#' @param annotations Optional dataframe with columns Date, Label, Side, Y_Pos for vertical annotations.
#' @param plot_width Width of the plot when displayed (default 12 inches).
#' @param plot_height Height of the plot when displayed (default 3 inches).
#'
#' @return A summary dataframe with period, numerator, denominator, proportion, and control limits.
#' Also displays the SPC chart.
#'
#' @examples
#' annotations <- data.frame(
#'   Date = as.Date(c("2024-09-01", "2025-02-15")),
#'   Label = c("Go Live", "Townhall"),
#'   Side = c("right", "left")
#' )
#' qi_spc(df, "Incident Date", "pcr", "hr > 100", "TRUE", "week", "High HR", annotations)

qi_spc <- function(df, date_col, id_col, num_condition, den_condition = "TRUE",
                   time_unit = c("week", "month", "quarter"),
                   name = "QI SPC Chart", annotations = NULL,
                   plot_width = 12, plot_height = 3) {
  
  time_unit <- match.arg(time_unit)
  
  # Check if date_col exists
  if (!(date_col %in% names(df))) {
    stop(paste("Column", date_col, "not found in dataframe. Check your column names: ", paste(names(df), collapse = ', ')))
  }
  
  # Validate condition inputs
  tryCatch({
    rlang::parse_expr(num_condition)
    rlang::parse_expr(den_condition)
  }, error = function(e) {
    stop("num_condition or den_condition contains invalid R code. Use '&' or '|' instead of commas.")
  })
  
  # Ensure date and period are Date class
  df <- df %>%
    mutate(date_var = as.Date(.data[[date_col]]),
           period = as.Date(lubridate::floor_date(date_var, time_unit)))
  
  den_df <- df %>%
    filter(!! rlang::parse_expr(den_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Denominator = dplyr::n(), .groups = "drop")
  
  num_df <- df %>%
    filter(!! rlang::parse_expr(num_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Numerator = dplyr::n(), .groups = "drop")
  
  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator),
           p = ifelse(Denominator > 0, Numerator / Denominator, NA),
           CL = mean(p, na.rm = TRUE),
           UCL = pmin(CL + 2 * sqrt((CL * (1 - CL)) / Denominator), 1),
           LCL = pmax(CL - 2 * sqrt((CL * (1 - CL)) / Denominator), 0))
  
  step_df <- summary %>%
    select(period, CL, UCL, LCL) %>%
    mutate(period = as.Date(period) - lubridate::days(3)) %>%
    dplyr::bind_rows(summary[nrow(summary), ] %>%
                       select(period, CL, UCL, LCL) %>%
                       mutate(period = as.Date(max(summary$period))))
  
  # Debug: Check classes before plotting
  message("period class: ", class(summary$period))
  if (!is.null(annotations)) {
    message("annotations$Date class: ", class(annotations$Date))
  }
  
  # Plot
  p <- ggplot(summary, aes(x = period, y = p)) +
    geom_line(linewidth = 1, color = "black") +
    geom_point(size = 2, color = "black") +
    geom_step(data = step_df, aes(x = period, y = UCL), linetype = "dotted", color = "red", linewidth = 1) +
    geom_step(data = step_df, aes(x = period, y = LCL), linetype = "dotted", color = "red", linewidth = 1) +
    geom_hline(yintercept = mean(summary$p, na.rm = TRUE), linetype = "solid", color = "blue", linewidth = 1) +
    labs(
      title = name,
      x = tools::toTitleCase(time_unit),
      y = "Percentage"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0.02, 0.02)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold")
    )
  
  # Annotations (if provided)
  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Date = as.Date(Date),
        hjust_value = ifelse(Side == "left", 1, 0),
        Y_Pos = ifelse(is.na(Y_Pos), max(summary$UCL, na.rm = TRUE) + 0.05, Y_Pos),
        x_adjusted = as.Date(ifelse(Side == "left", Date - lubridate::days(1), Date + lubridate::days(1)))
      )
    
    p <- p +
      geom_vline(data = annotations, aes(xintercept = Date), linetype = "dashed", color = "blue", linewidth = 1) +
      geom_text(data = annotations,
                aes(x = x_adjusted, y = Y_Pos, label = Label, hjust = hjust_value),
                color = "blue", angle = 0, vjust = -2, size = 4)
  }
  
  # Display the plot
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplotGrob(p))
  
  # Save plot + summary to global environment
  assign("last_qi_plot", p, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)
  
  return(summary)
}
