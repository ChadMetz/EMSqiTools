#' Plot R Chart
#'
#' Plot R Chart function from EMSqiTools package.
#'
#' @param ... Parameters passed to `plot_r_chart`.
#'
#' @return Output from `plot_r_chart`.
#' @export

plot_r_chart <- function(df, date_col, id_col, num_condition, den_condition = "TRUE",
                               time_unit = c("week", "month", "quarter"),
                               name = "QI Control Chart", annotations = NULL,
                               plot_width = 12, plot_height = 3) {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(grid)
  library(rlang)
  
  time_unit <- match.arg(time_unit)
  
  if (!(date_col %in% names(df))) {
    stop(paste("Column", date_col, "not found in dataframe."))
  }
  
  tryCatch({
    parse_expr(num_condition)
    parse_expr(den_condition)
  }, error = function(e) {
    stop("num_condition or den_condition contains invalid R code. Use '&' or '|' instead of commas.")
  })
  
  df <- df %>%
    mutate(date_var = as.Date(.data[[date_col]]),
           period = as.Date(floor_date(date_var, unit = time_unit)))
  
  den_df <- df %>%
    filter(!!parse_expr(den_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Denominator = n(), .groups = "drop")
  
  num_df <- df %>%
    filter(!!parse_expr(num_condition)) %>%
    distinct(period, .data[[id_col]]) %>%
    group_by(period) %>%
    summarise(Numerator = n(), .groups = "drop")
  
  summary <- den_df %>%
    left_join(num_df, by = "period") %>%
    mutate(
      Numerator = ifelse(is.na(Numerator), 0, Numerator),
      p = ifelse(Denominator > 0, Numerator / Denominator, NA)
    )
  
  # --- Detect Shifts ---
  side <- summary$p > mean(summary$p, na.rm = TRUE)
  rle_obj <- rle(side)
  lens <- rle_obj$lengths
  shift_points <- cumsum(lens)[lens >= 8]
  shift_starts <- shift_points - lens[which(lens >= 8)] + 1
  shift_starts <- sort(unique(c(1, shift_starts)))
  
  # --- Segmented Control Limits ---
  summary$CL_adj <- NA
  summary$UCL_adj <- NA
  summary$LCL_adj <- NA
  
  for (i in seq_along(shift_starts)) {
    start_idx <- shift_starts[i]
    end_idx <- if (i < length(shift_starts)) shift_starts[i + 1] - 1 else nrow(summary)
    segment <- summary[start_idx:end_idx, ]
    cl <- mean(segment$p, na.rm = TRUE)
    sd <- sqrt((cl * (1 - cl)) / segment$Denominator)
    
    summary$CL_adj[start_idx:end_idx] <- cl
    summary$UCL_adj[start_idx:end_idx] <- pmin(cl + 2 * sd, 1)
    summary$LCL_adj[start_idx:end_idx] <- pmax(cl - 2 * sd, 0)
  }
  
  step_df <- summary %>%
    select(period, UCL_adj, LCL_adj) %>%
    mutate(period = period - days(3)) %>%
    bind_rows(
      summary[nrow(summary), ] %>%
        select(period, UCL_adj, LCL_adj) %>%
        mutate(period = max(summary$period))
    )
  
  cl_labels <- summary %>%
    slice(shift_starts) %>%
    mutate(
      Label = paste0("CL = ", percent(CL_adj, accuracy = 1)),
      X_Adjust = period + 7,
      Y_Pos = CL_adj + 0.02
    )
  
  shift_lines <- summary %>%
    slice(shift_starts) %>%
    filter(row_number() != 1) %>%
    transmute(Start_Date = period)
  
  # --- Annotations ---
  if (!is.null(annotations)) {
    annotations <- annotations %>%
      mutate(
        Date = as.Date(Date),
        Side = ifelse(is.null(Side), "right", Side),
        hjust_value = ifelse(Side == "left", 1, 0),
        Y_Pos = ifelse(is.na(Y_Pos), max(summary$UCL_adj, na.rm = TRUE) + 0.05, Y_Pos),
        Shape = ifelse(is.null(Shape), 21, Shape),
        x_adjusted = ifelse(Side == "left", Date - days(1), Date + days(1))
      )
  }
  
  # --- Plot ---
  p <- ggplot(summary, aes(x = period, y = p)) +
    geom_line(linewidth = 1, color = "#333333") +
    geom_point(size = 3, color = "black") +
    geom_step(data = step_df, aes(x = period, y = UCL_adj), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_step(data = step_df, aes(x = period, y = LCL_adj), linetype = "dotted", color = "#C8102E", linewidth = 1) +
    geom_line(aes(y = CL_adj), color = "#003DA5", linewidth = 1) +
    geom_text(data = cl_labels, aes(x = X_Adjust - 4, y = Y_Pos + 0.02, label = Label),
              color = "#003DA5", size = 5, hjust = 0, fontface = "bold") +
    geom_segment(data = shift_lines, aes(x = Start_Date, xend = Start_Date, y = 0, yend = 1),
                 color = "#C8102E", linetype = "solid", linewidth = 1)
  
  if (!is.null(annotations)) {
    p <- p +
      geom_segment(data = annotations, aes(x = Date, xend = Date, y = 0, yend = Y_Pos),
                   color = "black", linetype = "dotted", linewidth = 0.8) +
      geom_point(data = annotations, aes(x = Date, y = Y_Pos, shape = Label),
                 size = 2.5, fill = "black", color = "black", stroke = 1.2) +
      scale_shape_manual(name = "Event Annotations", values = setNames(annotations$Shape, annotations$Label))
  }
  
  p <- p +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.1, 1.05)) +
    theme_minimal(base_size = 12, base_family = "Segoe UI") +
    theme(
      axis.text = element_text(face = "bold", color = "#1A1A1A", size = 12),
      axis.title = element_text(face = "bold", color = "#1A1A1A", size = 12),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(color = "#1A1A1A"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#003DA5"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(x = tools::toTitleCase(time_unit), y = "Proportion", title = name)
  
  # Display and Save
  grid.newpage()
  grid.draw(ggplotGrob(p))
  
  assign("last_qi_plot", p, envir = .GlobalEnv)
  assign("last_qi_plot_name", name, envir = .GlobalEnv)
  assign("last_qi_summary", summary, envir = .GlobalEnv)
  
  return(summary)
}
