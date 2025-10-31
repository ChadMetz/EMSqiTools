table_p_chart <- function(df,
                          date_col,
                          id_col = NULL,
                          num_col,
                          num_value,
                          den_col,
                          den_value = NULL,
                          time_unit = "month",
                          name = "Summary Table",
                          months_to_show = 6,
                          start_date = NULL,
                          end_date = NULL,
                          theme_font = 8,
                          benchmark_df = NULL,
                          annotations = NULL,
                          return_table = FALSE,
                          sccad_colors = list(
                            red = "#EB0029",
                            dark_blue = "#001689",
                            light_blue = "#4A5AC7",
                            light_red = "#F25C68",
                            gray = "#E1E1E1",
                            white = "#FFFFFF"
                          )) {

  library(dplyr)
  library(lubridate)
  library(rlang)

  # --- Added helper for NA-safe denominator matching ---
  den_match <- function(x, values) {                    # ← added
    has_na <- any(is.na(values))                        # ← added
    vals   <- values[!is.na(values)]                    # ← added
    (x %in% vals) | (has_na & is.na(x))                 # ← added
  }                                                     # ← added
  # -----------------------------------------------------

  output_type <- tryCatch(rmarkdown::pandoc_to(), error = function(e) "unknown")
  is_word <- output_type == "docx"
  is_html <- output_type == "html"
  is_latex <- output_type == "latex"

  df <- df %>%
    mutate(
      date_parsed = parse_date_time(.data[[date_col]], orders = c("ymd", "mdy", "dmy")),
      date = floor_date(date_parsed, unit = time_unit)
    ) %>%
    filter(!is.na(date))

  if (!is.null(start_date)) df <- df %>% filter(date >= as.Date(start_date))
  if (!is.null(end_date))   df <- df %>% filter(date <= as.Date(end_date))

  # --- Replaced denominator filter with NA-aware version ---
  if (!is.null(den_value)) df <- df %>% filter(den_match(.data[[den_col]], den_value))  # ← modified
  # ----------------------------------------------------------

  if (!is.null(id_col)) {
    den_df <- df %>%
      filter(!is.na(.data[[id_col]])) %>%
      distinct(date, .data[[id_col]]) %>%
      group_by(date) %>%
      summarise(Denominator = n(), .groups = "drop")

    num_df <- df %>%
      filter(.data[[num_col]] %in% num_value) %>%
      filter(!is.na(.data[[id_col]])) %>%
      distinct(date, .data[[id_col]]) %>%
      group_by(date) %>%
      summarise(Numerator = n(), .groups = "drop")
  } else {
    den_df <- df %>%
      group_by(date) %>%
      summarise(Denominator = n(), .groups = "drop")

    num_df <- df %>%
      filter(.data[[num_col]] %in% num_value) %>%
      group_by(date) %>%
      summarise(Numerator = n(), .groups = "drop")
  }

  df <- den_df %>%
    left_join(num_df, by = "date") %>%
    mutate(
      Numerator = ifelse(is.na(Numerator), 0, Numerator),
      Proportion = Numerator / Denominator
    )

  has_benchmark <- !is.null(benchmark_df)

  if (has_benchmark) {
    benchmark_df <- benchmark_df %>%
      mutate(date = floor_date(date, unit = time_unit)) %>%
      group_by(date) %>%
      summarise(`National Proportion` = mean(rate, na.rm = TRUE), .groups = "drop")

    df <- df %>% left_join(benchmark_df, by = "date")
  }

  df <- df %>%
    mutate(
      Date_fmt = date,
      Date = format(date, "%b %Y")
    )

  if (has_benchmark) {
    df <- df %>% select(Date_fmt, Date, Denominator, Numerator, Proportion, `National Proportion`)
  } else {
    df <- df %>% select(Date_fmt, Date, Denominator, Numerator, Proportion)
  }

  if (!is.null(end_date)) df <- df %>% filter(Date_fmt <= as.Date(end_date))

  df_filtered <- df %>%
    arrange(Date_fmt) %>%
    slice_tail(n = months_to_show) %>%
    select(-Date_fmt)

  if (return_table) return(df_filtered)

  if (is_html) {
    library(gt)
    gt_tbl <- gt::gt(df_filtered) %>%
      gt::tab_header(title = name) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::fmt_percent(columns = "Proportion", decimals = 1) %>%
      gt::fmt_number(columns = c("Denominator", "Numerator"), decimals = 0)

    if (has_benchmark) {
      gt_tbl <- gt_tbl %>%
        gt::fmt_percent(columns = "National Proportion", decimals = 1) %>%
        gt::cols_label(
          Date = "Date",
          Denominator = "Denominator",
          Numerator = "Numerator",
          Proportion = "Proportion",
          `National Proportion` = "National Proportion"
        )
    } else {
      gt_tbl <- gt_tbl %>%
        gt::cols_label(
          Date = "Date",
          Denominator = "Denominator",
          Numerator = "Numerator",
          Proportion = "Proportion"
        )
    }

    return(
      gt_tbl %>%
        gt::opt_table_font(size = theme_font) %>%
        gt::tab_options(
          table.border.top.width = gt::px(0),
          table_body.hlines.style = "solid",
          table_body.hlines.color = sccad_colors$gray,
          table_body.hlines.width = gt::px(1)
        )
    )

  } else if (is_word || is_latex) {
    library(flextable)
    df_flex <- df_filtered %>%
      mutate(Proportion = sprintf("%.1f%%", Proportion * 100))

    if (has_benchmark) {
      df_flex <- df_flex %>%
        mutate(`National Proportion` = sprintf("%.1f%%", `National Proportion` * 100))
    }

    cat(glue::glue("### {name}\n\n"))

    return(
      flextable::flextable(df_flex) %>%
        flextable::fontsize(size = theme_font, part = "all") %>%
        flextable::autofit()
    )

  } else {
    df_md <- df_filtered %>%
      mutate(Proportion = sprintf("%.1f%%", Proportion * 100))

    if (has_benchmark) {
      df_md <- df_md %>%
        mutate(`National Proportion` = sprintf("%.1f%%", `National Proportion` * 100))
    }

    return(knitr::kable(df_md, format = "markdown", caption = name))
  }
}
