#' save_qi_spc
#'
#' Save the last SPC plot (stored in 'last_qi_plot') to a PNG file.
#' Optionally save the summary table to a CSV.
#'
#' @param path Directory path to save the files (default '.').
#' @param width Width of the saved plot in inches (default 12).
#' @param height Height of the saved plot in inches (default 3).
#' @param dpi Resolution in dots per inch (default 300).
#' @param save_table Logical; if TRUE, also saves the last summary table as a CSV (default FALSE).
#'
#' @return Saves a PNG plot file and optionally a CSV table file. Prints save message.
#'
#' @examples
#' save_qi_spc(path = ".", width = 18, height = 6, save_table = TRUE)
save_qi_spc <- function(path = ".", width = 12, height = 3, dpi = 300, save_table = FALSE) {
  if (!exists("last_qi_plot") || !exists("last_qi_plot_name")) {
    stop("No plot found. Run qi_spc() first.")
  }
  
  safe_name <- gsub("[^A-Za-z0-9_]", "_", last_qi_plot_name)
  plot_file <- file.path(path, paste0(safe_name, ".png"))
  ggsave(plot_file, plot = last_qi_plot, width = width, height = height, dpi = dpi)
  message("Plot saved to: ", plot_file)
  
  if (save_table) {
    if (!exists("last_qi_summary")) {
      warning("No summary table found. Make sure you saved the summary from qi_spc().")
    } else {
      table_file <- file.path(path, paste0(safe_name, "_summary.csv"))
      readr::write_csv(last_qi_summary, table_file)
      message("Summary table saved to: ", table_file)
    }
  }
}
