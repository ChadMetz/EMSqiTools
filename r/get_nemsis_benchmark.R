#' Retrieve NEMSIS National Benchmark Performance Data
#'
#' Fetches national performance measure data from the NEMSIS SOAP web service for a given
#' benchmark ID across one or more years. Returns a tidy dataframe with monthly rates.
#'
#' @param benchmark_id Character. The benchmark ID to query (e.g., `"Seizure-02"`).
#' @param years Integer vector. One or more calendar years to include (default: `c(2023, 2024, 2025)`).
#'
#' @return A data frame with columns: `year`, `month`, `numerator`, `denominator`, `rate`, `date`, `rate_percent`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get national performance data for Seizure-02 benchmark
#' seizure_data <- get_nemsis_benchmark("Seizure-02", years = 2023:2025)
#'
#' # Plot monthly rate
#' library(ggplot2)
#' ggplot(seizure_data, aes(x = date, y = rate)) +
#'   geom_line(color = "darkred") +
#'   scale_y_continuous(labels = scales::percent_format()) +
#'   labs(title = "National Seizure-02 Performance Rate", y = "Rate", x = "Date")
#' }
get_nemsis_benchmark <- function(benchmark_id, years = c(2023, 2024, 2025)) {
  library(httr)
  library(xml2)
  library(dplyr)

  send_soap_request <- function(body_xml) {
    url <- "https://perfmeasures.nemsis.org/perfMeasureWs"
    response <- POST(url,
                     add_headers('Content-Type' = 'text/xml'),
                     body = body_xml)
    content(response, as = "text", encoding = "UTF-8")
  }

  get_nemsis_data <- function(benchmark_id, year) {
    soap_body <- paste0('
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
        xmlns:ws="http://ws.nemsis.org/">
        <soapenv:Header/>
        <soapenv:Body>
          <ws:RetrieveBenchmarkRequest>
            <ws:benchmark id="', benchmark_id, '">
              <ws:filter parameter="CalendarYear">', year, '</ws:filter>
              <ws:segment>CalendarMonth</ws:segment>
            </ws:benchmark>
          </ws:RetrieveBenchmarkRequest>
        </soapenv:Body>
      </soapenv:Envelope>')
    send_soap_request(soap_body)
  }

  parse_nemsis_response <- function(xml_string, year) {
    xml <- read_xml(xml_string)
    ns <- c(ns = "http://ws.nemsis.org/")
    results <- xml_find_all(xml, ".//ns:result", ns = ns)

    if (length(results) == 0) return(data.frame())

    data.frame(
      year = year,
      month = as.integer(substr(xml_attr(results, "segmentMember"), 5, 6)),
      numerator = as.numeric(xml_text(xml_find_first(results, ".//ns:numerator", ns = ns))),
      denominator = as.numeric(xml_text(xml_find_first(results, ".//ns:denominator", ns = ns)))
    ) %>%
      mutate(
        rate = numerator / denominator,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  }

  bind_rows(lapply(years, function(y) {
    raw <- tryCatch(get_nemsis_data(benchmark_id, y), error = function(e) NULL)
    if (!is.null(raw)) parse_nemsis_response(raw, y) else data.frame()
  })) %>%
    mutate(rate_percent = rate * 100)
}
