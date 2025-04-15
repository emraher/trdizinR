#' Search for Institutions in TR Dizin
#'
#' This function queries the TR Dizin API for institution data based on the specified search parameters.
#' It builds a GET request with the required parameters along with additional facet filters provided via the ellipsis (`...`).
#'
#' @param q A character string representing the search query.
#' @param order A character string defining the sort order. Default is "relevance-DESC".
#'   Allowed values typically include "title-ASC", "title-DESC", or "relevance-DESC".
#' @param page An integer indicating the page number of the results. Default is 1.
#' @param limit An integer indicating the maximum number of records per page. Default is 20.
#' @param ... Additional named parameters for facet filtering (e.g., `facet-type = "KAMUKURULUS"`).
#'
#' @return A parsed list containing the JSON response from the TR Dizin API.
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils modifyList
search_institution <- function(q, order = "relevance-DESC", page = 1, limit = 20, ...) {

  # Validate that 'q' is provided as a non-empty character string.
  if (missing(q) || !is.character(q) || nchar(q) == 0) {
    stop("Parameter 'q' must be a non-empty character string.")
  }

  # Base URL for institution search
  base_url <- "https://search.trdizin.gov.tr/api/defaultSearch/institution/"

  # Build the query parameters list with the required parameters.
  query_params <- list(
    q     = q,
    order = order,
    page  = page,
    limit = limit
  )

  # Merge any additional facet filters provided via the ellipsis.
  extra_params <- list(...)
  query_params <- modifyList(query_params, extra_params)

  # Execute the HTTP GET request using httr.
  response <- httr::GET(url = base_url, query = query_params)

  # Check for HTTP errors and stop execution if any errors occur.
  httr::stop_for_status(response)

  # Parse and return the JSON response.
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
