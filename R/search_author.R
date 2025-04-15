#' Search for Authors in TR Dizin
#'
#' This function queries the TR Dizin API for author data based on the specified search parameters.
#' It constructs a GET request that includes required parameters and additional facet filters via the ellipsis (`...`).
#'
#' @param q A character string representing the search query.
#' @param order A character string defining the sort order. Default is "relevance-DESC".
#'   Allowed values typically include "title-ASC", "title-DESC", "orderCitationCount-ASC",
#'   "orderCitationCount-DESC", "orderPublicationCount-ASC", "orderPublicationCount-DESC", or "relevance-DESC".
#' @param page An integer indicating the page number of the results. Default is 1.
#' @param limit An integer indicating the maximum number of records per page. Default is 20.
#' @param ... Additional named parameters for facet filtering.
#'
#' @return A parsed list containing the JSON response from the TR Dizin API.
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils modifyList
search_author <- function(q, order = "relevance-DESC", page = 1, limit = 20, ...) {

  # Validate the 'q' parameter.
  if (missing(q) || !is.character(q) || nchar(q) == 0) {
    stop("Parameter 'q' must be a non-empty character string.")
  }

  # Base URL for author search
  base_url <- "https://search.trdizin.gov.tr/api/defaultSearch/author/"

  # Build the query parameter list with required parameters.
  query_params <- list(
    q     = q,
    order = order,
    page  = page,
    limit = limit
  )

  # Incorporate additional facet filters from the ellipsis.
  extra_params <- list(...)
  query_params <- modifyList(query_params, extra_params)

  # Execute the HTTP GET request using httr.
  response <- httr::GET(url = base_url, query = query_params)

  # Check for HTTP errors and stop if one occurred.
  httr::stop_for_status(response)

  # Parse and return the JSON response.
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
