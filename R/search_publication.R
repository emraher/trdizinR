#' Search for Publications in TR Dizin
#'
#' This function queries the TR Dizin API for publication data based on the
#' specified search parameters. It constructs a GET request with the basic
#' parameters and additional facet filters provided via the ellipsis (`...`).
#'
#' @param q A character string representing the search query.
#' @param order A character string defining the sort order. Default is "relevance-DESC".
#' @param page An integer indicating the page number of the results. Default is 1.
#' @param limit An integer indicating the maximum number of records per page. Default is 20.
#' @param ... Additional named parameters for facet filtering (e.g., `facet-publication_year = 2020`).
#'
#' @return A parsed list containing the JSON response from the TR Dizin API.
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils modifyList

search_publication <- function(q, order = "relevance-DESC", page = 1, limit = 20, ...) {
  # Validate input 'q'
  if (missing(q) || !is.character(q) || nchar(q) == 0) {
    stop("Parameter 'q' must be a non-empty character string.")
  }

  # Base URL for TR Dizin publication search
  base_url <- "https://search.trdizin.gov.tr/api/defaultSearch/publication/"

  # Build the query parameters
  query_params <- list(
    q     = q,
    order = order,
    page  = page,
    limit = limit
  )

  # Merge additional parameters provided via ...
  extra_params <- list(...)
  query_params <- modifyList(query_params, extra_params)

  # Perform the HTTP GET request
  response <- httr::GET(url = base_url, query = query_params)

  # Check for HTTP errors
  httr::stop_for_status(response)

  # Parse the response as JSON
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
