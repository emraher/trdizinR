#' Get Journal Detail from TR Dizin
#'
#' This function retrieves detailed information about a specific journal
#' from the TR Dizin database using the journal's numeric ID.
#'
#' @param id A character string or numeric value representing the journal's ID.
#'
#' @return A parsed list containing the JSON response for the specified journal.
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom jsonlite fromJSON
get_journal_detail <- function(id) {

  # Input validation
  if (missing(id) || is.null(id)) {
    stop("Parameter 'id' must be provided.")
  }

  # Coerce to character for URL construction
  id <- as.character(id)

  # Construct the API URL
  json_url <- paste0("https://search.trdizin.gov.tr/api/journalById/", id)

  # Make the HTTP GET request
  response <- httr::GET(url = json_url)

  # Raise an error if the response is not successful
  httr::stop_for_status(response)

  # Parse and return the JSON content
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
