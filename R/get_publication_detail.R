#' Get Publication Detail from TR Dizin
#'
#' This function retrieves detailed information about a specific publication
#' from the TR Dizin database using the publication's unique ID.
#'
#' @param id A character string or numeric value representing the publication's ID.
#'
#' @return A parsed list containing the JSON response for the specified publication.
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom jsonlite fromJSON
get_publication_detail <- function(id) {

  # Validate that 'id' is provided
  if (missing(id) || is.null(id)) {
    stop("Parameter 'id' must be provided.")
  }

  # Convert numeric id to character if needed
  id <- as.character(id)

  # Construct the URL for publication detail API endpoint
  # Based on the "Tanımlı URL Adresleri" section in documentation
  json_url <- paste0("https://search.trdizin.gov.tr/api/publicationById/", id)

  # Execute the HTTP GET request
  response <- httr::GET(url = json_url)

  # Check for HTTP errors
  httr::stop_for_status(response)

  # Parse and return the JSON response
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
