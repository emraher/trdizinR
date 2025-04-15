#' Get Author Detail from TR Dizin
#'
#' This function retrieves detailed information about a specific author
#' from the TR Dizin database using the author's numeric ID.
#'
#' @param id A character string or numeric value representing the author's ID.
#'
#' @return A parsed list containing the JSON response for the specified author.
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom jsonlite fromJSON
get_author_detail <- function(id) {

  # Validate input
  if (missing(id) || is.null(id)) {
    stop("Parameter 'id' must be provided.")
  }

  # Ensure the ID is a character string
  id <- as.character(id)

  # Construct the API URL using the hidden endpoint discovered via DevTools
  json_url <- paste0("https://search.trdizin.gov.tr/api/authorById/", id)

  # Send HTTP GET request
  response <- httr::GET(url = json_url)

  # Raise error if the response fails
  httr::stop_for_status(response)

  # Parse the JSON content and return
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
