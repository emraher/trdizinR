#' Get Institution Detail from TR Dizin
#'
#' This function retrieves detailed information about a specific institution
#' from the TR Dizin database using the institution's numeric ID.
#'
#' @param id A character string or numeric value representing the institution's ID.
#'
#' @return A parsed list containing the JSON response for the specified institution.
#' @export
get_institution_detail <- function(id) {

  # Input validation
  if (missing(id) || is.null(id)) {
    stop("Parameter 'id' must be provided.")
  }

  # Convert to character for use in URL
  id <- as.character(id)

  # Construct API URL using inferred institution endpoint
  json_url <- paste0("https://search.trdizin.gov.tr/api/institutionByCode/", id)

  # Perform the GET request
  response <- httr::GET(url = json_url)

  # Handle HTTP errors
  httr::stop_for_status(response)

  # Parse and return JSON
  parsed_response <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

  return(parsed_response)
}
