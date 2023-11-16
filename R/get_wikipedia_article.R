#' Retrieve Wikipedia Article Text
#'
#' This function retrieves the text of a Wikipedia article based on the provided
#' title and language.
#'
#' @param title The title of the Wikipedia article to retrieve.
#' @param lang The language of the Wikipedia article to retrieve. Default is
#'   "en" for English.
#' @param verbose logical indicating whether or not to print progress updates
#'   (default is TRUE)
#'
#' @return A character string containing the text of the Wikipedia article.
#'
#' @examples
#' get_wikipedia_article("R programming language")
#' get_wikipedia_article("Programmation R", lang = "fr")
#'
#' @export
get_wikipedia_article = function (title, lang = "en", verbose = TRUE) {
  
  # Set the base URL for the MediaWiki API
  base_url = paste0("https://", lang, ".wikipedia.org/w/api.php")
  
  if (verbose) {message("Retriving text for the article: ", title)}
  
  # Set the parameters for the API call
  params = list(
    action = "query",
    format = "json",
    prop = "extracts",
    explaintext = "1",
    titles = title
  )
  
  # Send the GET request, parse the JSON response, and extract text
  response = httr::GET(base_url, query = params)
  content = httr::content(response, as = "text")
  data = jsonlite::fromJSON(content)
  page_text = data$query$pages[[1]]$extract
  
  return(page_text)
  
}