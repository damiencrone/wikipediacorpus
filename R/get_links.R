#' Retrieve links from a Wikipedia page
#'
#' @param page A title (string) of the Wikipedia page to retrieve links from
#' @param direction A string indicating the type of links to retrieve. Must be
#'   either "incoming" (links pointing to the specified page) or "outgoing"
#'   (links on the specified page). Default is "outgoing".
#' @param lang The language code of the Wikipedia to retrieve links from.
#'   Default is "en" (English).
#' @param namespaces An integer vector of namespaces to filter links by. Default
#'   is c(0) (main namespace).
#'
#' @return A data frame containing the title, namespace, and page ID of all
#'   retrieved links.
#'
#' @examples
#' get_links("Psychology")
#' get_links("Drop bear", direction = "outgoing")
#'
#' @export
get_links = function(page,
                     direction = "outgoing",
                     lang = "en",
                     namespaces = c(0)) {
  
  # Check inputs
  if (!is.numeric(namespaces)) {
    stop("namespaces input must be an integer vector")
  }
  if (!direction %in% c("incoming", "outgoing")) {
    stop("Invalid input for direction: Must be 'incoming' or 'outgoing'")
  }
  
  # Set the base URL for the MediaWiki Action API
  base_url = paste0("https://", lang, ".wikipedia.org/w/api.php")
  
  if (direction == "incoming") {
    prop = "linkshere"
    lhprop = "title"
    lhlimit = "max"
    plprop = NULL
    pllimit = NULL
  } else if (direction == "outgoing") {
    prop = "links"
    plprop = "ids"
    pllimit = "max"
    lhprop = NULL
    lhlimit = NULL
  }
  
  # Set the parameters for the API call
  params = list(
    action = "query",
    format = "json",
    titles = "page",
    prop = prop,
    lhprop = lhprop,
    lhlimit = lhlimit,
    plprop = plprop,
    pllimit = pllimit
  )
  
  # Initialize a list to store the links
  links_list = list()
  
  # Send the GET request, parse the JSON response, and extract links
  response = httr::GET(url = base_url, query = params)
  data = jsonlite::fromJSON(httr::content(response, "text"))
  
  if (direction == "incoming") {
    links_list[[1]] = data$query$pages[[1]]$linkshere
    tbc = !is.null(data$continue$lhcontinue)
  } else if (direction == "outgoing") {
    links_list[[1]] = data$query$pages[[1]]$links
    tbc = !is.null(data$continue$plcontinue)
  }
  
  # Check if there are more links to retrieve
  while (tbc) {
    
    # Update the parameters with the continue information
    if (direction == "incoming") {
      params[["lhcontinue"]] = data$continue$lhcontinue
    } else if (direction == "outgoing") {
      params[["plcontinue"]] = data$continue$plcontinue
    }
    
    # Send the GET request, parse the JSON response, and extract links
    response = httr::GET(url = base_url, query = params)
    data = jsonlite::fromJSON(httr::content(response, "text"))
    if (direction == "incoming") {
      links_list[[length(links_list) + 1]] = data$query$pages[[1]]$linkshere
      tbc = !is.null(data$continue$lhcontinue)
    } else if (direction == "outgoing") {
      links_list[[length(links_list) + 1]] = data$query$pages[[1]]$links
      tbc = !is.null(data$continue$plcontinue)
    }
    
  }
  
  # Concatenate all the links into a single list and filter based on namespace
  links = do.call(what = "rbind", args = links_list)
  links = links[links$ns %in% namespaces,]
  
  return(links)
  
}
