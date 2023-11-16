#' Get Category Members
#'
#' This function retrieves a list of pages or subcategories within a given
#' category from the Wikipedia MediaWiki API.
#'
#' @param category The name of the category to retrieve members for.
#' @param lang The language code for the Wikipedia site to query (default is
#'   "en").
#' @param namespace The namespace to retrieve members for (0 for pages, 14 for
#'   categories; default is 14).
#'
#' @return A dataframe with one row for each category member, containing
#'   information such as title and namespace.
#'
#' @examples
#' get_category_members("Animals")
#' get_category_members("Moral philosophers")
#'
#' @export
get_category_members = function(category,
                                lang = "en",
                                namespace = 14,
                                verbose = TRUE) {
  
  # Set the base URL for the MediaWiki Action API
  base_url = paste0("https://", lang, ".wikipedia.org/w/api.php")
  
  # Check inputs
  if (namespace == 0) {
    cmtype = "page"
  } else if (namespace == 14) {
    cmtype = "subcat"
  } else {
    stop("Invalid namespace input; must be 0 (page) or 14 (category)")
  }
  if (!startsWith(x = category, prefix = "Category:")) {
    cmtitle = paste0("Category:", category)
  } else {
    cmtitle = category
  }
  
  if (verbose) {
    message("Retrieving ", cmtype, "s for ", cmtitle)
  }
  
  params = list(
    action = "query",
    format = "json",
    list = "categorymembers",
    cmtitle = cmtitle,
    cmtype = cmtype,
    cmlimit = "max",
    cmnamespace = namespace
  )
  
  # Initialize a list to store the category members
  member_list = list()
  
  # Send the GET request, parse the JSON response, and extract subcategories
  response = httr::GET(url = base_url, query = params)
  data = jsonlite::fromJSON(httr::content(response, "text"))
  member_list[[1]] = data$query$categorymembers
  
  while ("continue" %in% names(data)) {
    
    params[["cmcontinue"]] = data$continue$cmcontinue
    
    response = httr::GET(url = base_url, query = params)
    data = jsonlite::fromJSON(httr::content(response, "text"))
    member_list[[length(member_list) + 1]] = data$query$categorymembers
    
  }
  
  # Concatenate all the links into a single list and filter based on namespace
  category_members = do.call(what = "rbind", args = member_list)
  
  # Return dataframe with one row for each category member
  return(category_members)
  
}
