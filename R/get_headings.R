#' Extract Headings from Wikipedia Text
#' 
#' This function extracts headings from a Wikipedia text using a regular expression pattern.
#' 
#' @param text A character string of Wikipedia text.
#' @param heading_pattern A regular expression pattern used to match headings. Default is "\n *={2} *[^=].+ *={2} *\n" which matches level 1 headings.
#' 
#' @return A character vector of headings, with the formatting ("==") removed.
#' 
#' @examples
#' text = "== Head1 ==\nText1\n== Head2 ==\nText2"
#' get_headings(text)
#' # Output: "Head1" "Head2"
#' @export
get_headings = function (text, heading_pattern = "\n *={2} *[^=].+ *={2} *\n") {
  
  # Get headings to name output and strip heading formatting ("==")
  headings = regmatches(x = text, gregexpr(heading_pattern, text, perl = TRUE))[[1]]
  headings = gsub("\n *={2} *", "", headings)
  headings = gsub(" *={2} *\n", "", headings)
  
  return(headings)
  
}