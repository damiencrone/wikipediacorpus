#' Split Wikipedia Text by Headings
#'
#' This function splits Wikipedia text into segments based on level 1 headings.
#'
#' @param text A character string of Wikipedia text.
#'
#' @return A named character vector, with the names corresponding to the
#'   headings in the input text. The first element of the list is named "Lead"
#'   and contains the text before the first heading.
#' @export
split_wikipedia_text = function (text) {
  
  # Create a regular expression pattern to match level 1 headings
  heading_pattern = "\n *={2} *[^=].+ *={2} *\n"
  
  # Split the input text by the heading pattern and extract headings
  split_text = strsplit(text, heading_pattern, perl = TRUE)[[1]]
  headings = get_headings(text, heading_pattern)
  
  names(split_text) = c("Lead", headings)
  
  return(split_text)
  
}