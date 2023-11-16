#' Cut article text at specified headings
#'
#' @param articles A character vector of articles to be processed
#' @param sections_to_remove A character vector of headings at which to cut the articles (removing all subsequent text)
#' @return A character vector of processed articles containing all text preceding headings
#' @export
cut_articles_at_headings = function (articles, sections_to_remove) {
  for (s in sections_to_remove) {
    heading_pattern = paste0( "\n *={2} *", s, " *={2} *\n")
    articles = strsplit(x = articles, split = heading_pattern)
    articles = sapply(X = articles,
                      FUN = function(x) {x[1]})
  }
  return(articles)
}