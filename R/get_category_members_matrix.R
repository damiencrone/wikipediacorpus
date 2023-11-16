#' Retrieve Category Members and Create a Binary Sparse Matrix
#'
#' Takes a vector of categories and retrieves all of the members (pages and
#' subcategories) associated with each category, returning a binary sparse
#' matrix denoting whether the entity in the column (a category or page) belongs
#' to the entity in the row (a category). This function can also be trace the
#' category hierarchy to a specified depth (e.g., retrieving members of
#' members).
#'
#' @param categories a list of categories to retrieve members for
#' @param depth the number of levels to retrieve members for (default is 1)
#' @param lang the language to retrieve members in (default is "en")
#' @param namespace the namespace to retrieve members from (default is 14,
#'   categories)
#' @param verbose logical indicating whether or not to print progress updates
#'   (default is TRUE)
#' @return a binary sparse matrix denoting whether the entity in the column
#'   belongs to the entity in the row
#' @examples
#' categories = c("Category:Animals", "Category:Plants")
#' get_category_members_matrix(categories)
#' @export
get_category_members_matrix = function(categories,
                                       depth = 1,
                                       lang = "en",
                                       namespace = 14,
                                       verbose = TRUE) {
  
  # Check inputs
  if (depth > 1 && namespace != 14) {
    stop("Invalid input: depth > 1 only applies to namespace 14 (categories)")
  }
  if (depth > 3) {
    warning("Input depth > 3 is probably a bad idea (i.e., may return too many results to be useful)")
  }
  
  # Initialize a list to store the category members for each input category
  member_list = list()
  
  # Retrieve all of the category members for each input category
  for (category in categories) {
    category_members = get_category_members(category, lang, namespace, verbose)
    category_members = category_members$title
    member_list[[category]] = gsub(pattern = "Category:",
                                   replacement = "",
                                   x = category_members)
  }
  
  # Repeat to specified depth
  if (depth > 1) {
    depth_list = list(sort(unique(unlist(member_list))))
    for (i in 2:depth) {
      if (verbose) {message("Retriving members at depth of ", i)}
      members = depth_list[[i-1]]
      members = members[!members %in% names(member_list)]
      for (member in members) {
        category_members = get_category_members(member, lang, namespace, verbose)
        category_members = category_members$title
        member_list[[member]] = gsub(pattern = "Category:",
                                     replacement = "",
                                     x = category_members)
      }
      depth_list[[i]] = sort(unique(unlist(member_list)))
      depth_list[[i]] = depth_list[[i]][!depth_list[[i]] %in% members]
    }
  }
  
  # Initialize binary sparse matrix
  if (verbose) {message("Constructing category member matrix")}
  member_mat = Matrix::sparseMatrix(i = numeric(),
                                    j = numeric(),
                                    x = numeric(),
                                    dims = c(length(member_list),
                                             length(unique(unlist(member_list)))),
                                    dimnames = list(names(member_list),
                                                    sort(unique(unlist(member_list)))))
  
  # Iterate through the list and add 1s to the matrix where member belongs to
  # category
  for (i in 1:length(member_list)) {
    if (length(member_list[[i]]) > 0) {
      j = which(member_mat@Dimnames[[2]] %in% member_list[[i]])
      member_mat[i, j] = 1
    }
  }
  
  return(member_mat)
  
}