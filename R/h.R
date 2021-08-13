#' Recursively constructs Abstract Syntax Tree for a given expression
#'
#' @param ee expression
#'
#' @return list
#' @export
#'
#' @source \url{https://stackoverflow.com/a/52080518/6873133}
#'
#' @examples
#' getAST(quote(a %>% b))
getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)


#' Pipe-able wrapper function around help()
#'
#' This function allows you to pipe forward functions to open its help page.
#'
#' Using the pipe operator is commonplace and can influence the way you think
#' about using functions. When looking up help pages, it is typical to add a
#' question mark `?` to the beginning of a function open its help page (e.g.,
#' `?dim`). This function allows you to write left-to-right and simply pipe the
#' function of interest into `h` to open its help page.
#'
#' @param x expression
#'
#' @return None
#' @export
#'
#' @importFrom utils help
#'
#' @source \url{https://stackoverflow.com/a/52080518/6873133}
#'
#' @examples
#' # Simple single function
#' eval %>% h
#'
#' # Simple single function with parentheses
#' dim() %>% h
#'
#' # Function but specifying package
#' dplyr::across %>% h
#'
#' # Function but specifying package with parentheses
#' dplyr::summarise() %>% h
#'
#' # Fails
#' iris %>% dplyr::mutate %>% h
#' iris %>% dplyr::mutate() %>% h
h <- function(x) {
  sc <- sys.calls()
  ASTs <-
    sc %>%
    as.list() %>%
    purrr::map(getAST) %>%
    purrr::keep(~identical(.[[1]], quote(`%>%`)))  # Match first element to %>%


  # Not in a pipe
  if (length(ASTs) == 0) return(dplyr::enexpr(x))


  # Simple single function without parentheses
  res <- dplyr::last(ASTs)[[2]]  # Second element is the left-hand side
  if (class(res) == "name") {
    help(as.character(res))

  # Function but specifying package with parentheses
  } else if (class(res) == "list" & length(res[[1]]) == 3) {
    help(as.character(res[[1]][[3]]), package = as.character(res[[1]][[2]]))

  # Stop if pipe character
  } else if (as.character(res[[1]]) == "%>%") {
    message("Cannot parse input.")

  # Simple single function with parentheses
  } else if (class(res) == "list" & length(res[[1]]) == 1 & res[[1]] != "::") {
    help(as.character(res))

  # Function but specifying package
  } else if (class(res) == "list" & res[[1]] == "::") {
    help(as.character(res[[3]]), package = as.character(res[[2]]))

  } else {
    message("Cannot parse input.")

  }
}

