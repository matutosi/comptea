#' Helper function to create group numbers
#'
#' @param x A vector of values.
#' @return  A numeric vector of group numbers.
#' @examples
#' group_no(c("a", "b", "a", "c"))
#' @export
group_no <- function(x){
  x |>
    as.character() |>
    forcats::fct_inorder() |>
    as.numeric()
}

#' Multiple version of reduce
#'
#' @references \url{https://github.com/tidyverse/purrr/issues/163}
#' @param .l     A list.
#' @param .f     A function to be applied to each element of the lists.
#' @param ...    Additional arguments to be passed to the function.
#' @param .init  An initial value for the reduction.
#' @param  .dir  The direction of reduction as a string, one of "forward" or "backward".
#' @return       The reduced result.
#' @examples
#' # Multiply corresponding elements from multiple lists
#' x <- list(a = 1, b = 2)
#' y <- list(a = 3, b = 4)
#' z <- list(a = 5, b = 6)
#' preduce(list(x, y, z), `*`)
#' @export
preduce <- function(.l, .f, ..., .init, .dir = c("forward", "backward")){
  .dir <- match.arg(.dir)
  purrr::reduce(
    purrr::transpose(.l),
    \(x, y){ rlang::exec(.f, x, !!!y, ...) },
    .init = .init, .dir = .dir)
}
