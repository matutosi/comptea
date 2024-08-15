#' Helper function to create group numbers
#'
group_no <- function(x){
  x |>
    as.character() |>
    forcats::fct_inorder() |>
    as.numeric()
}

#' multiple version of reduce
#'
preduce <- function(.l, .f, ..., .init, .dir = c("forward", "backward")){
  .dir <- match.arg(.dir)
  purrr::reduce(
    purrr::transpose(.l),
    \(x, y){ rlang::exec(.f, x, !!!y, ...) },
    .init = .init, .dir = .dir)
}

#' Wrapper for base::split
#'
split_by <- function(df, group){
  split(df, df[[group]])
}
