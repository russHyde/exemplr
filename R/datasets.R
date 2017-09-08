#' @import       datasets
#' @importFrom   tibble        tibble
#'
NULL

#' Creates a tidy-data version of the Anscombe quartet dataset
#'
#' @export
#'
get_tidy_anscombe <- function(){
  ans <- datasets::anscombe
  tidy_df <- tibble::tibble(
    group = factor(rep(paste0("Q", 1:4), each = 11),
                   levels = paste0("Q", 1:4)),
    x     = with(ans, c(x1, x2, x3, x4)),
    y     = with(ans, c(y1, y2, y3, y4))
  )
  tidy_df
}

#' Returns a tidy-data version of a named dataset.
#'
#' @param        dataset       The name of the dataset for which a tidy version
#'   is requested. Available datasets include "anscombe".
#'
#' @return       A tibble containing a tidied up version of the requested
#'   dataset.
#'
#' @export
#'
get_tidy <- function(dataset){
  available <- c("anscombe")
  stopifnot(dataset %in% available)
  get_tidy_anscombe()
}
