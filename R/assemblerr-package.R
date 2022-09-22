#' @keywords internal
#' @importFrom methods is getFunction
#' @importFrom rlang .data %||%
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name assemblerr-vctrs
NULL

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(".")
