#' Pipe operator
#'
#' Re-export of `%>%` from \pkg{dplyr}, so package code and downstream users can
#' rely on it without `library(magrittr)`.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @return The result of calling `rhs(lhs)`.
NULL
