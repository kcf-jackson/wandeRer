#' Least-squared loss function
#' @param x numeric vector.
#' @param y numeric vector.
#' @export
ls_loss <- function(x, y) mean((x - y) ^ 2)


#' Zero regularisation function
#' @export
zero_reg <- function(x) 0


#' L1 regularisation function
#' @export
L1_reg <- function(param) sum(abs(param))


#' L2 regularisation function
#' @export
L2_reg <- function(param) sum(param ^ 2)
