#' Modified sum function 
#'
#' Overwritten problematic behavior of sum(x, na.rm = T)
#' @param x A numeric or logical vector
#' @export

suma <-
function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)
