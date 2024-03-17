#' Square root function
#'
#' This function computes the square root of a number and throws an error if the input is negative.
#'
#' @param x numeric vector.
#' @return The square root of x if x is positive. An error if x is negative.
#' @export
sqrt.2 = function(x){
  if (x < 0) {
    stop("Error: negative input, NA introduced!")
  } else {
    sqrt(x)
  }
}

#' Logarithm function
#'
#' This function computes the natural logarithm of a number and will throw an error if the input is non-positive.
#'
#' @param x Numeric vector.
#' @return The natural logarithm of x if x is positive. An error if x is non-positive.
#' @export
log.2 = function(x){
  if (x <= 0) {
    stop("Error: non-positive input, NA introduced!")
  } else {
    log(x)
  }
}

#' Function operator for error handling
#'
#' This function operator transforms a function so that it returns an error condition object instead of throwing an error when the input is invalid.
#'
#' @param f A function.
#' @return A function that returns an error condition object when the input to f is invalid.
#' @export
f_operator = function(f) {
  force(f)
  function(x) {
    tryCatch(
      expr = {
        f(x)
      },
      error = function(e) {
        cnd <- rlang::error_cnd(
          message = "invalid input",
          class = "invalid_input",
          invalid_input = x,
          trace = rlang::trace_back(bottom = sys.frame(-1))
        )
        return(cnd)
      }
    )
  }
}
