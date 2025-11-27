#' @title Check for Numeric Vector without Missing Values
#' @description Internal function to validate if an input vector is numeric and
#'   contains no missing values. Stops execution if validation fails.
#'
#' @param x A vector to check.
#' @param arg_name Internal argument name for use in error messages.
#' @keywords internal
chk_numeric_no_na <- function(x, arg_name = deparse(substitute(x))) {

  if (!is.numeric(x)) {
    # Using call. = FALSE prevents the function call stack from appearing in the error message
    stop(paste0("`", arg_name, "` must be a numeric vector."), call. = FALSE)
  }

  if (any(is.na(x))) {
    stop(paste0("`", arg_name, "` must not contain missing values (`NA`)."), call. = FALSE)
  }

  # Return nothing invisibly upon success
  invisible(NULL)
}
