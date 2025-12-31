#' Get Unique Values of a Vector
#'
#' Returns the unique values of an atomic vector, with options to remove missing
#' values and sort the result.
#'
#' @param x An atomic vector.
#' @param na_rm Logical. If \code{TRUE} (default), \code{NA} values are removed
#'   from the result.
#' @param sort Logical. If \code{TRUE} (default), the unique values are sorted.
#'   If \code{na_rm = FALSE} and \code{sort = TRUE}, missing values are placed at the end.
#'
#' @return A vector of the same type as \code{x} containing the unique values.
#' @export
svy_unique <- function(x, na_rm = TRUE, sort = TRUE) {

  # 1. Get unique values using base R
  res <- unique(x)

  # 2. Handle NA removal
  if (na_rm) {
    res <- res[!is.na(res)]
  }

  # 3. Handle Sorting
  if (sort) {
    # We use na.last = TRUE to ensure NAs are kept at the end if na_rm = FALSE
    # instead of being dropped by default sort behavior.
    res <- sort(res, na.last = TRUE)
  }

  return(res)
}
