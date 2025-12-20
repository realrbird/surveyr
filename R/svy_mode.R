#' Calculate the Mode of a Vector
#'
#' Computes the mode (most frequent value) of an atomic vector.
#'
#' This function is designed for imputation:
#' \itemize{
#'   \item It always excludes \code{NA} values from the calculation.
#'   \item In the event of a tie (multimodal distribution), it returns the
#'         smallest value (numerically, alphabetically, or by factor level order)
#'         to ensure deterministic stability.
#' }
#'
#' @param x An atomic vector (numeric, character, factor, logical).
#' @param print_output Logical. If \code{TRUE} (default), prints the count and
#'   percentage of missing values found in the input vector.
#'
#' @return A single value of the same type as \code{x} representing the mode.
#'   Returns \code{NA} (of the appropriate type) if input is empty or contains only NAs.
#' @export
svy_mode <- function(x, print_output = TRUE) {

  # 1. Calculate and Report Missingness (Before removing NAs)
  n_miss <- sum(is.na(x))
  pct_miss <- mean(is.na(x)) * 100

  if (print_output) {
    message(sprintf("Missing cases: %d (%.2f%%)", n_miss, pct_miss))
  }

  # 2. Mandatory NA removal
  # We strictly remove NAs so they never become the mode
  x <- x[!is.na(x)]

  # 3. Handle empty/all-NA case
  if (length(x) == 0) {
    # Return NA of the same type as x to prevent crashes
    return(as.vector(NA, mode = typeof(x)))
  }

  # 4. Calculate Frequencies
  # match() + tabulate() is faster/safer than table()
  ux <- unique(x)
  freqs <- tabulate(match(x, ux))
  max_freq <- max(freqs)

  # Identify all modes
  modes <- ux[freqs == max_freq]

  # 5. Return Mode (Always tie-break with 'min')
  # Sort allows us to pick min (alphabetical for chars, numeric for numbers)
  # For factors, sort respects levels if ordered, or level order otherwise.
  if (length(modes) == 1) {
    return(modes)
  } else {
    return(sort(modes, decreasing = FALSE)[1])
  }
}
