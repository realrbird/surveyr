#' Normalize Weights to Sample Size
#'
#' Rescales a weight vector so that it sums to the sample size (length of the vector).
#' This is often used to return weights to the scale of the original sample after
#' population projection or other adjustments.
#'
#' @param wt_vec A numeric vector of weights.
#'
#' @return A numeric vector of normalized weights.
#' @export
svy_normalize <- function(wt_vec) {

  # ---------------------------------------------------------
  # 1. Validation Checks
  # ---------------------------------------------------------

  # Check if wt_vec is numeric
  if (!is.numeric(wt_vec)) {
    stop("Weight vector 'wt_vec' must be numeric.")
  }

  # Check for NAs
  if (any(is.na(wt_vec))) {
    stop("Weight vector 'wt_vec' cannot contain missing values (NA).")
  }

  # ---------------------------------------------------------
  # 2. Calculation
  # ---------------------------------------------------------

  sum_weights <- sum(wt_vec)

  # Prevent division by zero
  if (sum_weights == 0) {
    stop("Sum of input weights is 0; cannot normalize values.")
  }

  # Calculate normalization
  # Logic: (individual_weight / total_weight) * sample_size
  sample_size <- length(wt_vec)
  result <- wt_vec * (sample_size / sum_weights)

  return(result)
}
