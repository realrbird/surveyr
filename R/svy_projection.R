#' Project Weights to Population Size
#'
#' Rescales a weight vector so that it sums to a specific target population size.
#'
#' @param wt_vec A numeric vector of weights.
#' @param population A single numeric value representing the target population size.
#'
#' @return A numeric vector of projected weights.
#' @export
svy_projection <- function(wt_vec, population) {

  # ---------------------------------------------------------
  # 1. Validation Checks
  # ---------------------------------------------------------

  # Check if wt_vec is numeric
  if (!is.numeric(wt_vec)) {
    stop("Weight vector 'wt_vec' must be numeric.")
  }

  # Check for NAs (Standardized way: any(is.na(...)))
  if (any(is.na(wt_vec))) {
    stop("Weight vector 'wt_vec' cannot contain missing values (NA).")
  }

  # Check population argument
  if (!is.numeric(population) || length(population) != 1) {
    stop("Target 'population' must be a single numeric value.")
  }

  # ---------------------------------------------------------
  # 2. Calculation
  # ---------------------------------------------------------

  sum_weights <- sum(wt_vec)

  # Prevent division by zero
  if (sum_weights == 0) {
    stop("Sum of input weights is 0; cannot project values.")
  }

  # Calculate projection
  # Logic: (individual_weight / total_sample_weight) * target_population
  result <- wt_vec * (population / sum_weights)

  return(result)
}
