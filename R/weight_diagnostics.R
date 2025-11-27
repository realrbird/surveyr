#' @title Calculate Survey Weight Quantiles
#' @description Calculates and neatly formats the specified quantiles (or "tiles")
#'   of a vector of survey weights. This is useful for diagnostic purposes,
#'   particularly when assessing the need for weight trimming or identifying
#'   influential outliers.
#'
#' @param wt_vec A numeric vector containing the survey weights. Must not contain \code{NA} values.
#' @param probs A numeric vector of probabilities (between 0 and 1) at which to
#'   compute the quantiles. Defaults to a standard set of diagnostic quantiles.
#' @param print_all A logical flag. If \code{TRUE}, the \code{probs} argument is
#'   ignored and quantiles are calculated for every 1% (i.e., \code{seq(0, 1, 0.01)}).
#'
#' @return A \code{tibble} with two columns:
#'   \item{tile}{Character string representing the percentile (e.g., '5%', '50%').}
#'   \item{value}{Numeric value of the weight at that percentile.}
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   # Using the internal survey_df for demonstration
#'   data("survey_df")
#'
#'   # Default diagnostic quantiles
#'   svy_tiles(survey_df$WEIGHT)
#'
#'   # Full percentile breakdown
#'   svy_tiles(survey_df$WEIGHT, print_all = TRUE)
#'
#'   # Custom quantiles
#'   svy_tiles(survey_df$WEIGHT, probs = c(0.001, 0.999))
#' }
#'
#' @export
svy_tiles <- function(wt_vec, probs = c(0, 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), print_all = FALSE) {

  # --- 1. Argument Validation ---
  # Check for numeric and NA values using internal helper (from survey_checks.R)
  chk_numeric_no_na(wt_vec)

  # Determine the probabilities to use
  if (isTRUE(print_all)) {
    probs_to_use <- seq(0, 1, 0.01)
  } else {
    if (!is.numeric(probs) || any(probs < 0) || any(probs > 1)) {
      stop("`probs` must be a numeric vector of values between 0 and 1.", call. = FALSE)
    }
    probs_to_use <- sort(unique(probs)) # Ensure they are sorted and unique
  }

  # --- 2. Calculation and Formatting ---

  # Calculate quantiles
  quantile_values <- stats::quantile(wt_vec, probs = probs_to_use, names = FALSE)

  # Create a tibble for structured output
  result <- tibble::tibble(
    tile = paste0(round(probs_to_use * 100, 2), '%'), # Format probabilities as percentages
    value = unname(quantile_values)
  )

  return(result)
}

#' @title Calculate Weight Summary Statistics
#' @description Computes key descriptive and diagnostic statistics for a vector
#'   of survey weights, including the design effect (DEFF) and effective sample
#'   size (ESS).
#'
#' @param wt_vec A numeric vector containing the survey weights. Must not contain \code{NA} values.
#' @param conf_level The confidence level (as a proportion, e.g., 0.95 for 95\%)
#'   to use when calculating the design-adjusted margin of error (MOE).
#'
#' @return A \code{tibble} with a single row and seven columns:
#'   \item{n}{Numeric. The sample size (length of \code{wt_vec}).}
#'   \item{mean}{Numeric. The arithmetic mean of the weights.}
#'   \item{sd_wt}{Numeric. The standard deviation of the weights.}
#'   \item{deff}{Numeric. The Kish design effect.}
#'   \item{ess}{Numeric. The effective sample size.}
#'   \item{moe}{Numeric. The design-adjusted margin of error (in percentage points).}
#'   \item{wt_ratio}{Numeric. The ratio of max weight to min weight.}
#'
#' @details
#' \describe{
#'   \item{DEFF (Kish)}{The Kish approximation of the overall survey design effect,
#'     calculated as $n \sum w_i^2 / (\sum w_i)^2$. Represents the factor by which
#'     the sample size must be inflated to achieve the precision of a simple random
#'     sample.}
#'   \item{ESS}{The effective sample size, calculated as $n / DEFF$. This is the
#'     size of a simple random sample required to achieve the same precision.}
#'   \item{MOE}{The design-adjusted margin of error, calculated assuming a worst-case
#'     proportion of $0.5$ (50\%/50\% split) and expressed in percentage points.
#'     The formula is: $Z_{\alpha/2} \times \sqrt{0.25 / \text{ESS}} \times 100$.}
#' }
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE) && requireNamespace("stats", quietly = TRUE)) {
#'   # Using the internal survey_df for demonstration
#'   data("survey_df")
#'
#'   # Default 95% confidence level
#'   svy_stats(survey_df$WEIGHT)
#'
#'   # 99% confidence level
#'   svy_stats(survey_df$WEIGHT, conf_level = 0.99)
#' }
#'
#' @export
svy_stats <- function(wt_vec, conf_level = 0.95) {

  # --- 1. Argument Validation ---
  # Check for numeric and NA values using internal helper (from survey_checks.R)
  chk_numeric_no_na(wt_vec)

  if (!is.numeric(conf_level) || length(conf_level) != 1 || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  # --- 2. Core Calculations ---
  n <- length(wt_vec)
  sum_w <- sum(wt_vec)
  sum_w_sq <- sum(wt_vec^2)

  # Basic Stats
  mean_wt <- mean(wt_vec)
  sd_wt <- stats::sd(wt_vec)

  # Weight Ratio
  wt_ratio <- max(wt_vec) / min(wt_vec)

  # Design Effect (Kish)
  deff <- n * sum_w_sq / sum_w^2

  # Effective Sample Size (ESS)
  ess <- n / deff

  # Margin of Error (Design-Adjusted)
  # Calculate the Z-score for the given confidence level
  alpha_half <- (1 - conf_level) / 2
  z_score <- stats::qnorm(1 - alpha_half)

  # CORRECTED MOE FORMULA:
  # MOE = Z * sqrt(0.5 * 0.5 / ESS) * 100 (for 50/50 split, expressed as percentage points)
  moe <- z_score * 0.5 / sqrt(ess) * 100

  # --- 3. Format Output (Horizontal Tibble) ---
  result <- tibble::tibble(
    n = n,
    mean = mean_wt,
    sd_wt = sd_wt,
    deff = deff,
    ess = ess,
    moe = moe,
    wt_ratio = wt_ratio
  )

  return(result)
}
