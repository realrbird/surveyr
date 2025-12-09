#' @title Survey Weight Quantiles
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
#' \describe{
#'   \item{tile}{Character string representing the percentile (e.g., '5\%', '50\%').}
#'   \item{value}{Numeric value of the weight at that percentile.}
#' }
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
#' @importFrom tibble tibble
#' @export
svy_tiles <- function(wt_vec, probs = c(0, 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), print_all = FALSE) {

  # --- 1. Argument Validation ---
  # Check for numeric and NA values using internal helper (from survey_checks.R)
  chk_numeric_no_na(wt_vec)

  # Determine the probabilities to use
  if (isTRUE(print_all)) {
    probs_to_use <- seq(0, 1, 0.01)
  } else {
    if (!is.numeric(probs) | any(probs < 0) | any(probs > 1)) {
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
#'     calculated as \eqn{n \sum w_i^2 / (\sum w_i)^2}. Represents the factor by which
#'     the sample size must be inflated to achieve the precision of a simple random
#'     sample.}
#'   \item{ESS}{The effective sample size, calculated as \eqn{n / DEFF}. This is the
#'     size of a simple random sample required to achieve the same precision.}
#'   \item{MOE}{The design-adjusted margin of error, calculated assuming a worst-case
#'     proportion of 0.5 (50\%/50\% split) and expressed in percentage points.
#'     The formula is: \eqn{Z_{\alpha/2} \times \sqrt{0.25 / ESS} \times 100}.}
#' }
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE) & requireNamespace("stats", quietly = TRUE)) {
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

  if (!is.numeric(conf_level) | length(conf_level) != 1 | conf_level <= 0 | conf_level >= 1) {
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
  alpha_half <- (1 - conf_level) / 2
  z_score <- stats::qnorm(1 - alpha_half)

  # MOE formula
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

#' @title Compare Survey Frequencies to Population Targets
#' @description Calculates the difference between survey frequencies (weighted or
#'   unweighted) and corresponding population target percentages for multiple
#'   demographic variables.
#'
#' @param data A data frame or tibble containing the survey variables.
#' @param targets A named list of tibbles, where each tibble represents the population
#'   targets for a variable. List names must match variable names in \code{data}.
#' @param wt_var A **character string** (unquoted column name) in \code{data}
#'   containing the survey weights. If \code{NULL} (the default), the comparison
#'   uses unweighted sample frequencies.
#'
#' @return A single \code{tibble} that aggregates the comparison results for all
#'   variables in \code{targets}.
#'   \itemize{
#'     \item If \code{wt_var} is \code{NULL}, returns 5 columns: \code{VAR}, \code{LEVEL},
#'     \code{TARGET_PERCENT}, \code{UNWT_SAMPLE_PERCENT}, and \code{TARGET_UNWT_DIFF}.
#'     \item If \code{wt_var} is provided, returns 7 columns, adding
#'     \code{WT_SAMPLE_PERCENT} and \code{TARGET_WT_DIFF} to the unweighted comparison results.
#'   }
#'
#' @details
#' The resulting tibble is structured such that each row represents a category
#' (e.g., '18-30', 'male', 'South').
#' \describe{
#'   \item{TARGET_PERCENT}{The target percentage from the \code{targets} list.}
#'   \item{UNWT_SAMPLE_PERCENT}{The calculated sample percentage using unweighted counts.}
#'   \item{WT_SAMPLE_PERCENT}{The calculated sample percentage using the provided survey weights (only present if \code{wt_var} is not \code{NULL}).}
#'   \item{TARGET_UNWT_DIFF}{The difference between target and unweighted sample frequencies.}
#'   \item{TARGET_WT_DIFF}{The difference between target and weighted sample frequencies (only present if \code{wt_var} is not \code{NULL}).}
#' }
#'
#' The function relies on the `targets` list being correctly structured (validated
#' by internal checks in \code{chk_target_structure}).
#'
#' @importFrom rlang .data sym
#' @importFrom dplyr count mutate select relocate bind_rows left_join all_of
#' @importFrom stats na.omit
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'
#'   # Weighted comparison (includes unweighted and weighted results)
#'   svy_comps(survey_df, target_list, wt_var = "WEIGHT")
#'
#'   # Unweighted comparison (unweighted results only)
#'   svy_comps(survey_df, target_list)
#' }
#'
#' @export
svy_comps <- function(data, targets, wt_var = NULL) {

  # --- 1. Validation using internal helpers ---
  # Check targets structure and alignment with data (defined in R/survey_checks.R)
  chk_target_structure(targets, data)

  # If wt_var is provided, validate it as a character string
  if (!is.null(wt_var)) {
    if (!is.character(wt_var) | length(wt_var) != 1) {
      stop("`wt_var` must be a single character string representing the weight column name.", call. = FALSE)
    }

    # Check if the weight column exists in the data
    if (!wt_var %in% names(data)) {
      stop(paste0("Weight variable '", wt_var, "' not found in `data`."), call. = FALSE)
    }

    # Check weight vector itself for numeric and NAs using internal helper
    chk_numeric_no_na(data[[wt_var]], arg_name = wt_var)
  }

  all_results <- list()

  # --- 2. Iterate through each target variable ---
  for (var_name in names(targets)) {
    target_df <- targets[[var_name]]
    var_sym <- rlang::sym(var_name)

    # --- A. Calculate Unweighted Frequencies (ALWAYS needed) ---
    unwt_counts <- data |>
      dplyr::count(!!var_sym, .drop = FALSE) |>
      stats::na.omit() |>
      dplyr::mutate(UNWT_SAMPLE_PERCENT = (.data$n / sum(.data$n)) * 100) |>
      dplyr::select(dplyr::all_of(var_name), "UNWT_SAMPLE_PERCENT") # Fix: Use explicit string name and all_of for join var

    # --- B. Start Comparison DF ---

    comparison_df <- target_df |>
      dplyr::select(dplyr::all_of(var_name), TARGET_PERCENT = "Freq") |> # Fix: Use explicit string name

      # FIX: Use simple string join by var_name, which is now stable.
      dplyr::left_join(unwt_counts, by = var_name) |>

      dplyr::mutate(
        VAR = var_name,
        LEVEL = as.character(!!var_sym),
        # FIX: Correct arithmetic by referencing the numeric columns
        TARGET_UNWT_DIFF = .data$TARGET_PERCENT - .data$UNWT_SAMPLE_PERCENT
      )
    # FIX: DO NOT SELECT/DROP COLUMNS HERE.
    # We need 'var_name' column to persist for the weighted join in step C.


    # --- C. Handle Weighted Case (if wt_var is provided) ---
    if (!is.null(wt_var)) {
      wt_var_sym <- rlang::sym(wt_var)

      weighted_counts <- data |>
        dplyr::count(!!var_sym, wt = !!wt_var_sym, .drop = FALSE) |>
        stats::na.omit() |>
        dplyr::mutate(WT_SAMPLE_PERCENT = (.data$n / sum(.data$n)) * 100) |>
        dplyr::select(dplyr::all_of(var_name), "WT_SAMPLE_PERCENT") # Fix: Use explicit string name

      # Merge weighted counts and calculate difference
      comparison_df <- comparison_df |>
        # Now this join will work because 'var_name' column still exists in comparison_df
        dplyr::left_join(weighted_counts, by = var_name) |>

        dplyr::mutate(
          # FIX: Perform arithmetic on the named columns
          TARGET_WT_DIFF = .data$TARGET_PERCENT - .data$WT_SAMPLE_PERCENT
        )
    }

    # --- D. Final Column Selection ---

    # Define columns to keep
    cols_to_keep <- c("VAR", "LEVEL", "TARGET_PERCENT", "UNWT_SAMPLE_PERCENT", "TARGET_UNWT_DIFF")

    if (!is.null(wt_var)) {
      cols_to_keep <- c(cols_to_keep, "WT_SAMPLE_PERCENT", "TARGET_WT_DIFF")
    }

    # Final cleanup of columns using string selection
    comparison_df <- comparison_df |>
      dplyr::select(dplyr::all_of(cols_to_keep))

    all_results[[var_name]] <- comparison_df
  }

  # --- 3. Combine all results into a single tibble ---
  final_result <- dplyr::bind_rows(all_results)

  return(final_result)
}

#' @title Diagnostic Wrapper for Survey Weights
#' @description Runs \code{svy_tiles()}, \code{svy_stats()}, and \code{svy_comps()}
#'   and returns the results in a list. Designed for immediate weight quality assessment.
#'
#' @param data A data frame or tibble containing the survey variables.
#' @param targets A named list of tibbles, where each tibble represents the population
#'   targets (required for \code{svy_comps}).
#' @param wt_var A character string column name in \code{data} containing the survey weights.
#' @param print Logical. If \code{TRUE}, prints the full tibble output for diagnostics.
#'
#' @return A list with three elements: \code{tiles}, \code{stats}, and \code{comps}.
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'
#'   # Run full diagnostics report (prints output by default)
#'   report <- svy_diagnostics(survey_df, target_list, wt_var = "WEIGHT")
#'
#'   # Access specific reports
#'   # report$stats
#' }
#'
#' @export
svy_diagnostics <- function(data, targets, wt_var, print = TRUE) {

  # Check if wt_var is character and exists (svy_tiles/stats will run full check)
  if (!is.character(wt_var) | length(wt_var) != 1 | !wt_var %in% names(data)) {
    stop("`wt_var` must be a single character string column name present in `data`.", call. = FALSE)
  }

  # Create the weight vector
  wt_vec <- data[[wt_var]]

  # 1. Tiles
  tiles_result <- svy_tiles(wt_vec)

  # 2. Stats
  stats_result <- svy_stats(wt_vec)

  # 3. Comps
  comps_result <- svy_comps(data, targets, wt_var = wt_var)

  result_list <- list(
    tiles = tiles_result,
    stats = stats_result,
    comps = comps_result
  )

  if (isTRUE(print)) {
    cat("\n--- Survey Weight Diagnostics: ", wt_var, " ---\n")

    cat("\n[1] Tiles (Quantiles):\n")
    print(result_list$tiles, n = Inf)

    cat("\n[2] Stats (DEFF, ESS, MOE):\n")
    print(result_list$stats, n = Inf)

    cat("\n[3] Comps (Target Alignment):\n")
    print(result_list$comps, n = Inf)
    cat("---------------------------------------------\n\n")
  }

  return(result_list)
}

#' @title Compare Weight Quantiles Across Multiple Weight Vectors
#' @description Calculates the specified quantiles for multiple weight vectors and
#'   returns them in a single wide-format tibble for easy comparison.
#'
#' @param data A data frame or tibble containing the survey weight variables.
#' @param wt_vars A character vector of column names in \code{data} containing the
#'   survey weights to be compared.
#' @param probs A numeric vector of probabilities (between 0 and 1). Defaults to
#'   standard diagnostic quantiles.
#'
#' @return A wide-format \code{tibble} with the first column being \code{tile} and
#'   subsequent columns named according to the input \code{wt_vars}.
#'
#' @importFrom purrr map reduce
#' @importFrom dplyr left_join
#'
#' @examples
#' # Requires devtools::load_all()
#' if (requireNamespace("tibble", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'   data("survey_df")
#'   # Create a second dummy weight for comparison
#'   survey_df_comp <- survey_df |>
#'     dplyr::mutate(WEIGHT_2 = .data$WEIGHT * c(rep(1.2, 1000), rep(0.8, 1000)))
#'
#'   # Compare base weight to a modified weight
#'   svy_compare_tiles(survey_df_comp, wt_vars = c("WEIGHT", "WEIGHT_2"))
#' }
#'
#' @importFrom rlang :=
#' @export
svy_compare_tiles <- function(data, wt_vars, probs = c(0, 0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1)) {

  # --- 1. Validation ---
  if (!is.character(wt_vars) | length(wt_vars) < 1) {
    stop("`wt_vars` must be a character vector with at least one weight column name.", call. = FALSE)
  }

  # Check existence and numeric status for all weight columns
  purrr::map(wt_vars, function(var) {
    if (!var %in% names(data)) {
      stop(paste0("Column '", var, "' not found in `data`."), call. = FALSE)
    }
    # Check vector type and NAs (assuming chk_numeric_no_na is in survey_checks.R)
    chk_numeric_no_na(data[[var]], arg_name = var)
  })

  # --- 2. Calculation and Combination ---

  # Map svy_tiles over each weight vector
  tile_list <- purrr::map(wt_vars, function(var) {
    wt_vec <- data[[var]]

    # Run svy_tiles
    result <- svy_tiles(wt_vec, probs = probs)

    # Rename the value column to the weight variable name
    result |>
      dplyr::select(tile, !!var := value)
  })

  # Sequentially join all results by the 'tile' column
  purrr::reduce(tile_list, dplyr::left_join, by = "tile")
}

#' @title Compare Weight Summary Statistics Across Multiple Weight Vectors
#' @description Calculates and compares key diagnostic statistics (n, DEFF, ESS, MOE, etc.)
#'   for multiple weight vectors in a single long-format tibble.
#'
#' @param data A data frame or tibble containing the survey weight variables.
#' @param wt_vars A character vector of column names in \code{data} containing the
#'   survey weights to be compared.
#' @param conf_level The confidence level (as a proportion, e.g., 0.95 for 95\%)
#'   to use when calculating the design-adjusted margin of error (MOE).
#'
#' @return A long-format \code{tibble} where each row represents a statistic for a
#'   specific weight vector. It includes a \code{WT_NAME} column identifying the
#'   weight vector source, followed by the 7 statistic columns from \code{svy_stats}.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'   data("survey_df")
#'   # Create a second dummy weight for comparison
#'   survey_df_comp <- survey_df |>
#'     dplyr::mutate(WEIGHT_2 = .data$WEIGHT * c(rep(1.2, 1000), rep(0.8, 1000)))
#'
#'   # Compare base weight to a modified weight
#'   svy_compare_stats(survey_df_comp, wt_vars = c("WEIGHT", "WEIGHT_2"))
#' }
#'
#' @export
svy_compare_stats <- function(data, wt_vars, conf_level = 0.95) {

  # --- 1. Validation ---
  if (!is.character(wt_vars) | length(wt_vars) < 1) {
    stop("`wt_vars` must be a character vector with at least one weight column name.", call. = FALSE)
  }

  # Check existence and numeric status for all weight columns
  purrr::map(wt_vars, function(var) {
    if (!var %in% names(data)) {
      stop(paste0("Column '", var, "' not found in `data`."), call. = FALSE)
    }
    # Check vector type and NAs (assuming chk_numeric_no_na is in survey_checks.R)
    chk_numeric_no_na(data[[var]], arg_name = var)
  })

  # --- 2. Calculation and Combination ---

  # Map svy_stats over each weight vector
  stats_list <- purrr::map(wt_vars, function(var) {
    wt_vec <- data[[var]]

    # Run svy_stats on the vector and add the weight name column
    svy_stats(wt_vec, conf_level = conf_level) |>
      dplyr::mutate(WT_NAME = var, .before = 1)
  })

  # Bind all results together (long format)
  dplyr::bind_rows(stats_list)
}

#' @title Compare Survey Frequencies vs Targets Across Multiple Weight Vectors
#' @description Calculates and compares the difference between survey frequencies and
#'   population targets for multiple weight vectors in a single long-format tibble.
#'
#' @param data A data frame or tibble containing the survey variables.
#' @param targets A named list of tibbles containing the population targets.
#' @param wt_vars A character vector of column names in \code{data} containing the
#'   survey weights to be compared.
#'
#' @return A long-format \code{tibble} where each row represents a category/level.
#'   It includes a \code{WT_NAME} column identifying the weight vector source.
#'   The column structure is: \code{WT_NAME}, \code{VAR}, \code{LEVEL},
#'   \code{TARGET_PERCENT}, \code{UNWT_SAMPLE_PERCENT}, \code{TARGET_UNWT_DIFF},
#'   \code{WT_SAMPLE_PERCENT}, \code{TARGET_WT_DIFF}.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'   # Create a second dummy weight for comparison
#'   survey_df_comp <- survey_df |>
#'     dplyr::mutate(WEIGHT_2 = .data$WEIGHT * c(rep(1.2, 1000), rep(0.8, 1000)))
#'
#'   # Compare base weight to a modified weight
#'   svy_compare_comps(survey_df_comp, target_list, wt_vars = c("WEIGHT", "WEIGHT_2"))
#' }
#'
#' @export
svy_compare_comps <- function(data, targets, wt_vars) {

  # --- 1. Validation ---
  if (!is.character(wt_vars) | length(wt_vars) < 1) {
    stop("`wt_vars` must be a character vector with at least one weight column name.", call. = FALSE)
  }

  # Check targets structure and alignment with data (defined in R/survey_checks.R)
  chk_target_structure(targets, data)

  # Check existence and numeric status for all weight columns
  purrr::map(wt_vars, function(var) {
    if (!var %in% names(data)) {
      stop(paste0("Column '", var, "' not found in `data`."), call. = FALSE)
    }
    # Check vector type and NAs (assuming chk_numeric_no_na is in survey_checks.R)
    chk_numeric_no_na(data[[var]], arg_name = var)
  })

  # --- 2. Calculation and Combination ---

  # Map svy_comps over each weight vector
  comps_list <- purrr::map(wt_vars, function(var) {

    # Run svy_comps on the vector (always returns 7 columns)
    # Note: svy_comps performs all the necessary weighted calculations internally
    svy_comps(data, targets, wt_var = var) |>
      dplyr::mutate(WT_NAME = var, .before = 1)
  })

  # Bind all results together (long format)
  dplyr::bind_rows(comps_list)
}

#' @title Compare Multiple Weight Vectors Across All Diagnostic Tools
#' @description A wrapper function that runs \code{svy_compare_tiles()},
#'   \code{svy_compare_stats()}, and \code{svy_compare_comps()} and returns
#'   the results in a single structured list.
#'
#' @param data A data frame or tibble containing the survey variables.
#' @param targets A named list of tibbles containing the population targets (required for \code{svy_compare_comps}).
#' @param wt_vars A character vector of column names in \code{data} containing the
#'   survey weights to be compared.
#' @param conf_level The confidence level (as a proportion, e.g., 0.95 for 95\%)
#'   to use when calculating the design-adjusted margin of error (MOE).
#'
#' @return A list with three elements:
#'   \item{tiles}{The result from \code{svy_compare_tiles()}.}
#'   \item{stats}{The result from \code{svy_compare_stats()}.}
#'   \item{comps}{The result from \code{svy_compare_comps()}.}
#'
#' @examples
#' # Load the package data (requires devtools::load_all() during development)
#' if (requireNamespace("tibble", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'   # Create a second dummy weight for comparison
#'   survey_df_comp <- survey_df |>
#'     dplyr::mutate(WEIGHT_2 = .data$WEIGHT * c(rep(1.2, 1000), rep(0.8, 1000)))
#'
#'   # Run full comparison report
#'   comparison_report <- svy_compare(
#'     survey_df_comp,
#'     target_list,
#'     wt_vars = c("WEIGHT", "WEIGHT_2")
#'   )
#'   # Access specific reports
#'   # comparison_report$stats
#' }
#'
#' @export
svy_compare <- function(data, targets, wt_vars, conf_level = 0.95) {

  # Note: Validation for data, targets, and wt_vars is handled internally by
  # the three comparison functions.

  # 1. Tiles Comparison (Wide format)
  tiles_comp <- svy_compare_tiles(data, wt_vars)

  # 2. Stats Comparison (Long format)
  stats_comp <- svy_compare_stats(data, wt_vars, conf_level = conf_level)

  # 3. Comps Comparison (Long format)
  comps_comp <- svy_compare_comps(data, targets, wt_vars)

  # Return the list
  return(list(
    tiles = tiles_comp,
    stats = stats_comp,
    comps = comps_comp
  ))
}

#' @title Compare Survey Weights Across Different Datasets
#' @description Diagnoses and compares survey weights from multiple different datasets
#'   (e.g., Wave 1 vs Wave 2) by aggregating \code{svy_diagnostics} results.
#'
#' @param ... Named or unnamed list objects, where each list represents a dataset to compare.
#'   Each input list must contain the following named elements:
#'   \itemize{
#'     \item \code{dataset_name}: A string identifying the dataset (e.g., "Wave 1").
#'     \item \code{data}: The data frame or tibble.
#'     \item \code{wt_var}: A string for the weight column name in \code{data}.
#'     \item \code{target_list}: (Optional) The target list for \code{svy_comps}.
#'   }
#' @param print Logical. If \code{TRUE}, prints the aggregated diagnostic report.
#'
#' @return A list with three elements (\code{tiles}, \code{stats}, \code{comps})
#'   containing the combined diagnostic results.
#'
#' @examples
#' # Load package data
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'
#'   # Simulate a second wave of data
#'   survey_df_w2 <- survey_df
#'   survey_df_w2$WEIGHT <- survey_df$WEIGHT * 1.1 # Shift weights slightly
#'
#'   # Define inputs
#'   input_w1 <- list(
#'     dataset_name = "Wave 1",
#'     data = survey_df,
#'     wt_var = "WEIGHT",
#'     target_list = target_list
#'   )
#'
#'   input_w2 <- list(
#'     dataset_name = "Wave 2",
#'     data = survey_df_w2,
#'     wt_var = "WEIGHT",
#'     target_list = target_list
#'   )
#'
#'   # Run contrast
#'   # contrast <- svy_contrast(input_w1, input_w2)
#' }
#'
#' @export
svy_contrast <- function(..., print = TRUE) {

  inputs <- list(...)

  if (length(inputs) == 0) {
    stop("At least one input list must be provided.", call. = FALSE)
  }

  # --- 1. Validation & Processing Loop ---

  diagnostics_list <- purrr::map(inputs, function(item) {

    # Check keys
    if (is.null(item$dataset_name) || !is.character(item$dataset_name)) {
      stop("Each input list must have a 'dataset_name' string.", call. = FALSE)
    }
    if (is.null(item$data) || !is.data.frame(item$data)) {
      stop(paste0("Input '", item$dataset_name, "' is missing 'data' or it is not a data frame."), call. = FALSE)
    }
    if (is.null(item$wt_var) || !is.character(item$wt_var)) {
      stop(paste0("Input '", item$dataset_name, "' is missing 'wt_var' string."), call. = FALSE)
    }
    if (is.null(item$target_list) || !is.list(item$target_list)) {
      stop(paste0("Input '", item$dataset_name, "' is missing 'target_list'."), call. = FALSE)
    }

    # Run diagnostics internally
    # We suppress printing because we will aggregate later
    res <- svy_diagnostics(
      data = item$data,
      targets = item$target_list,
      wt_var = item$wt_var,
      print = FALSE
    )

    # Add the dataset identifier to the results
    res$dataset_name <- item$dataset_name
    return(res)
  })

  # --- 2. Aggregation ---

  # Check for duplicate dataset names
  names_vec <- purrr::map_chr(diagnostics_list, "dataset_name")
  if (any(duplicated(names_vec))) {
    stop("Duplicate 'dataset_name' values found. Names must be unique.", call. = FALSE)
  }

  # A. Tiles (Wide Format)
  # Extract tiles, rename 'value' to dataset_name, and join
  tiles_agg <- purrr::map(diagnostics_list, function(res) {
    res$tiles |>
      dplyr::select(tile, !!res$dataset_name := value)
  }) |>
    purrr::reduce(dplyr::left_join, by = "tile")

  # B. Stats (Long Format)
  # Extract stats, add DATASET column, bind rows
  stats_agg <- purrr::map(diagnostics_list, function(res) {
    res$stats |>
      dplyr::mutate(DATASET = res$dataset_name, .before = 1)
  }) |>
    dplyr::bind_rows()

  # C. Comps (Long Format)
  # Extract comps, add DATASET column, bind rows
  comps_agg <- purrr::map(diagnostics_list, function(res) {
    res$comps |>
      dplyr::mutate(DATASET = res$dataset_name, .before = 1)
  }) |>
    dplyr::bind_rows()

  # --- 3. Output ---

  final_list <- list(
    tiles = tiles_agg,
    stats = stats_agg,
    comps = comps_agg
  )

  if (isTRUE(print)) {
    cat("\n--- Survey Contrast Report (Multi-Dataset) ---\n")

    cat("\n[1] Tiles (Quantiles):\n")
    print(final_list$tiles, n = Inf)

    cat("\n[2] Stats (Summary Metrics):\n")
    print(final_list$stats, n = Inf)

    cat("\n[3] Comps (Target Alignment):\n")
    print(final_list$comps, n = Inf)
    cat("----------------------------------------------\n\n")
  }

  return(final_list)
}

# Add utils::globalVariables call to silence CHECK notes
if(getRversion() >= "2.15.1") utils::globalVariables(c("tile", "value", "n", "Freq", "TARGET_PERCENT", "UNWT_SAMPLE_PERCENT", "WT_SAMPLE_PERCENT", ":="))
