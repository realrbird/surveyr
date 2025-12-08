#' @title Survey Weight Creation Functions
#' @description This file contains functions for creating, adjusting, and
#'   validating survey weights, primarily using the stability and correctness
#'   of the 'survey' package for core calculations.
#' @name weight_creation
NULL

# Use specific @importFrom to resolve conflicts between dplyr, purrr, rlang, and stats
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom stats formula
#' @importFrom utils head
#' @import pewmethods

#' @title Calibrate Weights to Population Targets (Raking)
#' @description Adjusts survey weights to match population margins (targets) using
#'   the underlying Generalized Regression Estimator (GREG) provided by
#'   \code{pewmethods::rake_survey()}.
#'
#' @param df A data frame or tibble containing the survey variables.
#' @param targets A named list of tibbles containing the population targets. The names
#'   of the list elements must match the variable names in \code{df}. The calibration
#'   relies only on the \code{Freq} column (percentages 0-100) in each target tibble.
#' @param base_weight A character string for the column in \code{df} containing the
#'   initial weights, or a numeric constant (e.g., \code{1}) for unweighted data.
#'   Defaults to \code{1}.
#' @param print_output A logical flag. If \code{TRUE}, the diagnostic report from
#'   \code{svy_diagnostics()} is printed to the console using the full tibble print
#'   (\code{n = Inf}). Defaults to \code{TRUE}.
#' @param ... Additional arguments passed directly to \code{pewmethods::rake_survey}.
#'
#' @return A **numeric vector** containing the final calibrated weights (named \code{RAKE_WEIGHT}).
#'
#' @details
#' This function relies on the \code{pewmethods} package for the core raking calculation,
#' ensuring stability and statistical correctness.
#'
#' The function passes the \code{base\_weight} directly to \code{pewmethods::rake_survey},
#' which handles a numeric default of \code{1} or a character string column name.
#'
#' @examples
#' # Requires devtools::load_all() and the pewmethods package installed
#' if (requireNamespace("pewmethods", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # Load the internal data
#'   data("survey_df")
#'   data("target_list")
#'
#'   # 1. Rake an UNWEIGHTED sample (using base_weight = 1 default)
#'   raked_weights_1 <- svy_rake(survey_df, target_list)
#'
#'   # 2. Rake a PRE-WEIGHTED sample
#'   raked_weights_2 <- svy_rake(survey_df, target_list, base_weight = "WEIGHT")
#'
#'   # Check the length of the returned vector
#'   # length(raked_weights_1)
#' }
#'
#' @export
svy_rake <- function(df, targets, base_weight = 1, print_output = TRUE, ...) {

  if (!requireNamespace("pewmethods", quietly = TRUE)) {
    stop("The 'pewmethods' package is required for svy_rake. Please install it.", call. = FALSE)
  }

  # --- 1. Validation and Base Weight Preparation ---
  # Assuming chk_target_structure and chk_numeric_no_na are available in the package environment
  chk_target_structure(targets, df)

  # Check if base_weight is a column name (character)
  if (is.character(base_weight)) {
    if (!base_weight %in% names(df)) {
      # Issue a warning and fall back to numeric base weight of 1
      warning(paste0("Weight column '", base_weight, "' not found in `df`. Using base weight of 1."), call. = FALSE)
      base_weight <- 1
    } else {
      # Check weight vector itself for numeric and NAs only if the column exists
      chk_numeric_no_na(df[[base_weight]], arg_name = base_weight)
    }
  } else if (!is.numeric(base_weight) | length(base_weight) != 1 | base_weight < 0) {
    stop("`base_weight` must be a character string (column name) or a single non-negative numeric constant.", call. = FALSE)
  }

  # --- 2. Run Raking using pewmethods::rake_survey() ---

  # CRITICAL: Convert df to a base data.frame for compatibility with underlying survey package
  df_base <- as.data.frame(df)

  # Note: The weight_var argument can take either a string (column name) or
  # a numeric constant (like 1) which rake_survey handles.
  df_raked <- pewmethods::rake_survey(
    .data = df_base,
    pop_margins = targets,
    base_weight = base_weight, # Pass the argument directly (either the string or the fallback numeric 1)
    ...
  )

  # --- 3. Extract Weights and Finalize DF ---

  # pewmethods::rake_survey returns the weight vector directly
  final_weights <- df_raked

  # Re-create a data frame containing the weights for diagnostics
  df_diag <- df |>
    dplyr::mutate(RAKE_WEIGHT = final_weights)

  # --- 4. Print Diagnostics ---
  if (isTRUE(print_output)) {
    # Assuming svy_diagnostics is available internally
    # Note: target_list is needed for svy_comps inside svy_diagnostics
    diag_report <- svy_diagnostics(
      data = df_diag,
      targets = targets,
      wt_var = "RAKE_WEIGHT",
      print = FALSE
    )

    cat("\n--- Raking Diagnostic Report: RAKE_WEIGHT ---\n")
    cat("\n[1] Tiles (Quantiles):\n")
    print(diag_report$tiles, n = Inf)

    cat("\n[2] Stats (DEFF, ESS, MOE):\n")
    print(diag_report$stats, n = Inf)

    cat("\n[3] Comps (Target Alignment):\n")
    print(diag_report$comps, n = Inf)
    cat("---------------------------------------------\n\n")
  }

  # --- 5. Final Return ---
  return(final_weights)
}


#' @title Trim Extreme Survey Weights
#' @description Implements weight trimming (Winsorization) to reduce the influence
#'   of extreme weights, improving variance estimation and stability. This is a
#'   wrapper around \code{pewmethods::trim_weights()}.
#'
#' @param df A data frame or tibble containing the survey variables and the weight column.
#' @param wt_var A character string for the column in \code{df} containing the weights to be trimmed.
#' @param lower_quantile The lower quantile (e.g., 0.01 for 1st percentile) below which
#'   weights will be capped at the threshold. Defaults to 0.01.
#' @param upper_quantile The upper quantile (e.g., 0.99 for 99th percentile) above which
#'   weights will be capped at the threshold. Defaults to 0.99.
#' @param print_output A logical flag. If \code{TRUE}, the diagnostic report from
#'   \code{svy_diagnostics()} is printed to the console using the full tibble print
#'   (\code{n = Inf}). Defaults to \code{TRUE}.
#' @param ... Additional arguments passed directly to \code{pewmethods::trim_weights}
#'   (e.g., \code{minval}, \code{maxval}, \code{strict}).
#'
#' @return A **numeric vector** containing the final trimmed weights (named \code{TRIM_WEIGHT}).
#'
#' @details
#' Trimming replaces weights falling outside the specified quantiles with the
#' quantile values themselves. This function requires \code{pewmethods} to be installed.
#'
#' @examples
#' # Requires devtools::load_all() and the pewmethods package installed
#' if (requireNamespace("pewmethods", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # Load the internal data
#'   data("survey_df")
#'
#'   # Trim the existing WEIGHT column at the 1st and 99th percentiles (default)
#'   trimmed_weights <- svy_trim(survey_df, "WEIGHT")
#'
#'   # Check max original vs max trimmed
#'   # max(survey_df$WEIGHT)
#'   # max(trimmed_weights)
#' }
#'
#' @export
svy_trim <- function(df, wt_var, lower_quantile = 0.01, upper_quantile = 0.99, print_output = TRUE, ...) {

  if (!requireNamespace("pewmethods", quietly = TRUE)) {
    stop("The 'pewmethods' package is required for svy_trim. Please install it.", call. = FALSE)
  }

  # --- 1. Validation and Setup ---
  # Check if wt_var is character and exists
  if (!is.character(wt_var) | length(wt_var) != 1 | !wt_var %in% names(df)) {
    stop("`wt_var` must be a single character string column name present in `df`.", call. = FALSE)
  }
  # Check weight vector itself for numeric and NAs
  chk_numeric_no_na(df[[wt_var]], arg_name = wt_var)

  # Check quantiles
  if (!is.numeric(lower_quantile) | lower_quantile < 0 | lower_quantile >= 1 |
      !is.numeric(upper_quantile) | upper_quantile <= 0 | upper_quantile > 1 |
      lower_quantile >= upper_quantile) {
    stop("Quantiles must be numeric, between 0 and 1, and lower_quantile must be less than upper_quantile.", call. = FALSE)
  }

  # CRITICAL: Convert df to a base data.frame for compatibility
  df_base <- as.data.frame(df)

  # --- 2. Run Trimming using pewmethods::trim_weights() ---

  trimmed_weights <- pewmethods::trim_weights(
    weight = df_base[[wt_var]],
    lower_quantile = lower_quantile,
    upper_quantile = upper_quantile,
    ...
  )

  # --- 3. Run Diagnostics ---

  # Re-create a data frame containing the weights for diagnostics
  df_diag <- df |>
    dplyr::mutate(TRIM_WEIGHT = trimmed_weights)

  if (isTRUE(print_output)) {
    # Assuming svy_diagnostics is available internally
    # Note: target_list needs to be available for svy_comps inside svy_diagnostics
    diag_report <- svy_diagnostics(
      data = df_diag,
      targets = target_list, # Assumes target_list is loaded/available
      wt_var = "TRIM_WEIGHT",
      print = FALSE
    )

    cat("\n--- Trimming Diagnostic Report: TRIM_WEIGHT ---\n")
    cat("\n[1] Tiles (Quantiles):\n")
    print(diag_report$tiles, n = Inf)

    cat("\n[2] Stats (DEFF, ESS, MOE):\n")
    print(diag_report$stats, n = Inf)

    # Comps are less critical for trimming but included for completeness
    cat("\n[3] Comps (Target Alignment):\n")
    print(diag_report$comps, n = Inf)
    cat("---------------------------------------------\n\n")
  }

  # --- 4. Final Return ---
  return(trimmed_weights)
}

#' @title Iterative Raking and Trimming
#' @description Performs an iterative process of raking (to population targets) and
#'   trimming (to specific caps or quantiles) until the weights stabilize or the
#'   maximum number of iterations is reached. This replicates the logic of common
#'   SPSS raking macros.
#'
#' @param df A data frame or tibble containing the survey variables.
#' @param targets A named list of tibbles containing the population targets (see \code{svy_rake}).
#' @param base_weight A character string for the column in \code{df} containing the
#'   initial weights, or a numeric constant (e.g., \code{1}) for unweighted data.
#' @param max_iter The maximum number of rake-trim-rescale iterations to perform. Defaults to 10.
#' @param lower_quantile The lower quantile for trimming (passed to \code{svy_trim}). Defaults to 0.01.
#' @param upper_quantile The upper quantile for trimming (passed to \code{svy_trim}). Defaults to 0.99.
#' @param min_weight An optional absolute lower cap for weights (passed to \code{pewmethods::trim_weights} as \code{minval}).
#'   If provided, overrides \code{lower_quantile}.
#' @param max_weight An optional absolute upper cap for weights (passed to \code{pewmethods::trim_weights} as \code{maxval}).
#'   If provided, overrides \code{upper_quantile}.
#' @param print_output A logical flag. If \code{TRUE}, prints the final diagnostics.
#'
#' @return A **numeric vector** containing the final iteratively raked and trimmed weights.
#'
#' @details
#' The algorithm loop is:
#' 1. **Rake** to targets (using \code{svy_rake}).
#' 2. **Trim** weights (using \code{svy_trim}).
#' 3. **Rescale** trimmed weights to match the sum of the weights from step 1.
#' 4. Repeat until \code{max_iter} is reached or weights converge (not yet implemented).
#'
#' @examples
#' # Requires devtools::load_all()
#' if (requireNamespace("pewmethods", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'
#'   # Run iterative raking with strict caps (0.5 to 5.0)
#'   final_w <- svy_rake_with_trim(
#'     survey_df,
#'     target_list,
#'     base_weight = 1,
#'     min_weight = 0.2,
#'     max_weight = 5.0,
#'     max_iter = 5
#'   )
#' }
#'
#' @export
svy_rake_with_trim <- function(df,
                               targets,
                               base_weight = 1,
                               max_iter = 10,
                               lower_quantile = 0.01,
                               upper_quantile = 0.99,
                               min_weight = NULL,
                               max_weight = NULL,
                               print_output = TRUE) {

  # --- 1. Validation Checks (Using existing utilities) ---
  chk_target_structure(targets, df)

  # Initialize the working weight
  # We need a temporary column name in the data frame to pass to svy_trim/svy_rake
  temp_wt_col <- "INTERNAL_ITER_WEIGHT"

  # Setup initial weight vector
  if (is.character(base_weight)) {
    current_weights <- df[[base_weight]]
  } else {
    current_weights <- rep(base_weight, nrow(df))
  }

  # Add to dataframe for processing
  df_iter <- df |> dplyr::mutate(!!temp_wt_col := current_weights)

  cat(crayon::style(paste0("\n--- Starting Iterative Raking (Max Iter: ", max_iter, ") ---\n"), "bold"))

  for (i in 1:max_iter) {

    # --- Step A: Rake ---
    # Rake the current weights to targets
    # Note: We pass print_output=FALSE to suppress intermediate spam
    raked_weights <- svy_rake(
      df = df_iter,
      targets = targets,
      base_weight = temp_wt_col,
      print_output = FALSE
    )

    # Store the Total Sum of Weights (Target Sum) BEFORE trimming
    target_sum <- sum(raked_weights)

    # Update DF with raked weights for trimming step
    df_iter[[temp_wt_col]] <- raked_weights

    # --- Step B: Trim ---
    # Determine arguments for trimming. If min/max_weight are provided, they override quantiles.
    trim_args <- list(
      df = df_iter,
      wt_var = temp_wt_col,
      print_output = FALSE
    )

    if (!is.null(min_weight)) trim_args$minval <- min_weight
    if (!is.null(max_weight)) trim_args$maxval <- max_weight

    # Only use quantiles if absolute caps are NOT provided (pewmethods priority)
    # Actually, svy_trim defaults 0.01/0.99. We pass them explicitly if caps aren't set.
    # To disable quantile trimming when caps are used, we can pass 0 and 1.
    if (!is.null(min_weight) || !is.null(max_weight)) {
      trim_args$lower_quantile <- 0
      trim_args$upper_quantile <- 1
    } else {
      trim_args$lower_quantile <- lower_quantile
      trim_args$upper_quantile <- upper_quantile
    }

    # Call svy_trim using do.call to handle dynamic arguments
    trimmed_weights <- do.call(svy_trim, trim_args)

    # --- Step C: Rescale ---
    # Trimming reduces the total sum. We must scale back up to 'target_sum'.
    trimmed_sum <- sum(trimmed_weights)
    rescaling_factor <- target_sum / trimmed_sum

    final_iter_weights <- trimmed_weights * rescaling_factor

    # Update DF for next iteration
    df_iter[[temp_wt_col]] <- final_iter_weights

    # (Optional) Check convergence could go here
    # For now, we run the fixed number of iterations requested
    cat(".")
  }
  cat("\n")

  # --- Final Output ---
  final_weights <- df_iter[[temp_wt_col]]

  if (isTRUE(print_output)) {
    # Run final diagnostics
    diag_report <- svy_diagnostics(
      data = df_iter,
      targets = targets,
      wt_var = temp_wt_col,
      print = FALSE
    )

    cat("\n--- Iterative Raking Final Report ---\n")
    cat("\n[1] Tiles (Quantiles):\n")
    print(diag_report$tiles, n = Inf)

    cat("\n[2] Stats (DEFF, ESS, MOE):\n")
    print(diag_report$stats, n = Inf)

    cat("\n[3] Comps (Target Alignment):\n")
    print(diag_report$comps, n = Inf)
    cat("-------------------------------------\n\n")
  }

  return(final_weights)
}
