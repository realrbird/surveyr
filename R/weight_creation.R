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
#' if (requireNamespace("pewmethods", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
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
      stop(paste0("Base weight column '", base_weight, "' not found in `df`."), call. = FALSE)
    }
    # Check weight vector itself for numeric and NAs
    chk_numeric_no_na(df[[base_weight]], arg_name = base_weight)
  } else if (!is.numeric(base_weight) || length(base_weight) != 1 || base_weight < 0) {
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
    base_weight = base_weight, # Pass the argument directly
    ...
  )

  # --- 3. Extract Weights and Clean Up ---

  # Based on the structure of pewmethods::rake_survey, it returns the weight vector directly,
  # which means the variable 'df_raked' in the above line IS the weight vector.
  final_weights <- df_raked

  # Re-create a data frame containing the weights for diagnostics
  df_diag <- df %>%
    dplyr::mutate(RAKE_WEIGHT = final_weights)

  # --- 4. Print Diagnostics ---
  if (isTRUE(print_output)) {
    # Assuming svy_diagnostics is available internally
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
