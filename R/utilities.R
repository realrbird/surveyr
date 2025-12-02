#' @title Survey Utility Functions
#' @description This file contains utility and housekeeping functions for survey
#'   data preparation and quality assurance.
#' @name survey_utilities
NULL

# External Imports
#' @importFrom dplyr mutate select filter count distinct all_of
#' @importFrom rlang .data
#' @importFrom purrr map map_lgl map_chr
#' @importFrom stats cor model.matrix na.omit sd
#' @importFrom crayon green red yellow style bold

#' @title Pre-flight Raking Check
#' @description Performs essential validation checks on survey data and targets
#'   before running raking (\code{svy_rake}). Checks include factor alignment,
#'   cell counts, missing values, and high correlation between raking variables.
#'
#' @param data A data frame or tibble containing the survey variables.
#' @param target_list A named list of tibbles containing the population targets.
#' @param verbose Logical. If \code{TRUE}, prints the results of each check
#'   with color-coded PASS/FAIL/WARN messages. Defaults to \code{TRUE}.
#'
#' @return A single logical value: \code{TRUE} if all checks passed (only warnings occurred),
#'   \code{FALSE} if any check failed (an error occurred).
#'
#' @details
#' This function is designed to prevent failures in \code{svy_rake()}.
#' \itemize{
#'   \item **FAIL (Red)**: Indicates a fatal error (e.g., mismatched factor levels, missing columns) that will stop raking.
#'   \item **WARN (Yellow)**: Indicates a condition that may cause instability (e.g., low cell counts, high correlation) but allows raking to proceed.
#'   \item **PASS (Green)**: Indicates the condition is met.
#' }
#'
#' **High Correlation Check (Warning):** Issues a warning if the absolute correlation
#' between any pair of raking variables (when converted to dummy variables) is greater
#' than \code{0.8}. Highly correlated variables can lead to matrix singularity and unstable weights.
#'
#' @examples
#' # Requires devtools::load_all() and crayon package
#' if (requireNamespace("crayon", quietly = TRUE)) {
#'   data("survey_df")
#'   data("target_list")
#'
#'   # All checks should pass or warn on correlation
#'   svy_check(survey_df, target_list)
#'
#'   # Example of a FAIL case (missing data)
#'   # df_bad <- survey_df
#'   # df_bad$age_group[1] <- NA
#'   # svy_check(df_bad, target_list)
#' }
#'
#' @export
svy_check <- function(data, target_list, verbose = TRUE) {

  # --- Internal Helpers for Reporting ---
  pass <- function(msg) if (isTRUE(verbose)) cat(crayon::green$bold("PASS: "), msg, "\n")
  fail <- function(msg) {
    if (isTRUE(verbose)) cat(crayon::red$bold("FAIL: "), msg, "\n")
    return(FALSE)
  }
  warn <- function(msg) if (isTRUE(verbose)) cat(crayon::yellow$bold("WARN: "), msg, "\n")

  overall_result <- TRUE

  if (isTRUE(verbose)) cat(crayon::style("--- Raking Pre-flight Checks ---\n", "bold"))

  # --- CHECK 1-3 & 5 (Initial Structure and Factor Alignment) ---

  tryCatch({
    # chk_target_structure handles structure, alignment, factor types, and required columns (Freq)
    chk_target_structure(target_list, data)

    # Check 5 (Missing values check for target variables)
    target_vars <- names(target_list)
    na_check_result <- purrr::map_lgl(target_vars, function(v) {
      if (any(is.na(data[[v]]))) {
        fail(paste0("Target variable '", v, "' contains missing values (NA). Raking requires complete data."))
        return(FALSE)
      }
      TRUE
    })

    if (all(na_check_result)) {
      pass("Targets list structure, data alignment, factor levels, and NA checks passed.")
    } else {
      overall_result <- FALSE
    }

  }, error = function(e) {
    fail(paste("Structure/Alignment Check Failed:", gsub("Error in chk_target_structure\\(targets, df\\): ", "", e$message)))
    overall_result <<- FALSE
  })

  # Stop if the fundamental structure checks failed
  if (!overall_result) {
    if (isTRUE(verbose)) cat(crayon::red$bold("STOPPED: Fundamental raking structure failed. Fix the red items.\n"))
    return(FALSE)
  }

  # --- CHECK 4: Low Cell Counts (Warning) ---

  target_vars <- names(target_list)
  low_count_check <- TRUE

  for (v in target_vars) {
    # CRITICAL FIX: Use .drop = FALSE to ensure levels with 0 counts are included
    counts_df <- data %>%
      dplyr::count(.data[[v]], .drop = FALSE) %>%
      dplyr::mutate(cell_count = .data$n) %>%
      dplyr::filter(.data$cell_count < 10)

    if (nrow(counts_df) > 0) {
      cell_levels <- paste(as.character(counts_df[[1]]), collapse = ", ")
      warn(paste0("Low Cell Counts (<10): Variable '", v, "' has low counts for levels: ", cell_levels, ". Raking may be unstable."))
      low_count_check <- FALSE
      overall_result <- FALSE # Mark overall result as FALSE due to critical warning
    }
  }

  if (low_count_check) {
    pass("All target cells have acceptable sample sizes (>= 10).")
  }

  # --- CHECK 6: High Correlation (Warning) ---

  correlation_threshold <- 0.80

  tryCatch({
    # Select only the target variables (factors)
    data_for_cor <- data %>%
      dplyr::select(dplyr::all_of(target_vars)) %>%
      stats::na.omit()

    # Create dummy variables (model matrix) from the factors
    dummy_matrix <- stats::model.matrix(~ ., data = data_for_cor)

    # --- FIX for Zero Variance Columns (Collinearity) ---
    # Calculate standard deviations of dummy variables
    dummy_sds <- apply(dummy_matrix, 2, stats::sd)

    # Filter out zero variance columns (which cause NaN correlations)
    # Use a small tolerance for floating point comparisons
    valid_cols <- dummy_sds > 1e-8
    dummy_matrix_filtered <- dummy_matrix[, valid_cols, drop = FALSE]

    # Check if we still have variables left to correlate
    if (ncol(dummy_matrix_filtered) <= 1) {
      # If only 0 or 1 variable remains, correlation check is irrelevant or impossible
      pass("Collinearity check passed (only one effective dummy variable remains).")
    } else {
      # Calculate the correlation matrix
      cor_matrix <- stats::cor(dummy_matrix_filtered)

      # Check for high correlation (excluding self-correlation)
      high_cor <- FALSE

      for (i in 1:(ncol(cor_matrix) - 1)) {
        for (j in (i + 1):ncol(cor_matrix)) {
          # Check if the value is finite (required since we might still get Inf/NaN if SD is tiny)
          if (is.finite(cor_matrix[i, j]) && abs(cor_matrix[i, j]) > correlation_threshold) {
            warn(paste0("High Correlation (>", correlation_threshold * 100, "%): Raking variables ",
                        rownames(cor_matrix)[i], " and ",
                        colnames(cor_matrix)[j],
                        " are highly correlated. This can cause matrix singularity and unstable weights."))
            high_cor <- TRUE
            overall_result <- FALSE # Mark overall result as FALSE due to critical warning
            break
          }
        }
        if (high_cor) break
      }

      if (!high_cor) {
        pass("All raking variables show low inter-correlation (r <= 0.80).")
      }
    }

  }, error = function(e) {
    # This might catch deeper errors during matrix calculation
    warn(paste("Correlation Check Encountered a deeper issue (Skipped):", e$message))
    overall_result <- FALSE
  })

  # --- Final Conclusion ---
  if (isTRUE(verbose)) {
    if (overall_result) {
      cat(crayon::green$bold("\nConclusion: All essential checks passed. Raking should run successfully.\n"))
    } else {
      cat(crayon::red$bold("\nConclusion: One or more checks failed or issued critical warnings. Review the red/yellow messages before raking.\n"))
    }
  }

  # Return TRUE only if no fundamental failures occurred (i.e., overall_result remains TRUE)
  return(overall_result)
}
