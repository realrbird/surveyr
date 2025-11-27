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


#' @title Check Target List Structure and Data Alignment
#' @description Internal function to validate the structure of the target list
#'   and ensure it aligns with the survey data (\code{data}).
#'
#' @param targets A named list of target tibbles.
#' @param data The survey data tibble.
#' @keywords internal
chk_target_structure <- function(targets, data) {

  # 1. Check if targets is a named list
  if (!is.list(targets) || is.null(names(targets)) || any(names(targets) == "")) {
    stop("`targets` must be a named list, where names correspond to columns in `data`.", call. = FALSE)
  }

  # Iterate through each target variable (e.g., 'age_group', 'sex_group')
  for (var_name in names(targets)) {
    target_df <- targets[[var_name]]

    # Check 2: Element is a tibble/data.frame
    if (!inherits(target_df, "data.frame")) {
      stop(paste0("Element '", var_name, "' in `targets` must be a data frame or tibble."), call. = FALSE)
    }

    # Check 3: First column name matches list name
    first_col_name <- names(target_df)[1]
    if (first_col_name != var_name) {
      stop(paste0("The first column in target '", var_name, "' must be named '", var_name, "'."), call. = FALSE)
    }

    # Check 4: 'Freq' column exists and sums to 100 (approximately)
    if (!"Freq" %in% names(target_df)) {
      stop(paste0("Target '", var_name, "' must contain a column named 'Freq'."), call. = FALSE)
    }
    if (!is.numeric(target_df$Freq) || abs(sum(target_df$Freq) - 100) > 1e-4) {
      stop(paste0("The 'Freq' column in target '", var_name, "' must be numeric and sum to 100."), call. = FALSE)
    }

    # Check 5: First variable (the category column) in target is a factor
    target_factor <- target_df[[first_col_name]]
    if (!is.factor(target_factor)) {
      stop(paste0("The comparison variable ('", var_name, "') in target must be a factor."), call. = FALSE)
    }

    # Check 6: Corresponding variable in data exists, is factor, and has matching levels
    if (!var_name %in% names(data)) {
      stop(paste0("Variable '", var_name, "' from `targets` list is missing in `data`."), call. = FALSE)
    }

    data_factor <- data[[var_name]]
    if (!is.factor(data_factor)) {
      stop(paste0("Corresponding variable '", var_name, "' in `data` must be a factor."), call. = FALSE)
    }

    if (!all(levels(data_factor) == levels(target_factor))) {
      stop(paste0("Levels of factor '", var_name, "' do not match between `data` and `targets`."), call. = FALSE)
    }
  }

  invisible(NULL)
}
