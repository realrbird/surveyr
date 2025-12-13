#' Reproportion a Single Target for Missing Data
#'
#' Internal helper function that processes a single target data frame.
#' It identifies missing values in the corresponding variable in the dataset,
#' adds a "Missing" row to the target, and renormalizes the proportions.
#'
#' @param target_df A data frame containing the target distribution.
#' @param var_name The name of the variable (string) to check in `df`.
#' @param df The dataset (data.frame or tibble).
#' @param missing_cat Character string for the missing category label.
#'
#' @return A modified target data frame.
#' @importFrom dplyr mutate across all_of summarise pull bind_rows
#' @importFrom tibble tibble
#' @importFrom rlang sym :=
#' @export
svy_reproportion_target <- function(target_df, var_name, df, missing_cat = 'Missing') {

  # 1. Validation: Variable must exist in data
  if (!var_name %in% names(df)) {
    stop(paste0("Target variable '", var_name, "' is not present in the dataset."))
  }

  # 2. Check for required 'Freq' column
  if (!"Freq" %in% names(target_df)) {
    stop(paste0("Target table for '", var_name, "' must have a numeric column named 'Freq'."))
  }

  # 3. Calculate Missingness in Data
  # using tidyverse syntax as requested
  na_prop <- df |>
    summarise(prop = mean(is.na(!!sym(var_name)))) |>
    pull(prop)

  # If no missing data, return target as is
  if (na_prop == 0) {
    return(target_df)
  }

  # 4. Calculate New Row Values
  # Scale NA proportion to the current sum of weights (e.g. 100 or 1)
  current_sum <- sum(target_df$Freq, na.rm = TRUE)
  added_val <- na_prop * current_sum

  # 5. Create new row
  new_row <- tibble(
    !!sym(var_name) := missing_cat,
    Freq = added_val
  )

  # 6. Bind, Renormalize, and Return
  target_df |>
    # Convert factor to character to bind the new string label
    mutate(across(all_of(var_name), as.character)) |>
    bind_rows(new_row) |>
    # Renormalize Freq so it sums to 100
    mutate(Freq = Freq / sum(Freq) * 100) |>
    # Convert back to factor
    mutate(across(all_of(var_name), as.factor))
}

#' Reproportion Targets to Account for Missing Data
#'
#' Adjusts target distributions to include a category for missing values (NA) found in the
#' sample data. This ensures that records with missing data on weighting variables are
#' accounted for (assigned a specific target proportion) rather than being dropped or
#' causing errors during raking.
#'
#' @param df The dataset (data.frame or tibble).
#' @param targets A named list of data frames representing target distributions.
#'   The list names must correspond to variables in `df`.
#' @param missing_cat Character string for the missing category label. Default is 'Missing'.
#'
#' @return A modified list of targets.
#' @importFrom purrr imap
#' @export
svy_reproportion_targets <- function(df, targets, missing_cat = 'Missing') {

  # Validation
  if (!is.data.frame(df)) stop("Argument 'df' must be a data.frame or tibble.")

  # Validate structure (ensures 'Freq' column exists and vars match)
  chk_target_structure(targets, df)

  # Use purrr::imap to iterate over the list (target_df) and its names (var_name)
  targets |>
    imap(function(target_df, var_name) {
      svy_reproportion_target(target_df, var_name, df, missing_cat)
    })
}
