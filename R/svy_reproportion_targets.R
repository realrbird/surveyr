#' Reproportion a Single Target to Account for Missing Data
#'
#' Adjusts the target proportions of a variable to account for missing
#' cases in the survey data. The non-missing categories are scaled down
#' proportionally, and a new "Missing" category is added that exactly matches
#' the missing rate in the data.
#'
#' @param data A data frame or tibble containing the survey data.
#' @param target A data frame or tibble containing the targets. Must have
#'   a column matching \code{var_name} and a numeric column named \code{Freq}.
#' @param var_name A character string specifying the variable name.
#' @param missing_val A character string for the missing category label. Default is "Missing".
#'
#' @return A tibble with the adjusted targets and preserved factor levels.
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
svy_reproportion_target <- function(data, target, var_name, missing_val = "Missing") {

  # 1. Validation
  if (!var_name %in% names(data)) {
    stop(paste("Variable", var_name, "not found in data."))
  }
  if (!"Freq" %in% names(target)) {
    stop("Target data frame must contain a 'Freq' column.")
  }

  # 2. Calculate missing proportion in the sample data
  x <- data[[var_name]]
  pct_missing <- sum(is.na(x)) / length(x)

  # If no missing data, return target unmodified
  if (pct_missing == 0) {
    return(target)
  }

  # 3. Store original factor levels to preserve order
  orig_levels <- levels(target[[var_name]])
  if (is.null(orig_levels)) {
    # Fallback if target wasn't explicitly a factor
    orig_levels <- unique(as.character(target[[var_name]]))
  }
  new_levels <- c(orig_levels, missing_val)

  # 4. Reproportion frequencies
  # We scale the existing targets down by (1 - pct_missing)
  # and set the missing frequency to the exact total * pct_missing
  total_freq <- sum(target$Freq)

  target_new <- target
  target_new$Freq <- target_new$Freq * (1 - pct_missing)

  missing_freq <- total_freq * pct_missing

  # 5. Create missing row safely
  # Convert column to character temporarily to avoid factor binding warnings
  target_new[[var_name]] <- as.character(target_new[[var_name]])

  missing_row <- tibble::tibble(
    name = missing_val,
    Freq = missing_freq
  )
  # Rename 'name' to the actual variable name
  names(missing_row)[1] <- var_name

  # Bind rows together
  res <- dplyr::bind_rows(target_new, missing_row)

  # 6. Re-apply factor with exact preserved levels
  res[[var_name]] <- factor(res[[var_name]], levels = new_levels)

  return(res)
}


#' Reproportion Multiple Targets
#'
#' Applies \code{svy_reproportion_target} across a list of target data frames.
#'
#' @param data A data frame or tibble containing the survey data.
#' @param targets A named list of target data frames. The names of the list
#'   must correspond to the variable names in the data.
#' @param missing_val A character string for the missing category label. Default is "Missing".
#'
#' @return A named list of reproportioned target tibbles.
#' @importFrom purrr imap
#' @export
svy_reproportion_targets <- function(data, targets, missing_val = "Missing") {

  if (!is.list(targets) || is.null(names(targets))) {
    stop("'targets' must be a named list.")
  }

  purrr::imap(targets, ~ svy_reproportion_target(data, .x, .y, missing_val = missing_val))
}
