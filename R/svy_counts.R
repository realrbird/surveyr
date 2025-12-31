#' Count Observations by Group
#'
#' A wrapper around \code{dplyr::count} that adds survey-specific functionality:
#' \itemize{
#'   \item Automatically calculates a percentage column (\code{Freq}).
#'   \item Defaults to \code{.drop = FALSE} to show empty categories.
#'   \item Automatically converts \code{haven_labelled} variables to factors (using
#'         \code{levels = "both"}) to ensure all labels are counted.
#' }
#'
#' @param data A data frame or tibble.
#' @param vars A character vector of variable names to group by.
#' @param wt Frequency weights. Default is \code{NULL}.
#' @param .drop Logical. If \code{FALSE} (default), empty groups are included in the output.
#' @param na_rm Logical. If \code{TRUE} (default), \code{NA} values in the grouping variables
#'   are removed from the result before calculating frequencies.
#' @param ... Additional arguments passed to \code{dplyr::count} (e.g., \code{sort}, \code{name}).
#'
#' @return A tibble containing the grouping variables, \code{n} (count), and \code{Freq} (percentage).
#' @importFrom dplyr count mutate filter if_all all_of
#' @importFrom rlang syms
#' @importFrom haven as_factor
#' @importFrom purrr map
#' @export
svy_count <- function(data, vars, wt = NULL, .drop = FALSE, na_rm = TRUE, ...) {

  # 1. Validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble.")
  }
  if (!is.character(vars) || length(vars) == 0) {
    stop("'vars' must be a character vector of variable names.")
  }

  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste0("Variables not found in data: ", paste(missing_vars, collapse = ", ")))
  }

  # 2. Pre-process: Check for labelled variables and convert
  data_mod <- data
  converted_vars <- character()

  for (v in vars) {
    if (inherits(data_mod[[v]], "haven_labelled")) {
      data_mod[[v]] <- haven::as_factor(data_mod[[v]], levels = "both")
      converted_vars <- c(converted_vars, v)
    }
  }

  if (length(converted_vars) > 0) {
    message("Note: The following labelled variables were converted to factors: ",
            paste(converted_vars, collapse = ", "))
  }

  # 3. Perform Count
  # Pass wt explicitly. If NULL, count handles it (counts rows).
  res <- dplyr::count(data_mod, !!!rlang::syms(vars), wt = {{ wt }}, .drop = .drop, ...)

  # 4. Handle NA removal
  if (na_rm) {
    # Filter out rows where ANY of the grouping variables are NA
    res <- res |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(vars), ~ !is.na(.)))
  }

  # 5. Calculate Frequencies (Percentages)
  res <- res |>
    dplyr::mutate(Freq = n / sum(n) * 100)

  return(res)
}

#' Count Observations for Multiple Variable Sets
#'
#' Applies \code{svy_count} to a list of variable sets.
#'
#' @param data A data frame or tibble.
#' @param var_list A list where each element is a character vector of variable names.
#' @param wt Frequency weights. Default is \code{NULL}.
#' @param .drop Logical. If \code{FALSE} (default), empty groups are included in the output.
#' @param na_rm Logical. If \code{TRUE} (default), \code{NA} values in the grouping variables
#'   are removed from the result before calculating frequencies.
#' @param ... Additional arguments passed to \code{svy_count}.
#'
#' @return A list of count tibbles, one for each element in \code{var_list}.
#' @importFrom purrr map
#' @export
svy_counts <- function(data, var_list, wt = NULL, .drop = FALSE, na_rm = TRUE, ...) {

  if (!is.list(var_list)) {
    stop("'var_list' must be a list of character vectors.")
  }

  purrr::map(var_list, ~ svy_count(data, .x, wt = wt, .drop = .drop, na_rm = na_rm, ...))
}
