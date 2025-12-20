#' Create a Joint Factor Variable
#'
#' Combines multiple factor variables into a single joint factor variable.
#' The levels of the new factor are created by crossing the levels of the input
#' factors in the order specified. This is often used to create interaction
#' terms for raking targets (e.g., Age x Sex).
#'
#' @param data A data frame or tibble containing the survey data.
#' @param vars A character vector of the variable names to combine.
#'   The order of variables matters: the levels of the first variable will vary
#'   fastest in the resulting factor levels.
#' @param sep A character string to separate the levels. Default is "/".
#'
#' @return A factor vector representing the joint variable.
#' @importFrom forcats fct_cross
#' @importFrom purrr map_lgl
#' @importFrom rlang exec
#' @export
svy_create_joint_var <- function(data, vars, sep = "/") {

  # ---------------------------------------------------------
  # 1. Validation Checks
  # ---------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble.")
  }

  if (!is.character(vars) || length(vars) < 2) {
    stop("'vars' must be a character vector containing at least two variable names.")
  }

  if (!is.character(sep) || length(sep) != 1) {
    stop("'sep' must be a single string.")
  }

  # Check if variables exist in data
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste0("The following variables are not in the dataset: ", paste(missing_vars, collapse = ", ")))
  }

  # Check if all specified variables are factors
  # We select the columns and check their class using purrr::map_lgl
  are_factors <- purrr::map_lgl(data[vars], is.factor)

  if (!all(are_factors)) {
    non_factors <- vars[!are_factors]
    stop(paste0("All variables to combine must be factors. The following are not: ", paste(non_factors, collapse = ", ")))
  }

  # ---------------------------------------------------------
  # 2. Create Joint Variable
  # ---------------------------------------------------------
  # Extract the columns as a list
  cols <- as.list(data[vars])

  # Remove names to prevent 'fct_cross' from failing (it requires positional args)
  cols <- unname(cols)

  # Generate the joint factor
  # Note: By default, fct_cross varies the first factor SLOWEST.
  result <- rlang::exec(forcats::fct_cross, !!!cols, sep = sep, keep_empty = TRUE)

  # ---------------------------------------------------------
  # 3. Re-level (Enforce "First Factor Varies Fastest")
  # ---------------------------------------------------------
  # To match the requirement that the first variable varies fastest (e.g. Age changes, Sex fixed),
  # we must manually construct the level order using expand.grid (which varies first arg fastest).

  input_levels <- lapply(cols, levels)
  grid <- expand.grid(input_levels, stringsAsFactors = FALSE)
  ordered_levels <- do.call(paste, c(grid, sep = sep))

  # Re-apply the levels to the result
  result <- factor(result, levels = ordered_levels)

  return(result)
}
