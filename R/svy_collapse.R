#' Collapse Factor Levels in Data and Targets
#'
#' Collapses factor levels for a specific variable in both the dataset and the
#' target list according to a specified mapping. This is useful for combining
#' small or empty categories to ensure raking converges.
#'
#' @param data A data frame or tibble containing the survey data.
#' @param targets A named list of target data frames.
#' @param var_name The name of the variable to collapse (as a string).
#' @param mapping A named list specifying how to collapse levels. The names of the list
#'   are the new category labels, and the values are character vectors of the old categories
#'   to combine. Passed to `forcats::fct_collapse`.
#'
#' @return A list containing two elements:
#'   \item{data}{The modified dataset with collapsed factor levels.}
#'   \item{targets}{The modified target list with collapsed levels and aggregated frequencies.}
#'
#' @importFrom dplyr mutate group_by summarise ungroup
#' @importFrom forcats fct_collapse
#' @importFrom rlang sym exec !!! :=
#' @export
svy_collapse <- function(data, targets, var_name, mapping) {

  # ---------------------------------------------------------
  # 1. Validation Checks
  # ---------------------------------------------------------

  # Check var_name argument format
  if (!is.character(var_name) || length(var_name) != 1) {
    stop("'var_name' must be a single string.")
  }

  # Validate structure of data and targets using internal helper.
  # This performs the following checks:
  # - 'data' is a data.frame/tibble
  # - 'targets' is a named list of data frames
  # - All target tables have a 'Freq' column
  # - All variables in 'targets' exist in 'data'
  chk_target_structure(targets, data)

  # Check if the specific variable requested exists in the targets list
  if (!var_name %in% names(targets)) {
    stop(paste0("Variable '", var_name, "' not found in targets list."))
  }

  # Note: We don't need to explicitly check if var_name is in 'data' or if
  # the target table has 'Freq', because chk_target_structure already guaranteed
  # that for every element in the targets list.

  # ---------------------------------------------------------
  # 2. Modify Data
  # ---------------------------------------------------------
  # Use rlang::exec to pass the mapping list as named arguments to fct_collapse
  data_mod <- data |>
    mutate(!!sym(var_name) := rlang::exec(forcats::fct_collapse, !!sym(var_name), !!!mapping))

  # ---------------------------------------------------------
  # 3. Modify Targets
  # ---------------------------------------------------------
  target_df <- targets[[var_name]]

  # Collapse levels and sum the frequencies
  target_mod <- target_df |>
    mutate(!!sym(var_name) := rlang::exec(forcats::fct_collapse, !!sym(var_name), !!!mapping)) |>
    group_by(!!sym(var_name)) |>
    summarise(Freq = sum(Freq, na.rm = TRUE), .groups = "drop")

  # Update list
  targets_mod <- targets
  targets_mod[[var_name]] <- target_mod

  # ---------------------------------------------------------
  # 4. Return
  # ---------------------------------------------------------
  return(list(data = data_mod, targets = targets_mod))
}
