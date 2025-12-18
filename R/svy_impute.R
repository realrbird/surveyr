#' Impute Missing Values in Survey Data
#'
#' Imputes missing values for specified variables using the `pewmethods` package (which wraps `mice` and `ranger`).
#' It performs checks for variable existence, naming conflicts, and high correlation before imputation.
#'
#' @param data A data frame or tibble containing the survey data.
#' @param vars_to_impute A character vector of variable names to impute.
#' @param vars_to_keep A character vector of variable names to keep and return (imputed versions).
#' @param pre_fix A character string to prefix the returned variable names. Default is "imp_".
#' @param convert_to_fct Logical. If TRUE, converts labelled/character variables to factors before imputation. Default is TRUE.
#' @param method The imputation method to use (passed to `pewmethods::impute_vars`). Default is "ranger".
#' @param seed Numeric seed for reproducibility. Default is NA.
#' @param correlation_threshold Numeric value (0-1). Warns if variables are correlated above this threshold. Default is 0.99.
#' @param ... Additional arguments passed to `pewmethods::impute_vars`.
#'
#' @return A data frame containing the original ID (if applicable) and the imputed variables merged back.
#'
#' @importFrom dplyr select mutate rename_with left_join all_of across %>%
#' @importFrom haven as_factor
#' @importFrom stats cor
#' @export
svy_impute <- function(
    data,
    vars_to_impute,
    vars_to_keep,
    pre_fix = 'imp_',
    convert_to_fct = TRUE,
    method = 'ranger',
    seed = NA,
    correlation_threshold = 0.99,
    ...
) {

  # ------------------------------------------------------------------
  # 1. Validation Checks
  # ------------------------------------------------------------------

  # Check data type
  if (!is.data.frame(data)) stop('"data" must be a data.frame or tibble.')
  if (!is.character(vars_to_impute) || length(vars_to_impute) == 0) stop('"vars_to_impute" must be a character vector with length > 0.')
  if (!is.character(vars_to_keep) || length(vars_to_keep) == 0) stop('"vars_to_keep" must be a character vector with length > 0.')
  if (!is.character(pre_fix) || length(pre_fix) != 1) stop('"pre_fix" must be a single character string.')

  all_vars <- unique(c(vars_to_impute, vars_to_keep))
  missing_vars <- setdiff(all_vars, colnames(data))
  if (length(missing_vars) > 0) stop(paste0("The following variables are not in the dataset: ", paste(missing_vars, collapse = ", ")))

  new_names <- paste0(pre_fix, vars_to_keep)
  existing_conflicts <- intersect(new_names, colnames(data))
  if (length(existing_conflicts) > 0) stop(paste0("The following result variables already exist in the data: ", paste(existing_conflicts, collapse = ", ")))

  # ------------------------------------------------------------------
  # 2. Correlation/Collinearity Check
  # ------------------------------------------------------------------
  check_data <- data %>% select(all_of(all_vars))
  check_data_num <- check_data %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    mutate(across(where(is.character), ~as.numeric(as.factor(.)))) %>%
    select(where(is.numeric))

  if (ncol(check_data_num) > 1) {
    corr_matrix <- cor(check_data_num, use = "pairwise.complete.obs")
    diag(corr_matrix) <- 0
    high_corr <- which(abs(corr_matrix) > correlation_threshold, arr.ind = TRUE)
    if (nrow(high_corr) > 0) {
      warning("High correlation detected. This may affect imputation results.")
    }
  }

  # ------------------------------------------------------------------
  # 3. Setup Unique ID for Merging
  # ------------------------------------------------------------------
  keep_iterating <- TRUE
  while (keep_iterating) {
    unique_id_num <- sample(1:10000, 1)
    unique_id <- paste0('temp_id_for_imputation_', unique_id_num)
    if (!(unique_id %in% colnames(data))) {
      keep_iterating <- FALSE
      data[[unique_id]] <- 1:nrow(data)
    }
  }

  data_copy <- data

  # ------------------------------------------------------------------
  # 4. Pre-processing
  # ------------------------------------------------------------------
  if (convert_to_fct) {
    # First handle haven labels
    data_copy <- haven::as_factor(data_copy)

    # Explicitly convert remaining characters to factors
    data_copy <- data_copy %>%
      mutate(across(where(is.character), as.factor))
  }

  # ------------------------------------------------------------------
  # 5. Run Imputation
  # ------------------------------------------------------------------
  # Ensure pewmethods is available (suggested package)
  if (!requireNamespace("pewmethods", quietly = TRUE)) {
    stop("The 'pewmethods' package is required for this function. Please install it.")
  }

  data_copy <- data_copy %>%
    pewmethods::impute_vars(
      to_impute = all_vars,
      method = method,
      seed = seed,
      ...
    )

  # ------------------------------------------------------------------
  # 6. Post-Imputation Validation
  # ------------------------------------------------------------------
  missing_check <- colSums(is.na(data_copy[vars_to_keep]))
  if (any(missing_check > 0)) {
    failed_vars <- names(missing_check[missing_check > 0])
    stop(paste0(
      "Imputation Validation Failed: The following variables in 'vars_to_keep' still contain missing values: ",
      paste(failed_vars, collapse = ", ")
    ))
  }

  # ------------------------------------------------------------------
  # 7. Format and Return
  # ------------------------------------------------------------------
  final_data_copy <- data_copy %>%
    select(all_of(c(unique_id, vars_to_keep))) %>%
    rename_with(~ paste0(pre_fix, .), .cols = all_of(vars_to_keep))

  result <- data %>%
    left_join(final_data_copy, by = unique_id) %>%
    select(-all_of(unique_id))

  return(result)
}
