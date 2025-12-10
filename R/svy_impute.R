library(dplyr)
library(haven)

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

  # ... [Keep Validation Checks 1, 2, 3 same as before] ...

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

  # Correlation Logic [Same as before]
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

  # Setup ID
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
  # 4. Pre-processing (UPDATED)
  # ------------------------------------------------------------------
  if (convert_to_fct) {
    # First handle haven labels
    data_copy <- haven::as_factor(data_copy)

    # NEW: Explicitly convert remaining characters to factors
    # Imputation engines like ranger often skip character columns otherwise
    data_copy <- data_copy %>%
      mutate(across(where(is.character), as.factor))
  }

  # ------------------------------------------------------------------
  # 5. Run Imputation
  # ------------------------------------------------------------------
  data_copy <- data_copy %>%
    pewmethods::impute_vars(
      to_impute = all_vars,
      method = method,
      seed = seed,
      ...
    )

  # ... [Keep Post-Imputation Validation & Return same as before] ...

  # 6. Post-Imputation Validation
  missing_check <- colSums(is.na(data_copy[vars_to_keep]))
  if (any(missing_check > 0)) {
    failed_vars <- names(missing_check[missing_check > 0])
    stop(paste0(
      "Imputation Validation Failed: The following variables in 'vars_to_keep' still contain missing values: ",
      paste(failed_vars, collapse = ", ")
    ))
  }

  final_data_copy <- data_copy %>%
    select(all_of(c(unique_id, vars_to_keep))) %>%
    rename_with(~ paste0(pre_fix, .), .cols = all_of(vars_to_keep))

  result <- data %>%
    left_join(final_data_copy, by = unique_id) %>%
    select(-all_of(unique_id))

  return(result)
}
