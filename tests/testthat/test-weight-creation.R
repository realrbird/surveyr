# Load internal data for testing
data(survey_df)
data(target_list)

# Constants for test validation
N_SAMPLE <- 2000
ORIGINAL_DEFF <- 1.010107

# --- Test 1: svy_rake() Functionality ---

test_that("svy_rake creates valid weights and hits targets", {

  # Prepare data: Use base_weight = 1 (unweighted start) for raking
  # This provides a clean start for the raking algorithm.
  df_unweighted <- survey_df

  # Run Raking
  raked_weights <- svy_rake(df_unweighted, target_list, base_weight = 1, print_output = FALSE)

  # Check 1: Return Type and Length
  expect_type(raked_weights, "double")
  expect_length(raked_weights, N_SAMPLE)

  # Check 2: Functional Outcome (Target Achievement)
  # Raking should make target differences extremely close to zero.
  df_raked <- df_unweighted %>%
    dplyr::mutate(RAKE_WEIGHT = raked_weights)

  diag_comps <- svy_comps(df_raked, target_list, wt_var = "RAKE_WEIGHT")

  # Sum of absolute differences in final raked weights should be very small (near 0)
  abs_diff_sum <- sum(abs(diag_comps$TARGET_WT_DIFF))

  # Raking is expected to be highly accurate. Checking for sum of differences < 0.01%
  expect_lt(abs_diff_sum, 0.01)
})

test_that("svy_rake handles non-existent base_weight and errors gracefully", {

  # Test non-existent base_weight (should now issue a warning and fall back to 1)
  expect_warning(
    raked_weights <- svy_rake(survey_df, target_list, base_weight = "NON_EXISTENT"),
    "Weight column 'NON_EXISTENT' not found in `df`. Using base weight of 1."
  )
  expect_length(raked_weights, N_SAMPLE)

  # Test error when targets are mis-specified (using chk_target_structure)
  bad_targets <- target_list
  bad_targets$age_group$age_group <- as.character(bad_targets$age_group$age_group) # Non-factor
  expect_error(svy_rake(survey_df, bad_targets), "must be a factor")

  # Test passing non-numeric base weight column
  expect_error(svy_rake(survey_df, target_list, base_weight = "age_group"), "must be a numeric vector")
})

# --- Test 2: svy_trim() Functionality ---

test_that("svy_trim returns valid weights and reduces variance", {

  # Use existing WEIGHT column for trimming
  wt_var_name <- "WEIGHT"

  # Run Trimming at 5th and 95th percentiles
  trimmed_weights <- svy_trim(
    df = survey_df,
    wt_var = wt_var_name,
    lower_quantile = 0.05,
    upper_quantile = 0.95,
    print_output = FALSE
  )

  # Check 1: Return Type and Length
  expect_type(trimmed_weights, "double")
  expect_length(trimmed_weights, N_SAMPLE)

  # Check 2: Functional Outcome (Max and Min Reduced)
  original_max <- max(survey_df$WEIGHT)
  trimmed_max <- max(trimmed_weights)

  original_min <- min(survey_df$WEIGHT)
  trimmed_min <- min(trimmed_weights)

  # Check that the max weight has been reduced by trimming
  expect_true(trimmed_max < original_max)
  # Check that the min weight has been increased by trimming (since the lower quantile is > 0)
  expect_true(trimmed_min > original_min)

  # Check 3: DEFF Improvement
  # Trimming should result in an improved (lower) DEFF, which means higher ESS.
  df_trimmed <- survey_df %>%
    dplyr::mutate(TRIM_WEIGHT = trimmed_weights)

  trimmed_stats <- svy_stats(df_trimmed$TRIM_WEIGHT)

  # ESS should be greater than the original ESS, meaning DEFF is closer to 1
  expect_true(trimmed_stats$deff < ORIGINAL_DEFF)

})

test_that("svy_trim handles invalid input and bounds", {

  # Test non-existent weight column
  expect_error(svy_trim(survey_df, wt_var = "NON_EXISTENT"), "column name present in `df`")

  # Test invalid quantile values
  expect_error(svy_trim(survey_df, "WEIGHT", lower_quantile = 1.2), "Quantiles must be numeric")
  expect_error(svy_trim(survey_df, "WEIGHT", lower_quantile = 0.9, upper_quantile = 0.5), "lower_quantile must be less than upper_quantile")
})
