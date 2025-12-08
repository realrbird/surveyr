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
  df_raked <- df_unweighted |>
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
  df_trimmed <- survey_df |>
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

# --- Test 3: svy_rake_with_trim() Functionality ---

test_that("svy_rake_with_trim iterates and respects strict caps", {

  # Run iterative raking with a TIGHT cap to force trimming activity.
  final_weights <- svy_rake_with_trim(
    df = survey_df,
    targets = target_list,
    base_weight = 1,
    min_weight = 0.5,
    max_weight = 1.05, # Strict cap
    max_iter = 3,      # Short loop for testing
    print_output = FALSE
  )

  # Check Caps Respected
  expect_true(max(final_weights) <= 1.05 + 1e-5)
  expect_true(min(final_weights) >= 0.5 - 1e-5)

  # Check Target Alignment (Sum of abs differences should be reasonably low)
  df_iter <- survey_df |> dplyr::mutate(ITER_WEIGHT = final_weights)
  diag_iter <- svy_comps(df_iter, target_list, wt_var = "ITER_WEIGHT")

  abs_diff_sum <- sum(abs(diag_iter$TARGET_WT_DIFF))
  expect_lt(abs_diff_sum, 1.0)
})

# --- Test 4: svy_rake_optimize() (NEW) ---

test_that("svy_rake_optimize finds solution for max DEFF and Ratio", {

  # 1. Create a dataset with high variance to simulate a 'bad' rake
  # We introduce a random multiplier to make weights messy (Max ratio ~100)
  set.seed(123)
  df_messy <- survey_df |>
    dplyr::mutate(MESSY_BASE = stats::runif(dplyr::n(), 0.1, 10))

  # 2. Run Optimizer with a MAX DEFF constraint (Ceiling, not exact target)
  # The messy weights will have high DEFF. We want to force it down to 1.5
  optimized_weights_deff <- svy_rake_optimize(
    df = df_messy,
    targets = target_list,
    base_weight = "MESSY_BASE",
    max_deff = 1.5,      # RENAMED from target_deff
    max_weight_start = 5,
    max_weight_min = 1.1,
    step = 0.5
    # REMOVED print_output = FALSE because it conflicts with internal logic
  )

  # Verify DEFF goal met (should be <= 1.5 + tolerance)
  stats_deff <- svy_stats(optimized_weights_deff)
  expect_lte(stats_deff$deff, 1.55)

  # 3. Run Optimizer with a MAX Ratio constraint (Ceiling, not exact target)
  # We want to force the ratio (Max/Min) down to 20 or less
  optimized_weights_ratio <- svy_rake_optimize(
    df = df_messy,
    targets = target_list,
    base_weight = "MESSY_BASE",
    max_wt_ratio = 20,   # RENAMED from target_wt_ratio
    max_weight_start = 5,
    max_weight_min = 1.1,
    step = 0.5
    # REMOVED print_output = FALSE because it conflicts with internal logic
  )

  # Verify Ratio goal met
  stats_ratio <- svy_stats(optimized_weights_ratio)
  expect_lte(stats_ratio$wt_ratio, 20.5)
})
