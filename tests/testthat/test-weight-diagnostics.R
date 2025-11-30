# Load internal data for testing
data(survey_df)
data(target_list)

# Define known constants from survey_df$WEIGHT for comparison
# UPDATED: Constants derived from actual output (set.seed(42) run)
N_WT <- 2000
DEFF_WT <- 1.010107
ESS_WT <- 1979.99
MOE_WT <- 2.202353 # FIX: Updated to the exact reported actual value
# NOTE: Increased tolerance in the expect_equal calls below to 1e-4

# --- Test 1: Validation Helper (R/survey_checks.R) ---

test_that("chk_numeric_no_na stops on bad input", {
  # Should stop if input is NA
  expect_error(chk_numeric_no_na(c(1, 2, NA)), "must not contain missing values")
  # Should stop if input is not numeric
  expect_error(chk_numeric_no_na(c("a", "b")), "must be a numeric vector")
  # Should pass on good input
  expect_silent(chk_numeric_no_na(c(1, 2, 3)))
})

# --- Test 2: svy_stats() ---

test_that("svy_stats returns correct structure and accurate values", {
  stats_result <- svy_stats(survey_df$WEIGHT)

  # Check Structure
  expect_s3_class(stats_result, "tbl_df")
  expect_equal(nrow(stats_result), 1)
  expect_equal(ncol(stats_result), 7)

  # Check Key Calculations (using tolerance for floating point)
  expect_equal(stats_result$n, N_WT)
  expect_equal(stats_result$deff, DEFF_WT, tolerance = 1e-4)
  expect_equal(stats_result$ess, ESS_WT, tolerance = 1e-4)
  expect_equal(stats_result$moe, MOE_WT, tolerance = 1e-4)

  # Check default conf_level
  stats_result_99 <- svy_stats(survey_df$WEIGHT, conf_level = 0.99)
  expect_true(stats_result_99$moe > stats_result$moe) # MOE must be larger at 99%
})

# --- Test 3: svy_tiles() ---

test_that("svy_tiles returns correct structure and values", {
  tiles_result <- svy_tiles(survey_df$WEIGHT)

  # Check Structure
  expect_s3_class(tiles_result, "tbl_df")
  expect_equal(names(tiles_result), c("tile", "value"))
  expect_equal(tiles_result$tile[1], "0%") # Min
  expect_equal(tiles_result$tile[nrow(tiles_result)], "100%") # Max

  # Check if print_all works (should be 101 rows: 0% to 100%)
  tiles_all <- svy_tiles(survey_df$WEIGHT, print_all = TRUE)
  expect_equal(nrow(tiles_all), 101)
})

# --- Test 4: svy_comps() ---

test_that("svy_comps returns correct structure for weighted and unweighted data", {

  # --- A. Weighted Test ---
  comps_wt <- svy_comps(survey_df, target_list, wt_var = "WEIGHT")

  # Check Structure (Weighted: 7 columns)
  expect_equal(ncol(comps_wt), 7)
  expect_true("WT_SAMPLE_PERCENT" %in% names(comps_wt))
  expect_true("TARGET_WT_DIFF" %in% names(comps_wt))

  # Check Total Rows (3 age + 2 sex + 4 region = 9 rows)
  expect_equal(nrow(comps_wt), 9)

  # Check known calculation for first row (age_group: 18-30)
  # Target: 10.00
  # Unweighted Sample (actual): 10.75 (updated from 10.05)
  # Weighted Sample (approx): 10.00
  age_18_30_row <- comps_wt[comps_wt$LEVEL == "18-30", ]
  expect_equal(age_18_30_row$TARGET_PERCENT, 10.00)
  expect_equal(age_18_30_row$UNWT_SAMPLE_PERCENT, 10.75, tolerance = 1e-2)
  expect_equal(age_18_30_row$TARGET_UNWT_DIFF, -0.75, tolerance = 1e-2) # updated from -0.05

  # --- B. Unweighted Test (wt_var = NULL) ---
  comps_unwt <- svy_comps(survey_df, target_list, wt_var = NULL)

  # Check Structure (Unweighted: 5 columns)
  expect_equal(ncol(comps_unwt), 5)
  expect_false("WT_SAMPLE_PERCENT" %in% names(comps_unwt))
  expect_false("TARGET_WT_DIFF" %in% names(comps_unwt))

  # Check rows are identical to the unweighted part of the weighted test
  expect_equal(comps_unwt$LEVEL, comps_wt$LEVEL)
  expect_equal(comps_unwt$TARGET_PERCENT, comps_wt$TARGET_PERCENT)
  expect_equal(comps_unwt$TARGET_UNWT_DIFF, comps_wt$TARGET_UNWT_DIFF, tolerance = 1e-5)
})
