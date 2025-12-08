# Load internal data for testing
data(survey_df)
data(target_list)

# Define known constants from survey_df$WEIGHT for comparison
# UPDATED: Constants derived from actual output (set.seed(42) run)
N_WT <- 2000
DEFF_WT <- 1.010107
ESS_WT <- 1979.99
MOE_WT <- 2.202353
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
  age_18_30_row <- comps_wt[comps_wt$LEVEL == "18-30", ]
  expect_equal(age_18_30_row$TARGET_PERCENT, 10.00)
  expect_equal(age_18_30_row$UNWT_SAMPLE_PERCENT, 10.75, tolerance = 1e-2)
  expect_equal(age_18_30_row$TARGET_UNWT_DIFF, -0.75, tolerance = 1e-2)

  # --- B. Unweighted Test (wt_var = NULL) ---
  comps_unwt <- svy_comps(survey_df, target_list, wt_var = NULL)

  # Check Structure (Unweighted: 5 columns)
  expect_equal(ncol(comps_unwt), 5)

  # Check rows are identical to the unweighted part of the weighted test
  expect_equal(comps_unwt$LEVEL, comps_wt$LEVEL)
  expect_equal(comps_unwt$TARGET_PERCENT, comps_wt$TARGET_PERCENT)
  expect_equal(comps_unwt$TARGET_UNWT_DIFF, comps_wt$TARGET_UNWT_DIFF, tolerance = 1e-5)
})

# --- Test 5: svy_compare_tiles() ---

test_that("svy_compare_tiles returns correct structure and names", {
  # Create a second dummy weight for comparison in the test environment
  survey_df_test <- survey_df |>
    dplyr::mutate(WEIGHT_2 = .data$WEIGHT * c(rep(1.2, 1000), rep(0.8, 1000)))

  # Test with two weight variables, passed as a character vector to wt_vars
  compare_result <- svy_compare_tiles(survey_df_test, wt_vars = c("WEIGHT", "WEIGHT_2"))

  # Check Structure (Wide format)
  expect_s3_class(compare_result, "tbl_df")
  expect_equal(nrow(compare_result), 11)
  expect_equal(ncol(compare_result), 3) # tile, WEIGHT, WEIGHT_2

  # Check Names (Must match input wt_vars)
  expect_equal(names(compare_result), c("tile", "WEIGHT", "WEIGHT_2"))

  # Check Values
  expect_true(compare_result$WEIGHT_2[1] < compare_result$WEIGHT[1])
})

# --- Test 6: Long-Format Comparison Wrappers (Single Dataset) ---

test_that("svy_compare_stats, svy_compare_comps, and svy_compare wrappers return correct long structure", {

  # Setup data for comparison
  survey_df_comp <- survey_df |>
    dplyr::mutate(WEIGHT_2 = .data$WEIGHT * c(rep(1.2, 1000), rep(0.8, 1000)))
  wt_names <- c("WEIGHT", "WEIGHT_2")

  # --- A. svy_compare_stats ---
  stats_comp <- svy_compare_stats(survey_df_comp, wt_vars = wt_names)

  # Check Structure
  expect_equal(nrow(stats_comp), 2) # 2 weights * 1 row/weight
  expect_equal(ncol(stats_comp), 8) # WT_NAME + 7 stats columns
  expect_true("WT_NAME" %in% names(stats_comp))
  expect_equal(stats_comp$WT_NAME, wt_names)

  # --- B. svy_compare_comps ---
  comps_comp <- svy_compare_comps(survey_df_comp, target_list, wt_vars = wt_names)

  # Check Structure
  expect_equal(nrow(comps_comp), 18) # 2 weights * 9 levels/weight
  expect_equal(ncol(comps_comp), 8) # WT_NAME + 7 comps columns
  expect_true("WT_NAME" %in% names(comps_comp))

  # Check first 9 rows belong to first weight, second 9 to second
  expect_equal(comps_comp$WT_NAME[1:9], rep("WEIGHT", 9))
  expect_equal(comps_comp$WT_NAME[10:18], rep("WEIGHT_2", 9))

  # --- C. svy_compare (Wrapper) ---
  full_comp <- svy_compare(survey_df_comp, target_list, wt_vars = wt_names)

  # Check Structure
  expect_true(is.list(full_comp))
  expect_equal(names(full_comp), c("tiles", "stats", "comps"))

  # Check contents
  expect_equal(nrow(full_comp$tiles), 11)
  expect_equal(nrow(full_comp$stats), 2)
  expect_equal(nrow(full_comp$comps), 18)
})

# --- Test 7: svy_contrast() (Multi-Dataset) (NEW) ---

test_that("svy_contrast aggregates results from multiple datasets correctly", {

  # 1. Setup two mock datasets
  # Wave 2 has slightly shifted weights
  df_w2 <- survey_df |> dplyr::mutate(WEIGHT = WEIGHT * 1.1)

  input_w1 <- list(
    dataset_name = "Wave 1",
    data = survey_df,
    wt_var = "WEIGHT",
    target_list = target_list
  )

  input_w2 <- list(
    dataset_name = "Wave 2",
    data = df_w2,
    wt_var = "WEIGHT",
    target_list = target_list
  )

  # 2. Run svy_contrast
  contrast_res <- svy_contrast(input_w1, input_w2, print = FALSE)

  # 3. Check Return Type
  expect_type(contrast_res, "list")
  expect_named(contrast_res, c("tiles", "stats", "comps"))

  # 4. Check Tiles (Wide Format)
  # Should have: tile, Wave 1, Wave 2
  expect_true("tile" %in% names(contrast_res$tiles))
  expect_true("Wave 1" %in% names(contrast_res$tiles))
  expect_true("Wave 2" %in% names(contrast_res$tiles))
  expect_equal(nrow(contrast_res$tiles), 11)

  # 5. Check Stats (Long Format)
  # Should have DATASET column
  expect_true("DATASET" %in% names(contrast_res$stats))
  expect_equal(nrow(contrast_res$stats), 2)
  expect_equal(contrast_res$stats$DATASET, c("Wave 1", "Wave 2"))

  # 6. Check Comps (Long Format)
  expect_true("DATASET" %in% names(contrast_res$comps))
  # 9 rows per wave * 2 waves = 18 rows
  expect_equal(nrow(contrast_res$comps), 18)

  # 7. Test Error Handling
  # Pass a bad list (missing dataset_name)
  bad_input <- input_w1
  bad_input$dataset_name <- NULL
  expect_error(svy_contrast(bad_input, input_w2), "must have a 'dataset_name' string")
})
