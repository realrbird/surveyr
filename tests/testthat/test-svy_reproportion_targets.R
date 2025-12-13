library(testthat)
library(dplyr)
library(tibble)

# -------------------------------------------------------------------
# Unit Tests for svy_reproportion_targets & svy_reproportion_target
# -------------------------------------------------------------------

test_that("svy_reproportion_target (helper) works correctly", {
  # 1. Setup Data: 10 rows, 1 NA (10% missing)
  df <- tibble(
    sex = factor(c(rep("M", 9), NA))
  )

  # 2. Setup Target: 'Freq' sums to 100
  target_df <- tibble(
    sex = factor("M"),
    Freq = 100
  )

  # 3. Run helper
  res <- svy_reproportion_target(target_df, "sex", df, missing_cat = "(Missing)")

  # 4. Verify Structure
  expect_true("(Missing)" %in% res$sex)
  expect_true(is.factor(res$sex))

  # 5. Verify Math (Sums to 100)
  expect_equal(sum(res$Freq), 100)

  # Logic check:
  # NA prop in data = 0.1
  # Target Sum = 100
  # Added Value (Raw) = 0.1 * 100 = 10
  # New Total (Raw) = 110
  # Expected Missing % = (10 / 110) * 100 = 9.0909...

  missing_row <- res |> filter(sex == "(Missing)")
  expected_val <- (10/110) * 100
  expect_equal(missing_row$Freq, expected_val)
})

test_that("svy_reproportion_targets (wrapper) processes list correctly", {
  # 1. Setup Data with multiple variables
  # FIX: Ensure both columns are length 10 to satisfy tibble requirements
  df <- tibble(
    sex = factor(c(rep("M", 9), NA)),             # 10% NA (1/10)
    region = factor(c(rep("North", 8), NA, NA))   # 20% NA (2/10) - matches math below
  )

  # 2. Setup Targets List
  targets <- list(
    sex = tibble(sex = factor("M"), Freq = 100),
    region = tibble(region = factor("North"), Freq = 100)
  )

  # 3. Run wrapper
  res_list <- svy_reproportion_targets(df, targets, missing_cat = "Missing")

  # 4. Verify List Structure
  expect_equal(length(res_list), 2)
  expect_named(res_list, c("sex", "region"))

  # 5. Verify individual contents
  expect_true("Missing" %in% res_list$sex$sex)
  expect_true("Missing" %in% res_list$region$region)

  # Check Region Math:
  # NA prop = 0.2 (2/10). Raw Total = 120. Missing % = (20/120)*100 = 16.666...
  expect_equal(sum(res_list$region$Freq), 100)
  expect_equal(res_list$region$Freq[res_list$region$region == "Missing"], (20/120)*100)
})

test_that("svy_reproportion_target handles input validation", {
  df <- tibble(sex = factor("M"))

  # Error: Variable not in data
  target_ok <- tibble(sex = "M", Freq = 100)
  expect_error(
    svy_reproportion_target(target_ok, "bad_var", df),
    "Target variable 'bad_var' is not present"
  )

  # Error: Missing 'Freq' column
  target_bad_col <- tibble(sex = "M", pct = 100) # Wrong name 'pct'
  expect_error(
    svy_reproportion_target(target_bad_col, "sex", df),
    "must have a numeric column named 'Freq'"
  )
})

test_that("svy_reproportion_target leaves data unchanged if no NAs", {
  df <- tibble(sex = factor(c("M", "F"))) # 0% NA
  target_df <- tibble(sex = factor(c("M", "F")), Freq = c(50, 50))

  res <- svy_reproportion_target(target_df, "sex", df)

  # Should be identical to input
  expect_equal(res, target_df)
  expect_false("Missing" %in% res$sex)
})
