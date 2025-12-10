library(testthat)
library(dplyr)

# -------------------------------------------------------------------
# Unit Tests for svy_impute
# -------------------------------------------------------------------

test_that("Input validation catches invalid arguments", {
  # Setup dummy data
  df <- data.frame(a = 1:10, b = 1:10)

  # Test 1: Data must be a data.frame
  expect_error(
    svy_impute(data = "not a dataframe", vars_to_impute = "a", vars_to_keep = "b"),
    "must be a data.frame"
  )

  # Test 2: Character vector checks
  expect_error(
    svy_impute(df, vars_to_impute = numeric(0), vars_to_keep = "b"),
    "must be a character vector"
  )

  # Test 3: Pre-fix validation
  expect_error(
    svy_impute(df, "a", "b", pre_fix = c("imp_", "other_")),
    "single character string"
  )
})

test_that("Variable existence and naming conflict checks work", {
  df <- data.frame(var1 = 1:10, var2 = 1:10)

  # Test 1: Requesting a variable that doesn't exist
  expect_error(
    svy_impute(df, vars_to_impute = "ghost_var", vars_to_keep = "var2"),
    "variables are not in the dataset"
  )

  # Test 2: Naming conflict (result column already exists)
  df_conflict <- data.frame(var1 = 1:10, var2 = 1:10, imp_var2 = 1:10)

  expect_error(
    svy_impute(df_conflict, vars_to_impute = "var1", vars_to_keep = "var2", pre_fix = "imp_"),
    "result variables already exist"
  )
})

test_that("Correlation check triggers warning", {
  # Create data with perfect correlation
  df_corr <- data.frame(
    id = 1:20,
    x = 1:20,
    y = 1:20 # Perfect correlation with x
  )

  # UPDATED TEST APPROACH:
  # Because the data is perfectly correlated, the imputation engine (mice)
  # called later in the function will likely throw a SECOND warning (e.g., about singularities).
  # To avoid a "WARN" in the test report from that extra noise, we capture ALL warnings
  # and verify that our specific message is present.

  # capture_warnings runs the code and returns a character vector of all warnings
  all_warnings <- testthat::capture_warnings(
    try(
      svy_impute(df_corr, "x", "y", correlation_threshold = 0.95),
      silent = TRUE
    )
  )

  # Check if ANY of the captured warnings contain our message
  expect_true(
    any(grepl("High correlation detected", all_warnings)),
    label = "The expected correlation warning was not triggered."
  )
})

test_that("Full imputation flow returns correct structure", {
  if (!requireNamespace("pewmethods", quietly = TRUE)) {
    skip("pewmethods package not installed, skipping integration test.")
  }

  set.seed(123)
  df <- data.frame(
    age = rnorm(50, mean = 50, sd = 10),
    income = rnorm(50, mean = 50000, sd = 10000),
    # Explicitly use factor to ensure imputation engine picks it up
    category = factor(sample(c("A", "B", NA), 50, replace = TRUE))
  )

  result <- svy_impute(
    data = df,
    vars_to_impute = c("age", "income"),
    vars_to_keep = "category",
    pre_fix = "imp_",
    seed = 500
  )

  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df) + 1)
  expect_true("imp_category" %in% names(result))
  expect_equal(sum(is.na(result$imp_category)), 0)
})
