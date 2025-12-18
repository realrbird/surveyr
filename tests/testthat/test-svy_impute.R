library(testthat)
library(dplyr)

# -------------------------------------------------------------------
# Unit Tests for svy_impute
# -------------------------------------------------------------------

test_that("Input validation catches invalid arguments", {
  df <- data.frame(a = 1:10, b = 1:10)

  expect_error(
    svy_impute(data = "not a dataframe", vars_to_impute = "a", vars_to_keep = "b"),
    "must be a data.frame"
  )

  expect_error(
    svy_impute(df, vars_to_impute = numeric(0), vars_to_keep = "b"),
    "must be a character vector"
  )

  expect_error(
    svy_impute(df, "a", "b", pre_fix = c("imp_", "other_")),
    "single character string"
  )
})

test_that("Variable existence and naming conflict checks work", {
  df <- data.frame(var1 = 1:10, var2 = 1:10)

  expect_error(
    svy_impute(df, vars_to_impute = "ghost_var", vars_to_keep = "var2"),
    "variables are not in the dataset"
  )

  df_conflict <- data.frame(var1 = 1:10, var2 = 1:10, imp_var2 = 1:10)

  expect_error(
    svy_impute(df_conflict, vars_to_impute = "var1", vars_to_keep = "var2", pre_fix = "imp_"),
    "result variables already exist"
  )
})

test_that("Correlation check triggers warning", {
  df_corr <- data.frame(
    id = 1:20,
    x = 1:20,
    y = 1:20
  )

  all_warnings <- testthat::capture_warnings(
    try(
      svy_impute(df_corr, "x", "y", correlation_threshold = 0.95),
      silent = TRUE
    )
  )

  expect_true(
    any(grepl("High correlation detected", all_warnings)),
    label = "The expected correlation warning was not triggered."
  )
})

test_that("Full imputation flow returns correct structure", {
  # FIX: We must ATTACH the package so mice can find 'mice.impute.ranger'
  if (!suppressWarnings(require("pewmethods", quietly = TRUE))) {
    skip("pewmethods package not installed, skipping integration test.")
  }

  set.seed(123)
  df <- data.frame(
    age = rnorm(50, mean = 50, sd = 10),
    income = rnorm(50, mean = 50000, sd = 10000),
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
