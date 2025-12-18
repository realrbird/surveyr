library(testthat)
library(dplyr)
library(tibble)

test_that("svy_collapse correctly combines levels in data and targets", {

  # 1. Setup Data
  # 'C' is a small category we want to combine with 'B'
  df <- tibble(
    id = 1:10,
    cat = factor(c(rep("A", 5), rep("B", 3), rep("C", 2)))
  )

  # 2. Setup Targets
  # A=50, B=30, C=20
  targets <- list(
    cat = tibble(cat = factor(c("A", "B", "C")), Freq = c(50, 30, 20))
  )

  # 3. Define Mapping
  # Combine B and C into "B_plus_C"
  mapping <- list(
    "B_plus_C" = c("B", "C")
  )

  # 4. Run Function
  res <- svy_collapse(df, targets, "cat", mapping)

  # 5. Check Data Modifications
  # Should now only have levels "A" and "B_plus_C"
  expect_true("B_plus_C" %in% levels(res$data$cat))
  expect_false("C" %in% levels(res$data$cat))
  expect_equal(as.character(res$data$cat[10]), "B_plus_C") # Was 'C'

  # 6. Check Target Modifications
  target_res <- res$targets$cat

  # Should be 2 rows now
  expect_equal(nrow(target_res), 2)

  # Check frequency summation: B(30) + C(20) should be 50
  new_freq <- target_res |> filter(cat == "B_plus_C") |> pull(Freq)
  expect_equal(new_freq, 50)

  # A should remain 50
  old_freq <- target_res |> filter(cat == "A") |> pull(Freq)
  expect_equal(old_freq, 50)
})

test_that("svy_collapse validation works", {
  # FIX: Data must be factors to satisfy chk_target_structure
  df <- tibble(x = factor("A"))
  targets <- list(x = tibble(x = factor("A"), Freq = 100))

  # Test 1: Variable not in targets list
  # Logic: chk_target_structure passes (x matches x).
  # Then it checks if "y" is in targets. It is not.
  expect_error(
    svy_collapse(df, targets, "y", list()),
    "not found in targets list"
  )

  # Test 2: Variable not in targets list (empty list)
  # Logic: An empty list fails chk_target_structure because it expects names matching data
  expect_error(
    svy_collapse(df, list(), "x", list()),
    "must be a named list"
  )

  # Test 3: Missing Freq column in targets
  # Logic: chk_target_structure should catch this before var_name check
  bad_targets <- list(x = tibble(x = factor("A"), count = 100))
  expect_error(
    svy_collapse(df, bad_targets, "x", list()),
    "must contain a column named 'Freq'"
  )
})
