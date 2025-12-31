library(testthat)
library(dplyr)
library(tibble)

# -------------------------------------------------------------------
# Unit Tests for svy_count
# -------------------------------------------------------------------

test_that("svy_count works with standard vectors", {
  df <- tibble(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "Y", "X", "Y")
  )

  res <- svy_count(df, "g1")
  expect_equal(res$n, c(2, 2))
  expect_equal(res$Freq, c(50, 50))

  # Test multiple vars
  res_multi <- svy_count(df, c("g1", "g2"))
  expect_equal(nrow(res_multi), 4)
  expect_equal(res_multi$Freq, rep(25, 4))
})

test_that("svy_count .drop=FALSE works for factors (including empty levels)", {
  df <- tibble(
    f = factor(c("A", "B"), levels = c("A", "B", "C"))
  )

  # .drop = FALSE is default in svy_count
  res <- svy_count(df, "f")

  # Should have 3 rows (A, B, C), with C having n=0
  expect_equal(nrow(res), 3)
  expect_true("C" %in% res$f)
  expect_equal(res$n[res$f == "C"], 0)
  expect_equal(res$Freq[res$f == "C"], 0)
})

test_that("svy_count handles haven::labelled variables correctly", {
  # Skip if haven isn't installed
  if (!requireNamespace("haven", quietly = TRUE)) {
    skip("haven package not available")
  }

  # Create a labelled variable: 1=Yes, 2=No, 3=Maybe. Only 1 and 2 present.
  lbl_var <- haven::labelled(
    x = c(1, 2, 1, 2),
    labels = c("Yes" = 1, "No" = 2, "Maybe" = 3)
  )

  df <- tibble(q1 = lbl_var)

  # svy_count should convert to factor (levels="both"), showing labels
  # Expect a message about conversion
  expect_message(res <- svy_count(df, "q1"), "converted to factors")

  # Check result structure
  # Levels should include "[1] Yes", "[2] No", "[3] Maybe"
  # Since .drop=FALSE by default, we expect 3 rows
  expect_equal(nrow(res), 3)

  # Check that the factor levels look correct (haven default format)
  levels_found <- as.character(res$q1)
  expect_true(any(grepl("\\[1\\] Yes", levels_found)))
  expect_true(any(grepl("\\[3\\] Maybe", levels_found)))

  # Check zero count for Maybe
  maybe_row <- res |> filter(grepl("Maybe", q1))
  expect_equal(maybe_row$n, 0)
})

test_that("svy_count passes extra arguments (weights)", {
  df <- tibble(
    g = c("A", "B"),
    w = c(10, 20)
  )

  # Pass weight variable 'w'
  res <- svy_count(df, "g", wt = w)

  expect_equal(res$n, c(10, 20))
  expect_equal(res$Freq, c(10/30*100, 20/30*100))
})

test_that("svy_count handles na_rm correctly", {
  df <- tibble(
    g = c("A", "B", NA, "A")
  )

  # na_rm = TRUE (default)
  # Total n = 3 (A=2, B=1). NA row removed.
  res_rm <- svy_count(df, "g", na_rm = TRUE)
  expect_equal(sum(res_rm$n), 3)
  expect_false(any(is.na(res_rm$g)))

  # na_rm = FALSE
  # Total n = 4. NA included.
  res_keep <- svy_count(df, "g", na_rm = FALSE)
  expect_equal(sum(res_keep$n), 4)
  expect_true(any(is.na(res_keep$g)))
  # Percentage check: A should be 2/4 = 50%
  a_row <- res_keep |> filter(g == "A")
  expect_equal(a_row$Freq, 50)
})

# -------------------------------------------------------------------
# Unit Tests for svy_counts (wrapper)
# -------------------------------------------------------------------

test_that("svy_counts maps over a list of variables", {
  df <- tibble(
    a = c("1", "2"),
    b = c("3", "4")
  )

  var_list <- list(c("a"), c("b"))

  res_list <- svy_counts(df, var_list)

  expect_type(res_list, "list")
  expect_length(res_list, 2)
  expect_equal(names(res_list[[1]])[1], "a")
  expect_equal(names(res_list[[2]])[1], "b")
})
