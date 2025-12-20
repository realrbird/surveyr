library(testthat)
library(dplyr)

test_that("svy_mode calculates simple mode correctly", {
  # Numeric
  expect_equal(svy_mode(c(1, 2, 2, 3), print_output = FALSE), 2)

  # Character
  expect_equal(svy_mode(c("a", "b", "b", "c"), print_output = FALSE), "b")

  # Logical
  expect_equal(svy_mode(c(TRUE, FALSE, TRUE), print_output = FALSE), TRUE)

  # Factor
  f <- factor(c("low", "high", "high", "medium"), levels = c("low", "medium", "high"))
  expect_equal(svy_mode(f, print_output = FALSE), factor("high", levels = levels(f)))
})

test_that("svy_mode implicitly handles NAs", {
  x <- c(1, 2, 2, NA, NA, NA)

  # Even with more NAs than data, the mode of the *data* is 2
  expect_equal(svy_mode(x, print_output = FALSE), 2)

  # All NAs returns typed NA
  expect_true(is.na(svy_mode(c(NA, NA), print_output = FALSE)))

  # Check type preservation for empty/NA inputs
  expect_true(is.numeric(svy_mode(c(NA_real_), print_output = FALSE)))
  expect_true(is.character(svy_mode(c(NA_character_), print_output = FALSE)))
})

test_that("svy_mode handles tie_breaking (always min)", {
  # Data: 1 appears twice, 2 appears twice.
  # Min logic dictates 1 is returned (numerically smaller).
  x <- c(2, 1, 2, 1)
  expect_equal(svy_mode(x, print_output = FALSE), 1)

  # Character ties: "a" vs "b" -> "a" is returned (alphabetically smaller)
  char_x <- c("b", "a", "b", "a")
  expect_equal(svy_mode(char_x, print_output = FALSE), "a")
})

test_that("svy_mode handles factor ties correctly (always min)", {
  # Factor with levels: A, B
  # Data: B, A. Tie.
  # "A" is the first level / alphabetically first depending on ordered status
  # Default sort on factors sorts by the integer codes (levels).
  f <- factor(c("B", "A"), levels = c("A", "B"))

  expect_equal(svy_mode(f, print_output = FALSE), factor("A", levels = levels(f)))

  # Test with non-alphabetical levels to confirm it uses level order
  f2 <- factor(c("Z", "A"), levels = c("Z", "A"))
  # Here "Z" is level 1, "A" is level 2.
  # Since it's a tie, 'sort' should pick the one with the lower integer code ("Z").
  expect_equal(svy_mode(f2, print_output = FALSE), factor("Z", levels = levels(f2)))
})

test_that("svy_mode output printing works", {
  x <- c(1, 2, NA, NA) # 4 elements, 2 missing (50%)

  # Test message content
  expect_message(
    svy_mode(x, print_output = TRUE),
    "Missing cases: 2 \\(50.00%\\)"
  )

  # Test silence when FALSE
  expect_silent(svy_mode(x, print_output = FALSE))
})
