library(testthat)

# -------------------------------------------------------------------
# Unit Tests for svy_projection
# -------------------------------------------------------------------

test_that("svy_projection validates inputs correctly", {
  # 1. Non-numeric weights
  expect_error(svy_projection(c("a", "b"), 100), "must be numeric")

  # 2. Missing values in weights
  expect_error(svy_projection(c(1, NA, 2), 100), "cannot contain missing values")

  # 3. Invalid population argument (string or vector length > 1)
  expect_error(svy_projection(c(1, 2), "100"), "must be a single numeric value")
  expect_error(svy_projection(c(1, 2), c(100, 200)), "must be a single numeric value")

  # 4. Weights sum to zero
  expect_error(svy_projection(c(0, 0, 0), 100), "Sum of input weights is 0")
})

test_that("svy_projection correctly projects weights", {
  # Setup simple data
  weights <- c(10, 20, 10) # Sum = 40
  target_pop <- 1000       # Target Sum

  # Execute
  result <- svy_projection(weights, target_pop)

  # 1. Main Check: Sum of result equals target population
  # expect_equal allows for tiny floating point differences, which is safer than ==
  expect_equal(sum(result), target_pop)

  # 2. Check individual calculations
  # Scale factor should be 1000 / 40 = 25
  # Expected: 250, 500, 250
  expect_equal(result, c(250, 500, 250))
})

test_that("svy_projection handles edge cases", {
  # Single weight
  val <- 5
  pop <- 500
  expect_equal(svy_projection(val, pop), 500)

  # Large numbers
  weights <- c(1e6, 2e6)
  pop <- 6e6
  result <- svy_projection(weights, pop)
  expect_equal(sum(result), pop)
  expect_equal(result, c(2e6, 4e6))
})
