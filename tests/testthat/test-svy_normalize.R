library(testthat)

# -------------------------------------------------------------------
# Unit Tests for svy_normalize
# -------------------------------------------------------------------

test_that("svy_normalize validates inputs correctly", {
  # 1. Non-numeric weights
  expect_error(svy_normalize(c("a", "b")), "must be numeric")

  # 2. Missing values in weights
  expect_error(svy_normalize(c(1, NA, 2)), "cannot contain missing values")

  # 3. Weights sum to zero
  expect_error(svy_normalize(c(0, 0, 0)), "Sum of input weights is 0")
})

test_that("svy_normalize correctly normalizes weights", {
  # Setup simple data
  # Weights: 10, 30. Sum = 40. N = 2.
  weights <- c(10, 30)

  # Execute
  result <- svy_normalize(weights)

  # 1. Main Check: Sum of result equals sample size (length of vector)
  expect_equal(sum(result), length(weights))

  # 2. Check individual calculations
  # Scaling factor: sample_size / sum_weights = 2 / 40 = 0.05
  # Expected: 10 * 0.05 = 0.5
  #           30 * 0.05 = 1.5
  expect_equal(result, c(0.5, 1.5))
})

test_that("svy_normalize handles edge cases", {
  # Single weight
  # If there is only 1 person, their weight must normalize to 1
  val <- 5
  expect_equal(svy_normalize(val), 1)

  # Large numbers
  weights <- c(100, 100, 100, 100) # Sum=400, N=4
  result <- svy_normalize(weights)

  expect_equal(sum(result), 4)
  expect_equal(result, c(1, 1, 1, 1))
})
