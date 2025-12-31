library(testthat)

test_that("svy_unique works on standard numeric vectors", {
  x <- c(3, 1, 2, 3, 2, 1)

  # Default: sort = TRUE, na_rm = TRUE
  expect_equal(svy_unique(x), c(1, 2, 3))

  # No sort
  # unique() order usually preserves first appearance: 3, 1, 2
  expect_equal(svy_unique(x, sort = FALSE), c(3, 1, 2))
})

test_that("svy_unique handles NA values correctly", {
  x <- c(3, NA, 1, NA)

  # Default: na_rm = TRUE (NA removed)
  expect_equal(svy_unique(x), c(1, 3))

  # Keep NA, Sort = TRUE (NA at end)
  expect_equal(svy_unique(x, na_rm = FALSE), c(1, 3, NA))

  # Keep NA, Sort = FALSE (Order preserved: 3, NA, 1)
  expect_equal(svy_unique(x, na_rm = FALSE, sort = FALSE), c(3, NA, 1))
})

test_that("svy_unique works on character vectors", {
  x <- c("b", "a", "b", "c")

  expect_equal(svy_unique(x), c("a", "b", "c"))
  expect_equal(svy_unique(x, sort = FALSE), c("b", "a", "c"))
})

test_that("svy_unique works on factors", {
  # Factor levels order: "z", "a" (z=1, a=2)
  # Sorting a factor sorts by its integer codes (levels), not the labels alphabetical order
  f <- factor(c("a", "z", "a"), levels = c("z", "a"))

  # unique() returns factors
  res <- svy_unique(f)
  expect_s3_class(res, "factor")

  # sort(f) should be "z", "a" because "z" is level 1
  expect_equal(res, factor(c("z", "a"), levels = c("z", "a")))
})
