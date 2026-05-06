library(testthat)
library(dplyr)
library(tibble)

test_that("svy_reproportion_target corrects the math exactly", {
  # 100 rows total: 25 NAs -> 25% missing
  df <- tibble(
    sex_group = c(rep("Male", 40), rep("Female", 35), rep(NA, 25))
  )

  # Target sum is 100
  target <- tibble(
    sex_group = factor(c("Male", "Female"), levels = c("Male", "Female")),
    Freq = c(49, 51)
  )

  res <- svy_reproportion_target(df, target, "sex_group")

  # Check Total Sum (Should remain exactly 100)
  expect_equal(sum(res$Freq), 100)

  # Check Missing Target (Should be exactly 25% of 100 = 25)
  missing_freq <- res$Freq[res$sex_group == "Missing"]
  expect_equal(missing_freq, 25)

  # Check Male/Female adjustments
  # Male: 49 * 0.75 = 36.75
  # Female: 51 * 0.75 = 38.25
  expect_equal(res$Freq[res$sex_group == "Male"], 36.75)
  expect_equal(res$Freq[res$sex_group == "Female"], 38.25)
})

test_that("svy_reproportion_target preserves factor level order", {
  # 1 NA out of 10 -> 10% missing
  df <- tibble(
    sex_group = c(rep("Male", 4), rep("Female", 5), NA)
  )

  # Explicit order: Female first, then Male
  target <- tibble(
    sex_group = factor(c("Female", "Male"), levels = c("Female", "Male")),
    Freq = c(60, 40)
  )

  res <- svy_reproportion_target(df, target, "sex_group")

  # The levels should be Female, Male, Missing (Missing added to the end)
  expect_equal(levels(res$sex_group), c("Female", "Male", "Missing"))
})

test_that("svy_reproportion_target returns unmodified target if 0 missing cases", {
  df <- tibble(sex_group = c("Male", "Female")) # No NAs

  target <- tibble(
    sex_group = factor(c("Male", "Female"), levels = c("Male", "Female")),
    Freq = c(50, 50)
  )

  res <- svy_reproportion_target(df, target, "sex_group")

  # Should be perfectly identical
  expect_equal(res, target)
  expect_equal(levels(res$sex_group), c("Male", "Female"))
})

test_that("svy_reproportion_targets wrapper works over lists", {
  df <- tibble(
    v1 = c("A", NA, "A", "A"), # 25% missing
    v2 = c("X", "Y", "X", "Y") # 0% missing
  )

  t_list <- list(
    v1 = tibble(v1 = factor("A"), Freq = 100),
    v2 = tibble(v2 = factor(c("X", "Y")), Freq = c(50, 50))
  )

  res_list <- svy_reproportion_targets(df, t_list)

  # v1 should have missing appended and adjusted to 25
  expect_equal(nrow(res_list$v1), 2)
  expect_equal(res_list$v1$Freq[res_list$v1$v1 == "Missing"], 25)

  # v2 should be untouched
  expect_equal(nrow(res_list$v2), 2)
  expect_equal(levels(res_list$v2$v2), c("X", "Y"))
})
