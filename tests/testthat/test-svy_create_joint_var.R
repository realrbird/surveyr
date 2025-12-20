library(testthat)
library(dplyr)
library(tibble)

test_that("svy_create_joint_var creates correct levels and order", {

  # 1. Setup Data
  # Age: 3 levels (18-45, 46-64, 65-99)
  # Sex: 2 levels (Male, Female)
  # FIX: We explicitly set levels for sex_group to ensure "Male" comes first.
  # Without 'levels=', factor() sorts alphabetically ("Female", "Male"), causing the test mismatch.
  df <- tibble(
    id = 1:6,
    age_group = factor(rep(c("18-45", "46-64", "65-99"), 2), levels = c("18-45", "46-64", "65-99")),
    sex_group = factor(c(rep("Male", 3), rep("Female", 3)), levels = c("Male", "Female"))
  )

  # 2. Run Function (returns vector now)
  res_vec <- svy_create_joint_var(df, c("age_group", "sex_group"))

  # 3. Check Return Type
  expect_true(is.factor(res_vec))
  expect_equal(length(res_vec), nrow(df))

  # 4. Check Level Order
  # Expected: First var (Age) varies fastest.
  # Loop 1 (Sex=Male): 18-45/Male, 46-64/Male, 65-99/Male
  # Loop 2 (Sex=Female): 18-45/Female, 46-64/Female, 65-99/Female
  expected_levels <- c(
    "18-45/Male",
    "46-64/Male",
    "65-99/Male",
    "18-45/Female",
    "46-64/Female",
    "65-99/Female"
  )

  expect_equal(levels(res_vec), expected_levels)

  # 5. Check Row Values
  expect_equal(as.character(res_vec[1]), "18-45/Male")
  expect_equal(as.character(res_vec[4]), "18-45/Female")

  # 6. Test usage in mutate
  df_mutated <- df |> mutate(joint = svy_create_joint_var(df, c("age_group", "sex_group")))
  expect_equal(df_mutated$joint, res_vec)
})

test_that("svy_create_joint_var validation works", {
  df <- tibble(
    f1 = factor("a"),
    n1 = 1
  )

  # Not enough variables
  expect_error(svy_create_joint_var(df, "f1"), "at least two variable names")

  # Variable missing
  expect_error(svy_create_joint_var(df, c("f1", "f2")), "not in the dataset")

  # Variable not a factor
  expect_error(svy_create_joint_var(df, c("f1", "n1")), "must be factors")
})
