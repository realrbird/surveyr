#' Sample Survey Data
#'
#' A synthetic dataset resembling survey data for use in examples and testing.
#' The dataset contains 2000 observations and includes an initial survey
#' weight (WEIGHT) and demographic variables (age, sex, region).
#'
#' @format A tibble with 2000 rows and 4 variables:
#' \describe{
#'   \item{WEIGHT}{Initial survey weight. Generated to sum close to the population total of 100,000.}
#'   \item{age_group}{Factor. Categorical age group: '18-30', '31-54', '55+'.}
#'   \item{sex_group}{Factor. Categorical sex: 'male', 'female'.}
#'   \item{region}{Factor. Categorical region: 'Northeast', 'Midwest', 'South', 'West'.}
#' }
#' @source Synthetic data generated for the \code{surveyr} package.
"survey_df"

#' Population Target List for Weighting
#'
#' A list containing population totals and percentages for use in post-stratification
#' or calibration weighting examples. The total population count is 100,000.
#'
#' @format A list with 3 elements, one for each demographic variable:
#' \describe{
#'   \item{age_group}{A tibble with columns: `age_group` (Factor), `COUNT` (numeric population total), `Freq` (numeric population percentage, 0-100).}
#'   \item{sex_group}{A tibble with columns: `sex_group` (Factor), `COUNT` (numeric population total), `Freq` (numeric population percentage, 0-100).}
#'   \item{region}{A tibble with columns: `region` (Factor), `COUNT` (numeric population total), `Freq` (numeric population percentage, 0-100).}
#' }
#' @source Synthetic data generated for the \code{surveyr} package.
"target_list"
