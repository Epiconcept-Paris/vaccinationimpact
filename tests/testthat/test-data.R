# Test data objects

test_that("coverage_and_incidence_mock_data has correct structure", {
  # Check that data exists
  expect_true(exists("coverage_and_incidence_mock_data"))

  # Check that it's a list
  expect_type(coverage_and_incidence_mock_data, "list")

  # Check that it has the expected components
  expect_named(
    coverage_and_incidence_mock_data,
    c("incidence_data", "coverage_data")
  )

  # Check incidence_data structure
  expect_true(is.data.frame(coverage_and_incidence_mock_data$incidence_data))
  expect_true(
    "events" %in% names(coverage_and_incidence_mock_data$incidence_data)
  )

  # Check coverage_data structure
  expect_true(is.data.frame(coverage_and_incidence_mock_data$coverage_data))
  expect_true(
    "cumulative_coverage" %in%
      names(coverage_and_incidence_mock_data$coverage_data)
  )

  # Check that both data frames have the same number of rows
  expect_equal(
    nrow(coverage_and_incidence_mock_data$incidence_data),
    nrow(coverage_and_incidence_mock_data$coverage_data)
  )
})

test_that("ve_mock_data has correct structure", {
  # Check that data exists
  expect_true(exists("ve_mock_data"))

  # Check that it's a data frame
  expect_true(is.data.frame(ve_mock_data))

  # Check that it has the expected columns
  expect_named(ve_mock_data, c("week", "ve"))

  # Check that week is a Date
  expect_s3_class(ve_mock_data$week, "Date")

  # Check that ve is numeric
  expect_type(ve_mock_data$ve, "double")

  # Check that ve values are between 0 and 1
  expect_true(all(ve_mock_data$ve >= 0 & ve_mock_data$ve <= 1))

  # Check that there are no missing values
  expect_false(any(is.na(ve_mock_data$week)))
  expect_false(any(is.na(ve_mock_data$ve)))
})

test_that("data objects have consistent dimensions", {
  # Check that incidence_data and coverage_data have same number of rows
  expect_equal(
    nrow(coverage_and_incidence_mock_data$incidence_data),
    nrow(coverage_and_incidence_mock_data$coverage_data)
  )

  # Check that ve_mock_data has same number of rows as other data
  expect_equal(
    nrow(ve_mock_data),
    nrow(coverage_and_incidence_mock_data$incidence_data)
  )
})

test_that("data objects have reasonable values", {
  # Check that events are non-negative
  expect_true(all(coverage_and_incidence_mock_data$incidence_data$events >= 0))

  # Check that cumulative coverage is between 0 and 1
  expect_true(
    all(
      coverage_and_incidence_mock_data$coverage_data$cumulative_coverage >= 0 &
        coverage_and_incidence_mock_data$coverage_data$cumulative_coverage <= 1
    )
  )

  # Check that vaccine effectiveness is between 0 and 1
  expect_true(all(ve_mock_data$ve >= 0 & ve_mock_data$ve <= 1))

  # Check that cumulative coverage is non-decreasing
  cumulative_coverage <- coverage_and_incidence_mock_data$coverage_data$cumulative_coverage
  expect_true(all(diff(cumulative_coverage) >= 0))
})
