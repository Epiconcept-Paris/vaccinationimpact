# Test compute_new_vaccine_coverage function (internal function)

test_that("compute_new_vaccine_coverage works with valid inputs", {
  result <- compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    test_target_coverage,
    test_alpha
  )

  # Check that result is numeric
  expect_type(result, "double")

  # Check that result has correct length
  expect_length(result, length(test_diff_cumulative_coverage))

  # Check that result values are between 0 and 1 (coverage bounds)
  expect_true(all(result >= 0 & result <= 1))

  # Check that result is not NA
  expect_false(any(is.na(result)))
})

test_that("compute_new_vaccine_coverage handles edge cases", {
  # Test with alpha = 0 (no increase)
  result_zero <- compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    test_target_coverage,
    0
  )
  expect_equal(result_zero, test_diff_cumulative_coverage)

  # Test with alpha = 1 (maximum increase)
  result_max <- compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    test_target_coverage,
    1
  )
  expected_max <- pmin(
    test_diff_cumulative_coverage * (1 + (1 / test_target_coverage)),
    1
  )
  expect_equal(result_max, expected_max)
})

test_that("compute_new_vaccine_coverage validates alpha parameter", {
  # Test that alpha must be a single value
  expect_error(
    compute_new_vaccine_coverage(
      test_diff_cumulative_coverage,
      test_target_coverage,
      c(0.1, 0.2)
    ),
    "alpha must be a single value"
  )

  # Test that alpha must be between 0 and 1
  expect_error(
    compute_new_vaccine_coverage(
      test_diff_cumulative_coverage,
      test_target_coverage,
      -0.1
    ),
    "alpha must be between 0 and 1"
  )

  expect_error(
    compute_new_vaccine_coverage(
      test_diff_cumulative_coverage,
      test_target_coverage,
      1.1
    ),
    "alpha must be between 0 and 1"
  )
})

test_that("compute_new_vaccine_coverage handles boundary values", {
  # Test with alpha = 0
  expect_no_error(compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    test_target_coverage,
    0
  ))

  # Test with alpha = 1
  expect_no_error(compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    test_target_coverage,
    1
  ))
})

test_that("compute_new_vaccine_coverage with different target coverage values", {
  # Test with very small target coverage
  small_target <- 0.01
  result_small <- compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    small_target,
    test_alpha
  )
  expect_true(all(result_small >= 0 & result_small <= 1))

  # Test with large target coverage
  large_target <- 0.9
  result_large <- compute_new_vaccine_coverage(
    test_diff_cumulative_coverage,
    large_target,
    test_alpha
  )
  expect_true(all(result_large >= 0 & result_large <= 1))
})
