# Test compute_events_avertable_by_increasing_coverage function

test_that("compute_events_avertable_by_increasing_coverage works with valid inputs", {
  result <- compute_events_avertable_by_increasing_coverage(
    number_of_events = test_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = test_alpha,
    vaccine_effectiveness = test_vaccine_effectiveness
  )

  # Check that result is a list
  expect_type(result, "list")

  # Check that result has expected components
  expect_named(result, c("new_vaccine_coverage", "nabe"))

  # Check that nabe is numeric
  expect_type(result$nabe, "double")

  # Check that nabe has correct length
  expect_length(result$nabe, length(test_events))

  # Check that nabe is not NA
  expect_false(any(is.na(result$nabe)))

  # Check that nabe is non-negative
  expect_true(all(result$nabe >= 0))

  # Check that new_vaccine_coverage is numeric
  expect_type(result$new_vaccine_coverage, "double")

  # Check that new_vaccine_coverage has correct length
  expect_length(result$new_vaccine_coverage, length(test_events))

  # Check that new_vaccine_coverage is between 0 and 1
  expect_true(all(
    result$new_vaccine_coverage >= 0 & result$new_vaccine_coverage <= 1
  ))
})

test_that("compute_events_avertable_by_increasing_coverage validates vaccine_coverage_increase", {
  # Test with invalid vaccine_coverage_increase values
  expect_error(
    compute_events_avertable_by_increasing_coverage(
      number_of_events = test_events,
      cumulative_coverage = test_cumulative_coverage,
      vaccine_coverage_increase = -0.1,
      vaccine_effectiveness = test_vaccine_effectiveness
    ),
    "vaccine_coverage_increase must be between 0 and 1"
  )

  expect_error(
    compute_events_avertable_by_increasing_coverage(
      number_of_events = test_events,
      cumulative_coverage = test_cumulative_coverage,
      vaccine_coverage_increase = 1.1,
      vaccine_effectiveness = test_vaccine_effectiveness
    ),
    "vaccine_coverage_increase must be between 0 and 1"
  )
})

test_that("compute_events_avertable_by_increasing_coverage handles edge cases", {
  # Test with vaccine_coverage_increase = 0 (no increase in coverage)
  result_zero_alpha <- compute_events_avertable_by_increasing_coverage(
    number_of_events = test_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = 0,
    vaccine_effectiveness = test_vaccine_effectiveness
  )

  # Should be equivalent to regular events averted calculation
  expected_zero_alpha <- compute_events_averted_by_vaccination(
    number_of_events = test_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_effectiveness = test_vaccine_effectiveness
  )
  expect_equal(result_zero_alpha$nabe, expected_zero_alpha)

  # Test with zero events
  zero_events <- rep(0, length(test_cumulative_coverage))
  result_zero_events <- compute_events_avertable_by_increasing_coverage(
    number_of_events = zero_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = test_alpha,
    vaccine_effectiveness = test_vaccine_effectiveness
  )
  expect_equal(result_zero_events$nabe, rep(0, length(zero_events)))
})

test_that("compute_events_avertable_by_increasing_coverage with different vaccine_coverage_increase values", {
  # Test with different vaccine_coverage_increase values
  alpha_values <- c(0, 0.1, 0.5, 1.0)

  for (alpha in alpha_values) {
    result <- compute_events_avertable_by_increasing_coverage(
      number_of_events = test_events,
      cumulative_coverage = test_cumulative_coverage,
      vaccine_coverage_increase = alpha,
      vaccine_effectiveness = test_vaccine_effectiveness
    )

    expect_type(result, "list")
    expect_named(result, c("new_vaccine_coverage", "nabe"))
    expect_length(result$nabe, length(test_events))
    expect_false(any(is.na(result$nabe)))
    expect_true(all(result$nabe >= 0))
  }
})

test_that("compute_events_avertable_by_increasing_coverage mathematical properties", {
  # Test that higher vaccine_coverage_increase leads to more events averted
  result_alpha_0.1 <- compute_events_avertable_by_increasing_coverage(
    number_of_events = test_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = 0.1,
    vaccine_effectiveness = test_vaccine_effectiveness
  )

  result_alpha_0.5 <- compute_events_avertable_by_increasing_coverage(
    number_of_events = test_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = 0.5,
    vaccine_effectiveness = test_vaccine_effectiveness
  )

  expect_true(all(result_alpha_0.5$nabe >= result_alpha_0.1$nabe))

  # Test that more events lead to more events averted (proportionality)
  more_events <- test_events * 2
  result_more <- compute_events_avertable_by_increasing_coverage(
    number_of_events = more_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = test_alpha,
    vaccine_effectiveness = test_vaccine_effectiveness
  )
  result_original <- compute_events_avertable_by_increasing_coverage(
    number_of_events = test_events,
    cumulative_coverage = test_cumulative_coverage,
    vaccine_coverage_increase = test_alpha,
    vaccine_effectiveness = test_vaccine_effectiveness
  )

  expect_equal(result_more$nabe, result_original$nabe * 2)
})
