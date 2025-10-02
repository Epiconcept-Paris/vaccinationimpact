# Test compute_events_averted_by_vaccination function

test_that("compute_events_averted_by_vaccination works with valid inputs", {
  result <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  # Check that result is numeric
  expect_type(result, "double")

  # Check that result has correct length
  expect_length(result, length(test_events))

  # Check that result is not NA
  expect_false(any(is.na(result)))

  # Check that result is non-negative (events averted can't be negative)
  expect_true(all(result >= 0))
})

test_that("compute_events_averted_by_vaccination validates input lengths", {
  # Test with mismatched lengths
  expect_error(
    compute_events_averted_by_vaccination(
      test_events[1:3],
      test_cumulative_coverage,
      test_vaccine_effectiveness
    ),
    "events, cumulative_coverage, and vaccine_effectiveness must have the same length"
  )

  expect_error(
    compute_events_averted_by_vaccination(
      test_events,
      test_cumulative_coverage[1:3],
      test_vaccine_effectiveness
    ),
    "events, cumulative_coverage, and vaccine_effectiveness must have the same length"
  )

  expect_error(
    compute_events_averted_by_vaccination(
      test_events,
      test_cumulative_coverage,
      test_vaccine_effectiveness[1:3]
    ),
    "events, cumulative_coverage, and vaccine_effectiveness must have the same length"
  )
})

test_that("compute_events_averted_by_vaccination handles edge cases", {
  # Test with zero events
  zero_events <- rep(0, length(test_cumulative_coverage))
  result_zero <- compute_events_averted_by_vaccination(
    zero_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )
  expect_equal(result_zero, rep(0, length(zero_events)))

  # Test with zero coverage
  zero_coverage <- rep(0, length(test_events))
  result_zero_cov <- compute_events_averted_by_vaccination(
    test_events,
    zero_coverage,
    test_vaccine_effectiveness
  )
  expect_equal(result_zero_cov, rep(0, length(test_events)))

  # Test with zero vaccine effectiveness
  zero_ve <- rep(0, length(test_events))
  result_zero_ve <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    zero_ve
  )
  expect_equal(result_zero_ve, rep(0, length(test_events)))
})

test_that("compute_events_averted_by_vaccination handles boundary values", {
  # Test with maximum coverage and effectiveness
  max_coverage <- rep(1, length(test_events))
  max_ve <- rep(1, length(test_events))
  result_max <- compute_events_averted_by_vaccination(
    test_events,
    max_coverage,
    max_ve
  )

  # With max coverage and VE, all events should be averted
  expect_true(all(is.infinite(result_max)))
})

test_that("compute_events_averted_by_vaccination mathematical properties", {
  # Test that more events lead to more events averted (proportionality)
  more_events <- test_events * 2
  result_more <- compute_events_averted_by_vaccination(
    more_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )
  result_original <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  expect_equal(result_more, result_original * 2)

  # Test that higher coverage leads to more events averted
  higher_coverage <- test_cumulative_coverage + 0.1
  result_higher <- compute_events_averted_by_vaccination(
    test_events,
    higher_coverage,
    test_vaccine_effectiveness
  )
  result_original <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  expect_true(all(result_higher >= result_original))
})
