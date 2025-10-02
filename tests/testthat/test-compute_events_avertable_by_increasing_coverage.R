# Test compute_events_avertable_by_increasing_coverage function

test_that("compute_events_avertable_by_increasing_coverage works with valid inputs", {
  result <- compute_events_avertable_by_increasing_coverage(
    test_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    test_alpha,
    test_vaccine_effectiveness
  )

  # Check that result is numeric
  expect_type(result, "double")

  # Check that result has correct length
  expect_length(result, length(test_events))

  # Check that result is not NA
  expect_false(any(is.na(result)))

  # Check that result is non-negative
  expect_true(all(result >= 0))
})

test_that("compute_events_avertable_by_increasing_coverage validates input lengths", {
  # Test with mismatched lengths
  expect_error(
    compute_events_avertable_by_increasing_coverage(
      test_events[1:3],
      test_diff_cumulative_coverage,
      test_target_coverage,
      test_alpha,
      test_vaccine_effectiveness
    ),
    "events, diff_cumulative_coverage, and vaccine_effectiveness must have the same length"
  )

  expect_error(
    compute_events_avertable_by_increasing_coverage(
      test_events,
      test_diff_cumulative_coverage[1:3],
      test_target_coverage,
      test_alpha,
      test_vaccine_effectiveness
    ),
    "events, diff_cumulative_coverage, and vaccine_effectiveness must have the same length"
  )

  expect_error(
    compute_events_avertable_by_increasing_coverage(
      test_events,
      test_diff_cumulative_coverage,
      test_target_coverage,
      test_alpha,
      test_vaccine_effectiveness[1:3]
    ),
    "events, diff_cumulative_coverage, and vaccine_effectiveness must have the same length"
  )
})

test_that("compute_events_avertable_by_increasing_coverage handles edge cases", {
  # Test with alpha = 0 (no increase in coverage)
  result_zero_alpha <- compute_events_avertable_by_increasing_coverage(
    test_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    0,
    test_vaccine_effectiveness
  )

  # Should be equivalent to regular events averted calculation
  expected_zero_alpha <- compute_events_averted_by_vaccination(
    test_events,
    test_diff_cumulative_coverage,
    test_vaccine_effectiveness
  )
  expect_equal(result_zero_alpha, expected_zero_alpha)

  # Test with zero events
  zero_events <- rep(0, length(test_diff_cumulative_coverage))
  result_zero_events <- compute_events_avertable_by_increasing_coverage(
    zero_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    test_alpha,
    test_vaccine_effectiveness
  )
  expect_equal(result_zero_events, rep(0, length(zero_events)))
})

test_that("compute_events_avertable_by_increasing_coverage with different alpha values", {
  # Test with different alpha values
  alpha_values <- c(0, 0.1, 0.5, 1.0)

  for (alpha in alpha_values) {
    result <- compute_events_avertable_by_increasing_coverage(
      test_events,
      test_diff_cumulative_coverage,
      test_target_coverage,
      alpha,
      test_vaccine_effectiveness
    )

    expect_type(result, "double")
    expect_length(result, length(test_events))
    expect_false(any(is.na(result)))
    expect_true(all(result >= 0))
  }
})

test_that("compute_events_avertable_by_increasing_coverage mathematical properties", {
  # Test that higher alpha leads to more events averted
  result_alpha_0.1 <- compute_events_avertable_by_increasing_coverage(
    test_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    0.1,
    test_vaccine_effectiveness
  )

  result_alpha_0.5 <- compute_events_avertable_by_increasing_coverage(
    test_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    0.5,
    test_vaccine_effectiveness
  )

  expect_true(all(result_alpha_0.5 >= result_alpha_0.1))

  # Test that more events lead to more events averted (proportionality)
  more_events <- test_events * 2
  result_more <- compute_events_avertable_by_increasing_coverage(
    more_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    test_alpha,
    test_vaccine_effectiveness
  )
  result_original <- compute_events_avertable_by_increasing_coverage(
    test_events,
    test_diff_cumulative_coverage,
    test_target_coverage,
    test_alpha,
    test_vaccine_effectiveness
  )

  expect_equal(result_more, result_original * 2)
})


test_that("compute_events_avertable_by_increasing_coverage_many_alphas works", {

nabe_many_alphas <- compute_events_avertable_by_increasing_coverage_many_alphas(
  events = test_events,
  diff_cumulative_coverage = test_diff_cumulative_coverage,
  target_coverage = test_target_coverage,
  alphas = c(0, 0.1, 0.2, 0.3),
  vaccine_effectiveness = test_vaccine_effectiveness
)

nae <- compute_events_averted_by_vaccination(
  events = test_events,
  cumulative_coverage = test_diff_cumulative_coverage,
  vaccine_effectiveness = test_vaccine_effectiveness
)

nae01 <- compute_events_avertable_by_increasing_coverage(
  events = test_events,
  diff_cumulative_coverage = test_diff_cumulative_coverage,
  target_coverage = test_target_coverage,
  alpha = 0.1,
  vaccine_effectiveness = test_vaccine_effectiveness  
)

nae02 <- compute_events_avertable_by_increasing_coverage(
  events = test_events,
  diff_cumulative_coverage = test_diff_cumulative_coverage,
  target_coverage = test_target_coverage,
  alpha = 0.2,
  vaccine_effectiveness = test_vaccine_effectiveness  
)

nae03 <- compute_events_avertable_by_increasing_coverage(
  events = test_events,
  diff_cumulative_coverage = test_diff_cumulative_coverage,
  target_coverage = test_target_coverage,
  alpha = 0.3,
  vaccine_effectiveness = test_vaccine_effectiveness  
)

expect_equal(nabe_many_alphas$nabe[which(nabe_many_alphas$alpha == 0)], nae)
expect_equal(nabe_many_alphas$nabe[which(nabe_many_alphas$alpha == 0.1)], nae01)
expect_equal(nabe_many_alphas$nabe[which(nabe_many_alphas$alpha == 0.2)], nae02)
expect_equal(nabe_many_alphas$nabe[which(nabe_many_alphas$alpha == 0.3)], nae03)

})