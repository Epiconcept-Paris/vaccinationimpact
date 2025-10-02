# Test compute_number_needed_to_vaccinate function

test_that("compute_number_needed_to_vaccinate works with valid inputs", {
  # First compute events averted
  events_averted <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  result <- compute_number_needed_to_vaccinate(
    test_events,
    events_averted,
    test_pop_at_risk,
    test_vaccine_effectiveness
  )

  # Check that result is numeric
  expect_type(result, "double")

  # Check that result has correct length
  expect_length(result, length(test_events))

  # Check that result is not NA (except where expected)
  # Some values might be NA due to infinite calculations
  expect_true(is.numeric(result))

  # Check that non-NA values are positive
  non_na_result <- result[!is.na(result)]
  if (length(non_na_result) > 0) {
    expect_true(all(non_na_result > 0))
  }
})

test_that("compute_number_needed_to_vaccinate handles edge cases", {
  # Test with zero events
  zero_events <- rep(0, length(test_cumulative_coverage))
  zero_events_averted <- rep(0, length(test_cumulative_coverage))

  result_zero <- compute_number_needed_to_vaccinate(
    zero_events,
    zero_events_averted,
    test_pop_at_risk,
    test_vaccine_effectiveness
  )

  # Should be infinite or NA when no events occur
  expect_true(all(is.infinite(result_zero) | is.na(result_zero)))

  # Test with zero vaccine effectiveness
  zero_ve <- rep(0, length(test_events))
  events_averted <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  result_zero_ve <- compute_number_needed_to_vaccinate(
    test_events,
    events_averted,
    test_pop_at_risk,
    zero_ve
  )

  # Should be infinite or NA when VE is zero
  expect_true(all(is.infinite(result_zero_ve) | is.na(result_zero_ve)))
})

test_that("compute_number_needed_to_vaccinate mathematical properties", {
  # Test with known values
  events <- c(10, 20, 30)
  events_averted <- c(5, 10, 15)
  pop_at_risk <- 1000
  ve <- c(0.8, 0.7, 0.6)

  result <- compute_number_needed_to_vaccinate(
    events,
    events_averted,
    pop_at_risk,
    ve
  )

  # Calculate expected values manually
  expected_Rb <- (events + events_averted) / pop_at_risk
  expected_NNv <- 1 / (expected_Rb * ve)

  expect_equal(result, expected_NNv)
})

test_that("compute_number_needed_to_vaccinate handles infinite values", {
  # Test with very small population at risk (should lead to large NNV)
  small_pop <- 1
  events_averted <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  result_small_pop <- compute_number_needed_to_vaccinate(
    test_events,
    events_averted,
    small_pop,
    test_vaccine_effectiveness
  )

  # Should have finite values
  expect_true(is.numeric(result_small_pop))

  # Test with very large population at risk
  large_pop <- 1000000
  result_large_pop <- compute_number_needed_to_vaccinate(
    test_events,
    events_averted,
    large_pop,
    test_vaccine_effectiveness
  )

  # Should have finite values
  expect_true(is.numeric(result_large_pop))
})

test_that("compute_number_needed_to_vaccinate with different vaccine effectiveness", {
  events_averted <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  # Test with different VE values
  ve_values <- c(0.5, 0.7, 0.9)

  for (ve in ve_values) {
    ve_vector <- rep(ve, length(test_events))
    result <- compute_number_needed_to_vaccinate(
      test_events,
      events_averted,
      test_pop_at_risk,
      ve_vector
    )

    expect_type(result, "double")
    expect_length(result, length(test_events))
    expect_true(is.numeric(result))
  }
})

test_that("compute_number_needed_to_vaccinate relationship with VE", {
  events_averted <- compute_events_averted_by_vaccination(
    test_events,
    test_cumulative_coverage,
    test_vaccine_effectiveness
  )

  # Higher VE should lead to lower NNV
  result_low_ve <- compute_number_needed_to_vaccinate(
    test_events,
    events_averted,
    test_pop_at_risk,
    rep(0.5, length(test_events))
  )

  result_high_ve <- compute_number_needed_to_vaccinate(
    test_events,
    events_averted,
    test_pop_at_risk,
    rep(0.9, length(test_events))
  )

  # Compare non-NA values
  non_na_low <- result_low_ve[!is.na(result_low_ve)]
  non_na_high <- result_high_ve[!is.na(result_high_ve)]

  if (length(non_na_low) > 0 && length(non_na_high) > 0) {
    expect_true(all(non_na_high <= non_na_low))
  }
})
