# Integration tests - test functions working together

test_that("complete workflow works with mock data", {
  # Load the actual mock data
  data("coverage_and_incidence_mock_data")
  data("ve_mock_data")

  # Extract data
  events <- coverage_and_incidence_mock_data$incidence_data$events
  cumulative_coverage <- coverage_and_incidence_mock_data$coverage_data$cumulative_coverage
  vaccine_effectiveness <- ve_mock_data$ve

  # Calculate difference in cumulative coverage
  diff_cumulative_coverage <- c(0, diff(cumulative_coverage))
  target_coverage <- max(cumulative_coverage)
  alpha <- 0.1
  pop_at_risk <- 1000

  # Test the complete workflow
  # Step 1: Compute events averted by vaccination
  events_averted <- compute_events_averted_by_vaccination(
    events,
    cumulative_coverage,
    vaccine_effectiveness
  )

  expect_type(events_averted, "double")
  expect_length(events_averted, length(events))
  expect_false(any(is.na(events_averted)))
  expect_true(all(events_averted >= 0))

  # Step 2: Compute events averted by increasing coverage
  events_avertable <- compute_events_avertable_by_increasing_coverage(
    events,
    diff_cumulative_coverage,
    target_coverage,
    alpha,
    vaccine_effectiveness
  )

  expect_type(events_avertable, "double")
  expect_length(events_avertable, length(events))
  expect_false(any(is.na(events_avertable)))
  expect_true(all(events_avertable >= 0))

  # Step 3: Compute number needed to vaccinate
  nnv <- compute_number_needed_to_vaccinate(
    events,
    events_averted,
    pop_at_risk,
    vaccine_effectiveness
  )

  expect_type(nnv, "double")
  expect_length(nnv, length(events))
  expect_true(is.numeric(nnv))
})

test_that("workflow with different alpha values", {
  data("coverage_and_incidence_mock_data")
  data("ve_mock_data")

  events <- coverage_and_incidence_mock_data$incidence_data$events
  cumulative_coverage <- coverage_and_incidence_mock_data$coverage_data$cumulative_coverage
  vaccine_effectiveness <- ve_mock_data$ve
  diff_cumulative_coverage <- c(0, diff(cumulative_coverage))
  target_coverage <- max(cumulative_coverage)
  pop_at_risk <- 1000

  alpha_values <- c(0, 0.1, 0.5, 1.0)

  for (alpha in alpha_values) {
    # Compute events averted by increasing coverage
    events_avertable <- compute_events_avertable_by_increasing_coverage(
      events,
      diff_cumulative_coverage,
      target_coverage,
      alpha,
      vaccine_effectiveness
    )

    expect_type(events_avertable, "double")
    expect_length(events_avertable, length(events))
    expect_false(any(is.na(events_avertable)))
    expect_true(all(events_avertable >= 0))
  }
})

test_that("workflow handles edge cases gracefully", {
  # Test with minimal data
  minimal_events <- c(1, 2)
  minimal_coverage <- c(0.1, 0.2)
  minimal_ve <- c(0.5, 0.6)
  minimal_diff_coverage <- c(0, 0.1)
  minimal_target <- 0.2
  minimal_alpha <- 0.1
  minimal_pop <- 100

  # Test events averted by vaccination
  events_averted <- compute_events_averted_by_vaccination(
    minimal_events,
    minimal_coverage,
    minimal_ve
  )
  expect_length(events_averted, 2)
  expect_true(all(events_averted >= 0))

  # Test events averted by increasing coverage
  events_avertable <- compute_events_avertable_by_increasing_coverage(
    minimal_events,
    minimal_diff_coverage,
    minimal_target,
    minimal_alpha,
    minimal_ve
  )
  expect_length(events_avertable, 2)
  expect_true(all(events_avertable >= 0))

  # Test number needed to vaccinate
  nnv <- compute_number_needed_to_vaccinate(
    minimal_events,
    events_averted,
    minimal_pop,
    minimal_ve
  )
  expect_length(nnv, 2)
  expect_true(is.numeric(nnv))
})

test_that("functions work with single values", {
  # Test with single values (not vectors)
  single_events <- 10
  single_coverage <- 0.5
  single_ve <- 0.8
  single_diff_coverage <- 0.1
  single_target <- 0.6
  single_alpha <- 0.2
  single_pop <- 1000

  # Test events averted by vaccination
  events_averted <- compute_events_averted_by_vaccination(
    single_events,
    single_coverage,
    single_ve
  )
  expect_length(events_averted, 1)
  expect_true(events_averted >= 0)

  # Test events averted by increasing coverage
  events_avertable <- compute_events_avertable_by_increasing_coverage(
    single_events,
    single_diff_coverage,
    single_target,
    single_alpha,
    single_ve
  )
  expect_length(events_avertable, 1)
  expect_true(events_avertable >= 0)

  # Test number needed to vaccinate
  nnv <- compute_number_needed_to_vaccinate(
    single_events,
    events_averted,
    single_pop,
    single_ve
  )
  expect_length(nnv, 1)
  expect_true(is.numeric(nnv))
})
