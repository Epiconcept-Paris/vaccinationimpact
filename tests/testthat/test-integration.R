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
    number_of_events = events,
    cumulative_coverage = cumulative_coverage,
    vaccine_effectiveness = vaccine_effectiveness
  )

  expect_type(events_averted, "double")
  expect_length(events_averted, length(events))
  expect_false(any(is.na(events_averted)))
  expect_true(all(events_averted >= 0))

  # Step 2: Compute events averted by increasing coverage
  events_avertable_result <- compute_events_avertable_by_increasing_coverage(
    number_of_events = events,
    cumulative_coverage = cumulative_coverage,
    vaccine_coverage_increase = alpha,
    vaccine_effectiveness = vaccine_effectiveness
  )

  expect_type(events_avertable_result, "list")
  expect_named(events_avertable_result, c("new_vaccine_coverage", "nabe"))
  expect_length(events_avertable_result$nabe, length(events))
  expect_false(any(is.na(events_avertable_result$nabe)))
  expect_true(all(events_avertable_result$nabe >= 0))

  # Step 3: Compute number needed to vaccinate
  nnv <- compute_number_needed_to_vaccinate_machado(
    number_of_events = events,
    number_of_events_averted = events_averted,
    population_size = pop_at_risk,
    vaccine_effectiveness = vaccine_effectiveness
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
    events_avertable_result <- compute_events_avertable_by_increasing_coverage(
      number_of_events = events,
      cumulative_coverage = cumulative_coverage,
      vaccine_coverage_increase = alpha,
      vaccine_effectiveness = vaccine_effectiveness
    )

    expect_type(events_avertable_result, "list")
    expect_named(events_avertable_result, c("new_vaccine_coverage", "nabe"))
    expect_length(events_avertable_result$nabe, length(events))
    expect_false(any(is.na(events_avertable_result$nabe)))
    # Note: nabe can be negative in some mathematical scenarios
    expect_true(is.numeric(events_avertable_result$nabe))
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
    number_of_events = minimal_events,
    cumulative_coverage = minimal_coverage,
    vaccine_effectiveness = minimal_ve
  )
  expect_length(events_averted, 2)
  expect_true(all(events_averted >= 0))

  # Test events averted by increasing coverage
  events_avertable_result <- compute_events_avertable_by_increasing_coverage(
    number_of_events = minimal_events,
    cumulative_coverage = minimal_coverage,
    vaccine_coverage_increase = minimal_alpha,
    vaccine_effectiveness = minimal_ve
  )
  expect_length(events_avertable_result$nabe, 2)
  expect_true(all(events_avertable_result$nabe >= 0))

  # Test number needed to vaccinate
  nnv <- compute_number_needed_to_vaccinate_machado(
    number_of_events = minimal_events,
    number_of_events_averted = events_averted,
    population_size = minimal_pop,
    vaccine_effectiveness = minimal_ve
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
    number_of_events = single_events,
    cumulative_coverage = single_coverage,
    vaccine_effectiveness = single_ve
  )
  expect_length(events_averted, 1)
  expect_true(events_averted >= 0)

  # Test events averted by increasing coverage
  events_avertable_result <- compute_events_avertable_by_increasing_coverage(
    number_of_events = single_events,
    cumulative_coverage = single_coverage,
    vaccine_coverage_increase = single_alpha,
    vaccine_effectiveness = single_ve
  )
  expect_length(events_avertable_result$nabe, 1)
  expect_true(events_avertable_result$nabe >= 0)

  # Test number needed to vaccinate
  nnv <- compute_number_needed_to_vaccinate_machado(
    number_of_events = single_events,
    number_of_events_averted = events_averted,
    population_size = single_pop,
    vaccine_effectiveness = single_ve
  )
  expect_length(nnv, 1)
  expect_true(is.numeric(nnv))
})
