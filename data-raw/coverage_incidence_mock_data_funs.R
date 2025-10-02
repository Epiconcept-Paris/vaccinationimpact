generate_epidemic_curve <- function(
  n_weeks,
  peak_week = 4,
  max_rate = 0.8,
  base_rate = 0.05
) {
  # Create a gamma-like distribution curve
  weeks <- 1:n_weeks

  # Use a gamma distribution shape for epidemic curve
  shape <- 2
  scale <- peak_week / shape

  # Calculate relative intensity using gamma density
  relative_intensity <- dgamma(weeks, shape = shape, scale = scale)

  # Normalize and scale to create event rates
  normalized_intensity <- relative_intensity / max(relative_intensity)
  event_rates <- base_rate + (max_rate - base_rate) * normalized_intensity

  return(event_rates)
}

generate_weekly_number_of_vaccinated <- function(
  n_weeks,
  initial_N,
  peak_week = 0.3,
  return_to_zero_at_week = 0.7,
  shape = 2,
  total_vaccination_proportion = 0.4
) {
  # Create an increase-then-decrease vaccination pattern
  # Use a gamma-like distribution to simulate realistic vaccination rollout
  weeks <- 1:n_weeks
  peak_week <- round(n_weeks * peak_week) # Peak vaccination around 30% of the period

  # Use gamma distribution shape for increase-decrease pattern
  scale <- peak_week / shape

  # Calculate relative weights using gamma density
  weights <- dgamma(weeks, shape = shape, scale = scale)

  # Add some randomness while maintaining the overall pattern
  weights <- weights * runif(n_weeks, min = 0.8, max = 1.2)

  # Normalize weights to sum to 1
  weights <- weights / sum(weights)

  # Calculate total vaccinations as a proportion of initial population
  total_vaccinations <- round(initial_N * total_vaccination_proportion)

  # Allocate total vaccinations according to weights
  raw_weekly <- round(total_vaccinations * weights)

  # Ensure we don't exceed total and adjust for rounding errors
  total_allocated <- sum(raw_weekly)
  if (total_allocated != total_vaccinations) {
    # Adjust the first week to match exactly
    raw_weekly[1] <- raw_weekly[1] + (total_vaccinations - total_allocated)
  }

  # Ensure non-negative values
  raw_weekly <- pmax(0, raw_weekly)

  # Force vaccination to reach zero in later weeks
  zero_start_week <- round(n_weeks * return_to_zero_at_week) # Vaccination stops at 70% of the period
  if (zero_start_week <= n_weeks) {
    raw_weekly[zero_start_week:n_weeks] <- 0
  }

  return(raw_weekly)
}

get_number_of_events <- function(
  initial_N,
  epidemic_rates,
  start_of_week = as.Date("2023-10-01")
) {
  current_N <- initial_N
  number_of_events <- rep(0, length(epidemic_rates))
  for (i in 1:length(epidemic_rates)) {
    number_of_events[i] <- rpois(1, lambda = current_N * epidemic_rates[i])
    current_N <- current_N - number_of_events[i]
  }
  data.frame(
    start_of_week = seq(
      start_of_week,
      by = "week",
      length.out = length(number_of_events)
    ),
    events = number_of_events
  )
}

get_coverage_from_vaccinated <- function(
  initial_N,
  number_of_vaccinated,
  number_of_weeks_needed_before_immune = 2,
  start_of_week
) {
  df <- data.frame(
    start_of_week = seq(
      as.Date(start_of_week),
      by = "week",
      length.out = length(number_of_vaccinated)
    ),
    number_of_vaccinated = number_of_vaccinated
  )
  df
  # Create lagged version of number_of_vaccinated
  df$number_of_immune <- dplyr::lag(
    df$number_of_vaccinated,
    n = number_of_weeks_needed_before_immune,
    default = 0
  )
  df$coverage <- df$number_of_immune / initial_N
  df$cumulative_coverage <- cumsum(df$number_of_immune) / initial_N
  df
}
