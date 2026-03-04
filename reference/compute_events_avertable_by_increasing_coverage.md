# Compute events averted by increasing the final vaccine coverage

Compute events averted by increasing the final vaccine coverage

## Usage

``` r
compute_events_avertable_by_increasing_coverage(
  number_of_events,
  cumulative_coverage,
  vaccine_coverage_increase,
  vaccine_effectiveness
)
```

## Arguments

- number_of_events:

  number of events

- cumulative_coverage:

  cumulative vaccination coverage

- vaccine_coverage_increase:

  percentage increase in final vaccine coverage (between 0 and 1)

- vaccine_effectiveness:

  vaccine effectiveness

## Value

a list with the new vaccine coverage ("new_vaccine_coverage") and the
estimated number of events averted ("nabe")

## Examples

``` r
data(coverage_and_incidence_mock_data)
data(ve_mock_data)
coverage <- coverage_and_incidence_mock_data$coverage_data
incidence <- coverage_and_incidence_mock_data$incidence_data
vaccine_effectiveness <- ve_mock_data$ve
nabe <- compute_events_avertable_by_increasing_coverage(
  number_of_events = incidence$events,
  cumulative_coverage = coverage$cumulative_coverage,
  vaccine_coverage_increase = 0.1, # 10% increase in final coverage
  vaccine_effectiveness = vaccine_effectiveness
)
plot(nabe$new_vaccine_coverage, type = "l",
xlab = "Time", ylab = "Vaccine coverage with 10% increase")

plot(nabe$nabe, type = "l", xlab = "Time", ylab = "Events averted")
```
