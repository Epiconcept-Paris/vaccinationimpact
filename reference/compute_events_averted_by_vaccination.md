# Compute events averted by vaccination

Compute events averted by vaccination

## Usage

``` r
compute_events_averted_by_vaccination(
  number_of_events,
  cumulative_coverage,
  vaccine_effectiveness
)
```

## Arguments

- number_of_events:

  number of events

- cumulative_coverage:

  cumulative vaccination coverage

- vaccine_effectiveness:

  vaccine effectiveness

## Value

estimated number of events averted

## Details

The number of events averted by vaccination is calculated as described
by Machado et al. (2019) <doi:10.2807/1560-7917.ES.2019.24.45.1900268>.

## Examples

``` r
data(coverage_and_incidence_mock_data)
data(ve_mock_data)
coverage <- coverage_and_incidence_mock_data$coverage_data
incidence <- coverage_and_incidence_mock_data$incidence_data
vaccine_effectiveness <- ve_mock_data$ve
nae <- compute_events_averted_by_vaccination(
  number_of_events = incidence$events,
  cumulative_coverage = coverage$cumulative_coverage,
  vaccine_effectiveness = vaccine_effectiveness
)
plot(nae, type = "l", xlab = "Time", ylab = "Events averted")
```
