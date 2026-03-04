# Compute the number of individuals needed to vaccinate to prevent one event according to Machado et al. method

Compute the number of individuals needed to vaccinate to prevent one
event according to Machado et al. method

## Usage

``` r
compute_number_needed_to_vaccinate_machado(
  number_of_events,
  number_of_events_averted,
  population_size,
  vaccine_effectiveness
)
```

## Arguments

- number_of_events:

  number of events

- number_of_events_averted:

  number of events averted

- population_size:

  population size

- vaccine_effectiveness:

  vaccine effectiveness

## Value

The number of individuals needed to vaccinate to avert one event

## Details

The number of individuals needed to vaccinate to prevent one event is
calculated as described by Machado et al. (2019)
<doi:10.2807/1560-7917.ES.2019.24.45.1900268>.

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
nnv_machado <- compute_number_needed_to_vaccinate_machado(
  number_of_events = incidence$events,
  number_of_events_averted = nae,
  population_size = 1234,
  vaccine_effectiveness = vaccine_effectiveness
)
nnv_machado
#>  [1]  41.12997  29.50475  26.92473  27.41407  29.01461  27.30541  30.97586
#>  [8]  58.08314  60.58614  43.89116  93.86294 160.55538 137.54650 255.03374
#> [15] 230.88015 812.75083 511.59870 314.74289 351.58960 661.23225        NA
#> [22]        NA        NA        NA        NA        NA        NA        NA
#> [29]        NA        NA        NA        NA        NA        NA        NA
#> [36]        NA        NA        NA        NA        NA        NA        NA
#> [43]        NA        NA        NA        NA        NA        NA        NA
#> [50]        NA        NA        NA
```
