# Compute the number of individuals needed to vaccinate to prevent one event according to Tuite and Fisman method

Compute the number of individuals needed to vaccinate to prevent one
event according to Tuite and Fisman method

## Usage

``` r
compute_number_needed_to_vaccinate_tuite_fisman(
  number_of_vaccinated,
  number_of_events_averted
)
```

## Arguments

- number_of_vaccinated:

  number of vaccinated individuals

- number_of_events_averted:

  number of events averted

## Value

The number of individuals needed to vaccinate to avert one event

## Details

The number of individuals needed to vaccinate to prevent one event is
calculated as described by Tuite and Fisman (2013)
<doi:10.1016/j.vaccine.2012.11.097>.

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
nnv_tuite_fisman <- compute_number_needed_to_vaccinate_tuite_fisman(
  number_of_vaccinated = coverage$number_of_vaccinated,
  number_of_events_averted = nae
)
nnv_tuite_fisman
#>  [1] 41.1299696 18.3214994  9.4447994  7.4670700  5.5074023  2.6277170
#>  [7]  2.6109923  4.0057337  2.6850674  1.3534720  2.5150842  2.5404332
#> [13]  1.7193312  2.1077169  1.1864345  4.1551679  1.5645220  0.6403721
#> [19]  0.3573065  0.6713018         NA         NA         NA         NA
#> [25]         NA         NA         NA         NA         NA         NA
#> [31]         NA         NA         NA         NA         NA         NA
#> [37]         NA         NA         NA         NA         NA         NA
#> [43]         NA         NA         NA         NA         NA         NA
#> [49]         NA         NA         NA         NA
```
