
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vaccineimpact

<!-- badges: start -->

[![R-CMD-check](https://github.com/Epiconcept-Paris/vaccineimpact/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Epiconcept-Paris/vaccineimpact/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{vaccineimpact}` is to assess the impact of vaccines using
the following estimates:

- Number of events averted by vaccination (NAE)
- Number of avertable events considering an increase in final coverage
  (NAbE)
- Number needed to vaccinate (NNV) to prevent one event

## Installation

You can install the development version of vaccineimpact from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Epiconcept-Paris/vaccineimpact")
```

## Example

Some toy data is included in the package to illustrate the usage of the
package.

``` r
library(vaccineimpact)
data(coverage_and_incidence_mock_data)
data(ve_mock_data)
coverage <- coverage_and_incidence_mock_data$coverage_data
incidence <- coverage_and_incidence_mock_data$incidence_data
vaccine_effectiveness <- ve_mock_data$ve
```

### NAE

``` r
nae <- compute_events_averted_by_vaccination(
  events = incidence$events,
  cumulative_coverage = coverage$cumulative_coverage,
  vaccine_effectiveness = vaccine_effectiveness
)
nae
#>  [1]  0.0000000  0.0000000  1.8404774  6.6095710 11.0488282  7.3471758
#>  [7]  5.6472456 14.3417981  6.8704862  8.6083118  7.4468097  6.6467970
#> [13]  1.7373137  3.3178507  4.7765819  3.5559705  0.8146056  0.6098621
#> [19]  1.9862810  1.2618912  0.0000000  0.0000000  0.0000000  0.0000000
#> [25]  0.0000000  0.8982954  0.0000000  0.0000000  0.0000000  0.0000000
#> [31]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [37]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [43]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#> [49]  0.0000000  0.0000000  0.0000000  0.0000000
```

### NAbE

``` r
nabe <- compute_events_avertable_by_increasing_coverage(
  events = incidence$events,
  diff_cumulative_coverage = c(0, diff(coverage$cumulative_coverage)),
  target_coverage = max(coverage$cumulative_coverage),
  alpha = 0.1, # 10% increase in final coverage
  vaccine_effectiveness = vaccine_effectiveness
)
nabe
#>  [1] 0.000000000 0.000000000 2.083148922 4.199830115 4.256792132 1.599197939
#>  [7] 0.965054815 1.103605549 0.512949020 0.373944572 0.231478586 0.130971005
#> [13] 0.026780188 0.028099463 0.039313806 0.019047362 0.003860255 0.002157079
#> [19] 0.004239641 0.002186660 0.000000000 0.000000000 0.000000000 0.000000000
#> [25] 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
#> [31] 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
#> [37] 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
#> [43] 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
#> [49] 0.000000000 0.000000000 0.000000000 0.000000000
```

### NNV

``` r
initial_pop_at_risk <- 1000
pop_at_risk <- initial_pop_at_risk - cumsum(incidence$events)

nnv <- compute_number_needed_to_vaccinate(
  events = incidence$events,
  events_averted = nae,
  pop_at_risk = pop_at_risk,
  vaccine_effectiveness = vaccine_effectiveness
)
nnv
#>  [1]  51.28326  44.51670  37.08168  23.94800  21.99383  42.35913  68.40574
#>  [8]  29.51987  67.68764  57.01188  68.45710  78.07731 305.59363 161.87950
#> [15] 113.25044 152.61881 670.36609 898.66866 276.16938 434.53507        NA
#> [22]        NA        NA        NA        NA 610.93487        NA        NA
#> [29]        NA        NA        NA        NA        NA        NA        NA
#> [36]        NA        NA        NA        NA        NA        NA        NA
#> [43]        NA        NA        NA        NA        NA        NA        NA
#> [50]        NA        NA        NA
```

More information can be found in the vignette.
