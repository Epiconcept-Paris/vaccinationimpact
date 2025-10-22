
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vaccinationimpact

<!-- badges: start -->

[![R-CMD-check](https://github.com/Epiconcept-Paris/vaccinationimpact/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Epiconcept-Paris/vaccinationimpact/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{vaccinationimpact}` is to assess the impact of vaccination
campaigns using the following estimates:

- Number of events averted by vaccination (NAE)
- Number of avertable events considering an increase in final coverage
  (NAbE)
- Number needed to vaccinate (NNV) to prevent one event

## Installation

You can install the development version of vaccinationimpact from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Epiconcept-Paris/vaccinationimpact")
```

## Example

We use some toy data to illustrate the usage of the package: weekly
coverage, incidence and vaccine effectiveness are provided in the
package.

``` r
library(vaccinationimpact)
data(coverage_and_incidence_mock_data)
data(ve_mock_data)
coverage <- coverage_and_incidence_mock_data$coverage_data
incidence <- coverage_and_incidence_mock_data$incidence_data
vaccine_effectiveness <- ve_mock_data$ve
```

### NAE

``` r
nae <- compute_events_averted_by_vaccination(
  number_of_events = incidence$events,
  cumulative_coverage = coverage$cumulative_coverage,
  vaccine_effectiveness = vaccine_effectiveness
)
nae
#>  [1]  2.285438  8.405426 14.187702 19.150751 22.333578 26.258535 25.277745
#>  [8] 14.479245 14.524775 20.687536  9.940025  5.904505  6.979458  3.795576
#> [15]  4.214308  1.203321  1.917519  3.123184  2.798718  1.489643  0.000000
#> [22]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [29]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [36]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [43]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [50]  0.000000  0.000000  0.000000
```

### NAbE

``` r
nabe <- compute_events_avertable_by_increasing_coverage(
  number_of_events = incidence$events,
  cumulative_coverage = coverage$cumulative_coverage,
  vaccine_coverage_increase = 0.1, # 10% increase in final coverage
  vaccine_effectiveness = vaccine_effectiveness
)
nabe$nabe
#>  [1]  2.587606  9.637167 16.574011 22.651941 26.915232 32.599316 31.352360
#>  [8] 17.750085 18.067588 25.959763 12.974325  7.574670  8.970289  4.844898
#> [15]  5.299678  1.593502  2.451064  3.811307  3.564364  1.847972  0.000000
#> [22]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [29]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [36]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [43]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
#> [50]  0.000000  0.000000  0.000000
```

### NNV

NNV can be estimated using 2 methods: Machado et al. and Tuite and
Fisman (see vignette for more details).

#### Machado et al. method

``` r
sample_size <- 1234

nnv_machado <- compute_number_needed_to_vaccinate_machado(
  number_of_events = incidence$events,
  number_of_events_averted = nae,
  population_size = sample_size,
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

#### Tuite and Fisman method

``` r
nnv_tuite_fisman <- compute_number_needed_to_vaccinate_tuite_fisman(
  number_of_vaccinated = cumsum(coverage$number_of_vaccinated),
  number_of_events_averted = nae
)
nnv_tuite_fisman
#>  [1]  41.12997  29.50475  26.92473  27.41407  29.01461  27.30541  30.97586
#>  [8]  58.08314  60.58614  43.89116  93.86294 160.55538 137.54650 255.03374
#> [15] 230.88015 812.75083 511.59870 314.74289 351.58960 661.23225        NA
#> [22]        NA        NA        NA        NA        NA        NA        NA
#> [29]        NA        NA        NA        NA        NA        NA        NA
#> [36]        NA        NA        NA        NA        NA        NA        NA
#> [43]        NA        NA        NA        NA        NA        NA        NA
#> [50]        NA        NA        NA
```

More information can be found in the
[vignette](https://epiconcept-paris.github.io/vaccinationimpact/articles/vaccination_impact_estimates.html).
