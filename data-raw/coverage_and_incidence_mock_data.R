## code to prepare `coverage_and_incidence_mock_data` dataset goes here
source("data-raw/coverage_incidence_mock_data_funs.R")
set.seed(123)
n_weeks <- 52
initial_N <- 1000
vaccination_data <- generate_weekly_number_of_vaccinated(
  n_weeks = n_weeks,
  initial_N = initial_N,
  peak_week = 0.1,
  return_to_zero_at_week = 0.7,
  shape = 2,
  total_vaccination_proportion = 0.8
)

coverage_data <- get_coverage_from_vaccinated(
  initial_N = initial_N,
  number_of_vaccinated = vaccination_data,
  number_of_weeks_needed_before_immune = 2,
  start_of_week = as.Date("2023-10-01")
)

epidemic_rates <- generate_epidemic_curve(
  n_weeks = n_weeks,
  peak_week = 6,
  max_rate = 0.05,
  base_rate = 0.00
)

incidence_data <- get_number_of_events(
  initial_N = initial_N,
  epidemic_rates = epidemic_rates,
  start_of_week = as.Date("2023-10-01")
)
plot(incidence_data$start_of_week, incidence_data$events)
plot(coverage_data$start_of_week, coverage_data$cumulative_coverage)
sum(coverage_data$number_of_vaccinated)
sum(incidence_data$events)

coverage_data <-coverage_data  %>% 
dplyr::rename(
  week = start_of_week
) #%>% 
# dplyr::select(
#   -number_of_immune,
#   -coverage,
#   -cumulative_coverage
# ) %>% 
# dplyr::mutate(
#   coverage = number_of_vaccinated / initial_N,
#   cumulative_coverage = cumsum(number_of_vaccinated) / initial_N
# )

incidence_data <- incidence_data  %>% 
dplyr::rename(
  week = start_of_week
)

coverage_and_incidence_mock_data <- list(
  "incidence_data" = incidence_data,
  "coverage_data" = coverage_data
)

usethis::use_data(coverage_and_incidence_mock_data, overwrite = TRUE)
# checkhelper::use_data_doc("coverage_and_incidence_mock_data")
