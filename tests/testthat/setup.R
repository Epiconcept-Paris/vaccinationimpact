# Setup for tests
# This file is run before tests

# Load the package
library(vaccinationimpact)

# Load test data
data("coverage_and_incidence_mock_data")
data("ve_mock_data")

# Create test data for unit tests
test_events <- c(10, 15, 20, 25, 30)
test_cumulative_coverage <- c(0.1, 0.2, 0.3, 0.4, 0.5)
test_vaccine_effectiveness <- c(0.8, 0.75, 0.7, 0.65, 0.6)
test_diff_cumulative_coverage <- c(0, diff(test_cumulative_coverage))
test_target_coverage <- max(test_cumulative_coverage)
test_alpha <- 0.1
test_pop_at_risk <- 1000
