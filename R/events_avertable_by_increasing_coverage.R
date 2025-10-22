#' Compute events averted by increasing the final vaccine coverage
#' @param vaccine_coverage_increase percentage increase in final vaccine coverage (between 0 and 1)
#' @return a list with the new vaccine coverage ("new_vaccine_coverage") and the estimated number of events averted ("nabe")
#' @rdname compute_events_avertable_by_increasing_coverage
#' @inheritParams compute_events_averted_by_vaccination
#' @export
#' @examples
#' data(coverage_and_incidence_mock_data)
#' data(ve_mock_data)
#' coverage <- coverage_and_incidence_mock_data$coverage_data
#' incidence <- coverage_and_incidence_mock_data$incidence_data
#' vaccine_effectiveness <- ve_mock_data$ve
#' nabe <- compute_events_avertable_by_increasing_coverage(
#'   number_of_events = incidence$events,
#'   cumulative_coverage = coverage$cumulative_coverage,
#'   vaccine_coverage_increase = 0.1, # 10% increase in final coverage
#'   vaccine_effectiveness = vaccine_effectiveness
#' )
#' plot(nabe$new_vaccine_coverage, type = "l",
#' xlab = "Time", ylab = "Vaccine coverage with 10% increase")
#' plot(nabe$nabe, type = "l", xlab = "Time", ylab = "Events averted")
compute_events_avertable_by_increasing_coverage <- function(
  number_of_events,
  cumulative_coverage,
  vaccine_coverage_increase,
  vaccine_effectiveness
) {
  if (vaccine_coverage_increase < 0 || vaccine_coverage_increase > 1) {
    stop("vaccine_coverage_increase must be between 0 and 1")
  }

  lagged_coverage <- c(0, cumulative_coverage[-length(cumulative_coverage)])

  diff_cumulative_coverage <- cumulative_coverage - lagged_coverage
  target_coverage <- max(cumulative_coverage)

  new_VC <- compute_new_vaccine_coverage(
    diff_cumulative_coverage,
    target_coverage,
    vaccine_coverage_increase
  )

  new_VC <- cumsum(new_VC)
  nabe <- compute_events_averted_by_vaccination(
    number_of_events,
    new_VC,
    vaccine_effectiveness
  )

  return(
    list(
      new_vaccine_coverage = new_VC,
      nabe = nabe
    )
  )
}

#' Compute vaccine coverage for a given value of alpha
#' (percent of increase in final vaccine coverage)
#' @param diff_cumulative_coverage Vector of difference in cumulative coverage
#' (cumulative_coverage_t - cumulative_coverage_t-1)
#' @param target_coverage Vector of target coverage
#' @param alpha percent of increase in final vaccine coverage
#' @return The vaccine coverage
#' @noRd
compute_new_vaccine_coverage <- function(
  diff_cumulative_coverage,
  target_coverage,
  alpha
) {
  if (length(alpha) != 1) {
    stop("alpha must be a single value")
  }

  if (alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1")
  }

  return(pmin(diff_cumulative_coverage * (1 + (alpha / target_coverage)), 1))
}
