#' Compute events averted by increasing the final vaccine coverage 
#' @param vaccine_coverage_increase percentage increase in final vaccine coverage (between 0 and 1)
#' @return a list with the new vaccine coverage ("new_vaccine_coverage") and the estimated number of events averted ("nabe")
#' @rdname vaccine_impact
#' @export
#' @examples
compute_events_avertable_by_increasing_coverage <- function(
  number_of_events,
  cumulative_coverage,
  vaccine_coverage_increase,
  vaccine_effectiveness
) {

  if (vaccine_coverage_increase < 0 || vaccine_coverage_increase > 1) {
    stop("vaccine_coverage_increase must be between 0 and 1")
  }

  diff_cumulative_coverage <- cumsum(c(0, diff(cumulative_coverage)))
  target_coverage <- max(cumulative_coverage)

  new_VC <- compute_new_vaccine_coverage(
    diff_cumulative_coverage,
    target_coverage,
    vaccine_coverage_increase
  )
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
compute_new_vaccine_coverage <- function(diff_cumulative_coverage, target_coverage, alpha) {
  if (length(alpha) != 1) {
    stop("alpha must be a single value")
  }

  if (alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1")
  }

  return(pmin(diff_cumulative_coverage * (1 + (alpha / target_coverage)), 1))
}