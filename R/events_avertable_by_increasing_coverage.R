#' Compute vaccine coverage for a given value of alpha
#' (percent of increase in final vaccine coverage)
#' @param diff_cumulative_coverage Vector of difference in cumulative coverage
#' (cumulative_coverage_t - cumulative_coverage_t-1)
#' @param target_coverage Vector of target coverage
#' @param alpha percent of increase in final vaccine coverage
#' @return The vaccine coverage
#' @export
#' @rdname vaccine_impact
#' @examples
#' diff_cumulative_coverage <- c(0, diff(coverage_and_incidence_mock_data$coverage_data$
#' cumulative_coverage))
#' target_coverage <- max(coverage_and_incidence_mock_data$coverage_data$
#' cumulative_coverage)
#' alpha <- 0.1
#' compute_VC_alpha(diff_cumulative_coverage, target_coverage, alpha)
compute_VC_alpha <- function(diff_cumulative_coverage, target_coverage, alpha) {
  if (length(alpha) != 1) {
    stop("alpha must be a single value")
  }

  if (alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1")
  }

  pmin(diff_cumulative_coverage * (1 + (alpha / target_coverage)), 1)
}

#' Compute events averted by increasing the coverage to a given value of alpha
#' (percent of increase in final vaccine coverage)
#' @param events Vector of number of events
#' @param diff_cumulative_coverage Vector of difference in cumulative coverage
#' (cumulative_coverage_t - cumulative_coverage_t-1)
#' @param target_coverage Target coverage (maximum coverage)
#' @param alpha percent of increase in final vaccine coverage
#' @param vaccine_effectiveness Vector of vaccine effectiveness
#' @return The number of events averted
#' @rdname vaccine_impact
#' @export
#' @examples
#' events <- coverage_and_incidence_mock_data$incidence_data$events
#' diff_cumulative_coverage <- c(0, diff(coverage_and_incidence_mock_data$coverage_data$
#' cumulative_coverage))
#' target_coverage <- max(coverage_and_incidence_mock_data$coverage_data$
#' cumulative_coverage)
#' alpha <- 0.1
#' vaccine_effectiveness <- ve_mock_data$ve
#' compute_events_avertable_by_increasing_coverage(events, diff_cumulative_coverage,
#' target_coverage, alpha, vaccine_effectiveness)
compute_events_avertable_by_increasing_coverage <- function(
  events,
  diff_cumulative_coverage,
  target_coverage,
  alpha,
  vaccine_effectiveness
) {
  if (
    length(events) != length(diff_cumulative_coverage) ||
      length(events) != length(vaccine_effectiveness)
  ) {
    stop(
      "events, diff_cumulative_coverage, and vaccine_effectiveness must have the same length"
    )
  }
  vaccine_coverage_alpha <- compute_VC_alpha(
    diff_cumulative_coverage,
    target_coverage,
    alpha
  )
  compute_events_averted_by_vaccination(
    events,
    vaccine_coverage_alpha,
    vaccine_effectiveness
  )
}

#' Compute events averted by increasing the coverage to many values of alpha
#' @param alphas Vector of alpha values
#' @return A data frame with the alpha values and the events averted
#' @export
#' @rdname vaccine_impact
#' @examples
#' events <- coverage_and_incidence_mock_data$incidence_data$events
#' diff_cumulative_coverage <- c(0, diff(coverage_and_incidence_mock_data$coverage_data$
#' cumulative_coverage))
#' target_coverage <- max(coverage_and_incidence_mock_data$coverage_data$
#' cumulative_coverage)
#' alphas <- c(0.1, 0.2, 0.3)
#' vaccine_effectiveness <- ve_mock_data$ve
#' compute_events_avertable_by_increasing_coverage_many_alphas(events, diff_cumulative_coverage,
#' target_coverage, alphas, vaccine_effectiveness)
compute_events_avertable_by_increasing_coverage_many_alphas <- function(
  events,
  diff_cumulative_coverage,
  target_coverage,
  alphas,
  vaccine_effectiveness
) {

res <- lapply(alphas, function(alpha) {
  nabe <- compute_events_avertable_by_increasing_coverage(
    events = events,
    diff_cumulative_coverage = diff_cumulative_coverage,
    target_coverage = target_coverage,
    alpha = alpha,
    vaccine_effectiveness = vaccine_effectiveness
  )
  data.frame(alpha = alpha, nabe = nabe)
  })
  Reduce(rbind, res)
}
