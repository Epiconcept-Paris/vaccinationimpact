#' Compute events averted by vaccination
#' @param events Vector of number of events
#' @param cumulative_coverage Vector of cumulative coverage
#' @param vaccine_effectiveness Vector of vaccine effectiveness
#' @return The number of events averted
#' @export
#' @rdname vaccine_impact
#' @examples
#' compute_events_averted_by_vaccination(events = 100, cumulative_coverage = 0.5,
#' vaccine_effectiveness = 0.5)
#' data("coverage_and_incidence_mock_data")
#' data("ve_mock_data")
#' events <- coverage_and_incidence_mock_data$incidence_data$events
#' cumulative_coverage <- coverage_and_incidence_mock_data$coverage_data$cumulative_coverage
#' vaccine_effectiveness <- ve_mock_data$ve
#' compute_events_averted_by_vaccination(events, cumulative_coverage, vaccine_effectiveness)
compute_events_averted_by_vaccination <- function(
  events,
  cumulative_coverage,
  vaccine_effectiveness
) {
  if (
    length(events) != length(cumulative_coverage) ||
      length(events) != length(vaccine_effectiveness)
  ) {
    stop(
      "events, cumulative_coverage, and vaccine_effectiveness must have the same length"
    )
  }
  events *
    ((cumulative_coverage * vaccine_effectiveness) /
      (1 - (cumulative_coverage * vaccine_effectiveness)))
}
