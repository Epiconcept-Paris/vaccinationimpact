#' Compute the number of individuals needed to vaccinate to prevent
#' one event
#' @param events Vector of number of events
#' @param events_averted Vector of number of events averted
#' @param pop_at_risk Population at risk
#' @param vaccine_effectiveness Vector of vaccine effectiveness
#' @return The number of individuals needed to vaccinate to avert
#' one event
#' @export
#' @rdname vaccine_impact
#' @examples
#' events <- coverage_and_incidence_mock_data$incidence_data$events
#' cumulative_coverage <- coverage_and_incidence_mock_data$coverage_data$cumulative_coverage
#' events_averted <- compute_events_averted_by_vaccination(events,
#' cumulative_coverage, vaccine_effectiveness)
#' pop_at_risk <- 1000
#' vaccine_effectiveness <- ve_mock_data$ve
#' compute_number_needed_to_vaccinate(events, events_averted, pop_at_risk, vaccine_effectiveness)
compute_number_needed_to_vaccinate <- function(
  events,
  events_averted,
  pop_at_risk,
  vaccine_effectiveness
) {
  Rb <- (events + events_averted) / pop_at_risk
  NNv <- 1 / (Rb * vaccine_effectiveness)
  NNv[is.infinite(NNv)] <- NA
  return(NNv)
}
