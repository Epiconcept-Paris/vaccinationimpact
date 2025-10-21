#' Compute events averted by vaccination
#' @param number_of_events number of events
#' @param cumulative_coverage cumulative coverage
#' @param vaccine_effectiveness vaccine effectiveness
#' @return estimated number of events averted
#' @export
#' @rdname vaccine_impact
#' @examples
compute_events_averted_by_vaccination <- function(
  number_of_events,
  cumulative_coverage,
  vaccine_effectiveness
) {
  number_of_events *
    ((cumulative_coverage * vaccine_effectiveness) /
      (1 - (cumulative_coverage * vaccine_effectiveness)))
}
