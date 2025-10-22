#' Compute events averted by vaccination
#' @param number_of_events number of events
#' @param cumulative_coverage cumulative vaccination coverage
#' @param vaccine_effectiveness vaccine effectiveness
#' @return estimated number of events averted
#' @export
#' @details The number of events averted by vaccination is calculated as described by Machado et al. (2019) <doi:10.2807/1560-7917.ES.2019.24.45.1900268>.
#' @rdname compute_events_averted_by_vaccination
#' @examples
#' data(coverage_and_incidence_mock_data)
#' data(ve_mock_data)
#' coverage <- coverage_and_incidence_mock_data$coverage_data
#' incidence <- coverage_and_incidence_mock_data$incidence_data
#' vaccine_effectiveness <- ve_mock_data$ve
#' nae <- compute_events_averted_by_vaccination(
#'   number_of_events = incidence$events,
#'   cumulative_coverage = coverage$cumulative_coverage,
#'   vaccine_effectiveness = vaccine_effectiveness
#' )
#' plot(nae, type = "l", xlab = "Time", ylab = "Events averted")
compute_events_averted_by_vaccination <- function(
  number_of_events,
  cumulative_coverage,
  vaccine_effectiveness
) {
  return(
    number_of_events *
    ((cumulative_coverage * vaccine_effectiveness) /
      (1 - (cumulative_coverage * vaccine_effectiveness)))
  )
}
