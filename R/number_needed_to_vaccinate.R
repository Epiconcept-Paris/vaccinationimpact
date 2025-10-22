#' Compute the number of individuals needed to vaccinate to prevent
#' one event according to Machado et al. method
#' @param number_of_events_averted number of events averted
#' @param population_size population size
#' @inheritParams compute_events_averted_by_vaccination
#' @return The number of individuals needed to vaccinate to avert
#' one event
#' @details The number of individuals needed to vaccinate to prevent one event is calculated as described by Machado et al. (2019) <doi:10.2807/1560-7917.ES.2019.24.45.1900268>.
#' @export
#' @rdname compute_number_needed_to_vaccinate_machado
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
#' nnv_machado <- compute_number_needed_to_vaccinate_machado(
#'   number_of_events = incidence$events,
#'   number_of_events_averted = nae,
#'   population_size = 1234,
#'   vaccine_effectiveness = vaccine_effectiveness
#' )
#' nnv_machado
compute_number_needed_to_vaccinate_machado <- function(
  number_of_events,
  number_of_events_averted,
  population_size,
  vaccine_effectiveness
) {
  Rb <- (number_of_events + number_of_events_averted) / population_size
  NNv <- 1 / (Rb * vaccine_effectiveness)
  NNv[is.infinite(NNv)] <- NA
  return(NNv)
}

#' Compute the number of individuals needed to vaccinate to prevent
#' one event according to Tuite and Fisman method
#' @return The number of individuals needed to vaccinate to avert
#' one event
#' @export
#' @param number_of_vaccinated number of vaccinated individuals
#' @inheritParams compute_number_needed_to_vaccinate_machado
#' @rdname compute_number_needed_to_vaccinate_tuite_fisman
#' @details The number of individuals needed to vaccinate to prevent one event is calculated as described by Tuite and Fisman (2013) <doi:10.1016/j.vaccine.2012.11.097>.
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
#' nnv_tuite_fisman <- compute_number_needed_to_vaccinate_tuite_fisman(
#'   number_of_vaccinated = coverage$number_of_vaccinated,
#'   number_of_events_averted = nae
#' )
#' nnv_tuite_fisman
compute_number_needed_to_vaccinate_tuite_fisman <- function(
  number_of_vaccinated,
  number_of_events_averted
) {
  NNv <- number_of_vaccinated / number_of_events_averted
  NNv[is.infinite(NNv)] <- NA
  NNv[is.nan(NNv)] <- NA
  NNv[NNv == 0] <- NA
  return(NNv)
}
