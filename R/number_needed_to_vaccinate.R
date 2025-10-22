#' Compute the number of individuals needed to vaccinate to prevent
#' one event according to Machado et al. method
#' @param number_of_events_averted number of events averted
#' @param pop_at_risk population at risk
#' @inheritParams compute_events_averted_by_vaccination
#' @return The number of individuals needed to vaccinate to avert
#' one event
#' @details The number of individuals needed to vaccinate to prevent one event is calculated as described by Machado et al. (2019) <doi:10.2807/1560-7917.ES.2019.24.45.1900268>.
#' @export
#' @rdname vaccine_impact
#' @examples

compute_number_needed_to_vaccinate_machado <- function(
  number_of_events,
  number_of_events_averted,
  pop_at_risk,
  vaccine_effectiveness
) {
  Rb <- (number_of_events + number_of_events_averted) / pop_at_risk
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
#' @rdname vaccine_impact
#' @details The number of individuals needed to vaccinate to prevent one event is calculated as described by Tuite and Fisman (2013) <doi:10.1016/j.vaccine.2012.11.097>.
#' @examples

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
