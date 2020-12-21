
#' Ajusta un modelo zero-inflated Poisson a un set de datos
#'
#' IMPORTANTE: Para que funcione, lanzar library(gamlss) en la sesión.
#'
#' @param my_data Tibble con los datos (ver \code{\link{cargar_datos}}).
#' @param sample Punto al que ajustar el modelo
#'
#' @importFrom dplyr filter pull
#' @importFrom fitdistrplus fitdist
#' @importFrom gamlss.dist ZIP dZIP rZIP qZIP pZIP
#'
#' @return El modelo retornado por \code{\link{fitdist}}.
#'
#' @export
#'
ajustar_ZIP <- function(my_data, sample) {

  recuentos <- my_data %>%
    filter(punto == sample) %>%
    filter(!is.na(recuento)) %>%
    pull(recuento)

  start_sigma <- mean(recuentos == 0, na.rm = TRUE)
  max_mu <- max(recuentos, na.rm = TRUE)
  start_mu <- mean(recuentos[recuentos != 0], na.rm = TRUE)

  fit_zip <- fitdist(recuentos, 'ZIP',
                     method = "mle",
                     start = list(mu = start_mu, sigma = start_sigma),
                     upper = c(max_mu, 1),
                     lower = c(0, 0))

  fit_zip
}

#' Extrae los parametros de un modelo ZIP
#'
#' @param ZIP_model un modelo ZIP (ver \code{\link{ajustar_ZIP}}).
#'
#' @export
#'
parametros_ZIP <- function(ZIP_model) {

  ZIP_model$estimate

}


#' Simulación de una ZIP usando Monte Carlo
#'
#' @param ZIP_model un modelo ZIP (ver \code{\link{ajustar_ZIP}}).
#' @param n_sims numero de simulaciones de Monte Carlo (1000 by default).
#'
#' @export
#'
simular_ZIP <- function(model, n_sims = 1000) {

  mu <- model$estimate[["mu"]]
  sigma <- model$estimate[["sigma"]]

  out <- rpois(n_sims, mu)
  out <- out*sample(c(0, 1), n_sims, replace = TRUE, prob = c(sigma, 1-sigma))

  out
}










