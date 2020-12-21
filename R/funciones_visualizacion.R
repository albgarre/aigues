
#' Grafico de cajas y bigotes
#'
#' @param my_data Tibble con los datos (ver \code{\link{cargar_datos}}).
#' @param resaltar_sesgados Mostrar los valores sesgados como rombos rojos (FALSE by default).
#' @param orden_x Vector describiendo el orden de las cajas (alfabeticamente by default).
#' @param eje_x Nombre del eje x.
#' @param eje_y Nombre del eje y.
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_boxplot scale_y_log10 geom_point xlab ylab
#'
#' @export
#'
cajas_y_bigotes <- function(my_data, resaltar_sesgados = FALSE, orden_x = NULL,
                            eje_x = NULL, eje_y = NULL) {


  if (!is.null(orden_x)) {

    my_data <- my_data %>%
      mutate(punto = factor(punto, levels = orden_x))

  }

  p <- ggplot(my_data, aes(x = punto)) +
    geom_boxplot(aes(y = recuento)) +
    scale_y_log10()

  if (isTRUE(resaltar_sesgados)) {
    p <- p + geom_point(aes(y = rec_minimo), colour = "red",
                        size = 4, shape = 18)
  }

  if (!is.null(eje_x)) {
    p <- p + xlab(eje_x)
  }

  if (!is.null(eje_y)) {
    p <- p + ylab(eje_y)
  }

  p

}

#' Muestra el histograma del recuento en un punto de muestreo
#'
#' @param my_data tibble con los datos (ver \code{\link{cargar_datos}}).
#' @param sample punto de muestra.
#' @param eje_x nombre del eje x
#' @param eje_y nombre del eje  y
#'
#' @importFrom ggplot2 geom_histogram ggplot xlab ylab
#' @importFrom dplyr filter
#'
#' @export
#'
histograma_punto <- function(my_data, sample, eje_x = NULL, eje_y = NULL) {

  p <- my_data %>%
    filter(punto == sample) %>%
    ggplot() +
    geom_histogram(aes(recuento))

  if (!is.null(eje_x)) {
    p <- p + xlab(eje_x)
  }

  if (!is.null(eje_y)) {
    p <- p + ylab(eje_y)
  }

  p

}

#' Representacion de la variacion de la exposicion en planta
#'
#' La linea representa le mediana de la concentracion de Ecoli,
#' las zonas sombreadas el intervalo al 80 y al 90% de confianza.
#'
#' @param simulations exposicion de Ecoli simulada en la planta de acuerdo a
#' \code{\link{simular_riego}}.
#' @param eje_x nombre del eje x.
#' @param eje_y nombre del eje y.
#'
#' @importFrom dplyr ungroup summarize
#' @importFrom ggplot2 ggplot geom_line geom_ribbon aes scale_y_log10
#'
#' @export
#'
grafica_exposicion <- function(simulations, eje_x = NULL, eje_y = NULL) {

  p <- simulations %>%
    ungroup() %>%
    group_by(day) %>%
    summarize(m_count = median(count),
              q10 = quantile(count, probs = .1),
              q90 = quantile(count, probs = .9),
              q95 = quantile(count, probs = .95),
              q05 = quantile(count, probs = .05)
    ) %>%
    ggplot(aes(x = day)) +
    geom_line(aes(y = m_count)) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = .5) +
    scale_y_log10()

  if (!is.null(eje_x)) {
    p <- p + xlab(eje_x)
  }

  if (!is.null(eje_y)) {
    p <- p + ylab(eje_y)
  }

  p

}





