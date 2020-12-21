
#' Genera un resampling basado en bootstrap
#'
#' @param my_data tibble con los datos (ver \code{\link{cargar_datos}}).
#' @param nodes puntos de muestreo a utilizar
#' @param n_samples numero de repeticiones a incluir en cada bootstrap sample.
#' @param n_boots numero de bootstrap samples.
#'
#' @return Una lista en que cada miembro es un tibble con los resultados de cada
#' bootstrap sample.
#'
#' @importFrom dplyr ungroup filter mutate sample_n
#' @importFrom purrr map map_dfr
#' @importFrom tidyr spread
#'
#' @export
#'
bootstrap_data <- function(my_data, nodes, n_samples = 3, n_boots = 100) {

  d <- my_data %>%
    ungroup() %>%
    filter(punto %in% nodes) %>%
    mutate(punto = factor(punto, levels = nodes))

  lapply(1:n_boots, function(x) {

    d %>%
      split(.$fecha) %>%
      map(.,
          ~  split(., .$punto, drop = TRUE)
      ) %>%
      map(.,
          ~  map(., ~ sample_n(., n_samples, replace = TRUE))
      ) %>%
      map_dfr(.,
              ~ map_dfr(., ~ mutate(., boot_sample = 1:n_samples))
      ) %>%
      spread(punto, recuento)

  })
}


#' Define una network en base a una vector de nodos
#'
#' @param nodes un vector con los nombres de los nodos
#'
#' Define un grafo recto en que cada nodo es hijo del anterior.
#'
#' @importFrom bnlearn model2network
#'
#' @export
#'
definir_bn <- function(nodes) {

  out <- paste0("[", nodes[[1]], "]")

  for (i in 2:length(nodes)) {
    new_node <- paste0("[", nodes[[i]], "|", nodes[[i-1]], "]")
    out <- paste0(new_node, out)
  }

  my_net <- model2network(out)

  my_net

}



#' Ajuste de una Red Bayesiana a un set de datos
#'
#' @param network network definida con \code{\link{definir_bn}}.
#' @param data tibble con los datos (ver \code{\link{cargar_datos}}).
#'
#' @importFrom dplyr select mutate_all
#' @importFrom bnlearn bn.fit
#'
#' @export
#'
ajustar_bn <- function(network, data) {

  data %>%
    select(., names(network$nodes)) %>%
    mutate_all(log10) %>%
    bn.fit(network, .)
}

#' Ajuste de Red Bayesianas a una lista de bootstraped data
#'
#' @param network network definida con \code{\link{definir_bn}}.
#' @param data una lista de tibble de acuerdo a \code{\link{bootstrap_data}}.
#'
#' @export
#'
ajustar_bn_boot <- function(network, bootstraped_data) {

  redes_bayesianas <- bootstraped_data %>%
    map(~  ajustar_bn(network, .))

}

#' Extrae los parámetros de una lista de Redes Bayesianas
#'
#' @param my_nets una lista de redes bayesianas de acuerdo a \code{\link{ajustar_bn_boot}}.
#'
#' @importFrom purrr map imap_dfr
#' @importFrom tibble rownames_to_column
#'
#' @export
#'
#'
extraer_pars_de_bn <- function(my_nets) {
  my_nets %>%
    map(coef) %>%
    map(.,
        ~ map(., as.data.frame)
    ) %>%
    map(.,
        ~ map(., ~ rownames_to_column(., "par"))
    ) %>%
    map(.,
        ~ imap_dfr(., ~ mutate(.x, node = .y))
    ) %>%
    imap_dfr(., ~ mutate(.x, net = .y)) %>%
    set_names(c("par", "value", "node", "net"))
}

#' Muestra la incertidumbre de los parametros usando cajas y bigotes
#'
#' @param parametros_bn el resultado de \code{\link{extraer_pars_de_bn}}.
#' @param orden orden de los parametros
#'
#' @importFrom ggplot2 ggplot geom_boxplot facet_wrap
#'
#' @export
#'
plot_parametros_bn <- function(parametros_bn, orden) {

  parametros_bn %>%
    mutate(node = factor(node, levels = orden)) %>%
    mutate(par = ifelse(par == "(Intercept)", "Intercept", "Slope")) %>%
    ggplot(.) +
    geom_boxplot(aes(x = par, y = value)) +
    facet_wrap("node", scales = "free_x") +
    xlab("") +
    ylab("Distribution of parameter values in bootstrapped BN")

}

#' Resumen de la variacion de los parametros de una bootstrapped BN
#'
#' @param parametros_bn el resultado de \code{\link{extraer_pars_de_bn}}.
#'
#' @importFrom dplyr group_by summarize
#'
#' @export
#'
resumen_parametros_bn <- function(parametros_bn) {

  parametros_bn %>%
    group_by(node, par) %>%
    summarize(mediana = median(value), desv.standard =  sd(value))


}




