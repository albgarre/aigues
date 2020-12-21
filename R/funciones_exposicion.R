
#' Simulacion de la exposicion en planta
#'
#' @param ZIP_model un modelo ZIP (ver \code{\link{ajustar_ZIP}}).
#' @param Days numero de dias incluido en la simulacion.
#' @param Rain_days Numero de días de lluvia durante la simulación.
#' @param sun_hrs_mode Moda del número de horas de sol por día.
#' @param irrigation_type Tipo de irrigacion (normal, goteo o noSplashing).
#' @param n_sims numero de simulaciones de Monte Carlo (1000 by default).
#'
#' @return Un tibble con tres columnas: day, count (recuento microbiano en CFU/mL)
#' and sim (numero de la simulacion de Monte Carlo).
#'
#' @importFrom tibble tibble
#' @importFrom purrr imap_dfr
#'
#' @export
#'
simular_riego <- function(ZIP_model, Days, Rain_Days, sun_hrs_mode,
                          irrigation_type = "normal",
                          nsims = 1000) {

  target_function <- switch(irrigation_type,
                            normal = Ec_Leafy2,
                            goteo = Ec_Leafy2_drip,
                            noSplashing = Ec_Leafy2_noSplashing,
                            stop("Unknown irrigation type:", irrigation_type))
  c(1:nsims) %>%
    map(.,
        ~ simular_ZIP(ZIP_model, n_sims = Days)
    ) %>%
    map(.,
        ~ tibble(day = 1:Days,
                 count = Ec_Leafy2(Days = Days, P_Ec_Water = 1,
                                   C_Ecoli_Irr_Water = ./100,  # /100 to converto to CFU/ml.
                                   Rain_Days = Rain_Days,
                                   sun_hrs_mode = sun_hrs_mode)
        )
    ) %>%
    imap_dfr(.,
             ~  mutate(.x, sim = .y)
    )
}

