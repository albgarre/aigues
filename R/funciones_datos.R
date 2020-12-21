
#' Cargar datos de un Excel para el análisis
#'
#' @details El archivo de Excel debe tener 5 columnas en el siguiente orden:
#' \itemize{
#'  \item{Fecha de la muestra}
#'  \item{Identificador de la planta (EDAR, localización...)}
#'  \item{Identificador del punto de muestreo (Primario, PE...)}
#'  \item{Recuento microbioano. Se recomienda usar CFU/100 mL. Si no, habrá que hacer
#'  los cambios de unidades relevantes en las funciones de simulación.}
#'  \item{En el caso de recuentos sesgados (al menos X), el valor.}
#' }
#'
#' @param archivo Lugar donde esta guardado el fichero Excel. Se recomienda usar
#' paths relativos.
#' @param hoja Nombre de la hoja Excel donde estan los datos.
#'
#' @return
#' Un tibble con 5 columnas: fecha, planta, punto, recuento, rec_minimo.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr set_names
#' @importFrom readxl read_excel
#'
#' @export
#'
cargar_datos <- function(archivo, hoja) {

  read_excel(archivo, sheet = hoja) %>%
    set_names("fecha", "planta", "punto", "recuento", "rec_minimo")

}

#' Sustituye los datos sesgados por la mediana del punto de muestreo
#'
#' @param my_data Tibble con los datos (ver \code{\link{cargar_datos}}).
#'
#' @importFrom dplyr group_by mutate select
#'
#' @export
#'
sesgados_por_mediana <- function(my_data) {

  my_data %>%
    group_by(planta, punto) %>%
    mutate(rec_median = median(recuento, na.rm = TRUE)) %>%
    mutate(recuento_out = ifelse(is.na(recuento), rec_median, recuento)) %>%
    select(fecha, planta, punto, recuento = recuento_out)

}

#' Asigna el valor 0 a los datos de ausencia codificados con otro valor
#'
#' @param my_data Tibble con los datos (ver \code{\link{cargar_datos}}).
#' @param code_abscence Codigo utilizado en el Excel para los datos de ausencia
#'
#' @export
#'
codigo_ausencia <- function(my_data, code_abscence) {
  my_data %>%
    mutate(recuento = ifelse(recuento == code_abscence, 0, recuento))
}





