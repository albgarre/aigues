% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ZIP.R
\name{ajustar_ZIP}
\alias{ajustar_ZIP}
\title{Ajusta un modelo zero-inflated Poisson a un set de datos}
\usage{
ajustar_ZIP(my_data, sample)
}
\arguments{
\item{my_data}{Tibble con los datos (ver \code{\link{cargar_datos}}).}

\item{sample}{Punto al que ajustar el modelo}
}
\value{
El modelo retornado por \code{\link{fitdist}}.
}
\description{
IMPORTANTE: Para que funcione, lanzar library(gamlss) en la sesión.
}
