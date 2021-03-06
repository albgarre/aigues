% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funciones_visualizacion.R
\name{grafica_exposicion}
\alias{grafica_exposicion}
\title{Representacion de la variacion de la exposicion en planta}
\usage{
grafica_exposicion(simulations, eje_x = NULL, eje_y = NULL)
}
\arguments{
\item{simulations}{exposicion de Ecoli simulada en la planta de acuerdo a
\code{\link{simular_riego}}.}

\item{eje_x}{nombre del eje x.}

\item{eje_y}{nombre del eje y.}
}
\description{
La linea representa le mediana de la concentracion de Ecoli,
las zonas sombreadas el intervalo al 80 y al 90\% de confianza.
}
