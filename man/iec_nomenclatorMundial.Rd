\name{iec_nomenclatorMundial}
\alias{iec_nomenclatorMundial}
\docType{data}
\title{
Nomenclàtor mundial de l'Institut d'Estudis Catalans
}
\description{
Conté exotopònims d'arreu del món en català.
}
\usage{iec_nomenclatorMundial}
\format{
  Un data frame amb 7076 files per les següents 9 columnes.
  \describe{
    \item{\code{iec_id}}{Identificador únic de l'IEC.}
    \item{\code{iec_pais}}{Nom del país on es troba el topònim}
    \item{\samp{iec_nom}}{Una llista de vectors amb els topònims en català.}
    \item{\code{iec_nom_local}}{Una llista de vectors amb els topònims en la llengua local.}
    \item{\code{tipus_entitat}}{Tipus de l'element}
    \item{\code{latitud}}{Latitud en º.}
    \item{\code{longitud}}{Longitud en º.}
    \item{\code{url}}{Adreça web de la pàgina on s'ha extret.}
    \item{\code{nota}}{Notes.}
    \item{\code{osm_typePais}}{Tipus d'objecte del país (\code{iec_pais}) a OSM.}
    \item{\code{osm_idPais}}{Identificador numèric del país (\code{iec_pais}) a OSM.}
  }
}
\source{
\url{https://nomenclator-mundial.iec.cat}
}
\references{
Dades generades a \code{data-raw/IEC-nomenclator_mundial.R}.
}
\examples{
# Crea un objecte `sf` amb els punts de les coordenades
\dontrun{
library(sf)
geo <- st_as_sf(
  iec_nomenclatorMundial,
  coords = c("longitud", "latitud"),
  crs = st_crs(4326), na.fail = FALSE)
)
plot(geo[, "iec_pais"])
}
}
\seealso{
Altres dades:
\code{\link{tesaurus_iec_paisos}}, \code{\link{icgc_NGCat}}, \code{\link{icgc_toponimiaMajor}}
}
\concept{dades}
\keyword{datasets}

