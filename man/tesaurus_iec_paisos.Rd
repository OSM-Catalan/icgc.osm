\name{tesaurus_iec_paisos}
\alias{tesaurus_iec_paisos}
\docType{data}
\title{
Tesaurus de països i territoris IEC - OSM
}
\description{
Dades amb les correspondències entre els països i territoris del Nomenclàtor Mundial de l'IEC i OSM.
}
\usage{tesaurus_iec_paisos}
\format{
  Un data frame amb 253 files per les següents 7 columnes.
  \describe{
    \item{\code{iec_nom}}{Nom dels territoris segons l'IEC.}
    \item{\code{iec_pais}}{Nom original dels territoris segons l'IEC, corresponent al camps \code{pais} d'\code{\link{iec_nomenclatorMundial}}.}
    \item{\samp{name:ca}}{Valor de l'etiqueta l'etiqueta \code{name:ca} corresponent a OSM.}
    \item{\code{name}}{Valor de l'etiqueta \code{name} a OSM.}
    \item{\code{wikidata}}{Etiqueta wikidata de l'element d'OSM.}
    \item{\code{osm_type}}{Tipus de l'element corresponent a OSM.}
    \item{\code{osm_id}}{Identificador de l'element corresponent a OSM.}
  }
}
\references{
Dades generades a \code{data-raw/tesaurus_iec_paisos.R}.
}
\examples{
# Discrepàncies entre l'IEC i OSM
disc <- which(tesaurus_iec_paisos$iec_nom != tesaurus_iec_paisos$`name:ca`)
tesaurus_iec_paisos[disc, c("iec_nom", "name:ca", "name")]
}
\seealso{
Altres dades:
\code{\link{iec_nomenclatorMundial}}, \code{\link{tesaurus_comarques}}, \code{\link{tesaurus_municipis}},
\code{\link{icgc_NGCat}}, \code{\link{icgc_toponimiaMajor}}
}
\concept{dades}
\keyword{datasets}
