\name{tesaurus_municipis}
\alias{tesaurus_municipis}
\docType{data}
\title{
Tesaurus de municipis ICGC - OSM
}
\description{
Dades amb les correspondències entre els municipis de l'ICGC i els d'OSM.
}
\usage{tesaurus_municipis}
\format{
  Un data frame amb 967 files per les següents 7 columnes.
  \describe{
    \item{\code{icgc_NomMun}}{Nom dels municipis segons els camps \code{NomMun*} d'\code{\link{icgc_NGCat}}.}
    \item{\code{icgc_CodiMun}}{Codi dels municipis segons els camps \code{CodiMun*} d'\code{\link{icgc_NGCat}}.}
    \item{\code{icgc_MunicipiTM}}{Nom dels municipis segons els camps \code{Municipi.*} d'\code{\link{icgc_toponimiaMajor}}.}
    \item{\code{osm_name}}{Nom de dels municipis a OSM.}
    \item{\code{wikidata}}{Etiqueta wikidata de l'element d'OSM.}
    \item{\code{osm_type}}{Tipus de l'element corresponent a OSM.}
    \item{\code{osm_id}}{Identificador de l'element corresponent a OSM.}
  }
}
\references{
Dades generades a \code{data-raw/tesaurus_territoris.R}.
}
\examples{
# Discrepàncies entre l'ICGC i OSM
tesaurus_municipis[which(tesaurus_municipis$icgc_NomMun != tesaurus_municipis$osm_name), ]
tesaurus_municipis[which(tesaurus_municipis$icgc_MunicipiTM != tesaurus_municipis$osm_name), ]
tesaurus_municipis[which(
  gsub(" \\\\*", "", tesaurus_municipis$icgc_MunicipiTM) != tesaurus_municipis$osm_name
), ]
}
\seealso{
Altres dades:
\code{\link{tesaurus_comarques}}, \code{\link{icgc_NGCat}}, \code{\link{icgc_toponimiaMajor}},
\code{\link{tesaurus_iec_paisos}}
}
\concept{dades}
\keyword{datasets}
