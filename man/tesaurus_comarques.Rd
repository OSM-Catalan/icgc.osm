\name{tesaurus_comarques}
\alias{tesaurus_comarques}
\docType{data}
\title{
Tesaurus de comarques ICGC - OSM
}
\description{
Dades amb les relacions entre les comarques de l'ICGC i les d'OSM
}
\usage{tesaurus_comarques}
\format{
  Un data frame amb 48 files per les següents 5 columnes.
  \describe{
    \item{\code{icgc_NomCom}}{Nom de les comarques segons els camps \code{NomCom*} d'\code{\link{icgc_NGCat}}.}
    \item{\code{icgc_CodiCom}}{Codi de les comarques segons els camps \code{Comarca.*} d'\code{\link{icgc_toponimiaMajor}} i \code{CodiCom*} d'\code{\link{icgc_NGCat}}.}
    \item{\code{osm_name}}{Nom de les comarques a OSM.}
    \item{\code{osm_type}}{Tipus de l'element corresponent a OSM.}
    \item{\code{osm_id}}{Identificador de l'element corresponent a OSM.}
  }
}
\references{
Dades generades a \code{exec/tesaurus_territoris.r}.
}
\examples{
# Discrepàncies entre l'ICGC i OSM
tesaurus_comarques[which(tesaurus_comarques$icgc_nomCom != tesaurus_comarques$osm_name), ]
}
\seealso{
Altres dades:
\code{\link{tesaurus_municipis}}, \code{\link{icgc_NGCat}}, \code{\link{icgc_toponimiaMajor}},
\code{\link{tesaurus_iec_paisos}}
}
\concept{dades}
\keyword{datasets}
