\name{icgc_toponimiaMajor}
\alias{icgc_toponimiaMajor}
\docType{data}
\title{
Nomenclàtor oficial de toponímia major de Catalunya
}
\description{
Taula amb les dades de l'edició del 2020.
}
\usage{icgc_toponimiaMajor}
\format{
  Un data frame amb 52709 files per les següents 19 columnes.
  \describe{
    \item{\samp{name:icgc}}{Topònim normalitzat amb els articles a l'inici, obtingut a partir de la columna \code{Topònim}.}
    \item{\code{Concepte}}{La categoria geogràfica a què s'ha associat el topònim.}
    \item{\samp{ref:icgc}}{Número de fila al fitxer original.}
    \item{\code{Comarca.[1-5]}}{a character vector}
    \item{\code{Municipi.[1-4]}}{a character vector}
    \item{\code{osm_idCom}}{Identificador numèric de la comarca (\code{Comarca.1}) a OSM.}
    \item{\code{osm_idMun}}{Identificador numèric del municipi (\code{Municipi.1}) a OSM.}
    \item{\code{osm_typeCom}}{Tipus d'objecte de la comarca a OSM.}
    \item{\code{osm_typeMun}}{Tipus d'objecte del municipi a OSM.}
    \item{\code{Topònim}}{Nom original al recull de toponímia major. Vegeu \code{name:icgc}.}
    \item{\code{UTMX}}{Longitud del punt d'inserció.}
    \item{\code{UTMY}}{Latitud del punt d'inserció.}
  }
}
\details{
Les dades originals contenen 3 files duplicades que han estat eliminades.

Les coordenades(\code{UTMX}, \code{UTMY}) estan en metres i projecció ETRS89 UTM fus 31 Nord
(\url{https://epsg.io/25831}). Vegeu els exemples per veure com importar-les com a objectes espacials.
Les coordenades són les més properes a l'element geogràfic designat de què es disposa (poden referir-se al mateix
element geogràfic o al topònim; en aquest darrer cas, les coordenades poden correspondre a representacions del topònim
en cartografia de diferents escales).
}
\note{
Les dades estan sota llicència ODbL 1.0 segons l'\href{https://wiki.openstreetmap.org/wiki/File:20230109_CartaObertaICGC-OSM_SIGNADA.pdf}{acord amb l'ICGC}.
}
\source{
\url{https://icgc.cat/Ciutada/Informa-t/Llibres-i-fons-documentals/Llibres-en-PDF/Toponimia/Nomenclator-oficial-de-toponimia-major-de-Catalunya}
}
\references{
Dades generades a \code{exec/ICGC_toponimia_major.r}.
}
\examples{
# Crea un objecte `sf` amb els punts de les coordenades
\dontrun{
dgeo<- sf::st_as_sf(
  icgc_toponimiaMajor,
  coords=c("UTMX", "UTMY"),
  crs=sf::st_crs("EPSG:25831")
)
}
}
\seealso{
Altres dades:
\code{\link{tesaurus_municipis}}, \code{\link{tesaurus_comarques}}, \code{\link{icgc_NGCat}}
}
\concept{dades}
\keyword{datasets}
