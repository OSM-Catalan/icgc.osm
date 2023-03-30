\name{icgc_NGCat}
\alias{icgc_NGCat}
\docType{data}
\title{
Noms geogràfics (NGCat)
}
\description{
Dades de la Toponímia de l'ICGC
}
\usage{icgc_NGCat}
\format{
  Un data frame amb 223130 files per les següents 19 columnes.
  \describe{
    \item{\code{Toponim}}{Nom de l'element.}
    \item{\code{Concepte}}{La categoria geogràfica.}
    \item{\samp{ref:icgc}}{Codi de referència format per \code{NGCat} + any del fitxer + número de fila del fitxer original.}
    \item{\code{NomCom[1-5]}}{La comarca o comarques a les quals pertany.}
    \item{\code{NomMun[1-5]}}{El municipi o municipis als quals pertany.}
    \item{\code{osm_idCom}}{Identificador numèric de la comarca (\code{NomCom1}) a OSM.}
    \item{\code{osm_idMun}}{Identificador numèric del municipi (\code{NomMun1}) a OSM.}
    \item{\code{osm_typeCom}}{Tipus d'objecte de la comarca a OSM.}
    \item{\code{osm_typeMun}}{Tipus d'objecte del municipi a OSM.}
    \item{\code{UTMX_ETRS89}}{Longitud del punt d'inserció.}
    \item{\code{UTMY_ETRS89}}{Latitud del punt d'inserció.}
  }
}
\details{
Les dades originals contenen 109 files duplicades que han estat eliminades.

Les coordenades(\code{UTMX_ETRS89}, \code{UTMY_ETRS89}) estan en metres i projecció ETRS89 UTM fus 31 Nord
(\url{https://epsg.io/25831}). Vegeu els exemples per veure com importar-les com a objectes espacials.
Les coordenades són les més properes a l'element geogràfic designat de què es disposa (poden referir-se al mateix
element geogràfic o al topònim; en aquest darrer cas, les coordenades poden correspondre a representacions del topònim
en cartografia de diferents escales).
}
\note{
Les dades estan sota llicència ODbL 1.0 segons l'\href{https://wiki.openstreetmap.org/wiki/File:20230109_CartaObertaICGC-OSM_SIGNADA.pdf}{acord amb l'ICGC}.
}
\source{
\url{https://icgc.cat/Descarregues/Llocs/Noms-geografics-NGCat}
}
\references{
Dades generades a \code{exec/ICGC-NGCat.r}.
}
\examples{
# Crea un objecte `sf` amb els punts de les coordenades
\dontrun{
dgeo<- sf::st_as_sf(
  icgc_NGCat,
  coords=c("UTMX_ETRS89", "UTMY_ETRS89"),
  crs=sf::st_crs("EPSG:25831")
)
}
}
\seealso{
Altres dades:
\code{\link{tesaurus_municipis}}, \code{\link{tesaurus_comarques}}, \code{\link{icgc_toponimiaMajor}}
}
\concept{dades}
\keyword{datasets}
