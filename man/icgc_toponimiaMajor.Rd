\name{icgc_toponimiaMajor}
\alias{icgc_toponimiaMajor}
\docType{data}
\title{
Nomenclàtor oficial de toponímia major de Catalunya
}
\description{
Taula amb les dades de l'edició del 2020 amb columnes afegides amb informació
d'OSM tretes de `toponimsCat::municipis` (`municipi_*`) i
`toponimsCat::comarques` (`comarca_*`).
}
\usage{data("icgc_toponimiaMajor")}
\format{
  Un data frame amb 52923 files per les següents 34 columnes.
  \describe{
    \item{\code{Topònim}}{a character vector}
    \item{\code{Concepte}}{a character vector}
    \item{\code{Municipi.1}}{a character vector}
    \item{\code{Municipi.2}}{a character vector}
    \item{\code{Municipi.3}}{a character vector}
    \item{\code{Municipi.4}}{a character vector}
    \item{\code{Comarca.1}}{a character vector}
    \item{\code{Comarca.2}}{a character vector}
    \item{\code{Comarca.3}}{a character vector}
    \item{\code{Comarca.4}}{a character vector}
    \item{\code{Comarca.5}}{a character vector}
    \item{\code{UTM.X}}{a numeric vector}
    \item{\code{UTM.Y}}{a numeric vector}
    \item{\code{Volum}}{a numeric vector}
    \item{\code{Pàgina}}{a character vector}
    \item{\code{UTMX}}{Longitud del punt d'inserció en projecció ETRS89 UTM fus 31 Nord.}
    \item{\code{UTMY}}{Latitud del punt d'inserció en projecció ETRS89 UTM fus 31 Nord.}
    \item{\samp{name:icgc}}{Toponim normalitzat amb els articles a l'inici obtingut a partir de la columna `Topònim`.}
    \item{\code{filaICGC}}{Número de fila al fitxer original.}
    \item{\code{municipi_osm}}{Nom del municipi a OSM corresponent a `Municipi.1` de l'ICGC.}
    \item{\samp{municipi_name:ca}}{Etiqueta `name:ca` del municipi a OSM.}
    \item{\code{municipi_regio}}{Regio segons `toponimsCat::municipis`.}
    \item{\code{municipi_comarca}}{Comarca del municipi a OSM.}
    \item{\code{municipi_id}}{Identificador numèric del municipi a OSM.}
    \item{\code{municipi_type}}{Tipus d'objecte del municipi a OSM.}
    \item{\code{municipi_wikidata}}{Codi wikidata del municipi a OSM}
    \item{\code{municipi_admin_level}}{Etiqueta `admin_level` del municipi a OSM.}
    \item{\code{municipi_comarca.id}}{Identificador de la comarca del municipi a OSM.}
    \item{\code{comarca_osm}}{Nom de la comarca a OSM corresponent a `Comarca.1` de l'ICGC.}
    \item{\samp{comarca_name:ca}}{Etiqueta `name:ca` de la comarca a OSM.}
    \item{\code{comarca_regio}}{Regio segons `toponimsCat::comarca`.}
    \item{\code{comarca_id}}{Identificador numèric de la comarca a OSM.}
    \item{\code{comarca_type}}{Tipus d'objecte de la comarca a OSM.}
    \item{\code{comarca_admin_level}}{Etiqueta `admin_level` de la comarca a OSM.}
  }
}
\details{
Les coordenades del punt d'inserció dels topònims (`UTMX`, `UTMY`) està en la
projecció ETRS89 UTM fus 31 Nord (\url{https://epsg.io/25831}). Vegeu els
exemples.
}
\note{
Les dades estan sota llicència ODbL 1.0 segons l'\href{https://wiki.openstreetmap.org/wiki/File:20230109_CartaObertaICGC-OSM_SIGNADA.pdf}{acord amb l'ICGC}.
}
\source{
\url{https://icgc.cat/Ciutada/Informa-t/Llibres-i-fons-documentals/Llibres-en-PDF/Toponimia/Nomenclator-oficial-de-toponimia-major-de-Catalunya}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# Crea un objecte `sf` amb els punts de les coordenades
dgeo<- sf::st_as_sf(
  icgc_toponimiaMajor,
  coords=c("UTMX", "UTMY"),
  crs=sf::st_crs("EPSG:25831")
)
}
\seealso{
Altres dades:
\code{\link{icgc_toponimia}}
}
\concept{dades}
\keyword{datasets}
