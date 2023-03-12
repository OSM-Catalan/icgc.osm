\name{icgc_toponimia}
\alias{icgc_toponimia}
\docType{data}
\title{
Dades de la Toponímia de l'ICGC
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("icgc_toponimia")}
\format{
  A data frame with 225445 observations on the following 34 variables.
  \describe{
    \item{\code{filaICGC}}{Número de fila del fitxer original}
    \item{\code{Toponim}}{}
    \item{\code{CodiGeo}}{a numeric vector}
    \item{\code{Concepte}}{a character vector}
    \item{\code{CodiMun1}}{a numeric vector}
    \item{\code{NomMun1}}{a character vector}
    \item{\code{CodiMun2}}{a numeric vector}
    \item{\code{NomMun2}}{a character vector}
    \item{\code{CodiMun3}}{a numeric vector}
    \item{\code{NomMun3}}{a character vector}
    \item{\code{CodiMun4}}{a numeric vector}
    \item{\code{NomMun4}}{a character vector}
    \item{\code{CodiMun5}}{a numeric vector}
    \item{\code{NomMun5}}{a character vector}
    \item{\code{CodiCom1}}{a character vector}
    \item{\code{NomCom1}}{a character vector}
    \item{\code{CodiCom2}}{a character vector}
    \item{\code{NomCom2}}{a character vector}
    \item{\code{CodiCom3}}{a character vector}
    \item{\code{NomCom3}}{a character vector}
    \item{\code{CodiCom4}}{a character vector}
    \item{\code{NomCom4}}{a character vector}
    \item{\code{CodiCom5}}{a character vector}
    \item{\code{NomCom5}}{a character vector}
    \item{\code{UTMX_ETRS89}}{Longitud del punt d'inserció en projecció ETRS89 UTM fus 31 Nord.}
    \item{\code{UTMY_ETRS89}}{Latitud del punt d'inserció en projecció ETRS89 UTM fus 31 Nord.}
    \item{\code{municipi_osm}}{Nom del municipi a OSM corresponent a `Municipi.1` de l'ICGC.}
    \item{\samp{municipi_name:ca}}{Etiqueta `name:ca` del municipi a OSM.}
    \item{\code{municipi_type}}{Tipus d'objecte del municipi a OSM.}
    \item{\code{municipi_id}}{Identificador numèric del municipi a OSM.}
    \item{\samp{comarca_name:ca}}{Etiqueta `name:ca` de la comarca a OSM.}
    \item{\code{comarca_type}}{a character vector}
    \item{\code{comarca_id}}{Tipus d'objecte de la comarca a OSM.}
    \item{\code{filaToponimiaMajor}}{Fila del topònim corresponent a \code{\link{icgc_toponimiaMajor}}, columna `filaICGC`.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
Les coordenades del punt d'inserció dels topònims (`UTMX_ETRS89`, `UTMY_ETRS89`) està en la projecció ETRS89 UTM fus 31 Nord (\url{https://epsg.io/25831}). Vegeu els exemples.
}
\source{
\url{https://icgc.cat/Descarregues/Llocs/Noms-geografics-NGCat}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(icgc_toponimia)
# Crea un objecte `sf` amb els punts de les coordenades
dgeo<- sf::st_as_sf(icgc_toponimia, coords=c("UTMX_ETRS89", "UTMY_ETRS89"), crs=sf::st_crs("EPSG:25831"))
}
\keyword{datasets}
