
<!-- README.md is generated from README.Rmd. Please edit that file -->

# icgc.osm <a href="https://osm-catalan.github.io/icgc.osm"><img src='man/figures/logo.svg' align="right" height=110 alt="Web del projecte"/></a>

<!-- badges: start -->

[![pre-commit](https://github.com/OSM-Catalan/icgc.osm/actions/workflows/pre-commit.yaml/badge.svg)](https://github.com/OSM-Catalan/icgc.osm/actions/workflows/pre-commit.yaml)
[![R-CMD-check](https://github.com/OSM-Catalan/icgc.osm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OSM-Catalan/icgc.osm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/OSM-Catalan/icgc.osm/graph/badge.svg)](https://app.codecov.io/gh/OSM-Catalan/icgc.osm)
<!-- badges: end -->

`icgc.osm` és un repositori que serveix per coordinar la col·laboració
entre l’Institut Cartogràfic i Geològic de Catalunya (ICGC) i
OpenStreetMap (OSM) un cop [autoritzada la reutilització de les dades de
l’ICGC](https://wiki.openstreetmap.org/wiki/File:20230109_CartaObertaICGC-OSM_SIGNADA.pdf).

## Instal·lació del paquet d’R

``` r
devtools::install_github("OSM-Catalan/icgc.osm")
```

## Exemples

Mostra tots els caps de la base de dades de l’ICGC

``` r
library(icgc.osm)
caps <- icgc_NGCat[grepl("^Cap ", icgc_NGCat$Toponim) & icgc_NGCat$Concepte == "lit.", ]
caps <- sf::st_as_sf(
  caps,
  coords=c("UTMX_ETRS89", "UTMY_ETRS89"),
  crs=sf::st_crs("EPSG:25831")
)
mapview::mapview(caps)
```

![](man/figures/caps.png)
