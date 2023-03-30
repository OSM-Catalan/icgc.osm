.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "Dades sota llic\u00E8ncia ODbL 1.0. (c) Col\u00B7laboradors d'OpenStreetMap ",
    "https://www.openstreetmap.org/copyright i ICGC ",
    "https://wiki.openstreetmap.org/wiki/File:20230109_CartaObertaICGC-OSM_SIGNADA.pdf"
  )
  packageStartupMessage(msg)
}
