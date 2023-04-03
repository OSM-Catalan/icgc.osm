#' Consulta etiquetes d'OSM
#'
#' @param x un `data.frame` amb les columnes `osm_type` i `osm_id`.
#' @param etiquetes noms de les claus de les etiquetes a consultar. Si no s'especifica, s'afegeixen totes les etiquetes
#'   dels objectes.
#'
#' @return Retorna `x` amb les etiquetes dels objectes com a columnes. Si les columnes ja existien, actualitza els
#'   valors de les etiquetes i conserva l'ordre de les columnes originals afegint les noves al final.
#' @export
#'
#' @examples
#' sel_comarques <- !tesaurus_comarques$icgc_NomCom %in% c("Andorra", "Aragó", "Catalunya", "França", "País Valencià")
#' comarques <- consulta_etiquetes_osm(tesaurus_comarques[sel_comarques, ])
consulta_etiquetes_osm <- function(x, etiquetes) {
  if (!all(c("osm_id", "osm_type") %in% names(x))) {
    stop("Les dades no contenen columnes `osm_type` i `osm_id` que permetin identificar objectes d'OSM.")
  }

  x_unic <- unique(x[, c("osm_type", "osm_id")]) # minimitza la consulta

  consulta_osm <- osmdata::opq_osm_id(
    id = x_unic$osm_id,
    type = x_unic$osm_type,
    out = "tags"
  )
  if (!missing(etiquetes)) {
    consulta <- osmdata::opq_csv(consulta, fields = c("::id", "::type", etiquetes))
  }

  etiquetes <- osmdata::osmdata_data_frame(consulta_osm)
  names(etiquetes) <- gsub("^@", "osm_", names(etiquetes)) # per consultes csv, @id -> osm_id i @type -> osm_type


  columnes_actualitzades <- setdiff(intersect(names(x), names(etiquetes)), c("osm_id", "osm_type"))
  x$`_ordre_files_` <- seq_len(nrow(x))
  out <- merge(x[, setdiff(names(x), columnes_actualitzades)], etiquetes, by = c("osm_id", "osm_type"))

  # Conserva l'ordre de les files i columnes original.
  out <- out[order(out$`_ordre_files_`), unique(c(names(x), names(out)))]
  out$`_ordre_files_` <- NULL

  return(out)
}
