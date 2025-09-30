## Prepara la base de dades del Nomenclàtor Mundial de l'IEC
# https://nomenclator-mundial.iec.cat/
# Base de dades sincronitzada amb les de l'ICGC usades a
# https://www.icgc.cat/ca/Geoinformacio-i-mapes/Servei-de-Mapa-Base
# IEC en té uns uns 7000. L'ICGC en té més (uns 12000)

library(icgc.osm)
library(jsonlite)


## IEC: Nomenclàtor Mundial ----

d <- jsonlite::fromJSON("data-raw/nomenclator-mundial-iec.json")
# geo <- sf::st_as_sf(
#   d, coords = c("longitude", "latitude"),
#   na.fail = FALSE, crs = sf::st_crs(4326)
# )
# mapview::mapview(geo[, !sapply(geo, is.list)])


## Correccions ----
sort(table(d$is_in, useNA = "ifany"))

# Espais en blanc doblats
lapply(d, function(x) sum(grepl("\\s{2,}", x)))
grepv("\\s{2,}", d$`name:ca`)
grepv("\\s{2,}", d$name)
grepv("\\s{2,}", d$note)

## Test
gsub("\\s{2,}", " ", grepv("\\s{2,}", d$`name:ca`))
gsub("\\s{2,}", " ", grepv("\\s{2,}", d$name)) # relacionat amb alfabets no llatins?
data.frame(
  doble = grepv("\\s{2,}", d$name),
  simple = gsub("\\s{2,}", " ", grepv("\\s{2,}", d$name))
)

## Corregeix
d$`name:ca` <- lapply(d$`name:ca`, function(x) gsub("\\s{2,}", " ", x))


# Espai després d'apòstrof
grepv("' ", d$`name:ca`)
grepv("' ", d$name) # correcte

## Corregeix
d$`name:ca` <- lapply(d$`name:ca`, function(x) gsub("' ", "'", x))
grepv("'", d$`name:ca`)


# "" -> NA
sapply(d, function(x) sum(x == "", na.rm = TRUE))
d[which(d$is_in == ""), ]
d[which(d$`name:ca` == ""), ]
## CONCLUSIÓ: elements sense informació a la web

## Corregeix
d$is_in[which(d$is_in == "")] <- NA_character_
d$`name:ca`[which(d$`name:ca` == "")] <- NA_character_


# Elements amb NA
sapply(d, function(x) sum(is.na(x)))
d[is.na(d$is_in), ]
d[is.na(d$`name:ca`), ] # per eliminar
d[is.na(d$entity_type), ] # per eliminar
d[is.na(d$latitude), ] # Per eliminar
## CONCLUSIÓ: elements amb name:ca amb NAs sense informació a la web. Elimina

## Corregeix
elimina <- vapply(d$`name:ca`, function(x) {
  all(is.na(x))
}, FUN.VALUE = logical(1))
d <- d[!elimina, ]

# Múltiples noms
table(sapply(d$`name:ca`, length))
table(sapply(d$name, length))
d$`name:ca`[sapply(d$`name:ca`, length) > 1]
d$name[sapply(d$name, length) > 1]
d[sapply(d$`name:ca`, length) > 1 & sapply(d$name, length) > 1, ]


### Coordenades ----

coord <- d[, c("longitude", "latitude")]
coord[] <- lapply(coord, as.numeric)
lapply(coord, summary)

plot(coord)
coord$latitude[abs(coord$latitude) > 90] <- NA_real_
coord$longitude[abs(coord$longitude) > 180] <- NA_real_
plot(coord)

## Corregeix
sel_err <- which(abs(as.numeric(d$latitude)) > 90)
d[sel_err, ]

d$note[sel_err] <- paste(
  "Coordenades originals incorrectes:",
  apply(d[sel_err, c("latitude", "longitude")], 1, paste, collapse = ", ")
)
d$latitude[sel_err] <- NA
d$longitude[sel_err] <- NA


## Combina els paisos amb el tesaurus d'OSM ----

tes <- tesaurus_iec_paisos[, c("iec_pais", "osm_type", "osm_id")]
names(tes) <- c("iec_pais", "osm_typePais", "osm_idPais")
d <- merge(d, tes, by.x = "is_in", by.y = "iec_pais", all.x = TRUE)


## Desa ----

iec_nomenclatorMundial <- d
names(iec_nomenclatorMundial) <- c(
  "iec_pais", "iec_id", "iec_nom", "iec_nom_local", "tipus_entitat", "latitud",
  "longitud", "url", "nota", "osm_typePais", "osm_idPais"
)
ord_cols <- c(
  "iec_id", "iec_pais", "iec_nom", "iec_nom_local", "tipus_entitat", "latitud",
  "longitud", "url", "nota", "osm_typePais", "osm_idPais"
)
iec_nomenclatorMundial <- iec_nomenclatorMundial[, ord_cols]
usethis::use_data(iec_nomenclatorMundial, overwrite = TRUE)
load("data/iec_nomenclatorMundial.rda", verbose = TRUE) # iec_nomenclatorMundial

openxlsx::write.xlsx(
  iec_nomenclatorMundial,
  file = "inst/resultats/iec_nomenclatorMundial.xlsx",
  rowNames = FALSE, borders = "surrounding", colWidths = "auto",
  firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)


## Visualitza ----

library(tmap)

tmap_mode("view")
tmap_mode("plot")
geo <- sf::st_as_sf(iec_nomenclatorMundial, coords = c("longitud", "latitud"), crs = sf::st_crs(4326), na.fail = FALSE)

tm_shape(geo) +
  tm_dots(
    size = 0.2, fill = "iec_pais",
    fill.scale = tm_scale_categorical(n.max = 100, values = "poly.palette36"),
    fill.legend = NULL
  ) +
  tm_basemap("OpenStreetMap")
