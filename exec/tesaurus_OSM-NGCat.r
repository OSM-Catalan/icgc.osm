library(icgc.osm)
library(osmdata)
library(sf)
library(pbapply)


## Combinació de topònims ICGC - OSM ----

icgc_NGCat_municipis <- by(icgc_NGCat, icgc_NGCat$osm_idMun, function(x) x)

summary(sapply(icgc_NGCat_municipis, nrow))
summary(sapply(icgc_NGCat_municipis, function(x) length(unique(x$Toponim))))
summary(sapply(icgc_NGCat_municipis, function(x) sum(duplicated(x$Toponim))))


## Cerca per noms coincidents dins d'un municipi
osm_NGCatL <- list()
pb <- timerProgressBar(max = length(icgc_NGCat_municipis))
for (i in seq_along(icgc_NGCat_municipis)) {
  dicgc <- icgc_NGCat_municipis[[i]]

  message("\n", i, " / ", length(icgc_NGCat_municipis), "\t", unique(dicgc$NomMun1))

  consulta <- try(opq(
    bbox = paste0(unique(dicgc$osm_typeMun), "(id: ", unique(dicgc$osm_idMun), ")"),
    out = "tags center", osm_types = "nwr", timeout = 100
  ) |>
    add_osm_feature(key = "name", value = dicgc$Toponim, match_case = FALSE))

  if (!inherits(consulta, "overpass_query")) {
    next
  }

  dosm <- osmdata_data_frame(consulta)

  dosm$`name:minuscula` <- tolower(dosm$name)
  dicgc$`name:minuscula` <- tolower(dicgc$Toponim)
  dosm_icgc <- merge(dosm, dicgc, by = "name:minuscula")

  # dicgcGeo <- st_as_sf(
  #   dosm_icgc,
  #   coords = c("UTMX_ETRS89", "UTMY_ETRS89"),
  #   crs = st_crs("EPSG:25831")
  # )
  # dosmGeo <- st_as_sf(
  #   dosm_icgc,
  #   coords = c("osm_center_lon", "osm_center_lat"),
  #   crs = "+proj=longlat +datum=WGS84 +no_defs"
  # )
  #
  # dosmGeo<- st_transform(dosmGeo, crs = st_crs(dicgcGeo))
  # # dicgcGeo<- st_transform(dicgcGeo, crs = st_crs(dosmGeo))
  # # mapview::mapview(list(dosmGeo, dicgcGeo), col.regions=list("green","orange"), col=list("green","orange"))
  #
  # # TODO: usa distàncies per resoldre duplicats
  # dist <- st_distance(dosmGeo, dicgcGeo, by_element = TRUE)
  # dicgcGeo$dist_metres <- dist

  sel_osm_names <- intersect(c("name", "name:ca", "alt_name", "alt_name:ca"), names(dosm_icgc))
  sel_cols <- c(
    "Toponim", sel_osm_names, "wikidata", "ref:icgc", "osm_type", "osm_id",
    "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX_ETRS89", "UTMY_ETRS89",
    names(icgc_NGCat)
  )
  extra_cols <- sort(setdiff(names(dosm_icgc), sel_cols))
  cols <- intersect(c(sel_cols, extra_cols), names(dosm_icgc))
  rm_cols <- c("name:minuscula", grep("^(NomCom|NomMun)[2-5]", names(icgc_NGCat), value = TRUE))
  cols <- setdiff(cols, rm_cols)

  osm_NGCatL[[unique(dicgc$osm_idMun)]] <- dosm_icgc[, cols]
  setTimerProgressBar(pb, i)
}
close(pb)

# save(osm_NGCatL, file="data/icgc_NGCat-OSM_municipisL.RData", compress="xz")
load("data/icgc_NGCat-OSM_municipisL.RData", verbose = TRUE) # osm_NGCatL


osm_NGCat <- do.call(dbTools::rbind_addColumns, osm_NGCatL)
row.names(osm_NGCat) <- NULL

## Calcula distàncies
icgc <- st_as_sf(osm_NGCat,
                 coords = c("UTMX_ETRS89", "UTMY_ETRS89"), remove = FALSE,
                 crs = sf::st_crs("EPSG:25831")
) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
osm <- st_as_sf(osm_NGCat,
                coords = c("osm_center_lon", "osm_center_lat"), remove = FALSE,
                crs = "+proj=longlat +datum=WGS84 +no_defs"
)
osm <- st_transform(osm, crs = st_crs(icgc))

dist <- st_distance(osm, icgc, by_element = TRUE)
osm_NGCat$dist_metres <- as.numeric(dist)


# Selecciona columnes
sel_osm_names <- intersect(c("name", "name:ca", "alt_name", "alt_name:ca"), names(osm_NGCat))
sel_cols <- c(
  "Toponim", sel_osm_names, "wikidata", "ref:icgc", "osm_id", "osm_type", "Concepte",
  "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX_ETRS89", "UTMY_ETRS89"
)
extra_cols <- setdiff(intersect(names(osm_NGCat), names(icgc_NGCat)), sel_cols)
tags <- sort(setdiff(names(osm_NGCat), c(sel_cols, extra_cols)))

n_tags <- sapply(osm_NGCat[, tags], function(x) sum(!is.na(x)))
plot(sort(n_tags))
plot(sort(n_tags[n_tags > 1000]))
tags_comuns <- names(which(n_tags > 1000))
sel_tags <- setdiff(tags_comuns, c(grep("(^addr:|date$)", tags_comuns, value = TRUE)))

cols <- intersect(c(sel_cols, extra_cols, sel_tags), names(osm_NGCat))

osm_NGCat <- osm_NGCat[, cols]

# save(osm_NGCat, file="data/icgc_NGCat-OSM.RData", compress="xz")
load("data/icgc_NGCat-OSM.RData", verbose = TRUE) # osm_NGCat
# mapview::mapview(list(osm, icgc), col.regions=list("green","orange"), col=list("green","orange"))

head(osm_NGCat)


### Coincidències múltiples ----
sum(duplicated(osm_NGCat$`ref:icgc`))
sum(duplicated(osm_NGCat[, c("osm_type", "osm_id")]))

dup_icgc <- dbTools::duplicatedPK(osm_NGCat, pk = "ref:icgc")
dup_osm <- dbTools::duplicatedPK(osm_NGCat, pk = c("osm_type", "osm_id"))

no_unics_pk_icgc <- dbTools::nonUniqueValuesByPK(dup_icgc, pk = "ref:icgc")
no_unics_pk_osm <- dbTools::nonUniqueValuesByPK(dup_osm, pk = c("osm_type", "osm_id"))

sort(table(unlist(sapply(no_unics_pk_icgc, names))))
sort(table(unlist(sapply(no_unics_pk_osm, names))))

## CONCLUSIONS: les coincidències múltiples amb els objectes d'OSM venen de topònims amb diferents coordenades
# provinents de diferents escales. Les coincidències múltiples amb els objectes de l'NGCat venen de coincidències amb
# diferents elements d'OSM amb el mateix nom.
# Es poden resoldre les coincidències amb els valors de les etiquetes d'OSM i el camp Concepte de l'ICGC?
lapply(c("amenity", "natural", "building", "place"), function(x) {
  out <- table(osm_NGCat[, c("Concepte", x)])
  out <- out[, apply(out, 2, sum) > 0]
  out[apply(out, 1, sum) > 0, ]
})


noDup <- osm_NGCat[!osm_NGCat$`ref:icgc` %in% dup$`ref:icgc`, ]

plot(sort(noDup$dist_metres))
plot(sort(dup$dist_metres))

noDup[order(noDup$dist_metres, decreasing = TRUE), selCols]


### No trobats ----
icgc_pendents <- icgc_NGCat[!icgc_NGCat$`ref:icgc` %in% osm_NGCat$`ref:icgc`, ]


## Actualitza etiquetes ----
etiquetes <- c(
  "name", "name:ca", "alt_name", "alt_name:ca", "wikidata", "admin_level", "amenity", "boundary", "building", "capital",
  "denomination", "ele", "highway", "historic", "landuse", "natural", "operator", "place", "population",
  "public_transport", "ref", "ref:ine", "religion", "source", "source:ele", "source:name", "tourism", "type", "website",
  "wikipedia"
)
osm_NGCat_actualitzat <- consulta_etiquetes_osm(osm_NGCat, etiquetes = etiquetes)
