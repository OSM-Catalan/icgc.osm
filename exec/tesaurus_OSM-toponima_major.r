## Combinació de topònims ICGC- OSM ----

load("data/icgc_toponimiaMajor.rda", verbose = TRUE) # icgc_toponimiaMajor

# icgc_toponimiaMajorGeo<- st_as_sf(icgc_toponimiaMajor, coords=c("UTMX", "UTMY"), remove=FALSE,
#                                   crs=sf::st_crs("EPSG:x")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831

icgc_toponimiaMajor_municipis <- by(icgc_toponimiaMajor, icgc_toponimiaMajor$municipi_osm, function(x) x)
# icgc_toponimiaMajor_municipisGeo<- by(icgc_toponimiaMajorGeo, icgc_toponimiaMajorGeo$municipi_osm, function(x) x)

summary(sapply(icgc_toponimiaMajor_municipis, nrow))
summary(sapply(icgc_toponimiaMajor_municipis, function(x) length(unique(x$`name:icgc`))))
summary(sapply(icgc_toponimiaMajor_municipis, function(x) sum(duplicated(x$`name:icgc`))))


## Cerca per nom i prioritza objectes propers
osm_icgcL <- list()
pb <- timerProgressBar(max = length(icgc_toponimiaMajor_municipis))
for (i in seq_along(icgc_toponimiaMajor_municipis)) {
  dicgc <- icgc_toponimiaMajor_municipis[[i]]

  message("\n", i, " / ", length(icgc_toponimiaMajor_municipis), "\t", unique(dicgc$`municipi_name:ca`))

  consulta <- try(opq(
    bbox = paste0(unique(dicgc$municipi_type), "(id: ", unique(dicgc$municipi_id), ")"),
    out = "tags center", osm_types = "nwr", timeout = 100
  ) %>%
    add_osm_feature(key = "name", value = dicgc$`name:icgc`, match_case = FALSE))

  if (!inherits(consulta, "overpass_query")) {
    next
  }

  dosm <- osmdata_data_frame(consulta)

  dosm$`name:minuscula` <- tolower(dosm$name)
  dicgc$`name:minuscula` <- tolower(dicgc$`name:icgc`)
  dosm_icgc <- merge(dosm, dicgc, by = "name:minuscula")

  # dosm_icgc<- st_as_sf(merge(dosm, dicgc, by="name:minuscula"))
  # dosmGeo<- st_as_sf(
  #   dosm_icgc,
  #   coords=c("osm_center_lon", "osm_center_lat"),
  #   crs="+proj=longlat +datum=WGS84 +no_defs"
  # )  # ETRS89 UTM fus 31 Nord https://epsg.io/25831
  #
  # dosmGeo<- st_transform(dosmGeo, crs = st_crs(dicgc))
  # # mapview::mapview(list(dosmGeo, dosm_icgc), col.regions=list("green","orange"), col=list("green","orange"))
  #
  # # TODO: distancies totes 0m
  # dist<- st_distance(dosmGeo, dosm_icgc, by_element = TRUE)
  # dosm_icgc$dist_metres<- dist

  selNames <- c("name", "name:ca", "alt_name", "alt_name:ca")
  selCols <- c(
    "name:icgc", intersect(selNames, names(dosm_icgc)),
    "wikidata", "ref:icgc", "osm_type", "osm_id", "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX", "UTMY"
  )
  rmCols <- c(
    "name:minuscula", "municipi_name:ca", "municipi_regio", "municipi_comarca", "municipi_id", "municipi_type",
    "municipi_wikidata", "municipi_admin_level", "municipi_comarca.id",
    "comarca_osm", "comarca_name:ca", "comarca_regio", "comarca_id", "comarca_type", "comarca_admin_level"
  )
  extra_cols <- setdiff(names(dosm_icgc), c(selCols, rmCols))
  cols <- intersect(c(selCols, extra_cols), names(dosm_icgc))

  osm_icgcL[[unique(dicgc$municipi_osm)]] <- dosm_icgc[, cols]
  setTimerProgressBar(pb, i)
}
close(pb)

# save(osm_icgcL, file="data/icgc_toponimiaMajor-OSM_municipisL.RData", compress="xz")
load("data/icgc_toponimiaMajor-OSM_municipisL.RData", verbose = TRUE) # osm_icgcL


osm_icgc <- do.call(dbTools::rbind_addColumns, osm_icgcL)

## Calcula distàncies
icgc <- st_as_sf(osm_icgc,
                 coords = c("UTMX", "UTMY"), remove = FALSE,
                 crs = sf::st_crs("EPSG:25831")
) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
osm <- st_as_sf(osm_icgc,
                coords = c("osm_center_lon", "osm_center_lat"), remove = FALSE,
                crs = "+proj=longlat +datum=WGS84 +no_defs"
)
osm <- st_transform(osm, crs = st_crs(icgc))

dist <- st_distance(osm, icgc, by_element = TRUE)
osm_icgc$dist_metres <- as.numeric(dist)
# mapview::mapview(list(osm, icgc), col.regions=list("green","orange"), col=list("green","orange"))


nCols <- sapply(osm_icgc, function(x) sum(!is.na(x)))
plot(sort(nCols, decreasing = TRUE)[1:100])
selCols <- names(nCols)[nCols > 1000]

head(osm_icgc[, selCols], 15)

openxlsx::write.xlsx(osm_icgc[, selCols], file = "inst/resultats/icgc_toponimiaMajor-OSM_v0.xlsx")


### Coincidències multiples ----
sum(duplicated(osm_icgc$`ref:icgc`))
dup <- dbTools::duplicatedPK(osm_icgc, pk = "ref:icgc")
dup[, selCols]

noDup <- osm_icgc[!osm_icgc$`ref:icgc` %in% dup$`ref:icgc`, ]

plot(sort(noDup$dist_metres))
plot(sort(dup$dist_metres))

noDup[order(noDup$dist_metres, decreasing = TRUE), selCols]

### No trobats ----
icgc_pendents <- icgc_toponimiaMajor[!icgc_toponimiaMajor$filaICGC %in% osm_icgc$filaICGC, ]
