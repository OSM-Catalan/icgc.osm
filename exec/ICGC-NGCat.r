## Casa els Noms Geogràfics de l'ICGC amb els objectes d'OSM
# https://icgc.cat/Descarregues/Llocs/Noms-geografics-NGCat

library(icgc.osm)
library(osmdata)
library(sf)
library(pbapply)

d <- read.csv2("inst/extdata/ngcatv10cs0f1r011.txt", fileEncoding = "ISO-8859-15")


## ICGC: Noms Geogràfics (NGCat) ----

c(
  nrow(d), nrow(unique(d[, c("UTMX_ETRS89", "UTMY_ETRS89")])),
  nrow(unique(d[, c("Toponim", "UTMX_ETRS89", "UTMY_ETRS89")]))
)

d$`ref:icgc` <- paste0("NGCat23.", formatC(seq_len(nrow(d)), width = nchar(nrow(d)), flag = 0))

c(nrow(d), nrow(unique(d[, setdiff(names(d), "ref:icgc")])))
dup <- dbTools::duplicatedPK(d[, setdiff(names(d), "ref:icgc")])
dup <- duplicated(d[, setdiff(names(d), "ref:icgc")])
table(dup)
## CONCLUSIÓ: les coordenades combinades amb el topònim no són úniques i hi ha 109 files duplicades

# Elimina files duplicades
d <- d[!duplicated(d[, setdiff(names(d), "ref:icgc")]), ]

# Canvia "" -> NA
d[] <- lapply(d, function(x) {
  x[x == ""] <- NA
  x
})


## Combina CodiMun1, NomMun1, CodiCom1 i NomCom1 amb els municipis i comarques d'OSM ----

dosm <- merge(
  d,
  unique(tesaurus_municipis[, c("icgc_CodiMun", "icgc_NomMun", "osm_type", "osm_id")]),
  by.x = c("CodiMun1", "NomMun1"), by.y = c("icgc_CodiMun", "icgc_NomMun"), all.x = TRUE
)
names(dosm) <- gsub("^osm_(type|id)$", "osm_\\1Mun", names(dosm))

dosm <- merge(
  dosm,
  unique(tesaurus_comarques[, c("icgc_CodiCom", "icgc_NomCom", "osm_type", "osm_id")]),
  by.x = c("CodiCom1", "NomCom1"), by.y = c("icgc_CodiCom", "icgc_NomCom"), all.x = TRUE
)
names(dosm) <- gsub("^osm_(type|id)$", "osm_\\1Com", names(dosm))

dosm <- dosm[!is.na(dosm$Toponim), ]

c(nrow(d), nrow(dosm)) # número de casos ha de ser igual
setequal(dosm$`ref:icgc`, d$`ref:icgc`)

# reordena columnes
ord <- c("Toponim", "Concepte", "ref:icgc")
ord <- c(ord, sort(setdiff(grep("^Codi", names(dosm), value = TRUE, invert = TRUE), ord)))
dosm <- dosm[, ord]

## Casos sense municipi
no_municipi <- dosm[is.na(dosm$osm_idMun), ]
no_comarca <- dosm[is.na(dosm$osm_idCom), ]
no_municipi_ni_comarca <- dosm[is.na(dosm$osm_idMun) & is.na(dosm$osm_idCom), ]
## CONCLUSIÓ: tots els topònims sense municipi tenen comarca

icgc_NGCat <- dosm[order(dosm$`ref:icgc`), ]
rownames(icgc_NGCat) <- NULL

# save(icgc_NGCat, file="data/icgc_NGCat.RData", compress="xz")
load("data/icgc_NGCat.RData", verbose = TRUE) # icgc_NGCat


## TODO: Afegeix columna amb la referència del fitxer de la toponímia major de de l'ICGC ----
## PENDENT: força elements sense correspondència
# names(icgc_NGCat)
# names(icgc_toponimiaMajor)
# m<- merge(icgc_NGCat, icgc_toponimiaMajor,
#       by.x=c("UTMX_ETRS89", "UTMY_ETRS89"),
#       by.y=c("UTMX", "UTMY"))
m0 <- merge(icgc_NGCat, icgc_toponimiaMajor,
  by.x = c("Toponim", "Concepte"),
  by.y = c("name:icgc", "Concepte")
)
m0[, grep("^(municipi_|comarca_|CodiMun)", names(m0), invert = TRUE)]
m0[, c("Toponim", "Concepte", grep("^UTM", names(m0), value = TRUE))]

m1 <- merge(icgc_NGCat, icgc_toponimiaMajor,
  by.x = c("Toponim", "Concepte", "osm_idMun", "osm_typeMun"),
  by.y = c("name:icgc", "Concepte", "osm_idMun", "osm_typeMun")
)
m1[, grep("^(municipi_|comarca_|CodiMun)", names(m1), invert = TRUE)]
m1[, c("Toponim", "Concepte", grep("^UTM", names(m1), value = TRUE))]
m1[, c("Toponim", "Concepte", grep("^(NomMun|Municipi\\.)", names(m1), value = TRUE))]

m2 <- merge(icgc_NGCat, icgc_toponimiaMajor,
  by.x = c("Toponim", "Concepte", "osm_idCom", "osm_typeCom"),
  by.y = c("name:icgc", "Concepte", "osm_idCom", "osm_typeCom")
)
m2[, grep("^(municipi_|comarca_|CodiMun)", names(m2), invert = TRUE)]
m2[, c("Toponim", "Concepte", grep("^UTM", names(m2), value = TRUE))]
m2[, c("Toponim", "Concepte", grep("^(NomMun|Municipi\\.)", names(m2), value = TRUE))]

m3 <- merge(icgc_NGCat, icgc_toponimiaMajor,
  by.x = c("Toponim", "Concepte", "osm_idCom", "osm_idMun", "osm_typeCom", "osm_typeMun"),
  by.y = c("name:icgc", "Concepte", "osm_idCom", "osm_idMun", "osm_typeCom", "osm_typeMun")
)
m3[, grep("^(municipi_|comarca_|CodiMun)", names(m3), invert = TRUE)]
m3[, c("Toponim", "Concepte", grep("^UTM", names(m3), value = TRUE))]
m3[, c("Toponim", "Concepte", grep("^(NomMun|Municipi\\.)", names(m3), value = TRUE))]


# n files
sapply(list(NGCat = icgc_NGCat, TM = icgc_toponimiaMajor, m0 = m0, m1 = m1, m2 = m2, m3 = m3), nrow)

dup <- dbTools::duplicatedPK(m3, pk = c("Toponim", "Concepte", "osm_idCom", "osm_idMun", "osm_typeCom", "osm_typeMun"))
dup[, c("Toponim", "Concepte", grep("^(ref|UTM)", names(dup), value = TRUE))]
head(dup)
valors_no_unics <- dbTools::nonUniqueValuesByPK(
  dup,
  pk = c("Toponim", "Concepte", "osm_idCom", "osm_idMun", "osm_typeCom", "osm_typeMun")
)

dup_ref <- dbTools::duplicatedPK(m3, pk = "ref:icgc.x")
dup_ref[, c("Toponim", "Concepte", grep("^(ref|UTM)", names(dup_ref), value = TRUE))]

dup_refTM <- dbTools::duplicatedPK(m3, pk = "ref:icgc.y")
dup_refTM[, c("Toponim", "Concepte", grep("^(ref|UTM)", names(dup_refTM), value = TRUE))]

tmp <- unique(m1[, c(grep("^(NomMun[2-5]|Municipi\\.[2-5])", names(m1), value = TRUE))])

sapply(1:3, function(i) table(tmp[[i]] == tmp[[i + 4]], useNA = "always"))
m3[which(m3$NomMun2 != m3$Municipi.2), grep("^(Municipi|NomMun)", names(m3))]

plot(m3$UTMX_ETRS89, m3$UTMX)
abline(0, 1, col = "red")
plot(m3$UTMY_ETRS89, m3$UTMY)
abline(0, 1, col = "red")
plot(m3[, grep("^UTM", names(m3))])

table(duplicated(m3$`ref:icgc.x`))
dup <- dbTools::duplicatedPK(m3, pk = "ref:icgc.x")
## CONCLUSIONS: les coordenades del recull de toponímia major i NGCat de l'ICGC no coincideixen
# Alguns NomMun2 no coincideixen amb Municipi.2


icgc_NGCat <- merge(icgc_NGCat, m3[, c("ref:icgc.x", "ref:icgc.y")], by.x = "ref:icgc", by.y = "ref:icgc.x", all = TRUE)
names(icgc_NGCat) <- gsub("^ref:icgc.y$", "ref:icgc:TM", names(icgc_NGCat))
icgc_NGCat[is.na(icgc_NGCat$`ref:icgc`), ]
table(is.na(icgc_NGCat$filaToponimiaMajor))
nrow(icgc_toponimiaMajor)
## CONCLUSION: només 33502 de 52923 elements de la toponímia major trobats a la toponímia.

# save(icgc_NGCat, file="data/icgc_NGCat.RData", compress="xz")
load("data/icgc_NGCat.RData", verbose = TRUE) # icgc_NGCat


## Combinació de topònims ICGC - OSM ----

load("data/icgc_NGCat.RData", verbose = TRUE) # icgc_NGCat

icgc_NGCat_municipis <- by(icgc_NGCat, icgc_NGCat$osm_idMun, function(x) x)

summary(sapply(icgc_NGCat_municipis, nrow))
summary(sapply(icgc_NGCat_municipis, function(x) length(unique(x$Toponim))))
summary(sapply(icgc_NGCat_municipis, function(x) sum(duplicated(x$Toponim))))


## Cerca per nom i prioritza objectes propers
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

  sel_tags <- c("name", "name:ca", "alt_name", "alt_name:ca")
  sel_cols <- c(
    "Toponim", intersect(sel_tags, names(dosm_icgc)), "wikidata", "ref:icgc", "osm_type", "osm_id",
    "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX_ETRS89", "UTMY_ETRS89",
    names(icgc_NGCat)
  )
  rm_cols <- c("name:minuscula")
  extra_cols <- sort(setdiff(names(dosm_icgc), c(sel_cols, rm_cols)))
  cols <- intersect(c(sel_cols, extra_cols), names(dosm_icgc))

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

selNames <- c("name", "name:ca", "alt_name", "alt_name:ca")
selCols <- c(
  "Toponim", intersect(selNames, names(osm_NGCat)),
  "wikidata", "ref:icgc", "ref:icgc:TM", "osm_type", "osm_id",
  "dist_metres", "osm_center_lon", "osm_center_lat",
  "UTMX_ETRS89", "UTMY_ETRS89",
  names(icgc_NGCat)
)
selCols <- c(selCols, setdiff(names(d), selCols))
rmCols <- c(
  "name:minuscula", "municipi_name:ca", "municipi_regio", "municipi_comarca",
  "municipi_id", "municipi_type", "municipi_wikidata", "municipi_admin_level",
  "municipi_comarca.id", "comarca_osm", "comarca_name:ca", "comarca_regio",
  "comarca_id", "comarca_type", "comarca_admin_level"
)
extra_cols <- sort(setdiff(names(osm_NGCat), c(selCols, rmCols)))
cols <- intersect(c(selCols, extra_cols), names(osm_NGCat))

osm_NGCat <- osm_NGCat[, cols]
# save(osm_NGCat, file="data/icgc_NGCat-OSM.RData", compress="xz")
load("data/icgc_NGCat-OSM.RData", verbose = TRUE) # osm_NGCat
# mapview::mapview(list(osm, icgc), col.regions=list("green","orange"), col=list("green","orange"))


nCols <- sapply(osm_NGCat, function(x) sum(!is.na(x)))
plot(sort(nCols, decreasing = TRUE)[1:100])
selCols <- names(nCols)[nCols > 1000]

head(osm_NGCat[, selCols], 15)

write.xlsx(osm_NGCat[, selCols], file = "inst/resultats/icgc_NGCat-OSM_v0.xlsx")


### Coincidències múltiples ----
sum(duplicated(osm_NGCat$`ref:icgc`))
dup <- dbTools::duplicatedPK(osm_NGCat, pk = "ref:icgc")
dup[, selCols]

noDup <- osm_NGCat[!osm_NGCat$`ref:icgc` %in% dup$`ref:icgc`, ]

plot(sort(noDup$dist_metres))
plot(sort(dup$dist_metres))

noDup[order(noDup$dist_metres, decreasing = TRUE), selCols]

### No trobats ----
icgc_pendents <- icgc_NGCat[!icgc_NGCat$`ref:icgc` %in% osm_NGCat$`ref:icgc`, ]
