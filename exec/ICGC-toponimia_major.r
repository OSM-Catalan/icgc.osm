## Casa la toponímia major de l'ICGC amb els elements d'OSM
# https://icgc.cat/Ciutada/Informa-t/Llibres-i-fons-documentals/Llibres-en-PDF/Toponimia/Nomenclator-oficial-de-toponimia-major-de-Catalunya # nolint

library(openxlsx)
library(osmdata)
library(sf)
library(pbapply)

d <- read.xlsx("inst/extdata/icgc_nomenclator_2020.xlsx")


## ICGC: Toponímia major ----

# Naturalitza topònims (mou articles al principi)
d$`name:icgc` <- sapply(d$Topònim, function(x) {
  out <- strsplit(x, ", ")[[1]]
  # Separador buit si acaba en apòstrof
  ultimCaracter <- out[length(out)]
  ultimCaracter <- substr(ultimCaracter, nchar(ultimCaracter), nchar(ultimCaracter))
  separador <- ifelse(ultimCaracter == "'", "", " ")
  if (length(out) == 2) {
    out <- paste0(out[2], separador, out[1])
    # warning(out)
  } else if (length(out) > 2) {
    out <- paste0(out[length(out)], separador, paste(out[-length(out)], collapse = ", "), collapse = "")
    warning("Més d'una coma a: ", out, "\tori: ", x)
  }

  out
})

c(
  nrow(d), nrow(unique(d[, c("UTMX", "UTMY")])),
  nrow(unique(d[, c("Topònim", "UTMX", "UTMY")]))
)

d$`ref:icgc` <- paste0("TM20.", formatC(seq_len(nrow(d)), width = nchar(nrow(d)), flag = 0))

c(nrow(d), nrow(unique(d[, setdiff(names(d), "ref:icgc")])))
dup <- dbTools::duplicatedPK(d[, setdiff(names(d), "ref:icgc")])
dup <- duplicated(d[, setdiff(names(d), "ref:icgc")])
table(dup)
## CONCLUSIÓ: les coordenades combinades amb el topònim no són úniques i hi ha 3 files duplicades

# Elimina files duplicades
d <- d[!duplicated(d[, setdiff(names(d), "ref:icgc")]), ]

# Canvia "" -> NA
d[] <- lapply(d, function(x) {
  x[x == ""] <- NA
  x
})


## TODO: què fer amb aquests topònims?
grep("\\*$", d$Topònim, value = TRUE)
grep("   vegeu   ", d$Topònim, value = TRUE)
## de https://icgc.cat/content/download/16121/49625/version/1/file/proces_elaboracio_metodologia.pdf :
# L’autonomia de què disposa l’Administració local a l’hora de fixar el nom oficial dels nuclis de població, ha
# comportat la inclusió en el Nomenclàtor oficial de noms incorrectes ortogràficament o inadequats a la tradició
# toponomàstica catalana, segons els informes de l’IEC en què es proposaven formes adequades. Tots aquests noms
# apareixen finalment a l’índex del Nomenclàtor destacats gràficament amb un asterisc (*). Cal dir, tanmateix que molts
# dels noms corresponents a entitats de població que en la primera edició s’havien transcrit amb grafies inapropiades o
# amb formes estranyes a la llengua i no havien estat validats per l’IEC; en aquesta nova edició, i mercès als informes
# pertinents elaborats per la Comissió de Toponímia de Catalunya adreçats als ajuntaments, s’han pogut reduir fins a
# 102 que encara es consideren incorrectes segons els criteris de l’IEC. El fet que el Nomenclàtor sigui una obra viva
# com la toponímia fa esperar que aquesta llista proporcionalment petita respecte del corpus que normalitza el conjunt
# de l’obra, es vagi reduint i que esdevingui un al·licient perquè els consistoris vagin aprovant formes adequades a la
# normativa.


## Combina Municipi.1 i Comarca.1 amb els municipis i comarques d'OSM ----

dosm <- merge(
  d,
  unique(tesaurus_municipis[, c("icgc_MunicipiTM", "osm_type", "osm_id")]),
  by.x = "Municipi.1", by.y = "icgc_MunicipiTM", all.x = TRUE
)
names(dosm) <- gsub("^osm_(type|id)$", "osm_\\1Mun", names(dosm))

dosm <- merge(
  dosm,
  unique(tesaurus_comarques[, c("icgc_CodiCom", "osm_type", "osm_id")]),
  by.x = "Comarca.1", by.y = "icgc_CodiCom", all.x = TRUE
)
names(dosm) <- gsub("^osm_(type|id)$", "osm_\\1Com", names(dosm))

dosm <- dosm[!is.na(dosm$Topònim), ]

c(nrow(d), nrow(dosm)) # número de casos ha de ser igual
setequal(dosm$`ref:icgc`, d$`ref:icgc`)

# reordena columnes
ord <- c("name:icgc", "Concepte", "ref:icgc")
ord <- c(ord, sort(setdiff(names(dosm), c(ord, "Pàgina", "Volum", "UTM.X", "UTM.Y"))))
dosm <- dosm[, ord]

## Casos sense municipi
no_municipi <- dosm[is.na(dosm$osm_idMun), ]
no_comarca <- dosm[is.na(dosm$osm_idCom), ]
no_municipi_ni_comarca <- dosm[is.na(dosm$osm_idMun) & is.na(dosm$osm_idCom), ]
## CONCLUSIÓ: tots els topònims tenen municipi o comarca, excepte formes no normatives

icgc_toponimiaMajor <- dosm[order(dosm$`ref:icgc`), ]
rownames(icgc_toponimiaMajor) <- NULL

# save(icgc_toponimiaMajor, file="data/icgc_toponimiaMajor.RData", compress="xz")
load("data/icgc_toponimiaMajor.RData", verbose = TRUE) # icgc_toponimiaMajor


## Combinació de topònims ICGC- OSM ----

load("data/icgc_toponimiaMajor.RData", verbose = TRUE) # icgc_toponimiaMajor

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
