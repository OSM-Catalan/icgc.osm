## Casa els Noms Geogràfics de l'ICGC amb els objectes d'OSM
# https://icgc.cat/Descarregues/Llocs/Noms-geografics-NGCat

library(icgc.osm)


## ICGC: Noms Geogràfics (NGCat) ----
d <- read.csv2("data-raw/ngcatv10cs0f1r011.txt", fileEncoding = "ISO-8859-15")

c(
  nrow(d), nrow(unique(d[, c("UTMX_ETRS89", "UTMY_ETRS89")])),
  nrow(unique(d[, c("Toponim", "UTMX_ETRS89", "UTMY_ETRS89")]))
)

d$`ref:icgc` <- paste0("NGCat23.", formatC(seq_len(nrow(d)), width = nchar(nrow(d)), flag = 0))

c(nrow(d), nrow(unique(d[, setdiff(names(d), "ref:icgc")])))
dup <- dbTools::duplicatedPK(d[, setdiff(names(d), "ref:icgc")])
dup <- duplicated(d[, setdiff(names(d), "ref:icgc")])
table(dup)
## CONCLUSIONS: les coordenades combinades amb el topònim no són úniques i hi ha 109 files duplicades

# Elimina files duplicades
d <- d[!duplicated(d[, setdiff(names(d), "ref:icgc")]), ]

# Canvia "" -> NA
d[] <- lapply(d, function(x) {
  x[x == ""] <- NA
  x
})


### Combina CodiMun1, NomMun1, CodiCom1 i NomCom1 amb els municipis i comarques d'OSM ----

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
## CONCLUSIONS: tots els topònims sense municipi tenen comarca

icgc_NGCat <- dosm[order(dosm$`ref:icgc`), ]
rownames(icgc_NGCat) <- NULL

# save(icgc_NGCat, file="data/icgc_NGCat.RData", compress="xz")
load("data/icgc_NGCat.RData", verbose = TRUE) # icgc_NGCat


## Cerca topònims duplicats procedents de diferents escales ----

load("data/icgc_NGCat.RData", verbose = TRUE) # icgc_NGCat

icgc_NGCat_unics <- unique(icgc_NGCat[
  ,
  c(
    "Toponim", "Concepte", "NomCom1", "NomCom2", "NomCom3", "NomCom4", "NomCom5",
    "NomMun1", "NomMun2", "NomMun3", "NomMun4", "NomMun5", "osm_idCom", "osm_idMun", "osm_typeCom", "osm_typeMun"
  )
])

c(nrow(icgc_NGCat), nrow(icgc_NGCat_unics))

dup <- dbTools::duplicatedPK(icgc_NGCat_unics, pk = c("Toponim", "Concepte", "osm_idCom", "osm_idMun"))
no_unics_pk <- dbTools::nonUniqueValuesByPK(dup, pk = c("Toponim", "Concepte", "osm_idCom", "osm_idMun"))

# CONCLUSIONS: 7337 topònims duplicats que difereixen només en les coordenades.
# 11 topònims duplicats presents en més d'un municipis


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
## CONCLUSIONS: només 33502 de 52923 elements de la toponímia major trobats a la toponímia.

usethis::use_data(icgc_NGCat, overwrite = TRUE)
load("data/icgc_NGCat.rda", verbose = TRUE) # icgc_NGCat
