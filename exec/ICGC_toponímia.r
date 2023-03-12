## Casa la toponímia  de l'ICGC amb els elements d'OSM

library(toponimsCat)
library(osmdata)
library(sf)
library(pbapply)

d<- read.csv2("inst/extdata/ngcatv10cs0f1r011.txt", fileEncoding = "ISO-8859-15")

# dgeo<- sf::st_as_sf(d, coords=c("UTMX_ETRS89", "UTMY_ETRS89"), crs=sf::st_crs("EPSG:25831")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
# mapview::mapview(dgeo)


## Tesaure de comarques OSM - ICGC ----
comarquesICGC<- sort(unique(unlist(d[, grep("NomCom", names(d))])))
comarquesOSM<- sort(toponimsCat::comarques[toponimsCat::comarques$regio == "Principat", "name"])
setdiff(comarquesICGC, comarquesOSM)
setdiff(comarquesOSM, comarquesICGC)
## CONCLUSIÓ: Moianès encara no és present a les dades de l'ICGC. Els noms quadren bé

tesaureComarques<- toponimsCat::comarques[toponimsCat::comarques$regio == "Principat", c("name", "name:ca", "type", "id")]
names(tesaureComarques)<- paste0("comarca_", names(tesaureComarques))

altresArees<- data.frame(
  osm=c("Andorra", "Aragó", "Catalunya", "França", "País Valencià", "Vall d'Aran"),
  icgc=setdiff(comarquesICGC[!comarquesICGC %in% tesaureComarques$comarca_name], ""),
  regio=c("Andorra", "Franja de Ponent", "Principat", "Occitània", "País Valencià", "Vall d'Aran"),
  id=c(9407, 11744144, 11746917, 3792883, 11739086, 2810776),
  type="relation",
  name=c("Andorra", "Franja de Ponent", "Català com a llengua pròpia a Catalunya", "Occitanie", "Municipis de Predomini Lingüístic Valencià", "Val d'Aran"),
  admin_level=NA
)


## Tesaure de municipis OSM - ICGC ----
municipisICGC<- sort(unique(unlist(d[, grep("NomMun", names(d))])))
municipisOSM<- sort(toponimsCat::municipis[toponimsCat::municipis$regio == "Principat", "name"])

tesaureMunicipis<- data.frame(icgc=municipisICGC, osm=ifelse(municipisICGC %in% municipisOSM, municipisICGC, NA_character_))

### Arregla municipis no trobats ----
setdiff(municipisICGC, municipisOSM)
setdiff(municipisOSM, municipisICGC)
tesaureMunicipis$icgc[is.na(tesaureMunicipis$osm)]
setdiff(municipisOSM, tesaureMunicipis$osm)

municipisICGCpendents<- tesaureMunicipis$icgc[is.na(tesaureMunicipis$osm) & tesaureMunicipis$icgc != ""]
municipisCandidats<- lapply(municipisICGCpendents, function(x){
  toponimsCat::municipis[agrep(x, toponimsCat::municipis$name), ]
})
names(municipisCandidats)<- municipisICGCpendents
municipisCandidats[sapply(municipisCandidats, nrow) > 0]
municipisTrobats<- municipisCandidats[sapply(municipisCandidats, nrow) == 1]
municipisTrobats
municipisPerCorregir<- municipisTrobats[setdiff(names(municipisTrobats), "Andorra")]
# for (i in seq_along(municipisPerCorregir)){
#   cat("icgc:", names(municipisPerCorregir)[i], "\tosm:", municipisPerCorregir[[i]]$name, fill = TRUE)
# }
## CONCLUSIONS: consultant vissir, la majoria de municipis són de la Vall d'Aran i no inclosos a toponimsCat (TODO?)
# Calonge (la Selva), els Prats del Rei (Anoia) i les Piles de Gaià (Conca de Barberà) són caps de municipi i no municipi


for (i in seq_along(municipisPerCorregir)){
  tesaureMunicipis$osm[tesaureMunicipis$icgc == names(municipisPerCorregir)[i]]<- municipisPerCorregir[[i]]$name
}

# Correccions manuals
tesaureMunicipis$osm[tesaureMunicipis$icgc == "Sant Carles de la Ràpita"]<- "la Ràpita"

## Caps de municipi, però no municipis
tesaureMunicipis$osm[tesaureMunicipis$icgc == "Brunyola"]<- "Brunyola i Sant Martí Sapresa"
tesaureMunicipis$osm[tesaureMunicipis$icgc == "Calonge"]<- "Calonge i Sant Antoni"
tesaureMunicipis$osm[tesaureMunicipis$icgc == "Santa Maria de Corcó"]<- "l'Esquirol"

## Municipis d'OSM que no són a ICGC
setdiff(municipisOSM, tesaureMunicipis$osm)
tesaureMunicipis$osm[tesaureMunicipis$icgc == "?????"]<- "Medinyà" # Entitat de població segons ICGC


tesaureMunicipisOK<- merge(tesaureMunicipis, toponimsCat::municipis, by.x = "osm", by.y="name", all.x=TRUE)
names(tesaureMunicipisOK)<- paste0("municipi_", names(tesaureMunicipisOK))


### CONCLUSIONS municipis ----
## PENDENT de revisar diferències a OSM i ICGC:
tesaureMunicipis[which(tesaureMunicipis$icgc != tesaureMunicipis$osm), ]

## Municipis de la Val d'Aran i regions supramunicipals
tesaureMunicipisOK[is.na(tesaureMunicipisOK$municipi_id), ]

## Medinyà com a municipi a OSM, però entitat de població segons ICGC
setdiff(municipisOSM, tesaureMunicipis$osm)

## CONCLUSIONS per ICGC:
## PENDENT: Calonge (Baix Empordà) i els Prats del Rei (Anoia) les Piles de Gaià (Conca de Barberà) són caps de municipi i no municipi
### DISCREPÀNCIES trobades:
tesaureMunicipis[which(tesaureMunicipis$icgc != tesaureMunicipis$osm), ]


## ICGC: Toponímia ----

c(nrow(icgc_toponimia), nrow(unique(icgc_toponimia[, c("UTMX_ETRS89", "UTMY_ETRS89")])))
# dup<- dbTools::duplicatedPK(icgc_toponimia, pk=c("UTMX_ETRS89", "UTMY_ETRS89"))
## CONCLUSIÓ: les coordenades no són úniques
d$filaICGC<- 1:nrow(d)

dosm<- merge(d, tesaureMunicipisOK[, c("municipi_osm", "municipi_icgc", "municipi_name:ca", "municipi_type", "municipi_id")], by.x="NomMun1", by.y="municipi_icgc", all=TRUE)
dosm<- merge(
  dosm,
  tesaureComarques,
  by.x="NomCom1", by.y="comarca_name", all=TRUE
)

dosm<- dosm[!is.na(dosm$Toponim), ]

# reordena columnes
ord<- names(dosm)[-c(1, 2)]
ord<- c(ord[1:(grep("CodiMun2", ord) - 1)], "NomMun1", ord[grep("CodiMun2", ord):length(ord)])
ord<- c(ord[1:(grep("CodiCom2", ord) - 1)], "NomCom1", ord[grep("CodiCom2", ord):length(ord)])
dosm<- dosm[, ord]

## Casos sense municipi
noOSMmunicipi<- dosm[dosm$municipi_id %in% c(NA, ""), ]
noICGCmunicipi<- dosm[dosm$NomMun1 %in% c(NA, ""), ]
noOSMmunicipi_ICGCmunicipi<- dosm[dosm$municipi_id %in% c(NA, "") & !dosm$NomMun1 %in% c(NA, ""), ]
noComarca<- dosm[dosm$comarca_id %in% c(NA, "") & !dosm$NomCom1 %in% c(NA, ""), ]

lapply(noOSMmunicipi[, grep("NomMun", names(noOSMmunicipi))], table, useNA="ifany")
lapply(noOSMmunicipi[, grep("NomCom", names(noOSMmunicipi))], table, useNA="ifany")

lapply(noICGCmunicipi[, grep("NomMun", names(noICGCmunicipi))], table, useNA="ifany")
lapply(noICGCmunicipi[, grep("NomCom", names(noICGCmunicipi))], table, useNA="ifany")

lapply(noComarca[, grep("NomMun", names(noComarca))], table, useNA="ifany")
lapply(noComarca[, grep("NomCom", names(noComarca))], table, useNA="ifany")

## CONCLUSIÓ: tots els topònims sense municipi d'OSM són de la Val d'Aran, Aragó i França o amb NAs
## 15978 casos sense NomMun1 (noICGCmunicipi) i d'aquests 15421 sense comarca.
# noIdGeo<- sf::st_as_sf(noOSMmunicipi_ICGCmunicipi, coords=c("UTMX", "UTMY"), crs=sf::st_crs("EPSG:25831")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
# mapview::mapview(noIdGeo)
icgc_toponimia<- dosm[order(dosm$filaICGC), ]
rownames(icgc_toponimia)<- NULL

# Canvia "" -> NA
icgc_toponimia<- data.frame(lapply(icgc_toponimia, function(x) { x[x == ""]<- NA; x } ),
                            check.names=FALSE)


### Afegeix columna amb la fila del fitxer de la toponímia major de de l'ICGC ----
## PENDENT: falten força elements, potser per diferències en el nom de municipi
# load("data/icgc_toponímiaMajor.RData", verbose=TRUE)
# names(icgc_toponimia)
# names(icgc_toponimiaMajor)
# m<- merge(icgc_toponimia, icgc_toponimiaMajor,
#       by.x=c("UTMX_ETRS89", "UTMY_ETRS89"),
#       by.y=c("UTMX", "UTMY"))
m0<- merge(icgc_toponimia, icgc_toponimiaMajor,
     by.x=c("Toponim", "Concepte"),
     by.y=c("name:icgc", "Concepte"))
m0[, grep("^(municipi_|comarca_|CodiMun)", names(m0), invert = TRUE)]
m0[, c("Toponim", "Concepte", grep("^UTM", names(m0), value=TRUE))]
m1<- merge(icgc_toponimia, icgc_toponimiaMajor,
           by.x=c("Toponim", "Concepte", "NomMun1"),
           by.y=c("name:icgc", "Concepte", "Municipi.1"))
m1[, grep("^(municipi_|comarca_|CodiMun)", names(m1), invert = TRUE)]
m1[, c("Toponim", "Concepte", grep("^UTM", names(m1), value=TRUE))]
m1[, c("Toponim", "Concepte", grep("^(NomMun|Municipi\\.)", names(m1), value=TRUE))]
tmp<- unique(m1[, c(grep("^(NomMun[2-5]|Municipi\\.[2-5])", names(m1), value=TRUE))])
sapply(1:3, function(i) table(tmp[[i]] == tmp[[i + 4]], useNA="always"))
all.equal(m1$NomMun2, m1$Municipi.2)
m1[which(m1$NomMun2 != m1$Municipi.2), ]
## CONCLUSIONS: les coordenades del recull de toponímia major i la toponima de l'ICGC no coincideixen
# Alguns NomMun2 no coincideixen amb Municipi.2 (m1[which(m1$NomMun2 != m1$Municipi.2), ])


icgc_toponimia<- merge(icgc_toponimia, m1[, c("filaICGC.x", "filaICGC.y")], by.x="filaICGC", by.y="filaICGC.x", all = TRUE)
names(icgc_toponimia)<- gsub("^filaICGC.y$", "filaToponimiaMajor", names(icgc_toponimia))
icgc_toponimia[is.na(icgc_toponimia$filaICGC), ]
table(is.na(icgc_toponimia$filaToponimiaMajor))
nrow(icgc_toponimiaMajor)
## CONCLUSION: només 33926 de 52923 elements de la toponímia major trobats a la toponímia.

# save(icgc_toponimia, file="data/icgc_toponímia.RData", compress="xz")
load("data/icgc_toponímia.RData", verbose=TRUE) # icgc_toponimia


## Combinació de topònims ICGC- OSM ----

load("data/icgc_toponímia.RData", verbose=TRUE) # icgc_toponimia

# icgc_toponimiaGeo<- st_as_sf(icgc_toponimia, coords=c("UTMX", "UTMY"), remove=FALSE,
#                                   crs=sf::st_crs("EPSG:x")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831

icgc_toponimia_municipis<- by(icgc_toponimia, icgc_toponimia$municipi_osm, function(x) x)
# icgc_toponimia_municipisGeo<- by(icgc_toponimiaGeo, icgc_toponimiaGeo$municipi_osm, function(x) x)

summary(sapply(icgc_toponimia_municipis, nrow))
summary(sapply(icgc_toponimia_municipis, function(x) length(unique(x$Toponim))))
summary(sapply(icgc_toponimia_municipis, function(x) sum(duplicated(x$Toponim))))


## Cerca per nom i prioritza objectes propers
osm_icgcL<- list()
pb<- timerProgressBar(max=length(icgc_toponimia_municipis))
for (i in seq_along(icgc_toponimia_municipis)){
  dicgc<- icgc_toponimia_municipis[[i]]

  message("\n", i, " / ", length(icgc_toponimia_municipis), "\t", unique(dicgc$`municipi_name:ca`))

  consulta<- try(opq(bbox = paste0(unique(dicgc$municipi_type), "(id: ", unique(dicgc$municipi_id), ")"),
                 out = "tags center", osm_types = "nwr", timeout = 100) %>%
    add_osm_feature(key="name", value=dicgc$Toponim, match_case=FALSE))

  if (!inherits(consulta, "overpass_query"))
    next

  dosm<- osmdata_data_frame(consulta)

  dosm$`name:minuscula`<- tolower(dosm$name)
  dicgc$`name:minuscula`<- tolower(dicgc$Toponim)
  dosm_icgc<- merge(dosm, dicgc, by="name:minuscula")

  # dosm_icgc<- st_as_sf(merge(dosm, dicgc, by="name:minuscula"))
  # dosmGeo<- st_as_sf(dosm_icgc, coords=c("osm_center_lon", "osm_center_lat"), crs="+proj=longlat +datum=WGS84 +no_defs")
  #
  # dosmGeo<- st_transform(dosmGeo, crs = st_crs(dicgc))
  # # mapview::mapview(list(dosmGeo, dosm_icgc), col.regions=list("green","orange"), col=list("green","orange"))
  #
  # # TODO: distancies totes 0m
  # dist<- st_distance(dosmGeo, dosm_icgc, by_element = TRUE)
  # dosm_icgc$dist_metres<- dist

  selNames<- c("name", "name:ca", "alt_name", "alt_name:ca")
  selCols<- c("Toponim", intersect(selNames, names(dosm_icgc)),
              "wikidata", "filaICGC", "osm_type", "osm_id", "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX_ETRS89", "UTMY_ETRS89",
              names(icgc_toponimia))
  rmCols<- c("name:minuscula", "municipi_name:ca", "municipi_regio", "municipi_comarca", "municipi_id", "municipi_type", "municipi_wikidata", "municipi_admin_level", "municipi_comarca.id",
             "comarca_osm", "comarca_name:ca", "comarca_regio", "comarca_id", "comarca_type", "comarca_admin_level")
  extraCols<- sort(setdiff(names(dosm_icgc), c(selCols, rmCols)))
  cols<- intersect(c(selCols, extraCols), names(dosm_icgc))

  osm_icgcL[[unique(dicgc$municipi_osm)]]<- dosm_icgc[, cols]
  setTimerProgressBar(pb, i)
}
close(pb)

# save(osm_icgcL, file="data/icgc_toponímia-OSM_municipisL.RData", compress="xz")
load("data/icgc_toponímia-OSM_municipisL.RData", verbose=TRUE) # osm_icgcL


osm_icgc<- do.call(dbTools::rbind_addColumns, osm_icgcL)

## Calcula distàncies
icgc<- st_as_sf(osm_icgc, coords=c("UTMX_ETRS89", "UTMY_ETRS89"), remove=FALSE,
                crs=sf::st_crs("EPSG:25831")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
osm<- st_as_sf(osm_icgc, coords=c("osm_center_lon", "osm_center_lat"), remove=FALSE,
               crs="+proj=longlat +datum=WGS84 +no_defs")
osm<- st_transform(osm, crs = st_crs(icgc))

dist<- st_distance(osm, icgc, by_element = TRUE)
osm_icgc$dist_metres<- as.numeric(dist)

selNames<- c("name", "name:ca", "alt_name", "alt_name:ca")
selCols<- c("Toponim", intersect(selNames, names(osm_icgc)),
            "wikidata", "filaICGC", "filaToponimiaMajor", "osm_type", "osm_id", "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX_ETRS89", "UTMY_ETRS89",
            names(icgc_toponimia))
selCols<- c(selCols, setdiff(names(d), selCols))
rmCols<- c("name:minuscula", "municipi_name:ca", "municipi_regio", "municipi_comarca", "municipi_id", "municipi_type", "municipi_wikidata", "municipi_admin_level", "municipi_comarca.id",
           "comarca_osm", "comarca_name:ca", "comarca_regio", "comarca_id", "comarca_type", "comarca_admin_level")
extraCols<- sort(setdiff(names(osm_icgc), c(selCols, rmCols)))
cols<- intersect(c(selCols, extraCols), names(osm_icgc))

osm_icgc<- osm_icgc[, cols]
# save(osm_icgc, file="data/icgc_toponímia-OSM.RData", compress="xz")
load("data/icgc_toponímia-OSM.RData", verbose=TRUE) # osm_icgc
# mapview::mapview(list(osm, icgc), col.regions=list("green","orange"), col=list("green","orange"))


nCols<- sapply(osm_icgc, function(x) sum(!is.na(x)))
plot(sort(nCols, decreasing = TRUE)[1:100])
selCols<- names(nCols)[nCols > 1000]

head(osm_icgc[, selCols], 15)

openxlsx::write.xlsx(osm_icgc[, selCols], file="inst/resultats/icgc_toponímia-OSM_v0.xlsx")


### Coincidències multiples ----
sum(duplicated(osm_icgc$filaICGC))
dup<- dbTools::duplicatedPK(osm_icgc, pk="filaICGC")
dup[, selCols]

noDup<- osm_icgc[!osm_icgc$filaICGC %in% dup$filaICGC, ]

plot(sort(noDup$dist_metres))
plot(sort(dup$dist_metres))

noDup[order(noDup$dist_metres, decreasing=TRUE), selCols]

### No trobats ----
icgc_pendents<- icgc_toponimia[!icgc_toponimia$filaICGC %in% osm_icgc$filaICGC, ]
