## Casa la toponímia major de l'ICGC amb els elements d'OSM

library(toponimsCat)
library(osmdata)
library(sf)
library(pbapply)

d<- openxlsx::read.xlsx("inst/extdata/icgc_nomenclator_2020.xlsx")

# dgeo<- sf::st_as_sf(d, coords=c("UTMX", "UTMY"), crs=sf::st_crs("EPSG:25831")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
# mapview::mapview(dgeo)


## Tesaure de comarques OSM - ICGC ----
comarquesICGC<- sort(unique(unlist(d[, grep("Comarca", names(d))])))
comarquesOSM<- sort(toponimsCat::comarques[toponimsCat::comarques$regio == "Principat", "name"])
osm2icgc<- sapply(comarquesOSM, function(x) {
  if (any(grepl(" ", x))){
    codi<- sapply(strsplit(x, " |'")[[1]][-1], function(y){
      if (substr(y, 1, 1) %in% letters){
        out<- ""
      } else {
        out<- substr(y, 1, 2)
      }
    })
    codi<- paste0(c(substr(x, 1, 1), codi), collapse="")
  } else {
    codi<- substr(x, 1, 3)
  }
  toupper(codi)
})

tesaureComarques<- data.frame(osm=names(osm2icgc), icgc=osm2icgc, row.names=NULL)

## Codis duplicats
dup<- duplicated(tesaureComarques$icgc)
dup<- tesaureComarques$icgc[tesaureComarques$icgc %in% tesaureComarques$icgc[dup]]

## Comarques pendents
tesaureComarques[!tesaureComarques$icgc %in% comarquesICGC, ]
comarquesICGC[!comarquesICGC %in% tesaureComarques$icgc]
dup

# Corregeix codis
tesaureComarques$icgc[tesaureComarques$osm == "Segarra"]<- "SGA"
tesaureComarques$icgc[tesaureComarques$osm == "Segrià"]<- "SGR"
tesaureComarques$icgc[tesaureComarques$osm == "Garraf"]<- "GAF"
tesaureComarques$icgc[tesaureComarques$osm == "Garrotxa"]<- "GAX"

tesaureComarques<- merge(tesaureComarques, toponimsCat::comarques, by.x = "osm", by.y="name", all.x=TRUE)
tesaureComarques[, c("parcial", "historic:admin_level")]<- NULL
tesaureComarques[is.na(tesaureComarques$id),]

comarquesICGC[!comarquesICGC %in% tesaureComarques$icgc]
altresArees<- data.frame(
  osm=c("Andorra", "Aragó", "França", "País Valencià", "Vall d'Aran"),
  icgc=comarquesICGC[!comarquesICGC %in% tesaureComarques$icgc],
  regio=c("Andorra", "Franja de Ponent", "Occitània", "País Valencià", "Vall d'Aran"),
  id=c(9407, 11744144, 3792883, 11739086, 2810776),
  type="relation",
  name=c("Andorra", "Franja de Ponent", "Occitanie", "Municipis de Predomini Lingüístic Valencià", "Val d'Aran"),
  admin_level=NA
)

tesaureComarquesOK<- tesaureComarques
names(tesaureComarquesOK)<- paste0("comarca_", names(tesaureComarquesOK))


## Tesaure de municipis OSM - ICGC ----
municipisICGC<- sort(unique(unlist(d[, grep("Municipi", names(d))])))
municipisOSM<- sort(toponimsCat::municipis[toponimsCat::municipis$regio == "Principat", "name"])

## Municipis pendents
municipisICGC[!municipisICGC %in% municipisOSM]
municipisOSM[!municipisOSM %in% municipisICGC]

## Arregla municipis
icgc2osm<- sapply(municipisICGC, function(x){
  gsub(" \\*$", "", x)
})

tesaureMunicipis<- data.frame(osm=icgc2osm, icgc=names(icgc2osm), row.names=NULL)


### Municipis no trobats ----
tesaureMunicipis[!tesaureMunicipis$osm %in% municipisOSM, ]
municipisOSM[!municipisOSM %in% tesaureMunicipis$osm]

tesaureMunicipis[!tesaureMunicipis$osm %in% toponimsCat::municipis$name, ]
municipisPendents<- tesaureMunicipis$osm[!tesaureMunicipis$osm %in% toponimsCat::municipis$name]
municipisCandidats<- lapply(municipisPendents, function(x){
  toponimsCat::municipis[agrep(x, toponimsCat::municipis$name), ]
})
names(municipisCandidats)<- municipisPendents
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

## Discrepàncies
tesaureMunicipis$osm[tesaureMunicipis$icgc == "?????"]<- "Medinyà" # Entitat de població segons ICGC
tesaureMunicipis$osm[tesaureMunicipis$icgc == "les Piles de Gaià"]<- "les Piles"

## name VS name:ca. Usa name
# tesaureMunicipis$osm[tesaureMunicipis$icgc == "Massanes *"]<- "Maçanes"
# tesaureMunicipis$osm[tesaureMunicipis$icgc == "Capmany *"]<- "Campmany"
# tesaureMunicipis$osm[tesaureMunicipis$icgc == "la Febró"]<- "Febró, la"

tesaureMunicipisOK<- merge(tesaureMunicipis, toponimsCat::municipis, by.x = "osm", by.y="name", all.x=TRUE)
names(tesaureMunicipisOK)<- paste0("municipi_", names(tesaureMunicipisOK))


### CONCLUSIONS municipis ----
## PENDENT de revisar a OSM que name:ca per municipis amb * sigui correcte:
tesaureMunicipis[tesaureMunicipis$icgc != tesaureMunicipis$osm, ]

## Nom de Maçanes està dessincronitzat entre OSM i toponimsCat https://www.openstreetmap.org/changeset/132268084
tesaureMunicipisOK[is.na(tesaureMunicipisOK$municipi_osmId), ]

## Medinyà com a municipi a OSM, però entitat de població segons ICGC


## CONCLUSIONS per ICGC:
## PENDENT: Calonge (Baix Empordà) i els Prats del Rei (Anoia) les Piles de Gaià (Conca de Barberà) són caps de municipi i no municipi
### DISCREPÀNCIES trobades:
tesaureMunicipis[gsub(" \\*", "", tesaureMunicipis$icgc) != tesaureMunicipis$osm, ]


## ICGC: Toponímia major ----

# Naturalitza topònims (mou articles al principi)
d$`name:icgc`<- sapply(d$Topònim, function(x){
  out<- strsplit(x, ", ")[[1]]
  # Separador buit si acaba en apòstrof
  ultimCaracter<- out[length(out)]
  ultimCaracter<- substr(ultimCaracter, nchar(ultimCaracter), nchar(ultimCaracter))
  separador<- ifelse(ultimCaracter == "'", "", " ")
  if (length(out) == 2){
    out<- paste0(out[2], separador, out[1])
    # warning(out)
  } else if (length(out) > 2){
    out<- paste0(out[length(out)], separador, paste(out[-length(out)], collapse=", "), collapse="")
    warning("Més d'una coma a: ", out, "\tori: ", x)
  }

  out
})

d$filaICGC<- 1:nrow(d)

dosm<- merge(d, tesaureMunicipisOK, by.x="Municipi.1", by.y="municipi_icgc", all=TRUE)
dosm<- merge(dosm, tesaureComarquesOK, by.x="Comarca.1", by.y="comarca_icgc", all=TRUE)

dosm<- dosm[!is.na(dosm$Topònim), ]

# reordena columnes
ord<- names(dosm)[-c(1, 2)]
ord<- c(ord[1:(grep("Municipi.2", ord) - 1)], "Municipi.1", ord[grep("Municipi.2", ord):length(ord)])
ord<- c(ord[1:(grep("Comarca.2", ord) - 1)], "Comarca.1", ord[grep("Comarca.2", ord):length(ord)])
dosm<- dosm[, ord]

## Casos sense municipi
noOSMmunicipi<- dosm[is.na(dosm$municipi_id), ]
noICGCmunicipi<- dosm[is.na(dosm$Municipi.1), ]
noOSMmunicipi_ICGCmunicipi<- dosm[is.na(dosm$municipi_id) & !is.na(dosm$Municipi.1), ]
noComarca<- dosm[is.na(dosm$comarca_id) & !is.na(dosm$Comarca.1), ]

lapply(noOSMmunicipi[, grep("Municipi", names(noOSMmunicipi))], table, useNA="ifany")
lapply(noOSMmunicipi[, grep("Comarca", names(noOSMmunicipi))], table, useNA="ifany")

lapply(noICGCmunicipi[, grep("Municipi", names(noICGCmunicipi))], table, useNA="ifany")
lapply(noICGCmunicipi[, grep("Comarca", names(noICGCmunicipi))], table, useNA="ifany")

lapply(noComarca[, grep("Municipi", names(noComarca))], table, useNA="ifany")
lapply(noComarca[, grep("Comarca", names(noComarca))], table, useNA="ifany")

## CONCLUSIÓ: tots els topònims sense municipi d'OSM són de la Val d'Aran, Aragó i França o amb NAs
## 5824 casos sense Municipi.1 (noICGCmunicipi) però amb Comarca.* excepte 10 casos
# noIdGeo<- sf::st_as_sf(noOSMmunicipi_ICGCmunicipi, coords=c("UTMX", "UTMY"), crs=sf::st_crs("EPSG:25831")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
# mapview::mapview(noIdGeo)
icgc_toponimiaMajor<- dosm
# save(icgc_toponimiaMajor, file="data/icgc_toponímiaMajor.RData", compress="xz")
load("data/icgc_toponímiaMajor.RData", verbose=TRUE) # icgc_toponimiaMajor


## Combinació de topònims ICGC- OSM ----

load("data/icgc_toponímiaMajor.RData", verbose=TRUE) # icgc_toponimiaMajor

# icgc_toponimiaMajorGeo<- st_as_sf(icgc_toponimiaMajor, coords=c("UTMX", "UTMY"), remove=FALSE,
#                                   crs=sf::st_crs("EPSG:x")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831

icgc_toponimiaMajor_municipis<- by(icgc_toponimiaMajor, icgc_toponimiaMajor$municipi_osm, function(x) x)
# icgc_toponimiaMajor_municipisGeo<- by(icgc_toponimiaMajorGeo, icgc_toponimiaMajorGeo$municipi_osm, function(x) x)

summary(sapply(icgc_toponimiaMajor_municipis, nrow))
summary(sapply(icgc_toponimiaMajor_municipis, function(x) length(unique(x$`name:icgc`))))
summary(sapply(icgc_toponimiaMajor_municipis, function(x) sum(duplicated(x$`name:icgc`))))


## Cerca per nom i prioritza objectes propers
osm_icgcL<- list()
pb<- timerProgressBar(max=length(icgc_toponimiaMajor_municipis))
for (i in seq_along(icgc_toponimiaMajor_municipis)){
  dicgc<- icgc_toponimiaMajor_municipis[[i]]

  message("\n", i, " / ", length(icgc_toponimiaMajor_municipis), "\t", unique(dicgc$`municipi_name:ca`))

  consulta<- try(opq(bbox = paste0(unique(dicgc$municipi_type), "(id: ", unique(dicgc$municipi_id), ")"),
                 out = "tags center", osm_types = "nwr", timeout = 100) %>%
    add_osm_feature(key="name", value=dicgc$`name:icgc`, match_case=FALSE))

  if (!inherits(consulta, "overpass_query"))
    next

  dosm<- osmdata_data_frame(consulta)

  dosm$`name:minuscula`<- tolower(dosm$name)
  dicgc$`name:minuscula`<- tolower(dicgc$`name:icgc`)
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
  selCols<- c("name:icgc", intersect(selNames, names(dosm_icgc)),
              "wikidata", "filaICGC", "osm_type", "osm_id", "dist_metres", "osm_center_lon", "osm_center_lat", "UTMX", "UTMY")
  rmCols<- c("name:minuscula", "municipi_name:ca", "municipi_regio", "municipi_comarca", "municipi_id", "municipi_type", "municipi_wikidata", "municipi_admin_level", "municipi_comarca.id",
             "comarca_osm", "comarca_name:ca", "comarca_regio", "comarca_id", "comarca_type", "comarca_admin_level")
  extraCols<- setdiff(names(dosm_icgc), c(selCols, rmCols))
  cols<- intersect(c(selCols, extraCols), names(dosm_icgc))

  osm_icgcL[[unique(dicgc$municipi_osm)]]<- dosm_icgc[, cols]
  setTimerProgressBar(pb, i)
}
close(pb)

# save(osm_icgcL, file="data/icgc_toponímiaMajor-OSM_municipisL.RData", compress="xz")
load("data/icgc_toponímiaMajor-OSM_municipisL.RData", verbose=TRUE) # osm_icgcL


osm_icgc<- do.call(dbTools::rbind_addColumns, osm_icgcL)

## Calcula distàncies
icgc<- st_as_sf(osm_icgc, coords=c("UTMX", "UTMY"), remove=FALSE,
                crs=sf::st_crs("EPSG:25831")) # ETRS89 UTM fus 31 Nord https://epsg.io/25831
osm<- st_as_sf(osm_icgc, coords=c("osm_center_lon", "osm_center_lat"), remove=FALSE,
               crs="+proj=longlat +datum=WGS84 +no_defs")
osm<- st_transform(osm, crs = st_crs(icgc))

dist<- st_distance(osm, icgc, by_element = TRUE)
osm_icgc$dist_metres<- as.numeric(dist)
# mapview::mapview(list(osm, icgc), col.regions=list("green","orange"), col=list("green","orange"))


nCols<- sapply(osm_icgc, function(x) sum(!is.na(x)))
plot(sort(nCols, decreasing = TRUE)[1:100])
selCols<- names(nCols)[nCols > 1000]

head(osm_icgc[, selCols], 15)

openxlsx::write.xlsx(osm_icgc[, selCols], file="inst/resultats/icgc_toponímiaMajor-OSM_v0.xlsx")


### Coincidències multiples ----
sum(duplicated(osm_icgc$filaICGC))
dup<- dbTools::duplicatedPK(osm_icgc, pk="filaICGC")
dup[, selCols]

noDup<- osm_icgc[!osm_icgc$filaICGC %in% dup$filaICGC, ]

plot(sort(noDup$dist_metres))
plot(sort(dup$dist_metres))

noDup[order(noDup$dist_metres, decreasing=TRUE), selCols]

### No trobats ----
icgc_pendents<- icgc_toponimiaMajor[!icgc_toponimiaMajor$filaICGC %in% osm_icgc$filaICGC, ]
