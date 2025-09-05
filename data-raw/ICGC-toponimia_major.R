## Casa la toponímia major de l'ICGC amb els elements d'OSM
# https://icgc.cat/Ciutada/Informa-t/Llibres-i-fons-documentals/Llibres-en-PDF/Toponimia/Nomenclator-oficial-de-toponimia-major-de-Catalunya # nolint

library(icgc.osm)
library(openxlsx)
library(osmdata)
library(sf)
library(pbapply)


## ICGC: Toponímia major ----
d <- read.xlsx("data-raw/icgc_nomenclator_2020.xlsx")

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
## CONCLUSIONS: les coordenades combinades amb el topònim no són úniques i hi ha 3 files duplicades

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


### Combina Municipi.1 i Comarca.1 amb els municipis i comarques d'OSM ----

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
## CONCLUSIONS: tots els topònims tenen municipi o comarca, excepte formes no normatives

icgc_toponimiaMajor <- dosm[order(dosm$`ref:icgc`), ]
rownames(icgc_toponimiaMajor) <- NULL

usethis::use_data(icgc_toponimiaMajor, overwrite = TRUE)
load("data/icgc_toponimiaMajor.rda", verbose = TRUE) # icgc_toponimiaMajor
