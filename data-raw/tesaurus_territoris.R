## Construeix tesaurus de municipis i comarques
# RESULTATS:
load("data/tesaurus_comarques.RData", verbose = TRUE) # tesaurus_comarques
load("data/tesaurus_municipis.RData", verbose = TRUE) # tesaurus_municipis

library(osmdata)

dTM <- openxlsx::read.xlsx("data-raw/icgc_nomenclator_2020.xlsx")
dNGCat <- read.csv2("data-raw/ngcatv10cs0f1r011.txt", fileEncoding = "ISO-8859-15")


## Tesaurus de comarques ----

comarques_TM <- unique(unlist(dTM[, grep("^Comarca\\.", names(dTM))]))
comarques_NGCat <- unique(dNGCat[, grep("^(CodiCom|NomCom)", names(dNGCat))])
comarques_NGCatL <- list()
for (i in 1:5) {
  comarques_NGCatL[[i]] <- unique(comarques_NGCat[, c(i * 2 - 1, i * 2)])
  names(comarques_NGCatL[[i]]) <- c("CodiCom", "NomCom")
}
comarques_NGCat <- unique(do.call(rbind, comarques_NGCatL))

comarques_icgc <- merge(comarques_NGCat, comarques_TM, by = 1, all = TRUE)
names(comarques_icgc) <- paste0("icgc_", names(comarques_icgc))

consulta <- getbb("Catalunya", format_out = "osm_type_id") |>
  opq(osm_types = "relation", out = "tags", timeout = 200) |>
  opq_csv(fields = c("name", "wikidata", "::type", "::id")) |>
  add_osm_feature(key = "admin_level", value = "7") |>
  add_osm_feature(key = "boundary", value = "administrative")
comarques_osm <- osmdata_data_frame(consulta)
names(comarques_osm) <- c("osm_name", "wikidata", "osm_type", "osm_id")
comarques_osm$name <- comarques_osm$osm_name

comarques <- merge(comarques_icgc, comarques_osm, by.x = "icgc_NomCom", by.y = "name", all = TRUE)
comarques[apply(comarques, 1, anyNA), ]
## CONCLUSIONS: Els noms de l'ICGC i OSM coincideixen.
# Moianès encara no apareix a NGCat. comarques_NGCat[comarques_NGCat$CodiCom == "SEG",] sense nom

comarques$icgc_CodiCom[comarques$icgc_NomCom == "Moianès"] <- "MOI"
comarques$icgc_CodiCom[comarques$icgc_NomCom == "la Selva"] <- comarques$icgc_CodiCom[comarques$icgc_NomCom == "Selva"]
comarques$osm_name[comarques$icgc_NomCom == "Selva"] <- comarques$osm_name[comarques$icgc_NomCom == "la Selva"]
comarques$wikidata[comarques$icgc_NomCom == "Selva"] <- comarques$wikidata[comarques$icgc_NomCom == "la Selva"]
comarques$osm_type[comarques$icgc_NomCom == "Selva"] <- comarques$osm_type[comarques$icgc_NomCom == "la Selva"]
comarques$osm_id[comarques$icgc_NomCom == "Selva"] <- comarques$osm_id[comarques$icgc_NomCom == "la Selva"]
comarques <- comarques[!comarques$icgc_NomCom %in% c(NA, ""), ] # neteja
comarques <- comarques[order(comarques$osm_type, comarques$icgc_NomCom), ]
comarques[grep("Selva", comarques$icgc_NomCom), ]

## Cerca a nominatim (no comarques)
comarques_pendents <- comarques[is.na(comarques$osm_id), ]

comarques_nominatim_candidats <- lapply(na.omit(comarques_pendents$icgc_NomCom), function(x) {
  getbb(x, format_out = "data.frame")
})
names(comarques_nominatim_candidats) <- na.omit(comarques_pendents$icgc_NomCom)

comarques[comarques$icgc_NomCom %in% "País Valencià", grep("^osm_(type|id)", names(comarques))] <-
  comarques_nominatim_candidats$`País Valencià`[, c("osm_type", "osm_id")]

comarques[comarques$icgc_NomCom %in% "Catalunya", grep("^osm_(type|id)", names(comarques))] <-
  comarques_nominatim_candidats$Catalunya[
    comarques_nominatim_candidats$Catalunya$osm_type == "relation",
    c("osm_type", "osm_id")
  ]

# Manual
comarques[comarques$icgc_NomCom %in% "Andorra", grep("^osm_(type|id)", names(comarques))] <- c("relation", "9407")
comarques[comarques$icgc_NomCom %in% "Aragó", grep("^osm_(type|id)", names(comarques))] <- c("relation", "349045")
comarques[comarques$icgc_NomCom %in% "França", grep("^osm_(type|id)", names(comarques))] <- c("relation", "1403916")


## Afegeix name d'OSM per les relacions que no són comarques
consulta_osm_name <- opq_osm_id(
  id = comarques$osm_id[comarques$icgc_NomCom %in% comarques_pendents$icgc_NomCom],
  type = "relation", out = "tags"
) |>
  opq_csv(fields = c("name", "wikidata", "::id", "::type"))
no_comarques <- osmdata_data_frame(consulta_osm_name)

for (i in seq_len(nrow(no_comarques))) {
  comarques$osm_name[comarques$osm_id %in% no_comarques$`@id`[i]] <- no_comarques$name[i]
  comarques$wikidata[comarques$osm_id %in% no_comarques$`@id`[i]] <- no_comarques$wikidata[i]
}


comarques[apply(comarques, 1, anyNA), ]

compareDF::view_html(compareDF::compare_df(
  comarques[, names(tesaurus_comarques)],
  tesaurus_comarques,
  group_col = "icgc_NomCom"
))

tesaurus_comarques <- comarques
usethis::use_data(tesaurus_comarques, overwrite = TRUE)
load("data/tesaurus_comarques.rda", verbose = TRUE) # tesaurus_comarques


## Tesaurus de municipis ----

municipis_TM <- unique(unlist(dTM[, grep("^Municipi\\.", names(dTM))]))

## Elimina asterisc dels municipis amb noms no normatius
municipis_TM <- data.frame(
  nom = gsub(" \\*$", "", municipis_TM),
  MunicipiTM = municipis_TM
)

municipis_NGCat <- unique(dNGCat[, grep("^(CodiMun|NomMun)", names(dNGCat))])
municipis_NGCatL <- list()
for (i in 1:5) {
  municipis_NGCatL[[i]] <- unique(municipis_NGCat[, c(i * 2 - 1, i * 2)])
  names(municipis_NGCatL[[i]]) <- c("CodiMun", "NomMun")
}
municipis_NGCat <- unique(do.call(rbind, municipis_NGCatL))
municipis_NGCat$nom <- municipis_NGCat$NomMun

municipis_icgc <- merge(municipis_NGCat, municipis_TM, by = "nom", all = TRUE)
names(municipis_icgc) <- paste0("icgc_", names(municipis_icgc))

municipis_icgc[] <- lapply(municipis_icgc, function(x) {
  x[x == ""] <- NA
  x
})
municipis_icgc <- municipis_icgc[!municipis_icgc$icgc_nom %in% c(NA, "82233"), ]
municipis_icgc[apply(municipis_icgc[, c("icgc_NomMun", "icgc_MunicipiTM")], 1, anyNA), ]

## Clau primària municipis_icgc$icgc_nom
dup <- dbTools::duplicatedPK(municipis_icgc, pk = "icgc_nom")
dbTools::lumpDuplicatedByPK(dup, pk = "icgc_nom")
municipis_icgc <- dbTools::lumpDuplicatedByPK(municipis_icgc, pk = "icgc_nom")

if (is.null(municipis_icgc$dup)) {
  municipis_icgc <- municipis_icgc$df
  message("Duplicats suprimits (NAs)")
} else {
  stop("Hi ha duplicats a la clau primària municipis_icgc$icgc_nom")
  print(municipis_icgc$dup)
}


consulta <- getbb("Catalunya", format_out = "osm_type_id") |>
  opq(osm_types = "relation", out = "tags", timeout = 200) |>
  opq_csv(fields = c("name", "wikidata", "::type", "::id")) |>
  add_osm_feature(key = "admin_level", value = "8") |>
  add_osm_feature(key = "boundary", value = "administrative")
municipis_osm <- osmdata_data_frame(consulta)
names(municipis_osm) <- c("osm_name", "wikidata", "osm_type", "osm_id")
municipis_osm$name <- municipis_osm$osm_name


municipis <- merge(municipis_icgc, municipis_osm, by.x = "icgc_nom", by.y = "name", all = TRUE)
names(municipis) <- gsub("^icgc_nom", "nom", names(municipis))

municipis[apply(municipis, 1, anyNA), ]
municipis[
  apply(municipis[, c("nom", "icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, anyNA),
  c("nom", "icgc_NomMun", "icgc_MunicipiTM", "osm_name")
]
## CONCLUSIONS: discrepàncies entre noms de l'ICGC i OSM.


### Aparella municipis ICGC - OSM ----

municipis_pendents <- municipis[
  apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, function(x) all(is.na(x[1:2])) | is.na(x[3])),
]

noms_icgc_pendents <- unique(na.omit(unlist(municipis_pendents[, c("icgc_NomMun", "icgc_MunicipiTM")])))
municipis_osm_candidats <- lapply(noms_icgc_pendents, function(x) {
  na.omit(agrep(
    paste0("^", gsub(" \\*", "", x)),
    municipis_pendents$osm_name,
    value = TRUE, max.distance = .3, ignore.case = TRUE
  ))
})
names(municipis_osm_candidats) <- noms_icgc_pendents

municipis_osm_candidats[sapply(municipis_osm_candidats, length) > 0]
municipis_trobats <- municipis_osm_candidats[sapply(municipis_osm_candidats, length) == 1]
municipis_trobats
municipis_per_corregir <- municipis_trobats # [setdiff(names(municipis_trobats), "Brunyola")]
## CONCLUSIONS: Brunyola és un cap de municipi, no municipi

for (i in seq_along(municipis_per_corregir)) {
  osm_name <- municipis_per_corregir[[i]]
  icgc_nom <- names(municipis_per_corregir)[i]
  municipis_per_completar <- which(
    (municipis$icgc_NomMun %in% icgc_nom | municipis$icgc_MunicipiTM %in% icgc_nom) &
      is.na(municipis$osm_name)
  )

  if (length(municipis_per_completar) == 0) next

  if (!is.na(municipis$osm_name[municipis_per_completar])) {
    warning(
      "El municipi ja està assignat:\n",
      paste(capture.output(print(
        municipis[municipis_per_completar | municipis$osm_name %in% osm_name, ]
      )), collapse = "\n")
    )
    next
  }

  if (length(municipis_per_completar) > 1) stop("Més d'un municipi per completar")

  municipis[municipis_per_completar, grep("^osm_", names(municipis))] <-
    municipis[municipis$osm_name %in% osm_name, grep("^osm_", names(municipis))]
  municipis$wikidata[municipis_per_completar] <- unique(na.omit(municipis$wikidata[municipis$osm_name %in% osm_name]))

  # Elimina files que només contenen informació del cas per OSM i ja s'han creuat amb files de l'ICGC
  municipis <- municipis[
    !(municipis$osm_name %in% osm_name &
      apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM")], 1, function(x) all(is.na(x)))), # nolint
  ]
}


### Aparella municipis OSM - ICGC ----
municipis_pendents <- municipis[
  apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, function(x) all(is.na(x[1:2])) | is.na(x[3])),
]

noms_osm_pendents <- unique(na.omit(municipis_pendents$osm_name))
municipis_icgc_candidats <- lapply(noms_osm_pendents, function(x) {
  candidats_NGCat <- agrep(
    paste0("^", gsub(" \\*", "", x)),
    municipis_pendents$icgc_NomMun,
    value = TRUE, max.distance = .4, ignore.case = TRUE
  )
  candidats_TM <- agrep(
    paste0("^", gsub(" \\*", "", x)),
    municipis_pendents$icgc_MunicipiTM,
    value = TRUE, max.distance = .4, ignore.case = TRUE
  )
  unique(na.omit(c(candidats_NGCat, candidats_NGCat)))
})
names(municipis_icgc_candidats) <- noms_osm_pendents

municipis_icgc_candidats[sapply(municipis_icgc_candidats, length) > 0]
municipis_trobats <- municipis_icgc_candidats[sapply(municipis_icgc_candidats, length) == 1]
municipis_trobats
municipis_per_corregir <- municipis_trobats

for (i in seq_along(municipis_per_corregir)) {
  icgc_nom <- municipis_per_corregir[[i]]
  osm_name <- names(municipis_per_corregir)[i]
  municipis_per_completar <- which(
    (municipis$icgc_NomMun %in% icgc_nom | municipis$icgc_MunicipiTM %in% icgc_nom) &
      is.na(municipis$osm_name)
  )

  if (length(municipis_per_completar) == 0) next

  if (!is.na(municipis$osm_name[municipis_per_completar])) {
    warning(
      "El municipi ja està assignat:\n",
      paste(capture.output(print(
        municipis[municipis_per_completar | municipis$osm_name %in% osm_name, ]
      )), collapse = "\n")
    )
    next
  }
  municipis[municipis_per_completar, grep("^osm_", names(municipis))] <-
    municipis[municipis$osm_name %in% osm_name, grep("^osm_", names(municipis))]
  municipis$wikidata[municipis_per_completar] <- unique(na.omit(municipis$wikidata[municipis$osm_name %in% osm_name]))

  # Elimina files que només contenen informació del cas per OSM i ja s'han creuat amb files de l'ICGC
  municipis <- municipis[
    !(municipis$osm_name %in% osm_name &
      apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM")], 1, function(x) all(is.na(x)))), # nolint
  ]
}

municipis_pendents <- municipis[
  apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, function(x) all(is.na(x[1:2])) | is.na(x[3])),
]


### Correccions manuals ----

regex_municipis_pendents <- paste0("(", paste(municipis_pendents$nom, collapse = "|"), ")")
municipis[grep(regex_municipis_pendents, municipis$nom, ignore.case = TRUE), ]
municipis[agrep(regex_municipis_pendents, municipis$nom, max.distance = .01, ignore.case = TRUE, fixed = FALSE), ]
municipis[agrep(regex_municipis_pendents, municipis$nom, max.distance = .005, ignore.case = TRUE, fixed = FALSE), ]

municipis[municipis$icgc_NomMun %in% "Vielha e MIjaran", grep("^osm_", names(municipis))] <-
  unique(municipis[municipis$osm_name %in% "Vielha e Mijaran", grep("^osm_", names(municipis))])

municipis[municipis$icgc_NomMun %in% "Roda de Barà", grep("^osm_", names(municipis))] <-
  unique(municipis[municipis$osm_name %in% "Roda de Berà", grep("^osm_", names(municipis))])


municipis[municipis$icgc_MunicipiTM %in% "els Prats del Rei", grep("^osm_", names(municipis))] <-
  unique(municipis[municipis$osm_name %in% "els Prats de Rei", grep("^osm_", names(municipis))])

municipis[municipis$icgc_MunicipiTM %in% "Mont-roig del camp ", grep("^osm_", names(municipis))] <-
  unique(municipis[municipis$osm_name %in% "Mont-roig del Camp", grep("^osm_", names(municipis))])

municipis[municipis$icgc_MunicipiTM %in% "Sant Lulià del Llor i Bonmatí", grep("^osm_", names(municipis))] <-
  unique(municipis[municipis$osm_name %in% "Sant Julià del Llor i Bonmatí", grep("^osm_", names(municipis))])

municipis[municipis$icgc_MunicipiTM %in% "la Garnadella", grep("^osm_", names(municipis))] <-
  municipis[municipis$osm_name %in% "la Granadella", grep("^osm_", names(municipis))]

municipis[municipis$icgc_MunicipiTM %in% "les Piles de Gaià", grep("^osm_", names(municipis))] <-
  municipis[municipis$osm_name %in% "les Piles", grep("^osm_", names(municipis))]


## Caps de municipi, però no municipis
# municipis[municipis$icgc_NomMun %in% "Brunyola", grep("^osm_", names(municipis))] <-
#   municipis[municipis$osm_name %in% "Brunyola i Sant Martí Sapresa", grep("^osm_", names(municipis))]

municipis[municipis$icgc_NomMun %in% "Calonge", grep("^osm_", names(municipis))] <-
  municipis[municipis$osm_name %in% "Calonge i Sant Antoni", grep("^osm_", names(municipis))]

municipis[municipis$icgc_NomMun %in% "Santa Maria de Corcó", grep("^osm_", names(municipis))] <-
  municipis[municipis$osm_name %in% "l'Esquirol", grep("^osm_", names(municipis))]


municipis_pendents <- municipis[
  apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, function(x) all(is.na(x[1:2])) | is.na(x[3])),
]


## Cerca a nominatim (no municipis)
municipis_nominatim_candidats <- lapply(na.omit(municipis_pendents$icgc_MunicipiTM), function(x) {
  getbb(x, format_out = "data.frame")
})
names(municipis_nominatim_candidats) <- na.omit(municipis_pendents$icgc_MunicipiTM)

municipis[municipis$icgc_MunicipiTM %in% "País Valencià", grep("^osm_(type|id)", names(municipis))] <-
  municipis_nominatim_candidats$`País Valencià`[, c("osm_type", "osm_id")]

municipis[municipis$icgc_MunicipiTM %in% "Catalunya", grep("^osm_(type|id)", names(municipis))] <-
  municipis_nominatim_candidats$Catalunya[
    municipis_nominatim_candidats$Catalunya$osm_type == "relation",
    c("osm_type", "osm_id")
  ]

# Manual
municipis[municipis$icgc_MunicipiTM %in% "Andorra", grep("^osm_(type|id)", names(municipis))] <- c("relation", "9407")
municipis[municipis$icgc_MunicipiTM %in% "Aragó", grep("^osm_(type|id)", names(municipis))] <- c("relation", "349045")
municipis[municipis$icgc_MunicipiTM %in% "França", grep("^osm_(type|id)", names(municipis))] <- c("relation", "1403916")

municipis_pendents <- municipis[
  apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, function(x) all(is.na(x[1:2])) | is.na(x[3])),
]


### Afegeix name d'OSM per les relacions que no són municipis ----

consulta_osm_name <- opq_osm_id(
  id = na.omit(municipis$osm_id[is.na(municipis$osm_name)]),
  type = "relation", out = "tags"
) |>
  opq_csv(fields = c("name", "wikidata", "::id", "::type"))
no_municipis <- osmdata_data_frame(consulta_osm_name)

for (i in seq_len(nrow(no_municipis))) {
  municipis$osm_name[municipis$osm_id %in% no_municipis$`@id`[i]] <- no_municipis$name[i]
  municipis$wikidata[municipis$osm_id %in% no_municipis$`@id`[i]] <- no_municipis$wikidata[i]
}

municipis[
  apply(municipis[, c("icgc_NomMun", "icgc_MunicipiTM", "osm_name")], 1, function(x) all(is.na(x[1:2])) | is.na(x[3])),
]

## Afegeix dades d'OSM al antic nom de Castell d'Aro, Platja d'Aro i s'Agaró
municipis$icgc_CodiMun[municipis$nom == "Castell d'Aro, Platja d'Aro i s'Agaró"] <-
  municipis$icgc_CodiMun[municipis$nom == "Castell-Platja d'Aro"]
municipis[municipis$nom == "Castell-Platja d'Aro", c("osm_name", "wikidata", "osm_type", "osm_id")] <-
  municipis[municipis$nom == "Castell d'Aro, Platja d'Aro i s'Agaró", c("osm_name", "wikidata", "osm_type", "osm_id")]


### Complementa wikidata buit de les complecions manuals ----

no_wikidata <- municipis[is.na(municipis$wikidata), ]
for (i in seq_len(nrow(no_wikidata))) {
  muns <- municipis[municipis$osm_id == no_wikidata$osm_id[i], ]

  if (nrow(muns) < 2) next

  municipis$wikidata[municipis$osm_id == no_wikidata$osm_id[i] & is.na(municipis$wikidata)] <-
    unique(na.omit(muns$wikidata))
}
municipis[is.na(municipis$wikidata), ]


### Combina NGCat i toponímia major ----

# Evita la multiplicació de files per cross join amb NAs

municipis0 <- merge(
  unique(municipis[
    apply(municipis[, c("icgc_CodiMun", "icgc_NomMun")], 1, function(x) !all(is.na(x))),
    c("icgc_CodiMun", "icgc_NomMun", "osm_name", "wikidata", "osm_type", "osm_id")
  ]),
  unique(municipis[
    !is.na(municipis$icgc_MunicipiTM),
    c("icgc_MunicipiTM", "osm_name", "wikidata", "osm_type", "osm_id")
  ]),
  all = TRUE
)

# ordena i elimina columna nom
cols <- c("icgc_CodiMun", "icgc_NomMun", "icgc_MunicipiTM", "osm_name", "wikidata", "osm_id", "osm_type")
municipis <- municipis[, cols]
municipis0 <- municipis0[, cols]

compareDF::view_html(compareDF::compare_df(
  df_new = municipis0, df_old = municipis, group_col = "osm_id"
))
dbTools::duplicatedPK(municipis, pk = "osm_id")
dbTools::duplicatedPK(municipis0, pk = "osm_id")
municipis[apply(municipis, 1, anyNA), ] # municipis sense icgc_CodiMun
municipis0[apply(municipis0, 1, anyNA), ] # municipis sense icgc_CodiMun
municipis[apply(municipis[, setdiff(names(municipis), "icgc_CodiMun")], 1, anyNA), ]
municipis0[apply(municipis0[, setdiff(names(municipis0), "icgc_CodiMun")], 1, anyNA), ]

setdiff(municipis_icgc$icgc_NomMun, municipis0$icgc_NomMun)
setdiff(municipis_icgc$icgc_MunicipiTM, municipis0$icgc_MunicipiTM)
setdiff(municipis_icgc$icgc_CodiMun, municipis0$icgc_CodiMun)

setdiff(municipis_icgc$icgc_NomMun, municipis$icgc_NomMun)
setdiff(municipis_icgc$icgc_MunicipiTM, municipis$icgc_MunicipiTM)
setdiff(municipis_icgc$icgc_CodiMun, municipis$icgc_CodiMun)

municipis <- municipis0


# CONCLUSIONS: afegeix icgc_NomMun pels casos que falten, encara que no existeixin a NGCat (cap clau primària buida)
sel <- apply(municipis[, setdiff(names(municipis), "icgc_CodiMun")], 1, anyNA)
municipis[sel, ]
municipis$icgc_NomMun[sel] <- municipis$icgc_MunicipiTM[sel]
grepv("\\*", municipis$icgc_NomMun)
grepv("\\*", municipis$icgc_MunicipiTM)
municipis$icgc_NomMun <- gsub(" \\*", "", municipis$icgc_NomMun)

tesaurus_municipis0 <- rbind(
  municipis[!municipis$osm_id %in% no_municipis$`@id`, ],
  municipis[municipis$osm_id %in% no_municipis$`@id`, ]
)

tesaurus_municipis0[apply(tesaurus_municipis0, 1, function(x) any(is.na(x))), ]

compareDF::view_html(comp <- compareDF::compare_df(
  df_new = tesaurus_municipis0[, names(icgc.osm::tesaurus_municipis)],
  df_old = icgc.osm::tesaurus_municipis,
  group_col = "osm_id"
))
table(trobat <- comp$comparison_df$osm_id %in% tesaurus_municipis0$osm_id)
comp$comparison_df[!trobat, ]


tesaurus_municipis <- tesaurus_municipis0
usethis::use_data(tesaurus_municipis, overwrite = TRUE)
load("data/tesaurus_municipis.rda", verbose = TRUE) # tesaurus_municipis


### CONCLUSIONS municipis ----
# Municipis d'OSM que no són a ICGC https://ca.wikipedia.org/wiki/Mediny%C3%A0

# Municipis amb noms diferents:
tesaurus_municipis[which(tesaurus_municipis$icgc_NomMun != tesaurus_municipis$osm_name), ]
tesaurus_municipis[which(tesaurus_municipis$icgc_MunicipiTM != tesaurus_municipis$osm_name), ]
tesaurus_municipis[which(gsub(" \\*", "", tesaurus_municipis$icgc_MunicipiTM) != tesaurus_municipis$osm_name), ]
