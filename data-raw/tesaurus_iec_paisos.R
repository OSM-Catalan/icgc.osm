## Construeix tesaurus de països pel nomenclator mundial de l'IEC i OSM
# RESULTATS:
load("data/tesaurus_iec_paisos.rda", verbose = TRUE) # tesaurus_iec_paisos

library(osmdata)

load("data/iec_nomenclatorMundial.rda", verbose = TRUE) # iec_nomenclatorMundial

## Tesaurus de països ----

iec_pais <- na.omit(unique(iec_nomenclatorMundial$iec_pais))
iec_nom <- gsub("(.+), (.+)", "\\2 \\1", iec_pais)
iec_nom <- gsub("' ", "'", iec_nom)
paisos_iec <- data.frame(iec_pais, iec_nom)
paisos_monitorOSM <- monitorOSM::estats

table(paisos_iec$iec_pais %in% paisos_monitorOSM$`name:ca`)
table(paisos_iec$iec_nom %in% paisos_monitorOSM$`name:ca`)


paisos_coincidents <- merge(
  paisos_iec, paisos_monitorOSM,
  by.x = "iec_nom", by.y = "name:ca", all.x = TRUE
)
paisos_pendents <- na.omit(paisos_coincidents$iec_nom[is.na(paisos_coincidents$osm_id)])
paisos_pendents
## CONCLUSIONS: falten 64 països. Els de monitorOSM que no corresponen es poden
# descartar
paisos_coincidents <- paisos_coincidents[!is.na(paisos_coincidents$osm_id), ]


## Cerca a nominatim els pendents ----

paisos_nominatim_candidats0 <- lapply(paisos_pendents, function(x) {
  osmdata::getbb(x, format_out = "data.frame", featuretype = "country")
})
names(paisos_nominatim_candidats0) <- paisos_pendents

# estats_nominatim_candidats <- lapply(paisos_pendents, function(x) {
#   osmdata::getbb(x, format_out = "data.frame", featuretype = "state")
# })
# names(estats_nominatim_candidats) <- paisos_pendents
#
# establiments_nominatim_candidats <- lapply(paisos_pendents, function(x) {
#   osmdata::getbb(x, format_out = "data.frame", featuretype = "settlement")
# })
# names(establiments_nominatim_candidats) <- paisos_pendents

paisos_pendents_error <- names(which(sapply(paisos_nominatim_candidats0, nrow) == 0))
paisos_nominatim_candidats0 <- paisos_nominatim_candidats0[
  sapply(paisos_nominatim_candidats0, nrow) > 0
]
paisos_nominatim_candidats0 <- lapply(paisos_nominatim_candidats0, function(x) {
  x$licence <- NULL
  x
})

# estats_pendents_error <- names(which(!sapply(estats_nominatim_candidats, inherits, what = "data.frame")))
# estats_nominatim_candidats <- estats_nominatim_candidats[
#   sapply(estats_nominatim_candidats, inherits, what = "data.frame")
# ]
# estats_nominatim_candidats <- lapply(estats_nominatim_candidats, function(x) {
#   x$licence <- NULL
#   x
# })
#
# establiments_pendents_error <- names(which(!sapply(establiments_nominatim_candidats, inherits, what = "data.frame")))
# establiments_nominatim_candidats <- establiments_nominatim_candidats[
#   sapply(establiments_nominatim_candidats, inherits, what = "data.frame")
# ]
# establiments_nominatim_candidats <- lapply(establiments_nominatim_candidats, function(x) {
#   x$licence <- NULL
#   x
# })
#
# setdiff(names(paisos_nominatim_candidats0), names(estats_nominatim_candidats))
# setdiff(names(estats_nominatim_candidats), names(paisos_nominatim_candidats0))
# setdiff(names(paisos_nominatim_candidats0), names(establiments_nominatim_candidats))
# setdiff(names(establiments_nominatim_candidats), names(paisos_nominatim_candidats0))
# diffobj::diffObj(
#   lapply(paisos_nominatim_candidats0, function(x) x[, setdiff(names(x), "place_id")]),
#   lapply(estats_nominatim_candidats, function(x) x[, setdiff(names(x), "place_id")])
# )
# diffobj::diffObj(
#   lapply(paisos_nominatim_candidats0, function(x) x[, setdiff(names(x), "place_id")]),
#   lapply(establiments_nominatim_candidats, function(x) x[, setdiff(names(x), "place_id")])
# )
## CONCLUSIONS: paisos dona millor resultats pq no inclou nodes

table(sapply(paisos_nominatim_candidats0, function(x) any(x$osm_type != "node")))
nomes_nodes <- sapply(paisos_nominatim_candidats0, function(x) all(x$osm_type == "node") & nrow(x) > 1)
paisos_nominatim_candidats0[nomes_nodes]
paisos_nominatim_candidats0 <- lapply(paisos_nominatim_candidats0, function(x) {
  x[x$osm_type != "node", ]
})


table(sapply(paisos_nominatim_candidats0, nrow))
paisos_nominatim0 <- do.call(rbind, paisos_nominatim_candidats0[sapply(paisos_nominatim_candidats0, nrow) == 1])


table(paisos_nominatim0$addresstype)
table(paisos_nominatim0[, c("addresstype", "type")])
table(paisos_nominatim0[, c("addresstype", "class")])

split(
  paisos_nominatim0[, c("osm_type", "osm_id", "class", "type", "place_rank", "addresstype", "name", "display_name")],
  paisos_nominatim0$addresstype
)

# Selecciona països correctes de nominatim
paisos_nominatim <- paisos_nominatim0[paisos_nominatim0$addresstype %in% c(
  "administrative", # OK: Sàhara Occidental -> relation 2559126
  "archipelago", # OK: illes Australs -> rel 16111361; illes Òrcades Australs -> rel 938061;
  # illes Pitcairn -> rel 2185375; illes Salomó -> rel 9526687; illes Shetland del Sud -> rel938065;
  # Nova Caledònia -> rel 3407643
  "country", "continent", # OK
  "county", # OK: illa Bouvet -> relation 2425963
  # "lake", # INCORRECTE: les TAAF (Terres Australs Antàrtiques Franceses) -> relation 2186658
  "municipality", # OK: illes Éparses -> relation 6063099
  "region", # OK: illa de l'Ascensió -> 156166; INCORRECTE: Svalbard i Jan Mayen -> relation 3245620
  "state", # OK, excepte INCORRECTE: la Reunió -> relation 1785276
  "territory" # OK
), ]
## Elimina casos erronis
paisos_nominatim <- paisos_nominatim[!rownames(paisos_nominatim) %in% c("Svalbard i Jan Mayen", "la Reunió"), ]

# Països pendents
paisos_nominatim_candidats <- paisos_nominatim_candidats0[
  !names(paisos_nominatim_candidats0) %in% rownames(paisos_nominatim)
]

lapply(paisos_nominatim_candidats, function(x) {
  table(x[, c("type", "class")])
})
lapply(paisos_nominatim_candidats, function(x) {
  table(x$addresstype)
})


### Selecció manual ----

paisos_nominatim_candidats$Åland <- paisos_nominatim_candidats$Åland[
  paisos_nominatim_candidats$Åland$osm_id == "1650407",
]
paisos_nominatim_candidats$Aruba <- paisos_nominatim_candidats$Aruba[
  paisos_nominatim_candidats$Aruba$osm_id == "1231749",
]
paisos_nominatim_candidats$Clipperton <- paisos_nominatim_candidats$Clipperton[
  paisos_nominatim_candidats$Clipperton$osm_id == "2573009",
]
paisos_nominatim_candidats$`Estats Units` <- paisos_nominatim_candidats$`Estats Units`[
  paisos_nominatim_candidats$`Estats Units`$osm_id == "148838",
]
paisos_nominatim_candidats$Guadalupe <- paisos_nominatim_candidats$Guadalupe[
  paisos_nominatim_candidats$Guadalupe$osm_id == "1401835",
]
paisos_nominatim_candidats$`Guaiana Francesa` <- paisos_nominatim_candidats$`Guaiana Francesa`[
  paisos_nominatim_candidats$`Guaiana Francesa`$osm_id == "1260551",
]
paisos_nominatim_candidats$Guam <- paisos_nominatim_candidats$Guam[
  paisos_nominatim_candidats$Guam$osm_id == "306001",
]
paisos_nominatim_candidats$`illa de Man` <- paisos_nominatim_candidats$`illa de Man`[
  paisos_nominatim_candidats$`illa de Man`$osm_id == "62269",
]
## NO TROBAT: illes Cocos (Keeling) -> rel 82636
paisos_nominatim_candidats$`illes Cocos (Keeling)` <- paisos_nominatim_candidats$`illes Cocos (Keeling)`[
  paisos_nominatim_candidats$`illes Cocos (Keeling)`$osm_id == "82636", ## No trobat
]
paisos_nominatim_candidats$`illes Fèroe` <- paisos_nominatim_candidats$`illes Fèroe`[
  paisos_nominatim_candidats$`illes Fèroe`$osm_id == "52939",
]
## NO TROBAT: la Reunió -> rel 1785276
paisos_nominatim_candidats$`la Reunió` <- paisos_nominatim_candidats$`la Reunió`[
  paisos_nominatim_candidats$`la Reunió`$osm_id == "1785276", ## No trobat
]
## NO TROBAT:
paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)` <-
  paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)`[
    paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)`$osm_id == "2186658", ## No trobat
  ]
paisos_nominatim_candidats$Martinica <- paisos_nominatim_candidats$Martinica[
  paisos_nominatim_candidats$Martinica$osm_id == "1891495",
]
paisos_nominatim_candidats$Mayotte <- paisos_nominatim_candidats$Mayotte[
  paisos_nominatim_candidats$Mayotte$osm_id == "1259885",
]
paisos_nominatim_candidats$Montserrat <- paisos_nominatim_candidats$Montserrat[
  paisos_nominatim_candidats$Montserrat$osm_id == "537257",
]
paisos_nominatim_candidats$Palestina <- paisos_nominatim_candidats$Palestina[
  paisos_nominatim_candidats$Palestina$osm_id == "1703814",
]
paisos_nominatim_candidats$`Saint Helena` <- paisos_nominatim_candidats$`Saint Helena`[
  paisos_nominatim_candidats$`Saint Helena`$osm_id == "155987",
]
## NO TROBAT: Santa Seu -> rel 1703814
paisos_nominatim_candidats$`Santa Seu` <- paisos_nominatim_candidats$`Santa Seu`[
  paisos_nominatim_candidats$`Santa Seu`$osm_id == "1703814", ## No trobat
]
paisos_nominatim_candidats$`Sint Maarten` <- paisos_nominatim_candidats$`Sint Maarten`[
  paisos_nominatim_candidats$`Sint Maarten`$osm_id == "1231790",
]
## NO TROBAT: Svalbard i Jan Mayen -> rel 3245620
paisos_nominatim_candidats$`Svalbard i Jan Mayen` <- paisos_nominatim_candidats$`Svalbard i Jan Mayen`[
  paisos_nominatim_candidats$`Svalbard i Jan Mayen`$osm_id == "3245620",
]
paisos_nominatim_candidats$`Xina - Hong Kong` <- paisos_nominatim_candidats$`Xina - Hong Kong`[
  paisos_nominatim_candidats$`Xina - Hong Kong`$osm_id == "913110",
]

names(paisos_nominatim_candidats[sapply(paisos_nominatim_candidats, function(x) is.null(x) || nrow(x) == 0)])


### Cerca manual ----
names(paisos_nominatim_candidats[sapply(paisos_nominatim_candidats, nrow) != 1])

paisos_nominatim_candidats$`illes Cocos (Keeling)` <- osmdata::getbb(
  "Cocos (Keeling) Islands / Pulu Kokos (Keeling)",
  format_out = "data.frame"
)
paisos_nominatim_candidats$`illes Cocos (Keeling)` <- paisos_nominatim_candidats$`illes Cocos (Keeling)`[
  paisos_nominatim_candidats$`illes Cocos (Keeling)`$osm_id == "82636",
]
paisos_nominatim_candidats$`la Reunió` <- osmdata::getbb(
  "illa de la Reunió",
  format_out = "data.frame", featuretype = "state"
)
paisos_nominatim_candidats$`la Reunió` <- paisos_nominatim_candidats$`la Reunió`[
  paisos_nominatim_candidats$`la Reunió`$osm_id == "1785276",
]
paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)` <- osmdata::getbb(
  "Terres australes et antarctiques françaises",
  format_out = "data.frame"
)
paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)` <-
  paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)`[
    paisos_nominatim_candidats$`les TAAF (Terres Australs Antàrtiques Franceses)`$osm_id == "2186658",
  ]
paisos_nominatim_candidats$`Santa Seu` <- osmdata::getbb("Vaticà", format_out = "data.frame")
paisos_nominatim_candidats$`Santa Seu` <- paisos_nominatim_candidats$`Santa Seu`[
  paisos_nominatim_candidats$`Santa Seu`$osm_id == "36989",
]

## NO TROBAT:
paisos_nominatim_candidats$`Svalbard i Jan Mayen` <- osmdata::getbb("Svalbard og Jan Mayen", format_out = "data.frame")
# https://nominatim.openstreetmap.org/ui/details.html?osmtype=R&osmid=3245620&class=boundary
paisos_nominatim_candidats$`Svalbard i Jan Mayen` <- data.frame(
  place_id = "252972723", osm_type = "relation", osm_id = "3245620",
  lat = "78.5124027", lon = "16.6055741", class = "boundary", type = "statistical",
  place_rank = 0, importance = 0.06667666666666666, addresstype = "statistical",
  name = "Svalbard og Jan Mayen", display_name = "Svalbard og Jan Mayen, 9170, Norge", boundingbox = NA
)


### Afegeix paisos amb error a la consulta nominatim (resultats buits) ----
paisos_pendents_error
paisos_nominatim_error <- lapply(paisos_pendents_error, function(x) {
  x <- gsub("^Antilles Neerlandeses$", "Caribisch Nederland", x)
  x <- gsub("^Kirguizstan$", "Kyrgyzstan", x)
  x <- gsub("^illes Marianes del Nord$", "Northern Mariana Islands", x)
  x <- gsub("^Illes Menors Allunyades dels Estats Units$", "United States Minor Outlying Islands", x)
  x <- gsub("^illes Òrcades Australs$", "South Orkney Islands", x)
  x <- gsub("^illes Verges dels Estats Units$", "United States Virgin Islands", x)
  x <- gsub("^illes Heard i McDonald$", "Heard Island and McDonald Islands", x)
  x <- gsub("^illes Tristan da Cunha$", "Tristan da Cunha", x)
  x <- gsub("^[iI]lles", "islands", x)

  osmdata::getbb(x, format_out = "data.frame")
})
names(paisos_nominatim_error) <- paisos_pendents_error

paisos_nominatim_error$`Antilles Neerlandeses` <- paisos_nominatim_error$`Antilles Neerlandeses`[
  paisos_nominatim_error$`Antilles Neerlandeses`$osm_id == "1216720",
]
paisos_nominatim_error$`Illes Menors Allunyades dels Estats Units` <-
  paisos_nominatim_error$`Illes Menors Allunyades dels Estats Units`[
    paisos_nominatim_error$`Illes Menors Allunyades dels Estats Units`$osm_id == "2185386",
  ]
paisos_nominatim_error$`illes Tokelau` <- paisos_nominatim_error$`illes Tokelau`[
  paisos_nominatim_error$`illes Tokelau`$osm_id == "2186600",
]
paisos_nominatim_error$`illes Tristan da Cunha` <- paisos_nominatim_error$`illes Tristan da Cunha`[
  paisos_nominatim_error$`illes Tristan da Cunha`$osm_id == "3672278",
]
paisos_nominatim_error$Kirguizstan <- paisos_nominatim_error$Kirguizstan[
  paisos_nominatim_error$Kirguizstan$osm_id == "178009",
]


table(sapply(paisos_nominatim_error, nrow))


## Afegeix errors corregits
if (all(correcte <- sapply(paisos_nominatim_error, function(x) is.data.frame(x) && nrow(x) == 1))) {
  paisos_nominatim_candidats <- c(paisos_nominatim_candidats, paisos_nominatim_error)
} else {
  err <- paisos_nominatim_error[!correcte]
  stop("Paisos amb amb errors: ", paste(names(err), collapse = ", "))
}
paisos_nominatim_candidats <- lapply(paisos_nominatim_candidats, function(x) {
  x$licence <- NULL
  x
})


### Uneix ----

paisos_nominatim_manual <- do.call(
  rbind,
  paisos_nominatim_candidats[sapply(paisos_nominatim_candidats, nrow) == 1]
)
paisos_nominatim_candidats <- paisos_nominatim_candidats[
  sapply(paisos_nominatim_candidats, nrow) > 1
]

paisos_nominatim_revisats <- rbind(paisos_nominatim, paisos_nominatim_manual)
tesaurus_paisos <- rbind(
  paisos_coincidents[, c("iec_nom", "osm_type", "osm_id")],
  data.frame(
    iec_nom = rownames(paisos_nominatim_revisats),
    paisos_nominatim_revisats[, c("osm_type", "osm_id")]
  )
)

setdiff(paisos_iec$iec_nom, tesaurus_paisos$iec_nom)
setdiff(tesaurus_paisos$iec_nom, paisos_iec$iec_nom)


## Consulta paisos a OSM ----
paisos_osm <- osmdata::opq_osm_id(
  id = tesaurus_paisos$osm_id,
  type = tesaurus_paisos$osm_type,
  out = "tags"
) |>
  osmdata::opq_csv(
    fields = c(
      setdiff(names(paisos_monitorOSM), c("osm_type", "osm_id")),
      "::type", "::id"
    )
  ) |>
  osmdata::osmdata_data_frame()
names(paisos_osm) <- gsub("^@", "osm_", names(paisos_osm))

table(paisos_osm$osm_type)
ord <- match(paisos_osm$osm_id, tesaurus_paisos$osm_id)
rownames(paisos_osm) <- tesaurus_paisos$iec_nom[ord]
paisos_osm[, c("name:ca", "name")]

all(rownames(paisos_osm) %in% paisos_iec$iec_nom)


tesaurus_iec_paisos <- merge(
  paisos_iec, paisos_osm,
  by.x = "iec_nom", by.y = 0, all.y = TRUE
)
rownames(tesaurus_iec_paisos) <- NULL
all(rownames(tesaurus_iec_paisos$iec_nom) %in% paisos_iec$iec_nom)
all(rownames(icgc.osm::tesaurus_iec_paisos$iec_nom) %in% paisos_iec$iec_nom)


## TODO: països pendents d'afegir a monitorOSM i de traduïr a OSM ----

tesaurus_iec_paisos[is.na(tesaurus_iec_paisos$`name:ca`), ]
tesaurus_iec_paisos[!tesaurus_iec_paisos$osm_id %in% monitorOSM::estats$osm_id, ]

sel_cols <- c("iec_nom", "iec_pais", "name:ca", "name", "wikidata", "osm_type", "osm_id")
tesaurus_iec_paisos <- tesaurus_iec_paisos[, sel_cols]

cols <- c("osm_id", setdiff(names(tesaurus_iec_paisos), "osm_id"))
diferencies <- compareDF::compare_df(
  df_new = tesaurus_iec_paisos[, cols],
  df_old = icgc.osm::tesaurus_iec_paisos[, cols],
  group_col = "iec_nom"
)
compareDF::view_html(diferencies)

usethis::use_data(tesaurus_iec_paisos, overwrite = TRUE)
load("data/tesaurus_iec_paisos.rda", verbose = TRUE) # tesaurus_iec_paisos

openxlsx::write.xlsx(
  tesaurus_iec_paisos,
  file = "inst/resultats/tesaurus_iec_paisos.xlsx",
  rowNames = FALSE, borders = "surrounding", colWidths = "auto",
  firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)


## Comprova etiquetatge ----

paisos_etiquetes_osm <- osmdata::opq_osm_id(
  id = tesaurus_iec_paisos$osm_id, type = tesaurus_iec_paisos$osm_type, out = "tags"
) |>
  osmdata::osmdata_data_frame()

grepv("name", names(paisos_etiquetes_osm), invert = TRUE)
sel_etiquetes_revisar <- c(
  "admin_level", "border_type", "boundary", "capital", "capital_city", "ISO3166-1", "ISO3166-2",
  "land_area", "landuse", "maritime", "state_code", "wikidata"
)
lapply(paisos_etiquetes_osm[, sel_etiquetes_revisar], table, useNA = "always")


sel_etiquetes <- c("osm_id", "osm_type", "name", "name:ca", sel_etiquetes_revisar)

comprova_admin_level <- paisos_etiquetes_osm[which(as.integer(paisos_etiquetes_osm$admin_level) > 4), ]
comprova_admin_level[, sel_etiquetes]
## CONCLUSIONS: corregit "illes Éparses" (era admin_level=10)  i "illes Cocos (Keeling)"

comprova_boundary <- paisos_etiquetes_osm[!paisos_etiquetes_osm$boundary %in% c("administrative", "continent"), ]
comprova_boundary[, sel_etiquetes]
tesaurus_iec_paisos[tesaurus_iec_paisos$osm_id %in% comprova_boundary$osm_id, ]
## CONCLUSIONS: CORREGIT Antilles Neerlandeses -> rel 1216720; Illa Bouvet -> rel 2425963.
# Per Illes Salomó no tenen relació millor amb boundary o admin_level

comprova_ISO3166 <- paisos_etiquetes_osm[is.na(paisos_etiquetes_osm$`ISO3166-1`), ]
comprova_ISO3166[, sel_etiquetes]
tesaurus_iec_paisos[tesaurus_iec_paisos$osm_id %in% comprova_ISO3166$osm_id, ]
## CONCLUSIONS: endavant

paisos_etiquetes_osm[!is.na(paisos_etiquetes_osm$landuse), sel_etiquetes]
paisos_etiquetes_osm[is.na(paisos_etiquetes_osm$wikidata), sel_etiquetes]
