test_that("consulta_etiquetes_osm works", {
  sel_comarques <- !tesaurus_comarques$icgc_NomCom %in% c("Andorra", "Aragó", "Catalunya", "França", "País Valencià")

  httptest2::with_mock_dir("consulta_comarques", {
    comarques <- consulta_etiquetes_osm(tesaurus_comarques[sel_comarques, ])
  })

  httptest2::with_mock_dir("consulta_noms_comarques", {
    comarques_noms <- consulta_etiquetes_osm(
      tesaurus_comarques[sel_comarques, ],
      etiquetes = c("name", "name:ca", "name:oc")
    )
  })

  httptest2::with_mock_dir("consulta_centre_comarques", {
    comarques_centre <- consulta_etiquetes_osm(
      tesaurus_comarques[sel_comarques, ],
      etiquetes = c("name", "name:ca", "name:oc"),
      centre = TRUE
    )
  })

  expect_s3_class(comarques, "data.frame")
  expect_s3_class(comarques_noms, "data.frame")
  expect_s3_class(comarques_centre, "data.frame")

  expect_true(all(names(tesaurus_comarques) %in% names(comarques)))
  expect_equal(names(tesaurus_comarques), names(comarques)[1:ncol(tesaurus_comarques)])
  expect_named(comarques_noms, c(names(tesaurus_comarques), "name", "name:ca", "name:oc"))
  expect_named(
    comarques_centre,
    c(names(tesaurus_comarques), "osm_center_lon", "osm_center_lat", "name", "name:ca", "name:oc")
  )

  expect_equal(nrow(comarques), sum(sel_comarques))
  expect_equal(nrow(comarques_noms), sum(sel_comarques))
  expect_equal(nrow(comarques_centre), sum(sel_comarques))

  expect_equal(comarques$osm_name, tesaurus_comarques$osm_name[sel_comarques])
  expect_equal(comarques_noms$osm_name, tesaurus_comarques$osm_name[sel_comarques])
  expect_equal(comarques_centre$osm_name, tesaurus_comarques$osm_name[sel_comarques])
})
