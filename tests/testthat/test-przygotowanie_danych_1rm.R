if (dir.exists("../../data_local")) {
  context("Wczytanie danych z 1. rundy monitoringu")
  w1rm = wczytaj_wyniki_1rm("../../data_local/MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav")
  test_that("wczytaj_wyniki_1rm()", {

    expect_type(w1rm, "list")
    expect_named(w1rm, c("dane", "epizody", "gospDom"))
    for (i in names(w1rm)) {
      expect_is(w1rm[[i]], "data.frame", label = i)
    }
  })

  context("Imputacja czasów rozpoczęcia i zakończenia w danych z 1. rundy monitoringu")
  wi1rm = imputuj_miesiac_pk_1rm(w1rm)
  test_that("imputuj_miesiac_pk_pilrm()", {
    expect_type(wi1rm, "list")
    expect_named(wi1rm, c("dane", "epizody", "gospDom"))
    for (i in names(wi1rm)) {
      expect_is(wi1rm[[i]], "data.frame", label = i)
    }
  })

  context("Tworzenie zbioru osobo-miesięcy na podstawie danych z 1. rundy monitoringu")
  om1rm = przygotuj_zbior_osobo_miesiecy_1rm(wi1rm)
  test_that("przygotuj_zbior_osobo_miesiecy_1rm()", {
    expect_is(om1rm, "data.frame")
    nazwy = c("ID", "data", "czas", "status", "praca", "nauka", "bezrobocie",
              "praca_a_bezrobocie", "korekta_ciaglosc_nauki",
              "imput_praca", "imput_nauka", "imput_bezrobocie")
    expect_named(om1rm[, names(om1rm) %in% nazwy], ignore.order = TRUE)
  })
}
