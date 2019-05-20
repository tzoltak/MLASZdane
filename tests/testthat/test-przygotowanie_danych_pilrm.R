if (file.exists("MLEZAMiD_absolwent_n2959_20171013.sav")) {
  context("Wczytanie danych z pilotażowej rundy monitoringu")
  wpilrm = wczytaj_wyniki_pilrm("MLEZAiMD_absolwent_n2959_20171013.sav")
  test_that("wczytaj_wyniki_pilrm()", {
    expect_type(wpilrm, "list")
    expect_named(wpilrm, c("dane", "epizody", "gospDom", "czasy"))
    for (i in names(wpilrm)) {
      expect_is(wpilrm[[i]], "data.frame", label = i)
    }
  })

  context("Imputacja czasów rozpoczęcia i zakończenia w danych z pilotażowej rundy monitoringu")
  wipilrm = imputuj_miesiac_pk_pilrm(wpilrm)
  test_that("imputuj_miesiac_pk_pilrm()", {
    expect_type(wipilrm, "list")
    expect_named(wipilrm, c("dane", "epizody", "gospDom", "czasy"))
    for (i in names(wipilrm)) {
      expect_is(wipilrm[[i]], "data.frame", label = i)
    }
  })

  context("Tworzenie zbioru osobo-miesięcy na podstawie danych z pilotażowej rundy monitoringu")
  ompilrm = przygotuj_zbior_osobo_miesiecy_pilrm(wipilrm)
  test_that("przygotuj_zbior_osobo_miesiecy_1rm()", {
    expect_is(ompilrm, "data.frame")
    nazwy = c("ID", "data", "czas", "status", "praca", "nauka", "bezrobocie",
              "praca_a_bezrobocie", "korekta_ciaglosc_nauki",
              "imput_praca", "imput_nauka", "imput_bezrobocie")
    expect_named(ompilrm[, names(ompilrm) %in% nazwy], ignore.order = TRUE)
  })
}
