context("Przetwarzanie danych z BDL")

# Pobieranie danych z API BDL jest procesem może nie tyle czasochłonnym, co
# obarczonym ryzykiem niepowodzenia z różnych względów, a w szczególności
# z powodu przekroczenia absurdalnie niskiego limitu wywołań API, więc nie
# jest tu testowane.

load("wskazniki_BDL.RData")

test_that("przeksztalc_dane_bdl()", {
  wskaznikiBdl = przeksztalc_dane_bdl(get("wskaznikiBdl"), 2017, "SZK_")
  expect_is(wskaznikiBdl, "data.frame")
  expect_equal(nrow(wskaznikiBdl), 380)
  expect_equal(nrow(na.omit(wskaznikiBdl)), 380)
  expect_named(wskaznikiBdl, c('SZK_teryt', 'SZK_powiat', 'powiat_bezrobocie_0m',
                               'powiat_bezrobocie_10m', 'powiat_bezrobocie_11m',
                               'powiat_bezrobocie_12m', 'powiat_bezrobocie_13m',
                               'powiat_bezrobocie_14m', 'powiat_bezrobocie_15m',
                               'powiat_bezrobocie_16m', 'powiat_bezrobocie_17m',
                               'powiat_bezrobocie_1m', 'powiat_bezrobocie_2m',
                               'powiat_bezrobocie_3m', 'powiat_bezrobocie_4m',
                               'powiat_bezrobocie_5m', 'powiat_bezrobocie_6m',
                               'powiat_bezrobocie_7m', 'powiat_bezrobocie_8m',
                               'powiat_bezrobocie_9m', 'powiat_sr_wynagrodzenia_0r'),
               ignore.order = TRUE)
})

rm(wskaznikiBdl)
