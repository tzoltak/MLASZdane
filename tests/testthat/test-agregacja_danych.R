if (file.exists("wskInd1RM_1szkola.RData")) {
  context("Działanie agreguj_wskazniki() i sprawdz_definicje_grup()")

  load("wskInd1RM_1szkola.RData")
  grupy = data.frame(grupa = 1:3)
  grupy$grupa = list(quote(SZK_kod %in% "AP1185-1161-1"),
                     ~ SZK_kod %in% "AP1185-1161-1",
                     "SZK_kod %in% \"AP1185-1161-1\"")
  grupy$odniesienie = list(quote(SZK_typ %in% "Szkoła policealna"),
                           ~ SZK_typ %in% "Szkoła policealna",
                           "SZK_typ %in% \"Szkoła policealna\"")
  grupy$id_grupa = 1:3
  grupy$id_odniesienie = 11:13

  grupyBledy = data.frame(grupa = 1:3)
  grupyBledy$grupa = list(quote(SZK_koda %in% "AP1185-1161-"),
                          ~ SZK_kod %in% "AP1185-1161-",
                          "SZK_kod %in% \"AP1185-1161-\"")
  grupyBledy$odniesienie = list(quote(SZK_typ %in% "Szkołaa policealna"),
                                ~ SZK_typ %in% "Szkołaa policealna",
                                "SZK_typa %in% \"Szkołaa policealna\"")

  wskZagr = agreguj_wskazniki(wskInd, grupy,
                              liczba_zbadanych = liczba_zbadanych(.data))
  test_that("agreguj_wskazniki() agreguje", {
    expect_type(wskZagr, "list")
    expect_named(wskZagr, c("grupy", "grupyOdniesienia"))
    for (i in names(wskZagr)) {
      expect_is(wskZagr[[i]], "data.frame", label = i)
      expect_named(wskZagr[[i]], c("grupa", "odniesienie", "id_grupa",
                                   "id_odniesienie", "liczba_zbadanych"))
      expect_equal(nrow(wskZagr[[i]]), nrow(grupy))
    }
  })

  test_that("sprawdz_definicje_grup() wyłapuje nieistniejące grupy", {
    expect_error(agreguj_wskazniki(wskInd, grupyBledy))
  })
}
