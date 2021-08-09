if (file.exists("dane_2rm.RData")) {
  load("dane_2rm.RData")
  wskCawi = agreguj_cawi_ucz_2rm(dane_cut, gru_cut)

  test_that("agreguj_cawi_ucz_2rm() agreguje", {
    expect_type(wskCawi, "list")
    expect_named(wskCawi, c("grupy", "grupyOdniesienia"))
    for (i in names(wskCawi)) {
      expect_is(wskCawi[[i]], "data.frame", label = i)
      expect(any(names(wskCawi[[i]]) %in% (c("grupa", "odniesienie", "firma",
                                         "formy", "nauka_zawod",
                                         "plany_edu_tak"))),
             "Nazwy kolumn obiektu nie zawierają wymaganych nazw.")
      expect_equal(nrow(wskCawi[[i]]), nrow(gru_cut))
    }
  })

  wskCawiSplGrupy = splaszcz_wskazniki_zagregowane(wskCawi$grupy)
  wskCawiSplGrupyOdn = splaszcz_wskazniki_zagregowane(wskCawi$grupyOdniesienia)
  test_that("Zbiory wsk. zagregowanych z 2. RM dają się spłaszczyć.", {
    expect_is(wskCawiSplGrupy, "data.frame")
    expect_equal(ncol(wskCawiSplGrupy), 179)
    expect_equal(nrow(wskCawiSplGrupy), 20)
    expect_is(wskCawiSplGrupyOdn, "data.frame")
    expect_equal(ncol(wskCawiSplGrupyOdn), 179)
    expect_equal(nrow(wskCawiSplGrupyOdn), 20)
  })
}
