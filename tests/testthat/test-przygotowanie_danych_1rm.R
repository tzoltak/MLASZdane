if (file.exists("MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav")) {
  context("Wczytanie danych z 1. rundy monitoringu")

  w1rm = wczytaj_wyniki_1rm("MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav")
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

  context("Obliczenie wskaźników na poziomie indywidualnym")

  wskInd = oblicz_wskazniki_ind_1rm(wi1rm)
  test_that("oblicz_wskazniki_ind_1r()", {
    expect_is(wskInd, "data.frame")
    expect_equal(nrow(wskInd), nrow(wi1rm$dane))
    nazwy = c('ID_RESP', 'SZK_typ', 'SZK_kod', 'SZK_pozycja_w_operacie',
              'SZK_teryt', 'SZK_regon', 'SZK_id_sio',
              'UCZ_kod_zawodu', 'UCZ_zawod', 'UCZ_branza', 'UCZ_obszar',
              'UCZ_plec',
              'matura_zdana', 'egz_zaw_zdany',
              'pio1_pierwsza', 'pio4_pierwsza', 'pi5_pierwsza',
              'praca_czas_rozp_pierwsza',
              'praca_przed_ukonczeniem_szkoly_pierwsza',
              'pg2gh.1_pierwsza', 'pg2gh.2_pierwsza', 'pg2gh.3_pierwsza',
              'pg2gh.4_pierwsza', 'pg2gh.5_pierwsza', 'pg2gh.6_pierwsza',
              'pg2gh.7_pierwsza', 'pg2gh.8_pierwsza', 'pg2gh.9_pierwsza',
              'pg2i.1_pierwsza', 'pg2i.2_pierwsza', 'pg2i.3_pierwsza',
              'pg2i.9_pierwsza',
              'pio1_ostatnia', 'pio4_ostatnia', 'po5_ostatnia',
              'po6_1_ostatnia', 'po6_2_ostatnia', 'po6_3_ostatnia',
              'po6_4_ostatnia', 'po6_5_ostatnia', 'po6_6_ostatnia',
              'pg2gh.1_ostatnia', 'pg2gh.2_ostatnia', 'pg2gh.3_ostatnia',
              'pg2gh.4_ostatnia', 'pg2gh.5_ostatnia', 'pg2gh.6_ostatnia',
              'pg2gh.7_ostatnia', 'pg2gh.8_ostatnia', 'pg2gh.9_ostatnia',
              'pg2i.1_ostatnia', 'pg2i.2_ostatnia', 'pg2i.3_ostatnia',
              'pg2i.9_ostatnia',
              'pg2gh.1_6m', 'pg2gh.2_6m', 'pg2gh.3_6m', 'pg2gh.4_6m', 'pg2gh.5_6m',
              'pg2gh.6_6m', 'pg2gh.7_6m', 'pg2gh.8_6m', 'pg2gh.9_6m',
              'pg2i.1_6m', 'pg2i.2_6m', 'pg2i.3_6m', 'pg2i.9_6m',
              'pg2gh.1_9m', 'pg2gh.2_9m', 'pg2gh.3_9m', 'pg2gh.4_9m', 'pg2gh.5_9m',
              'pg2gh.6_9m', 'pg2gh.7_9m', 'pg2gh.8_9m', 'pg2gh.9_9m',
              'pg2i.1_9m', 'pg2i.2_9m', 'pg2i.3_9m', 'pg2i.9_9m',
              'praca_czas_gdy_bez_nauki_p9m', 'praca_czas_gdy_nauka_p9m',
              'praca_czas_p9m',
              'praca_czas_gdy_bez_nauki_uop_p9m',
              'praca_czas_gdy_nauka_uop_p9m', 'praca_czas_uop_p9m',
              'bezrobocie_czas_gdy_bez_nauki_p9m',
              'bezrobocie_czas_gdy_nauka_p9m', 'bezrobocie_czas_p9m',
              'bezrobocie_1m', 'bezrobocie_2m', 'bezrobocie_3m',
              'bezrobocie_4m', 'bezrobocie_5m', 'bezrobocie_6m',
              'bezrobocie_7m', 'bezrobocie_8m', 'bezrobocie_9m',
              'nauka_6m', 'nauka_platna_6m', 'nauka_9m', 'nauka_platna_9m',
              'studia_kierunek_pierwsze', 'studia_uczelnia_pierwsze',
              'studia_bezplatne_pierwsze', 'studia_tryb_pierwsze',
              'praca_nauka_0m', 'praca_nauka_1m', 'praca_nauka_2m',
              'praca_nauka_3m', 'praca_nauka_4m', 'praca_nauka_5m',
              'praca_nauka_6m', 'praca_nauka_7m', 'praca_nauka_8m',
              'praca_nauka_9m')
    expect_named(wskInd[, names(wskInd) %in% nazwy], ignore.order = TRUE)
  })

  if (file.exists("wskazniki_BDL.RData")) {
    context("Obliczenie wskaźników na poziomie zagregowanym")

    wskaznikiBdlNazwa = load("wskazniki_BDL.RData")
    wskaznikiBdl = przeksztalc_dane_bdl(get(wskaznikiBdlNazwa), 2017, "SZK_")
    wskaznikiInd = dplyr::left_join(wskInd, wskaznikiBdl)
    set.seed(71624681)
    wskaznikiInd = wskaznikiInd[wskaznikiInd$SZK_kod %in% sample(unique(wskaznikiInd$SZK_kod), 20), ]

    nazwy = c('SZK_typ', 'liczba_zbadanych', 'liczba_zbadanych_kobiet',
              'liczba_szkol', 'zawody', 'praca_nauka_0m', 'praca_nauka_1m',
              'praca_nauka_2m', 'praca_nauka_3m', 'praca_nauka_4m',
              'praca_nauka_5m', 'praca_nauka_6m', 'praca_nauka_7m',
              'praca_nauka_8m', 'praca_nauka_9m',
              'egz_zaw_zdawalnosc', 'matura_zdawalnosc',
              'praca_przed_ukonczeniem_szkoly', 'praca_czas_rozp',
              'praca_forma_pierwsza', 'praca_forma_ostatnia',
              'praca_forma2_pierwsza', 'praca_forma_6m', 'praca_forma_9m',
              'praca_forma2_6m', 'praca_forma2_9m', 'praca_forma2_bu_6m',
              'praca_forma2_bu_9m',
              'praca_zamieszkanie_pierwsza', 'praca_zamieszkanie_ostatnia',
              'praca_zamieszkanie_6m', 'praca_zamieszkanie_9m',
              'praca_zgodna_z_wyksztalceniem_pierwsza',
              'praca_zgodna_z_wyksztalceniem_ostatnia',
              'praca_zarobki_pierwsza', 'praca_zarobki_ostatnia',
              'praca_spelnienie_oczekiwan_ostatnia',
              'praca_czas_p9m_rozklad', 'praca_czas_uop_p9m_rozklad',
              'praca_czas_gdy_bez_nauki_p9m_rozklad',
              'praca_czas_gdy_bez_nauki_uop_p9m_rozklad',
              'bezrobocie_czas_p9m_rozklad',
              'bezrobocie_czas_gdy_bez_nauki_p9m_rozklad',
              'praca_czas_p9m', 'praca_czas_gdy_bez_nauki_p9m',
              'praca_czas_gdy_nauka_p9m', 'praca_czas_uop_p9m',
              'praca_czas_gdy_bez_nauki_uop_p9m', 'praca_czas_gdy_nauka_uop_p9m',
              'bezrobocie_czas_p9m', 'bezrobocie_czas_gdy_bez_nauki_p9m',
              'bezrobocie_czas_gdy_nauka_p9m', 'bezrobocie_1m', 'bezrobocie_2m',
              'bezrobocie_3m', 'bezrobocie_4m', 'bezrobocie_5m', 'bezrobocie_6m',
              'bezrobocie_7m', 'bezrobocie_8m', 'bezrobocie_9m',
              'nauka_6m', 'nauka_9m', 'studia_gdzie_pierwsze',
              'studia_odplatnosc_pierwsze', 'studia_tryb_pierwsze', 'GRUPA_kod')

    wskaznikiSzk = agreguj_wskazniki_szk(wskaznikiInd)
    test_that("agreguj_wskazniki_szk()", {
      expect_is(wskaznikiSzk, "data.frame")
      expect_named(wskaznikiSzk, c("SZK_kod", nazwy), ignore.order = TRUE)
    })

    wskaznikiTypSzk = agreguj_wskazniki_typ_szk(wskaznikiInd)
    test_that("agreguj_wskazniki_typ_szk()", {
      expect_is(wskaznikiTypSzk, "data.frame")
      expect_named(wskaznikiTypSzk, c("GRUPA_nazwa", nazwy), ignore.order = TRUE)
    })

    wskaznikiSzkBranza = agreguj_wskazniki_szk_branza(wskaznikiInd)
    test_that("agreguj_wskazniki_szk_branza()", {
      expect_is(wskaznikiSzkBranza, "data.frame")
      expect_named(wskaznikiSzkBranza, c("SZK_kod", "UCZ_branza", nazwy),
                   ignore.order = TRUE)
    })

    wskaznikiTypSzkBranza = agreguj_wskazniki_typ_szk_branza(wskaznikiInd)
    test_that("agreguj_wskazniki_typ_szk_branza()", {
      expect_is(wskaznikiTypSzkBranza, "data.frame")
      expect_named(wskaznikiTypSzkBranza, c("UCZ_branza", "GRUPA_nazwa", nazwy),
                   ignore.order = TRUE)
    })
  }
}
