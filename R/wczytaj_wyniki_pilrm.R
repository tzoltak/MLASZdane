#' @title Wczytuje zbiór z wynikami pierwszej rundy monitorowania losow
#' absolwentow
#' @description
#' Funkcja wczytuje plik z wynikami pilotażowej rundy monitoringu losów
#' absolwentóW i przekształca go na zbiory: czasów odpowiedzi, epizodów,
#' członków gospodarstw domowych i zbiór obejmujący pozostałe zmienne
#' @param x nazwa pliku z wynikami (ciąg znaków)
#' @details Jeśli przy wczytywaniu pliku z wynikami badania pojawiając się
#' błędy dotyczące alokacji pamięci, może pomóc downgrade pakietu \emph{haven}
#' do wersji 1.1.0: \code{devtools::install_github("tidyverse/haven@v1.1.0")}.
#' @return lista ramek danych
#' @export
#' @importFrom stats setNames
#' @importFrom haven read_spss
#' @importFrom tidyr gather spread
#' @importFrom dplyr bind_cols bind_rows case_when filter_ funs group_by_
#' left_join mutate mutate_ mutate_all mutate_at one_of rename_ select select_
#' starts_with summarise_ vars
wczytaj_wyniki_pilrm = function(x){
  stopifnot(is.character(x), length(x) == 1)
  stopifnot(file.exists(x),
            file.access(x, 4) == 0)

  dane = read_spss(x)
  names(dane) = names(dane) %>% tolower()
  names(dane) = sub("^id_ibe$", "ID", names(dane))
  labTemp = attributes(dane$ID)
  dane$ID = as.numeric(dane$ID)
  attributes(dane$ID) = labTemp
  ################################################################################
  # czasy
  ################################################################################
  #|->
  message("Wyłączanie zbioru z czasami odpowiedzi.")
  czasy = dane %>%
    select_(~ID, ~r41, ~starts_with("t_")) %>%
    mutate_at(vars(starts_with("t_")), funs(as.difftime(., "%H:%M:%S",
                                                        units = "secs")))
  for (i in grep("t_", names(czasy))) {
    attributes(czasy[[i]])$label = paste0("Czas odp. na pyt. opisywane zmienną ",
                                          names(czasy)[i], " [s]")
  }
  czasy = bind_cols(czasy,
                    t_laczny_czas = czasy %>%
                      select(starts_with("t_")) %>%
                      mutate_all(as.numeric) %>%
                      rowSums(na.rm = TRUE))
  attributes(czasy$t_laczny_czas)$label = "Łączy czas trwania wywiadu [s]"
  #|<-
  ################################################################################
  # epizody
  ################################################################################
  message("Wyłączanie zbiorów z epizodami:")
  dane = dane %>%
    select(-starts_with("t_"))
  labWspolne = list(typ_epizodu = list(), nr = list(), czas_rozp = list(),
                    czas_kon = list(), czy_zakonczony = list())
  attributes(labWspolne$typ_epizodu)$label = "Typ epizodu (nauki lub pracy)"
  attributes(labWspolne$nr)$label = "Nr epizodu danego typu (jako który został wymieniony przez respondenta)"
  attributes(labWspolne$czas_rozp)$label = "Czas rozpoczęcia epizodu - licząc w miesiącach od czerwca 2015 r."
  attributes(labWspolne$czas_kon)$label = "Czas zakończenia epizodu - licząc w miesiącach od czerwca 2015 r."
  attributes(labWspolne$czy_zakonczony)$label = "Czy epizod się zakończył?"
  attributes(labWspolne$czy_zakonczony)$labels = c("Tak" = 1, "Nie" = 2)
  #|-> nauka w LO dla dorosłych
  message("  nauki w LO dla dorosłych,")
  labTemp = dane %>%
    filter_(~zp1 %in% 1) %>%
    select_(~ID, ~matches("^zp2"))
  naukaLOdD = labTemp %>%
    mutate_(.dots = list(
      zp2a = ~ifelse(zp2a %in% 1:12, zp2a, NA),
      zp2b = ~ifelse(zp2b %in% 2000:2100, zp2b, NA),
      zp2c = ~ifelse(zp2c %in% 1:4, zp2c, NA),
      zp2d = ~ifelse(zp2d %in% 1:2, zp2d, NA),
      zp2e = ~ifelse(zp2e %in% 1:2, zp2e, NA),
      zp2f = ~ifelse(zp2f %in% 1:12, zp2f, NA),
      zp2g = ~ifelse(zp2g %in% 2000:2100, zp2g, NA),
      zp2h = ~ifelse(zp2h %in% c(1:2, 8), zp2h, NA),
      zp2i = ~ifelse(zp2i %in% 1:2, zp2i, NA),
      zp2j = ~ifelse(zp2j %in% 1:2, zp2j, NA),
      zp2k = ~ifelse(zp2k %in% 1:3, zp2k, NA),
      typ_epizodu = ~"LO dla dorosłych",
      nr = ~1,
      czas_rozp = ~ifelse(is.na(zp2a), 7, zp2a) - 6 + 12 * (zp2b - 2015),
      czas_kon = ~ifelse(is.na(zp2f), 6, zp2f) - 6 + 12 * (zp2g - 2015),
      czy_zakonczony = ~ifelse(zp2c %in% c(1, 4), 1, 2))) %>%
    select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
            ~starts_with("zp"))
  naukaLOdD = przywroc_etykiety(naukaLOdD, labWspolne)
  naukaLOdD = przywroc_etykiety(naukaLOdD, labTemp)
  for (i in c("zp2a", "zp2b", "zp2f", "zp2g")) {
    attributes(naukaLOdD[[i]])$labels = NULL
  }
  #|<-
  #|-> studia (i zdawanie na nie)
  message("  studiowania,")
  labTemp = dane %>%
    filter_(~sp1_1 %in% 1) %>%
    select_(~ID, ~matches("^sp(2|3a|[45]_1)$|^sp[3]_1_|^sp6([dfg]|[ce][12])_1$|sp6h_1_[123]$")) %>%
    setNames(sub("_1", "", names(.)))
  studia =  suppressWarnings(
    dane %>%
      filter_(~sp1_1 %in% 1) %>%
      select_(~ID, ~matches("^sp[23456]")) %>%
      gather("zmienna", "value", one_of(setdiff(names(.), c("ID", "sp2", "sp3a")))) %>%
      mutate_(.dots = list(
        nr = ~as.numeric(sub("^sp([345]|6[dfgh]|6[ce][12])_([[:digit:]]+).*$", "\\2", zmienna)),
        zmienna = ~sub("^sp([345]|6[dfgh]|6[ce][12])_[[:digit:]]+", "sp\\1", zmienna))) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter_(~nr <= sp2) %>%
      mutate_(.dots = list(
        sp3a = ~ifelse(sp3a < sp2, sp3a, NA),
        sp3_kierunek = ~enc2native(tolower(sp3_kierunek)),
        sp3_uczelnia = ~enc2native(tolower(sp3_uczelnia)),
        czy_preferowany = ~ifelse(sp3a < 6, 2 - as.numeric(nr == sp3a), NA),
        czy_preferowany = ~ifelse(sp2 %in% 1, 1, czy_preferowany),
        sp4 = ~ifelse(sp4 %in% 1:2, sp4, NA),
        sp5 = ~ifelse(sp5 %in% 1:2, sp5, NA),
        sp6c1 = ~ifelse(sp6c1 %in% 1:12, sp6c1, NA),
        sp6c2 = ~ifelse(sp6c2 %in% 2000:2100, sp6c2, NA),
        sp6d = ~ifelse(sp6d %in% 1:4, sp6d, NA),
        sp6e1 = ~ifelse(sp6e1 %in% 1:12, sp6e1, NA),
        sp6e2 = ~ifelse(sp6e2 %in% 2000:2100, sp6e2, NA),
        sp6f = ~ifelse(sp6f %in% 1:3, sp6f, NA),
        sp6g = ~ifelse(sp6g %in% 1:2, sp6g, NA),
        sp6h_1 = ~ifelse(sp6h_1 %in% 0:1, 2 - sp6h_1, NA),
        sp6h_2 = ~ifelse(sp6h_2 %in% 0:1, 2 - sp6h_2, NA),
        sp6h_3 = ~ifelse(sp6h_3 %in% 0:1, 2 - sp6h_3, NA),
        typ_epizodu = ~ifelse(sp5 %in% 1, "studia", "zdawanie na studia"),
        czas_rozp = ~ifelse(is.na(sp6c1), 7, sp6c1) - 6 + 12 * (sp6c2 - 2015),
        czas_kon = ~ifelse(is.na(sp6e1), 6, sp6e1) - 6 + 12 * (sp6e2 - 2015),
        czy_zakonczony = ~ifelse(sp6d %in% c(1, 4), 1, 2),
        czy_zakonczony = ~ifelse(typ_epizodu %in% "studia", czy_zakonczony, NA))) %>%
      select_(~-sp6h_4) %>%
      select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
              ~czy_preferowany, ~starts_with("sp")))
  studia = przywroc_etykiety(studia, labWspolne)
  attributes(studia$czy_preferowany)$label = "Czy był to kierunek, na który najbardziej chciał(a) się Pan(i) dostać?"
  studia = przywroc_etykiety(studia, labTemp)
  for (i in c("czy_preferowany", "sp6h_1", "sp6h_2", "sp6h_3")) {
    attributes(studia[[i]])$labels = c("Tak" = 1, "Nie" = 2)
  }
  for (i in c("sp3a", "sp6c1", "sp6c2", "sp6e1", "sp6e2")) {
    attributes(studia[[i]])$labels = NULL
  }
  #|<-
  #|-> szkoły policealne (i zdawanie do nich)
  message("  nauki w SPolic.,")
  names(dane) = sub("pp6i([[:digit:]]+_.*)$", "pp6i_\\1", names(dane))
  labTemp = dane %>%
    filter_(~sp1_2 %in% 1) %>%
    select_(~ID, ~matches("^pp(2|3a|[45]_1)$|^pp[3]_1_|^pp6([degh]|[cf][12])_1$|pp6i_1_[123]$")) %>%
    setNames(sub("_1", "", names(.)))
  spolic =  suppressWarnings(
    dane %>%
      filter_(~sp1_2 %in% 1) %>%
      select_(~ID, ~matches("^pp[23456]")) %>%
      gather("zmienna", "value", one_of(setdiff(names(.), c("ID", "pp2", "pp3a")))) %>%
      mutate_(.dots = list(
        nr = ~as.numeric(sub("^pp([345]|6[deghi]|6[cf][12])_([[:digit:]]+).*$", "\\2", zmienna)),
        zmienna = ~sub("^pp([345]|6[deghi]|6[cf][12])_[[:digit:]]+", "pp\\1", zmienna))) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter_(~nr <= pp2) %>%
      mutate_(.dots = list(
        pp3a = ~ifelse(pp3a < pp2, pp3a, NA),
        pp3_kierunek = ~enc2native(tolower(pp3_kierunek)),
        pp3_miejscowosc = ~enc2native(tolower(pp3_miejscowosc)),
        czy_preferowany = ~ifelse(pp3a < 6, 2 - as.numeric(nr == pp3a), NA),
        czy_preferowany = ~ifelse(pp2 %in% 1, 1, czy_preferowany),
        pp4 = ~ifelse(pp4 %in% 1:2, pp4, NA),
        pp5 = ~ifelse(pp5 %in% 1:2, pp5, NA),
        pp6c1 = ~ifelse(pp6c1 %in% 1:12, pp6c1, NA),
        pp6c2 = ~ifelse(pp6c2 %in% 2000:2100, pp6c2, NA),
        pp6d = ~ifelse(pp6d %in% 1:4, pp6d, NA),
        pp6e = ~ifelse(pp6e %in% 1:4, pp6e, NA),
        pp6f1 = ~ifelse(pp6f1 %in% 1:12, pp6f1, NA),
        pp6f2 = ~ifelse(pp6f2 %in% 2000:2100, pp6f2, NA),
        pp6g = ~ifelse(pp6g %in% 1:3, pp6g, NA),
        pp6h = ~ifelse(pp6h %in% 1:2, pp6h, NA),
        pp6i_1 = ~ifelse(pp6i_1 %in% 0:1, 2 - pp6i_1, NA),
        pp6i_2 = ~ifelse(pp6i_2 %in% 0:1, 2 - pp6i_2, NA),
        pp6i_3 = ~ifelse(pp6i_3 %in% 0:1, 2 - pp6i_3, NA),
        typ_epizodu = ~ifelse(pp5 %in% 1, "SPolic.", "zdawanie do SPolic."),
        czas_rozp = ~ifelse(is.na(pp6c1), 7, pp6c1) - 6 + 12 * (pp6c2 - 2015),
        czas_kon = ~ifelse(is.na(pp6f1), 6, pp6f1) - 6 + 12 * (pp6f2 - 2015),
        czy_zakonczony = ~ifelse(pp6d %in% c(1, 4), 1, 2),
        czy_zakonczony = ~ifelse(typ_epizodu %in% "SPolic.", czy_zakonczony, NA))) %>%
      select_(~-pp6i_4) %>%
      select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
              ~czy_preferowany, ~starts_with("pp")))
  spolic = przywroc_etykiety(spolic, labWspolne)
  attributes(spolic$czy_preferowany)$label = "Czy był to kierunek/zawód, na który najbardziej chciał(a) się Pan(i) dostać?"
  spolic = przywroc_etykiety(spolic, labTemp)
  for (i in c("czy_preferowany", "pp6i_1", "pp6i_2", "pp6i_3")) {
    attributes(spolic[[i]])$labels = c("Tak" = 1, "Nie" = 2)
  }
  for (i in c("pp3a", "pp6c1", "pp6c2", "pp6f1", "pp6f2")) {
    attributes(spolic[[i]])$labels = NULL
  }
  #|<-
  #|-> egzaminy (uprawnienia)
  message("  odbywania kursów i zdobywania uprawnień,")
  labTemp = dane %>%
    filter_(~u1 %in% 1) %>%
    select_(~ID, ~matches("^u2[bcdfg]_1|^u2e[12]_1")) %>%
    setNames(sub("_1", "", names(.)))
  uprawnienia =  suppressWarnings(
    dane %>%
      filter_(~u1 %in% 1) %>%
      select_(~ID, ~matches("^u2")) %>%
      gather("zmienna", "value", one_of(setdiff(names(.), "ID"))) %>%
      mutate_(.dots = list(
        nr = ~as.numeric(sub("^u2([bcdfg]|e[12])_([[:digit:]]+).*$", "\\2", zmienna)),
        zmienna = ~sub("^u2([bcdfg]|e[12])_[[:digit:]]+(|_in)$", "u2\\1\\2", zmienna))) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter_(~u2b != "NIE DOTYCZY") %>%
      mutate_(.dots = list(
        u2b = ~enc2native(tolower(u2b)),
        u2c = ~ifelse(u2c %in% 1:5, u2c, NA),
        u2c_in = ~ifelse(u2c_in != "NIE DOTYCZY", enc2native(tolower(u2c_in)), "."),
        u2d = ~ifelse(u2d %in% 1:2, u2d, NA),
        u2e1 = ~ifelse(u2e1 %in% 1:12, u2e1, NA),
        u2e2 = ~ifelse(u2e2 %in% 2000:2100, u2e2, NA),
        u2f = ~ifelse(u2f %in% 1:4, u2f, NA),
        u2g = ~ifelse(u2g %in% 1:4, u2g, NA),
        typ_epizodu = ~"uprawnienia",
        czas_rozp = ~NA,
        czas_kon = ~ifelse(is.na(u2e1), 6, u2e1) - 6 + 12 * (u2e2 - 2015),
        czy_zakonczony = ~ifelse(u2d %in% 1, 1, 2))) %>%
      select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
              ~starts_with("u2")))
  uprawnienia = przywroc_etykiety(uprawnienia, labWspolne)
  uprawnienia = przywroc_etykiety(uprawnienia, labTemp)
  for (i in c("u2e1", "u2e2")) {
    attributes(uprawnienia[[i]])$labels = NULL
  }
  names(attributes(uprawnienia$u2c)$labels) =
    enc2native(sub("placówka kształcenia ustawicznego, kształcenia praktycznego, ośrodki dokształcania lub doskonalenia zawodowego",
                   "placówka kształcenia ustawiczn., praktyczn., ośrodki dokształc. lub dosk. zaw.",
                   names(attributes(uprawnienia$u2c)$labels)))
  #|<-
  #|-> prace
  message("  pracy,")
  names(dane) = sub("^pg5(.*)$", "pg2\\1_99", names(dane))
  names(dane) = sub("^pg4$", "pg1b_99", names(dane))
  names(dane) = sub("^pi5$", "pi7", names(dane))
  labTemp = dane %>%
    filter_(~pg1b_97_1 %in% 0) %>%
    select_(~ID, ~matches("^pg2[bcdefghij].*_1$")) %>%
    setNames(sub("_1", "", names(.)))
  labTempPI =  dane %>%
    filter_(~pg1b_97_1 %in% 0) %>%
    select_(~ID, ~matches("^p[i]")) %>%
    setNames(sub("^pi", "pio", names(.)))
  labTempPO =  dane %>%
    filter_(~pg1b_97_1 %in% 0) %>%
    select_(~ID, ~matches("^p[o]")) %>%
    setNames(sub("^po", "pio", names(.)))
  prace = suppressWarnings(
    dane %>%
      select_(~ID, ~matches("^pg[12]")) %>%
      gather("zmienna", "value", one_of(setdiff(names(.), "ID"))) %>%
      mutate_(.dots = list(
        nr = ~as.numeric(sub("^.*_([[:digit:]]+)$", "\\1", zmienna)),
        zmienna = ~sub("^(.*)_[[:digit:]]+$", "\\1", zmienna))) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter_(~pg1b != "NIE DOTYCZY") %>%
      select_(~-pg1b_97, ~-pg2a) %>%
      mutate_(.dots = list(
        pg1b = ~ifelse(grepl("^MP", pg1b), pg1b, "MP99"),
        pg2c = ~ifelse(pg2c %in% 1:12, pg2c, NA),
        pg2d = ~ifelse(pg2d %in% 2000:2100, pg2d, NA) %>% as.numeric(),
        pg2e = ~ifelse(pg2e %in% 1:12, pg2e, NA),
        czy_zakonczony = ~ifelse(pg2f %in% 9995, 2, 1),
        pg2f = ~ifelse(pg2f %in% 2000:2100, pg2f, NA),
        pg2g = ~ifelse(pg2g %in% 1:5, pg2g, NA),
        pg2h = ~ifelse(pg2h %in% c(1:5, 7:8), pg2h, NA),
        pg2h_in = ~ifelse(pg2h_in == "NIE DOTYCZY", ".", enc2native(tolower(pg2h_in))),
        pg2i = ~ifelse(pg2i %in% 1:3, pg2i, NA),
        pg2ia = ~ifelse(pg2ia %in% 1:16, pg2ia, NA),
        pg2ib = ~ifelse(pg2ib %in% 1:9995, pg2ib, NA),
        pg2ic = ~ifelse(pg2ic %in% c(1:14, 98), pg2ic, NA),
        pg2j = ~ifelse(pg2j %in% 1:2, pg2j, NA),
        typ_epizodu = ~"praca",
        czas_rozp = ~ifelse(is.na(pg2c), 7, pg2c) - 6 + 12 * (pg2d - 2015),
        czas_kon = ~ifelse(is.na(pg2e), 6, pg2e) - 6 + 12 * (pg2f - 2015))))
  lPrac = prace %>%
    group_by_(~ID) %>%
    summarise_(.dots = list(
      nr_min = ~min(nr),
      nr_max = ~max(nr)))
  pracePO = suppressWarnings(suppressMessages(
    bind_rows(
      dane %>%
        select_(~ID, ~matches("^pi")) %>%
        setNames(sub("^pi", "pio", names(.))) %>%
        left_join(lPrac %>%
                    select_(~ID, ~nr_min) %>%
                    rename_(.dots = list(nr = ~nr_min))),
      dane %>%
        select_(~ID, ~matches("^po")) %>%
        setNames(sub("^po", "pio", names(.))) %>%
        left_join(lPrac %>%
                    select_(~ID, ~nr_max) %>%
                    rename_(.dots = list(nr = ~nr_max)))) %>%
      filter_(~pio1 %in% 1:4) %>%
      mutate_(.dots = list(
        pio0 = ~ifelse(pio0 %in% 0:95, pio0, NA),
        pio1 = ~ifelse(pio1 %in% 1:4, pio1, NA),
        pio2 = ~ifelse(!(tolower(pio2) %in% c("nie dotyczy", "nie pracowałem zawodowo")),
                       enc2native(tolower(pio2)), "."),
        pio3 = ~ifelse(!(tolower(pio3) %in% c("nie dotyczy", "nie pracowałem zawodowo")),
                       enc2native(tolower(pio3)), "."),
        pio4 = ~ifelse(pio4 %in% 1:99995, pio4, NA),
        pio5 = ~ifelse(pio5 %in% 1:6, pio5, NA),
        pio6_1 = ~ifelse(pio6_1 %in% 1:6, pio6_1, NA),
        pio6_2 = ~ifelse(pio6_2 %in% 1:6, pio6_2, NA),
        pio6_3 = ~ifelse(pio6_3 %in% 1:6, pio6_3, NA),
        pio6_4 = ~ifelse(pio6_4 %in% 1:6, pio6_4, NA),
        pio6_5 = ~ifelse(pio6_5 %in% 1:6, pio6_5, NA),
        pio6_6 = ~ifelse(pio6_6 %in% 1:6, pio6_6, NA),
        pio7 = ~ifelse(pio7 %in% 1:4, pio7, NA)))))
  prace = suppressWarnings(suppressMessages(
    prace %>%
      left_join(pracePO) %>%
      select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
              ~starts_with("pg"), ~starts_with("pio"))))
  attributes(prace$pg1b)$label = "Identyfikator pracodawcy (unikalny w ramach osoby ale nie między badanymi)"
  prace = przywroc_etykiety(prace, labWspolne)
  prace = przywroc_etykiety(prace, labTemp)
  prace = przywroc_etykiety(prace, labTempPI)
  prace = przywroc_etykiety(prace, labTempPO)
  for (i in c("pg2c", "pg2d", "pg2e", "pg2f", "pg2ib", "pio0")) {
    attributes(prace[[i]])$labels = NULL
  }
  names(attributes(prace$pg2g)$labels) =
    enc2native(sub("praca u kogoś w firmie państwowej / w instytucji publicznej / organizacji pozarządowej",
                   "praca w firmie państwowej / w instytucji publicznej / organizacji pozarządowej",
                   names(attributes(prace$pg2g)$labels)))
  #|<-
  #|-> okresy bezrobocia
  message("  bezrobocia.")
  labTemp = dane %>%
    filter_(~pb0 %in% 1) %>%
    select_(~ID, ~matches("^pb1[bcdefgh]_1$")) %>%
    setNames(sub("_1", "", names(.)))
  bezrobocie =  suppressWarnings(
    dane %>%
      filter_(~pb0 %in% 1) %>%
      select_(~ID, ~matches("^pb1[bcdefgh]")) %>%
      gather("zmienna", "value", one_of(setdiff(names(.), "ID"))) %>%
      mutate_(.dots = list(
        nr = ~as.numeric(sub("^pb1[bcdefgh]_([[:digit:]]+)$", "\\1", zmienna)),
        zmienna = ~sub("^(pb1[bcdefgh])_[[:digit:]]+$", "\\1", zmienna))) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      mutate_(.dots = list(
        pb1b = ~ifelse(pb1b %in% 1:12, pb1b, NA),
        pb1c = ~ifelse(pb1c %in% 2000:2100, pb1c, NA),
        pb1d = ~ifelse(pb1d %in% 1:12, pb1d, NA),
        czy_zakonczony = ~ifelse(pb1e %in% 9995, 2, 1),
        pb1e = ~ifelse(pb1e %in% 2000:2100, pb1e, NA),
        pb1f = ~ifelse(pb1f %in% 1:2, pb1f, NA),
        pb1g = ~ifelse(pb1g %in% 1:8, pb1g, NA),
        pb1h = ~ifelse(pb1h %in% 1:2, pb1h, NA),
        typ_epizodu = ~"bezrobocie",
        czas_rozp = ~ifelse(is.na(pb1b), 7, pb1b) - 6 + 12 * (pb1c - 2015),
        czas_kon = ~ifelse(is.na(pb1d), 6, pb1d) - 6 + 12 * (pb1e - 2015))) %>%
      filter_(~rowSums(is.na(.)) < 7) %>%
      select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
              ~starts_with("pb")))
  bezrobocie = przywroc_etykiety(bezrobocie, labWspolne)
  bezrobocie = przywroc_etykiety(bezrobocie, labTemp)
  for (i in c("pb1b", "pb1c", "pb1d", "pb1e")) {
    attributes(bezrobocie[[i]])$labels = NULL
  }
  #|<-
  #|-> wszystkie epizody w jeden plik
  message("  Łączenie różnych typów epizodów.")
  epizody = suppressWarnings(
    bind_rows(naukaLOdD,
              studia,
              spolic,
              uprawnienia,
              prace,
              bezrobocie))
  epizody = przywroc_etykiety(epizody, naukaLOdD) %>%
    przywroc_etykiety(studia) %>%
    przywroc_etykiety(spolic) %>%
    przywroc_etykiety(uprawnienia) %>%
    przywroc_etykiety(prace) %>%
    przywroc_etykiety(bezrobocie)
  #|<-
  #|-> członkowie gosp. dom.
  message("Wyłączanie zbioru członków gosp. dom.")
  labTemp = dane %>%
    select_(~ID, ~m9, ~matches("^m10[bcdef].*_1$")) %>%
    setNames(sub("_1", "", names(.)))
  gospDom = suppressWarnings(
    dane %>%
      select_(~ID, ~m9, ~matches("^m10[bcdef]_")) %>%
      gather("zmienna", "value", one_of(setdiff(names(.), c("ID", "m9")))) %>%
      mutate_(.dots = list(
        nr_osoby = ~as.numeric(sub("^.*_([[:digit:]]+)$", "\\1", zmienna)),
        zmienna = ~sub("_[[:digit:]]+$", "", zmienna))) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter_(~nr_osoby < m9) %>%
      mutate_(.dots = list(
        nr_osoby = ~nr_osoby + 1,
        m10c = ~ifelse(m10c > 99995, NA, m10c),
        m10c_wiek = ~2017 - m10c,
        m10d_in = ~ifelse(m10d_in %in% c("NIE DOTYCZY", "odmowa"),
                          ".", tolower(m10d_in)),
        m10d_rekod = ~case_when(
          grepl("brat taty|w[óu]jek|ciocia|ciotka|kuzyn", m10d_in) ~ "wuj/ciotka",
          grepl("brat babci", m10d_in) ~ "dziadkowie moi lub partnera",
          grepl("chłopak", m10d_in) ~ "mąż/żona/partner /partnerka",
          grepl("drugi maz mamy|ojczym|partner mamy|ojciec partnera", m10d_in) ~ "rodzice/teściowie",
          grepl("rodzina zast[eę]pcza", m10d_in) ~ "moje rodzeństwo",
          grepl("brata( syn|nek|nica)|c[oó]rka (brata|siostry)|siostrzen(iec|ica)|synek brata|ciotki syn", m10d_in) ~ "bratanek(ica)/siostrzeniec(ica)",
          grepl("kolega", m10d_in) ~ "współlokator/ka",
          grepl("brat (meza|partnera)|szwagier|bratowa|mąż córki", m10d_in) ~ "szwagier/szwagierka/bratowa",
          TRUE ~ m10d_in),
        m10f = ~ifelse(m10f %in% 7, NA, m10e))))
  gospDom$m10d_rekod = ifelse(
    gospDom$m10d %in% 7,
    gospDom$m10d_rekod,
    names(attributes(labTemp$m10d)$labels)[gospDom$m10d] %>%
      factor(levels = c("mąż/żona/partner /partnerka", "moje rodzeństwo",
                        "rodzice/teściowie", "moje dzieci",
                        "dziadkowie moi lub partnera", "współlokator/ka",
                        "wuj/ciotka", "bratanek(ica)/siostrzeniec(ica)",
                        "szwagier/szwagierka/bratowa")))
  attributes(gospDom$nr_osoby)$label = "Numer porządkowy osoby w ramach gosp. dom."
  attributes(gospDom$m10c_wiek)$label = "Wiek tej osoby"
  attributes(gospDom$m10d_rekod)$label = "Kim ta osoba jest dla Pana(i)? - z domkniętymi odp. 'inne'"
  gospDom = przywroc_etykiety(gospDom, labTemp)
  attributes(gospDom$m10f)$label = "Czy ta osoba pracuje lub szuka pracy?"
  #|<-
  ################################################################################
  # zasadniczy zbiór
  ################################################################################
  #|->
  message("Czynności końcowe.")
  dane = dane %>%
    select_(~-matches("^zp2")) %>%
    select_(~-matches("^sp[23456]")) %>%
    select_(~-matches("^pp[23456]")) %>%
    select_(~-matches("^u2")) %>%
    select_(~-matches("^pg[12]")) %>%
    select_(~-matches("^pb1")) %>%
    select_(~-matches("^m10[bcdef]_"))
  #|<-
  return(list(dane = dane,
              epizody = epizody,
              gospDom = gospDom,
              czasy = czasy))
}
#' @title Obrobka danych etykietowanych
#' @description
#' Funkcja pozwala szybko i wygodnie przywracać etykiety zmiennych i wartości,
#' które "gubią się" w niektórych operacjach na ramkach danych (o ile wcześniej
#' zostały one zapisane).
#' @param x ramka danych, której zmiennym mają zostać przypisane etykiety
#' @param labTemp ramka danych (lub lista) z której mają zostać skopiowane
#' etykiety zmiennych i wartości zmiennych
#' @return ramka danych
#' @importFrom haven read_spss
przywroc_etykiety = function(x, labTemp) {
  nieMaZm = setdiff(names(labTemp), names(x))
  if (length(nieMaZm) > 0) {
    warning("W ramce z danymi nie ma zmiennych: '", paste(nieMaZm, collapse = "', '"), "'.")
    labTemp = labTemp[, names(labTemp) %in% names(x)]
  }
  for (i in names(labTemp)) {
    attributes(x[[i]])$label = enc2native(attributes(labTemp[[i]])$label)
    attributes(x[[i]])$labels = attributes(labTemp[[i]])$labels
    if (!is.null(attributes(x[[i]])$labels)) {
      names(attributes(x[[i]])$labels) =
        enc2native(names(attributes(x[[i]])$labels))
    }
  }
  return(x)
}
