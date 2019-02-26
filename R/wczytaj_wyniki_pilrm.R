#' @title Wczytuje zbior z wynikami pilotazowej rundy monitorowania losow absolwentow
#' @description
#' Funkcja wczytuje plik z wynikami pilotażowej rundy monitoringu losów
#' absolwentów i przekształca go na zbiory: czasów odpowiedzi, epizodów,
#' członków gospodarstw domowych i zbiór obejmujący pozostałe zmienne.
#' @param x nazwa pliku z wynikami (ciąg znaków)
#' @details
#' Funkcja jest całkowicie nieodporna na niewystępowanie w zbiorze kolumn,
#' których istnienia oczekuje - w takiej sytuacji będzie rzucać umiarkowanie
#' informatywnymi błędami. Nazwa ostatecznej wersji zbioru z wynikami badania,
#' do wykorzystania, na której testowane było działanie funkcji to:
#' \emph{MLEZAMiD_absolwent_n2959_20171013.sav}.
#'
#' Więcej szczegółów - patrz dokumentacja dot. struktury i zawartości zbiorów
#' danych z \href{../doc/runda_pilotazowa-dokumentacja.html}{rundy pilotażowej}
#' tworzonych przy pomocy funkcji pakietu MLASZdane.
#' @return lista ramek danych
#' @export
#' @importFrom stats setNames
#' @importFrom haven read_spss
#' @importFrom tidyr gather spread
#' @importFrom dplyr .data bind_cols bind_rows case_when filter group_by
#' left_join matches mutate mutate_all mutate_at one_of rename select
#' starts_with summarise vars
wczytaj_wyniki_pilrm = function(x){
  stopifnot(is.character(x), length(x) == 1)
  if (!grepl("[.]sav$", x)) {
    stop("Podany plik nie jest w formacie .sav.")
  }
  if (!file.exists(x)) {
    stop("Plik o podanej nazwie nie istnieje.")
  }
  if (file.access(x, 4) != 0) {
    stop("Brak uprawnień do odczytania podanego pliku.")
  }
  dane = read_spss(x)
  if (!any(grepl("^ID_IBE$", names(dane), ignore.case = TRUE))) {
    stop("W zbiorze brak identyfikatora respondenta (tj. zmiennej `ID_IBE`).")
  }
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
    select("ID", "r41", starts_with("t_")) %>%
    mutate_at(vars(starts_with("t_")), list(~as.difftime(., "%H:%M:%S",
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
    filter(.data$zp1 %in% 1) %>%
    select("ID", matches("^zp2"))
  naukaLOdD = labTemp %>%
    mutate(zp2a = ifelse(.data$zp2a %in% 1:12, .data$zp2a, NA),
           zp2b = ifelse(.data$zp2b %in% 2000:2100, .data$zp2b, NA),
           zp2c = ifelse(.data$zp2c %in% 1:4, .data$zp2c, NA),
           zp2d = ifelse(.data$zp2d %in% 1:2, .data$zp2d, NA),
           zp2e = ifelse(.data$zp2e %in% 1:2, .data$zp2e, NA),
           zp2f = ifelse(.data$zp2f %in% 1:12, .data$zp2f, NA),
           zp2g = ifelse(.data$zp2g %in% 2000:2100, .data$zp2g, NA),
           zp2h = ifelse(.data$zp2h %in% c(1:2, 8), .data$zp2h, NA),
           zp2i = ifelse(.data$zp2i %in% 1:2, .data$zp2i, NA),
           zp2j = ifelse(.data$zp2j %in% 1:2, .data$zp2j, NA),
           zp2k = ifelse(.data$zp2k %in% 1:3, .data$zp2k, NA),
           typ_epizodu = "LO dla dorosłych",
           nr = 1,
           czas_rozp = ifelse(is.na(.data$zp2a), 7, .data$zp2a) - 6 +
             12 * (.data$zp2b - 2015),
           czas_kon = ifelse(is.na(.data$zp2f), 6, .data$zp2f) - 6 +
             12 * (.data$zp2g - 2015),
           czy_zakonczony = ifelse(.data$zp2c %in% c(1, 4), 1, 2)) %>%
    select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony",
            starts_with("zp"))
  naukaLOdD = przywroc_etykiety(naukaLOdD, labWspolne)
  naukaLOdD = przywroc_etykiety(naukaLOdD, labTemp)
  for (i in c("zp2a", "zp2b", "zp2f", "zp2g")) {
    attributes(naukaLOdD[[i]])$labels = NULL
  }
  #|<-
  #|-> studia (i zdawanie na nie)
  message("  studiowania,")
  labTemp = dane %>%
    filter(.data$sp1_1 %in% 1) %>%
    select("ID", matches("^sp(2|3a|[45]_1)$|^sp[3]_1_|^sp6([dfg]|[ce][12])_1$|sp6h_1_[123]$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  studia = suppressWarnings(
    dane %>%
      filter(.data$sp1_1 %in% 1) %>%
      select("ID", matches("^sp[23456]")) %>%
      gather("zmienna", "value", -one_of("ID", "sp2", "sp3a"))) %>%
      mutate(nr = as.numeric(sub("^sp([345]|6[dfgh]|6[ce][12])_([[:digit:]]+).*$",
                                 "\\2", .data$zmienna)),
             zmienna = sub("^sp([345]|6[dfgh]|6[ce][12])_[[:digit:]]+",
                           "sp\\1", .data$zmienna)) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter(.data$nr <= .data$sp2) %>%
    mutate(sp3a = ifelse(.data$sp3a < .data$sp2, .data$sp3a, NA),
           sp3_kierunek = enc2native(tolower(.data$sp3_kierunek)),
           sp3_uczelnia = enc2native(tolower(.data$sp3_uczelnia)),
           czy_preferowany = ifelse(.data$sp3a < 6,
                                    2 - as.numeric(.data$nr == .data$sp3a), NA),
           czy_preferowany = ifelse(.data$sp2 %in% 1, 1, .data$czy_preferowany),
           sp4 = ifelse(.data$sp4 %in% 1:2, .data$sp4, NA),
           sp5 = ifelse(.data$sp5 %in% 1:2, .data$sp5, NA),
           sp6c1 = ifelse(.data$sp6c1 %in% 1:12, .data$sp6c1, NA),
           sp6c2 = ifelse(.data$sp6c2 %in% 2000:2100, .data$sp6c2, NA),
           sp6d = ifelse(.data$sp6d %in% 1:4, .data$sp6d, NA),
           sp6e1 = ifelse(.data$sp6e1 %in% 1:12, .data$sp6e1, NA),
           sp6e2 = ifelse(.data$sp6e2 %in% 2000:2100, .data$sp6e2, NA),
           sp6f = ifelse(.data$sp6f %in% 1:3, .data$sp6f, NA),
           sp6g = ifelse(.data$sp6g %in% 1:2, .data$sp6g, NA),
           sp6h_1 = ifelse(.data$sp6h_1 %in% 0:1, 2 - .data$sp6h_1, NA),
           sp6h_2 = ifelse(.data$sp6h_2 %in% 0:1, 2 - .data$sp6h_2, NA),
           sp6h_3 = ifelse(.data$sp6h_3 %in% 0:1, 2 - .data$sp6h_3, NA),
           typ_epizodu = ifelse(.data$sp5 %in% 1, "studia", "zdawanie na studia"),
           czas_rozp = ifelse(is.na(.data$sp6c1), 7, .data$sp6c1) - 6 +
             12 * (.data$sp6c2 - 2015),
           czas_kon = ifelse(is.na(.data$sp6e1), 6, .data$sp6e1) - 6 +
             12 * (.data$sp6e2 - 2015),
           czy_zakonczony = ifelse(.data$sp6d %in% c(1, 4), 1, 2),
           czy_zakonczony = ifelse(.data$typ_epizodu %in% "studia",
                                   .data$czy_zakonczony, NA)) %>%
  select(-"sp6h_4") %>%
  select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony",
         "czy_preferowany", starts_with("sp"))
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
    filter(.data$sp1_2 %in% 1) %>%
    select("ID", matches("^pp(2|3a|[45]_1)$|^pp[3]_1_|^pp6([degh]|[cf][12])_1$|pp6i_1_[123]$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  spolic =  suppressWarnings(
    dane %>%
      filter(.data$sp1_2 %in% 1) %>%
      select("ID", matches("^pp[23456]")) %>%
      gather("zmienna", "value", -one_of("ID", "pp2", "pp3a"))) %>%
      mutate(nr = as.numeric(sub("^pp([345]|6[deghi]|6[cf][12])_([[:digit:]]+).*$",
                                 "\\2", .data$zmienna)),
             zmienna = sub("^pp([345]|6[deghi]|6[cf][12])_[[:digit:]]+",
                           "pp\\1", .data$zmienna)) %>%
      spread("zmienna", "value", convert = TRUE) %>%
      filter(.data$nr <= .data$pp2) %>%
    mutate(pp3a = ifelse(.data$pp3a < .data$pp2, .data$pp3a, NA),
           pp3_kierunek = enc2native(tolower(.data$pp3_kierunek)),
           pp3_miejscowosc = enc2native(tolower(.data$pp3_miejscowosc)),
           czy_preferowany = ifelse(.data$pp3a < 6,
                                    2 - as.numeric(.data$nr == .data$pp3a), NA),
           czy_preferowany = ifelse(.data$pp2 %in% 1, 1, .data$czy_preferowany),
           pp4 = ifelse(.data$pp4 %in% 1:2, .data$pp4, NA),
           pp5 = ifelse(.data$pp5 %in% 1:2, .data$pp5, NA),
           pp6c1 = ifelse(.data$pp6c1 %in% 1:12, .data$pp6c1, NA),
           pp6c2 = ifelse(.data$pp6c2 %in% 2000:2100, .data$pp6c2, NA),
           pp6d = ifelse(.data$pp6d %in% 1:4, .data$pp6d, NA),
           pp6e = ifelse(.data$pp6e %in% 1:4, .data$pp6e, NA),
           pp6f1 = ifelse(.data$pp6f1 %in% 1:12, .data$pp6f1, NA),
           pp6f2 = ifelse(.data$pp6f2 %in% 2000:2100, .data$pp6f2, NA),
           pp6g = ifelse(.data$pp6g %in% 1:3, .data$pp6g, NA),
           pp6h = ifelse(.data$pp6h %in% 1:2, .data$pp6h, NA),
           pp6i_1 = ifelse(.data$pp6i_1 %in% 0:1, 2 - .data$pp6i_1, NA),
           pp6i_2 = ifelse(.data$pp6i_2 %in% 0:1, 2 - .data$pp6i_2, NA),
           pp6i_3 = ifelse(.data$pp6i_3 %in% 0:1, 2 - .data$pp6i_3, NA),
           typ_epizodu = ifelse(.data$pp5 %in% 1, "SPolic.", "zdawanie do SPolic."),
           czas_rozp = ifelse(is.na(.data$pp6c1), 7, .data$pp6c1) - 6 +
             12 * (.data$pp6c2 - 2015),
           czas_kon = ifelse(is.na(.data$pp6f1), 6, .data$pp6f1) - 6 +
             12 * (.data$pp6f2 - 2015),
           czy_zakonczony = ifelse(.data$pp6d %in% c(1, 4), 1, 2),
           czy_zakonczony = ifelse(.data$typ_epizodu %in% "SPolic.",
                                   .data$czy_zakonczony, NA)) %>%
  select(-"pp6i_4") %>%
  select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony",
         "czy_preferowany", starts_with("pp"))
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
    filter(.data$u1 %in% 1) %>%
    select("ID", matches("^u2[bcdfg]_1|^u2e[12]_1"))
  names(labTemp) = sub("_1", "", names(labTemp))
  uprawnienia =  suppressWarnings(
    dane %>%
      filter(.data$u1 %in% 1) %>%
      select("ID", matches("^u2")) %>%
      gather("zmienna", "value", -"ID")) %>%
    mutate(nr = as.numeric(sub("^u2([bcdfg]|e[12])_([[:digit:]]+).*$", "\\2",
                               .data$zmienna)),
           zmienna = sub("^u2([bcdfg]|e[12])_[[:digit:]]+(|_in)$", "u2\\1\\2",
                         .data$zmienna)) %>%
    spread("zmienna", "value", convert = TRUE) %>%
    filter(.data$u2b != "NIE DOTYCZY") %>%
    mutate(u2b = enc2native(tolower(.data$u2b)),
           u2c = ifelse(.data$u2c %in% 1:5, .data$u2c, NA),
           u2c_in = ifelse(.data$u2c_in != "NIE DOTYCZY",
                           enc2native(tolower(.data$u2c_in)), "."),
           u2d = ifelse(.data$u2d %in% 1:2, .data$u2d, NA),
           u2e1 = ifelse(.data$u2e1 %in% 1:12, .data$u2e1, NA),
           u2e2 = ifelse(.data$u2e2 %in% 2000:2100, .data$u2e2, NA),
           u2f = ifelse(.data$u2f %in% 1:4, .data$u2f, NA),
           u2g = ifelse(.data$u2g %in% 1:4, .data$u2g, NA),
           typ_epizodu = "uprawnienia",
           czas_rozp = NA,
           czas_kon = ifelse(is.na(.data$u2e1), 6, .data$u2e1) - 6 +
             12 * (.data$u2e2 - 2015),
           czy_zakonczony = ifelse(.data$u2d %in% 1, 1, 2)) %>%
    select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony",
           starts_with("u2"))
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
    filter(.data$pg1b_97_1 %in% 0) %>%
    select("ID", matches("^pg2[bcdefghij].*_1$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  labTempPI =  dane %>%
    filter(.data$pg1b_97_1 %in% 0) %>%
    select("ID", matches("^p[i]"))
  names(labTempPI) = sub("^pi", "pio", names(labTempPI))
  labTempPO =  dane %>%
    filter(.data$pg1b_97_1 %in% 0) %>%
    select("ID", matches("^p[o]"))
  names(labTempPO) = sub("^po", "pio", names(labTempPO))
  prace = suppressWarnings(
    dane %>%
      select("ID", matches("^pg[12]")) %>%
      gather("zmienna", "value", -"ID")) %>%
    mutate(nr = as.numeric(sub("^.*_([[:digit:]]+)$", "\\1", .data$zmienna)),
           zmienna = sub("^(.*)_[[:digit:]]+$", "\\1", .data$zmienna)) %>%
    spread("zmienna", "value", convert = TRUE) %>%
    filter(.data$pg1b != "NIE DOTYCZY") %>%
    select(-"pg1b_97", -"pg2a") %>%
    mutate(pg1b = ifelse(grepl("^MP", .data$pg1b), .data$pg1b, "MP99"),
           pg2c = ifelse(.data$pg2c %in% 1:12, .data$pg2c, NA),
           pg2d = ifelse(.data$pg2d %in% 2000:2100, .data$pg2d, NA) %>% as.numeric(),
           pg2e = ifelse(.data$pg2e %in% 1:12, .data$pg2e, NA),
           czy_zakonczony = ifelse(.data$pg2f %in% 9995, 2, 1),
           pg2f = ifelse(.data$pg2f %in% 2000:2100, .data$pg2f, NA),
           pg2g = ifelse(.data$pg2g %in% 1:5, .data$pg2g, NA),
           pg2h = ifelse(.data$pg2h %in% c(1:5, 7:8), .data$pg2h, NA),
           pg2h_in = ifelse(.data$pg2h_in == "NIE DOTYCZY",
                            ".", enc2native(tolower(.data$pg2h_in))),
           pg2i = ifelse(.data$pg2i %in% 1:3, .data$pg2i, NA),
           pg2ia = ifelse(.data$pg2ia %in% 1:16, .data$pg2ia, NA),
           pg2ib = ifelse(.data$pg2ib %in% 1:9995, .data$pg2ib, NA),
           pg2ic = ifelse(.data$pg2ic %in% c(1:14, 98), .data$pg2ic, NA),
           pg2j = ifelse(.data$pg2j %in% 1:2, .data$pg2j, NA),
           typ_epizodu = "praca",
           czas_rozp = ifelse(is.na(.data$pg2c), 7, .data$pg2c) - 6 +
             12 * (.data$pg2d - 2015),
           czas_kon = ifelse(is.na(.data$pg2e), 6, .data$pg2e) - 6 +
             12 * (.data$pg2f - 2015))
  lPrac = prace %>%
    group_by(.data$ID) %>%
    summarise(nr_min = min(.data$nr),
              nr_max = max(.data$nr))
  pracePO = suppressWarnings(suppressMessages(
    bind_rows(
      dane %>%
        select("ID", matches("^pi")) %>%
        setNames(sub("^pi", "pio", names(select(dane, "ID", matches("^pi"))))) %>%
        left_join(lPrac %>%
                    select("ID", "nr_min") %>%
                    rename(nr = .data$nr_min)),
      dane %>%
        select("ID", matches("^po")) %>%
        setNames(sub("^po", "pio", names(select(dane, "ID", matches("^po"))))) %>%
        left_join(lPrac %>%
                    select("ID", "nr_max") %>%
                    rename(nr = .data$nr_max)))) %>%
      filter(.data$pio1 %in% 1:4) %>%
      mutate(pio0 = ifelse(.data$pio0 %in% 0:95, .data$pio0, NA),
             pio1 = ifelse(.data$pio1 %in% 1:4, .data$pio1, NA),
             pio2 = ifelse(!(tolower(.data$pio2) %in% c("nie dotyczy",
                                                        "nie pracowałem zawodowo")),
                           enc2native(tolower(.data$pio2)), "."),
             pio3 = ifelse(!(tolower(.data$pio3) %in% c("nie dotyczy",
                                                        "nie pracowałem zawodowo")),
                           enc2native(tolower(.data$pio3)), "."),
             pio4 = ifelse(.data$pio4 %in% 1:99995, .data$pio4, NA),
             pio5 = ifelse(.data$pio5 %in% 1:6, .data$pio5, NA),
             pio6_1 = ifelse(.data$pio6_1 %in% 1:6, .data$pio6_1, NA),
             pio6_2 = ifelse(.data$pio6_2 %in% 1:6, .data$pio6_2, NA),
             pio6_3 = ifelse(.data$pio6_3 %in% 1:6, .data$pio6_3, NA),
             pio6_4 = ifelse(.data$pio6_4 %in% 1:6, .data$pio6_4, NA),
             pio6_5 = ifelse(.data$pio6_5 %in% 1:6, .data$pio6_5, NA),
             pio6_6 = ifelse(.data$pio6_6 %in% 1:6, .data$pio6_6, NA),
             pio7 = ifelse(.data$pio7 %in% 1:4, .data$pio7, NA)))
  prace = suppressWarnings(suppressMessages(
    prace %>%
      left_join(pracePO))) %>%
    select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony",
           starts_with("pg"), starts_with("pio"))
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
    filter(.data$pb0 %in% 1) %>%
    select("ID", matches("^pb1[bcdefgh]_1$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  bezrobocie =  suppressWarnings(
    dane %>%
      filter(.data$pb0 %in% 1) %>%
      select("ID", matches("^pb1[bcdefgh]")) %>%
      gather("zmienna", "value", -"ID")) %>%
    mutate(nr = as.numeric(sub("^pb1[bcdefgh]_([[:digit:]]+)$","\\1",
                               .data$zmienna)),
           zmienna = sub("^(pb1[bcdefgh])_[[:digit:]]+$", "\\1",
                         .data$zmienna)) %>%
    spread("zmienna", "value", convert = TRUE) %>%
    mutate(pb1b = ifelse(.data$pb1b %in% 1:12, .data$pb1b, NA),
           pb1c = ifelse(.data$pb1c %in% 2000:2100, .data$pb1c, NA),
           pb1d = ifelse(.data$pb1d %in% 1:12, .data$pb1d, NA),
           czy_zakonczony = ifelse(.data$pb1e %in% 9995, 2, 1),
           pb1e = ifelse(.data$pb1e %in% 2000:2100, .data$pb1e, NA),
           pb1f = ifelse(.data$pb1f %in% 1:2, .data$pb1f, NA),
           pb1g = ifelse(.data$pb1g %in% 1:8, .data$pb1g, NA),
           pb1h = ifelse(.data$pb1h %in% 1:2, .data$pb1h, NA),
           typ_epizodu = "bezrobocie",
           czas_rozp = ifelse(is.na(.data$pb1b), 7, .data$pb1b) - 6 +
             12 * (.data$pb1c - 2015),
           czas_kon = ifelse(is.na(.data$pb1d), 6, .data$pb1d) - 6 +
             12 * (.data$pb1e - 2015))
  bezrobocie = bezrobocie %>%
    filter(rowSums(is.na(bezrobocie)) < 7) %>%
    select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony",
           starts_with("pb"))
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
    select("ID", "m9", matches("^m10[bcdef].*_1$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  gospDom = suppressWarnings(
    dane %>%
      select("ID", "m9", matches("^m10[bcdef]_")) %>%
      gather("zmienna", "value", -one_of(c("ID", "m9")))) %>%
    mutate(nr_osoby = as.numeric(sub("^.*_([[:digit:]]+)$", "\\1", .data$zmienna)),
           zmienna = sub("_[[:digit:]]+$", "", .data$zmienna)) %>%
    spread("zmienna", "value", convert = TRUE) %>%
    filter(.data$nr_osoby < .data$m9) %>%
    mutate(nr_osoby = .data$nr_osoby + 1,
           m10c = ifelse(.data$m10c > 99995, NA, .data$m10c),
           m10c_wiek = 2017 - .data$m10c,
           m10d_in = ifelse(.data$m10d_in %in% c("NIE DOTYCZY", "odmowa"),
                            ".", tolower(.data$m10d_in)),
           m10d_rekod = case_when(
             grepl("brat taty|w[óu]jek|ciocia|ciotka|kuzyn", .data$m10d_in) ~ "wuj/ciotka",
             grepl("brat babci", .data$m10d_in) ~ "dziadkowie moi lub partnera",
             grepl("chłopak", .data$m10d_in) ~ "mąż/żona/partner /partnerka",
             grepl("drugi maz mamy|ojczym|partner mamy|ojciec partnera", .data$m10d_in) ~ "rodzice/teściowie",
             grepl("rodzina zast[eę]pcza", .data$m10d_in) ~ "moje rodzeństwo",
             grepl("brata( syn|nek|nica)|c[oó]rka (brata|siostry)|siostrzen(iec|ica)|synek brata|ciotki syn", .data$m10d_in) ~ "bratanek(ica)/siostrzeniec(ica)",
             grepl("kolega", .data$m10d_in) ~ "współlokator/ka",
             grepl("brat (meza|partnera)|szwagier|bratowa|mąż córki", .data$m10d_in) ~ "szwagier/szwagierka/bratowa",
             TRUE ~ m10d_in),
           m10f = ifelse(.data$m10f %in% 7, NA, .data$m10e))
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
    select(-matches("^zp2")) %>%
    select(-matches("^sp[23456]")) %>%
    select(-matches("^pp[23456]")) %>%
    select(-matches("^u2")) %>%
    select(-matches("^pg[12]")) %>%
    select(-matches("^pb1")) %>%
    select(-matches("^m10[bcdef]_"))
  #|<-
  return(list(dane = dane,
              epizody = epizody,
              gospDom = gospDom,
              czasy = czasy))
}
