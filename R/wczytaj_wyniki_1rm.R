#' @title Wczytuje zbior z wynikami 1. rundy monitorowania losow absolwentow
#' @description
#' Funkcja wczytuje plik z wynikami 1. rundy monitoringu losów
#' absolwentów i przekształca go na zbiory: epizodów, członków gospodarstw
#' domowych i zbiór obejmujący pozostałe zmienne. \strong{Uwaga}, funkcja
#' zakłada, że \strong{do zbioru zostały już dopisane kody zawodów} (zakodowane
#' z odpowiedzi otwartych udzielonych przez respondentów).
#' @param x nazwa pliku z wynikami (ciąg znaków)
#' @details
#' Funkcja jest całkowicie nieodporna na niewystępowanie w zbiorze kolumn,
#' których istnienia oczekuje - w takiej sytuacji będzie rzucać umiarkowanie
#' informatywnymi błędami. Nazwa ostatecznej wersji zbioru z wynikami badania,
#' do wykorzystania, na której testowane było działanie funkcji to:
#' \emph{MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav}.
#'
#' Więcej szczegółów - patrz dokumentacja dot. struktury i zawartości zbiorów
#' danych z \href{../doc/runda_1-dokumentacja.html}{1. rundy monitoringu}
#' tworzonych przy pomocy funkcji pakietu MLASZdane.
#' @return lista ramek danych
#' @export
#' @importFrom stats setNames
#' @importFrom haven read_spss
#' @importFrom tidyr gather spread
#' @importFrom dplyr .data bind_rows case_when everything filter
#' group_by left_join matches mutate one_of rename select starts_with summarise
wczytaj_wyniki_1rm = function(x){
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
  if (!any(grepl("^ID_RESP$", names(dane), ignore.case = TRUE))) {
    stop("W zbiorze brak identyfikatora respondenta (tj. zmiennej `ID_RESP`).")
  }
  names(dane) = names(dane) %>% tolower()
  names(dane) = sub("abs_", "", names(dane))
  names(dane) = sub("^id_resp$", "ID_RESP", names(dane))
  ################################################################################
  # epizody
  ################################################################################
  message("Wyłączanie zbiorów z epizodami:")
  dane = dane %>%
    select(-starts_with("t_"))
  labWspolne = list(typ_epizodu = c(), nr = c(), czas_rozp = c(),
                    czas_zakon = c(), czy_zakonczony = c(), swiadectwo = c(),
                    czas_rozp_imput = c(), czas_zakon_imput = c())
  attributes(labWspolne$typ_epizodu)$label = "Typ epizodu (nauki lub pracy)"
  attributes(labWspolne$nr)$label = "Nr epizodu danego typu (jako który został wymieniony przez respondenta)"
  attributes(labWspolne$czas_rozp)$label = "Czas rozpoczęcia epizodu - licząc w miesiącach od czerwca 2017 r."
  attributes(labWspolne$czas_zakon)$label = "Czas zakończenia epizodu - licząc w miesiącach od czerwca 2017 r."
  attributes(labWspolne$czy_zakonczony)$label = "Czy epizod się zakończył?"
  attributes(labWspolne$czy_zakonczony)$labels = c("Tak" = 1, "Nie" = 2)
  attributes(labWspolne$swiadectwo)$label = "Czy uzyskał świadectwo ukończenia szkoły?"
  attributes(labWspolne$swiadectwo)$labels = c("Tak" = 1, "Nie" = 2)
  attributes(labWspolne$czas_rozp_imput)$label = "Czy czas rozpoczęcia epizodu był imputowany?"
  attributes(labWspolne$czas_rozp_imput)$labels = c("Tak" = 1, "Nie" = 2)
  attributes(labWspolne$czas_zakon_imput)$label = "Czy czas zakończenia epizodu był imputowany?"
  attributes(labWspolne$czas_zakon_imput)$labels = c("Tak" = 1, "Nie" = 2)
  #|-> ew. dalsza nauka w szkole, jako uczeń której był badany
  message("  kontynuacja nauki w badanej szkole,")
  labTemp = dane %>%
    select("ID_RESP", matches("^f[567]"))
  naukaSzkAbs = labTemp %>%
    mutate(typ_epizodu = "szkoła objęta badaniem",
           nr = NA,
           czas_rozp = NA,
           czas_zakon =
             case_when(
               .data$f5 %in% 1 & .data$f7 %in% (2017:2018) ~ 12 * (.data$f7 - 2017),
               .data$f5 %in% 2 & .data$f6a_rok %in% (2017:2018) & !is.na(.data$f6a_miesiac) ~
                 6 - .data$f6a_miesiac + 12 * (.data$f6a_rok - 2017),
               .data$f5 %in% 2 & .data$f6a_rok %in% (2017:2018) ~ 12 * (.data$f6a_rok - 2017)) %>%
             as.numeric(),
           czy_zakonczony = ifelse(.data$f5 %in% 1 | !(.data$f6 %in% 1), 1, 2),
           swiadectwo = ifelse(.data$f5 %in% 1, 1, 0),
           czas_rozp_imput = 2,
           czas_zakon_imput = 2) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput")
  naukaSzkAbs = przywroc_etykiety(naukaSzkAbs, labWspolne)
  #|<-
  #|-> nauka w LO dla dorosłych
  message("  nauki w LO dla dorosłych,")
  labTemp = dane %>%
    filter(.data$zp1 %in% 1) %>%
    select("ID_RESP", matches("^zp2"))
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
             12 * (.data$zp2b - 2017),
           czas_zakon = ifelse(is.na(.data$zp2f), 6, .data$zp2f) - 6 +
             12 * (.data$zp2g - 2017),
           czas_zakon = ifelse(.data$czas_zakon < .data$czas_rozp,
                               .data$czas_rozp + 1, .data$czas_zakon),
           czy_zakonczony = ifelse(.data$zp2c %in% c(1, 4), 1, 2),
           swiadectwo = ifelse(.data$zp2c %in% 1, 1, 2),
           czas_rozp_imput = ifelse(is.na(.data$zp2a), 1, 2),
           czas_zakon_imput = ifelse(is.na(.data$zp2f), 1, 2)) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
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
    select("ID_RESP",
           matches("^sp(2|3a|[45]_1)$|^sp[3]_1_|^sp6([dfgx]|[ce][12])_1$|sp6h_1_[123]$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  studia = suppressWarnings(
    dane %>%
      filter(.data$sp1_1 %in% 1) %>%
      select("ID_RESP", matches("^sp[23456]")) %>%
      gather("zmienna", "wartosc", -"ID_RESP", -"sp2", -"sp3a")) %>%
    mutate(nr = as.numeric(sub("^sp([345]|6[dfghx]|6[ce][12])_([[:digit:]]+).*$",
                               "\\2", .data$zmienna)),
           zmienna = sub("^sp([345]|6[dfghx]|6[ce][12])_[[:digit:]]+",
                         "sp\\1", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    filter(.data$nr <= .data$sp2) %>%
    mutate(sp3a = ifelse(.data$sp3a < .data$sp2, .data$sp3a, NA),
           sp3_kierunek = enc2native(tolower(.data$sp3_kierunek)),
           sp3_uczelnia = enc2native(tolower(.data$sp3_uczelnia)),
           czy_preferowany = ifelse(.data$sp3a < 11,
                                    2 - as.numeric(.data$nr == .data$sp3a), NA),
           czy_preferowany = ifelse(.data$sp2 %in% 1, 1, .data$czy_preferowany),
           sp4 = ifelse(.data$sp4 %in% 1:2, .data$sp4, NA),
           sp5 = ifelse(.data$sp5 %in% 1:2, .data$sp5, NA),
           sp6c1 = ifelse(.data$sp6c1 %in% 1:12, .data$sp6c1, NA),
           sp6c2 = ifelse(.data$sp6c2 %in% 2000:2100, .data$sp6c2, NA),
           sp6d = ifelse(.data$sp6d %in% 1:4, .data$sp6d, NA),
           sp6e1 = ifelse(.data$sp6e1 %in% 1:12, .data$sp6e1, NA),
           sp6e2 = ifelse(.data$sp6e2 %in% 2000:2100, .data$sp6e2, NA),
           sp6x = ifelse(.data$sp6x %in% 0:60, .data$sp6x, NA_integer_),
           sp6f = ifelse(.data$sp6f %in% 1:3, .data$sp6f, NA),
           sp6g = ifelse(.data$sp6g %in% 1:2, .data$sp6g, NA),
           sp6h_1 = ifelse(.data$sp6h_1 %in% 0:1, 2 - .data$sp6h_1, NA),
           sp6h_2 = ifelse(.data$sp6h_2 %in% 0:1, 2 - .data$sp6h_2, NA),
           sp6h_3 = ifelse(.data$sp6h_3 %in% 0:1, 2 - .data$sp6h_3, NA),
           typ_epizodu = ifelse(.data$sp5 %in% 1, "studia", "zdawanie na studia"),
           czas_rozp = ifelse(is.na(.data$sp6c1), 7, .data$sp6c1) - 6 +
             12 * (.data$sp6c2 - 2017),
           czas_zakon = ifelse(is.na(.data$sp6e1), 6, .data$sp6e1) - 6 +
             12 * (.data$sp6e2 - 2017),
           czas_zakon = ifelse(.data$czas_zakon < .data$czas_rozp,
                               .data$czas_rozp + 1, .data$czas_zakon),
           czy_zakonczony = ifelse(.data$sp6d %in% c(1, 4), 1, 2),
           czy_zakonczony = ifelse(.data$typ_epizodu %in% "studia",
                                   .data$czy_zakonczony, NA),
           swiadectwo = ifelse(.data$sp6d %in% 1, 1, 2),
           czas_rozp_imput = ifelse(is.na(.data$sp6c1), 1, 2),
           czas_zakon_imput = ifelse(is.na(.data$sp6e1), 1, 2)) %>%
    select(-"sp6h_4") %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           "czy_preferowany", starts_with("sp"))
  studia = przywroc_etykiety(studia, labWspolne)
  attributes(studia$czy_preferowany)$label =
    "Czy był to kierunek, na który najbardziej chciał(a) się Pan(i) dostać?"
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
    select("ID_RESP", matches("^pp(2|3a|[45]_1)$|^pp[3]_1_|^pp6([deghx]|[cf][12])_1$|pp6i(|_)1_[123]$"))
  names(labTemp) = sub("pp6i(|_)1(_[123])$", "pp6i\\2_1", names(labTemp))
  names(labTemp) = sub("_1", "", names(labTemp))
  spolic = suppressWarnings(
    dane %>%
      filter(.data$sp1_2 %in% 1) %>%
      select("ID_RESP", matches("^pp[23456]")) %>%
      gather("zmienna", "wartosc", -"ID_RESP", -"pp2", -"pp3a")) %>%
    mutate(nr = as.numeric(sub("^pp([345]|6[deghix]|6[cf][12])(|_)([[:digit:]]+).*$",
                               "\\3", .data$zmienna)),
           zmienna = sub("^pp([345]|6[deghix]|6[cf][12])(|_)[[:digit:]]+",
                         "pp\\1", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    filter(.data$nr <= .data$pp2) %>%
    mutate(pp3a = ifelse(.data$pp3a < .data$pp2, .data$pp3a, NA),
           pp3_kierunek = enc2native(tolower(.data$pp3_kierunek)),
           pp3_miejscowosc = enc2native(tolower(.data$pp3_miejscowosc)),
           czy_preferowany = ifelse(.data$pp3a < 11,
                                    2 - as.numeric(.data$nr == .data$pp3a), NA),
           czy_preferowany = ifelse(.data$pp2 %in% 1, 1, .data$czy_preferowany),
           pp4 = ifelse(.data$pp4 %in% 1:2, .data$pp4, NA),
           pp5 = ifelse(.data$pp5 %in% 1:2, .data$pp5, NA),
           pp6c1 = ifelse(.data$pp6c1 %in% 1:12, .data$pp6c1, NA),
           pp6c2 = ifelse(.data$pp6c2 %in% 2000:2100, .data$pp6c2, NA),
           pp6d = ifelse(.data$pp6d %in% 1:4, .data$pp6d, NA),
           pp6e = ifelse(.data$pp6e %in% 1:2, .data$pp6e, NA),
           pp6f1 = ifelse(.data$pp6f1 %in% 1:12, .data$pp6f1, NA),
           pp6f2 = ifelse(.data$pp6f2 %in% 2000:2100, .data$pp6f2, NA),
           pp6x = ifelse(.data$pp6x %in% 0:60, .data$pp6x, NA_integer_),
           pp6g = ifelse(.data$pp6g %in% 1:3, .data$pp6g, NA),
           pp6h = ifelse(.data$pp6h %in% 1:2, .data$pp6h, NA),
           pp6i_1 = ifelse(.data$pp6i_1 %in% 0:1, 2 - .data$pp6i_1, NA),
           pp6i_2 = ifelse(.data$pp6i_2 %in% 0:1, 2 - .data$pp6i_2, NA),
           pp6i_3 = ifelse(.data$pp6i_3 %in% 0:1, 2 - .data$pp6i_3, NA),
           typ_epizodu = ifelse(.data$pp5 %in% 1, "SPolic.", "zdawanie do SPolic."),
           czas_rozp = ifelse(is.na(.data$pp6c1), 7, .data$pp6c1) - 6 +
             12 * (.data$pp6c2 - 2017),
           czas_zakon = ifelse(is.na(.data$pp6f1), 6, .data$pp6f1) - 6 +
             12 * (.data$pp6f2 - 2017),
           czas_zakon = ifelse(.data$czas_zakon < .data$czas_rozp,
                               .data$czas_rozp + 1, .data$czas_zakon),
           czy_zakonczony = ifelse(.data$pp6d %in% c(1, 4), 1, 2),
           czy_zakonczony = ifelse(.data$typ_epizodu %in% "SPolic.",
                                   .data$czy_zakonczony, NA),
           swiadectwo = ifelse(.data$pp6d %in% 1, 1, 2),
           czas_rozp_imput = ifelse(is.na(.data$pp6c1), 1, 2),
           czas_zakon_imput = ifelse(is.na(.data$pp6f1), 1, 2)) %>%
      left_join(get("kzsb") %>% select("kod_zawodu", "branza_2019"),
                by = c("pp3_kierunek_kod" = "kod_zawodu")) %>%
    rename(pp3_kierunek_branza_kzsb = .data$branza_2019) %>%
    select(-"pp6i_4") %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           "czy_preferowany", starts_with("pp"))
  spolic = przywroc_etykiety(spolic, labWspolne)
  attributes(spolic$czy_preferowany)$label = "Czy był to kierunek/zawód, na który najbardziej chciał(a) się Pan(i) dostać?"
  attributes(spolic$pp3_kierunek_branza_kzsb)$label = "Branża, do której należy nauczany zawód"
  spolic = przywroc_etykiety(spolic, labTemp)
  for (i in c("czy_preferowany", "pp6i_1", "pp6i_2", "pp6i_3")) {
    attributes(spolic[[i]])$labels = c("Tak" = 1, "Nie" = 2)
  }
  for (i in c("pp3a", "pp6c1", "pp6c2", "pp6f1", "pp6f2",
              "pp3_kierunek_branza_kzsb")) {
    attributes(spolic[[i]])$labels = NULL
  }
  #|<-
  #|-> egzaminy (szkolenia)
  message("  odbywania kursów i zdobywania uprawnień,")
  labTemp = dane %>%
    filter(.data$u1 %in% 1) %>%
    select("ID_RESP", matches("^u2[bcdfgh]_1(|_in|_kod)$|^u2e[12]_1$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  szkoleniaU2 = suppressWarnings(
    dane %>%
      filter(.data$u1 %in% 1) %>%
      select("ID_RESP", matches("^u2")) %>%
      gather("zmienna", "wartosc", -"ID_RESP")) %>%
    mutate(nr = as.numeric(sub("^u2([bcdfgh]|e[12])_([[:digit:]]+).*$",
                               "\\2", .data$zmienna)),
           zmienna = sub("^u2([bcdfgh]|e[12])_[[:digit:]]+(|_in|_kod)$",
                         "u2\\1\\2", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    filter(.data$u2b != "NIE DOTYCZY") %>%
    mutate(u2b = enc2native(tolower(.data$u2b)),
           u2c = ifelse(.data$u2c %in% 1:8, .data$u2c, NA),
           u2c_in = ifelse(.data$u2c_in != "NIE DOTYCZY",
                           enc2native(tolower(.data$u2c_in)), "."),
           u2d = ifelse(.data$u2d %in% 1:2, .data$u2d, NA),
           u2e1 = ifelse(.data$u2e1 %in% 1:12, .data$u2e1, NA),
           u2e2 = ifelse(.data$u2e2 %in% 2000:2100, .data$u2e2, NA),
           u2f = ifelse(.data$u2f %in% 1:4, .data$u2f, NA),
           u2g = ifelse(.data$u2g %in% 1:2, .data$u2g, NA),
           u2h = ifelse(.data$u2h %in% 1:2, .data$u2h, NA),
           typ_epizodu = "szkolenia u2",
           czas_rozp = NA,
           czas_zakon = ifelse(is.na(.data$u2e1), 6, .data$u2e1) - 6 +
             12 * (.data$u2e2 - 2017),
           czy_zakonczony = ifelse(.data$u2d %in% 1, 1, 2),
           swiadectwo = ifelse(.data$u2f %in% (1:3), 1, 2),
           czas_rozp_imput = 2,
           czas_zakon_imput = ifelse(is.na(.data$u2e1), 1, 2)) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           starts_with("u2"))
  szkoleniaU2 = przywroc_etykiety(szkoleniaU2, labWspolne)
  szkoleniaU2 = przywroc_etykiety(szkoleniaU2, labTemp)
  for (i in c("u2e1", "u2e2")) {
    attributes(szkoleniaU2[[i]])$labels = NULL
  }
  names(attributes(szkoleniaU2$u2c)$labels) =
    enc2native(sub("placówka kształcenia ustawicznego, kształcenia praktycznego, ośrodki dokształcania lub doskonalenia zawodowego",
                   "placówka kształcenia ustawiczn., praktyczn., ośrodki dokształc. lub dosk. zaw.",
                   names(attributes(szkoleniaU2$u2c)$labels)))
  #|<-
  #|-> inne szkolenia
  labTemp = dane %>%
    filter(.data$u3 %in% 1) %>%
    select("ID_RESP", matches("^u4[bcdg]_1(|_in|_kod)$|^u4e[12]_1$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  szkoleniaU4 = suppressWarnings(
    dane %>%
      filter(.data$u3 %in% 1) %>%
      select("ID_RESP", matches("^u4[bcdge]")) %>%
      gather("zmienna", "wartosc", -"ID_RESP")) %>%
    mutate(nr = as.numeric(sub("^u4([bcdg]|e[12])_([[:digit:]]+).*$",
                               "\\2", .data$zmienna)),
           zmienna = sub("^u4([bcdg]|e[12])_[[:digit:]]+(|_in|_kod)$",
                         "u4\\1\\2", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    filter(.data$u4b != "NIE DOTYCZY") %>%
    mutate(u4b = enc2native(tolower(.data$u4b)),
           u4c = ifelse(.data$u4c %in% 1:8, .data$u4c, NA),
           u4c_in = ifelse(.data$u4c_in != "NIE DOTYCZY",
                           enc2native(tolower(.data$u4c_in)), "."),
           u4d = ifelse(.data$u4d %in% 1:2, .data$u4d, NA),
           u4e1 = ifelse(.data$u4e1 %in% 1:12, .data$u4e1, NA),
           u4e2 = ifelse(.data$u4e2 %in% 2000:2100, .data$u4e2, NA),
           u4g = ifelse(.data$u4g %in% 1:2, .data$u4g, NA),
           typ_epizodu = "szkolenia u4",
           czas_rozp = NA,
           czas_zakon = ifelse(is.na(.data$u4e1), 6, .data$u4e1) - 6 +
             12 * (.data$u4e2 - 2017),
           czy_zakonczony = ifelse(.data$u4d %in% 1, 1, 2),
           swiadectwo = NA,
           czas_rozp_imput = 2,
           czas_zakon_imput = ifelse(is.na(.data$u4e1), 1, 2)) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           starts_with("u4"))
  szkoleniaU4 = przywroc_etykiety(szkoleniaU4, labWspolne)
  szkoleniaU4 = przywroc_etykiety(szkoleniaU4, labTemp)
  for (i in c("u4e1", "u4e2")) {
    attributes(szkoleniaU4[[i]])$labels = NULL
  }
  names(attributes(szkoleniaU4$u4c)$labels) =
    enc2native(sub("placówka kształcenia ustawicznego, kształcenia praktycznego, ośrodki dokształcania lub doskonalenia zawodowego",
                   "placówka kształcenia ustawiczn., praktyczn., ośrodki dokształc. lub dosk. zaw.",
                   names(attributes(szkoleniaU4$u4c)$labels)))
  #|<-
  #|-> łączenie obu rodzajóW uprawnień
  szkoleniaU4 = szkoleniaU4 %>%
    rename(u4h = .data$u4g)
  names(szkoleniaU4) = sub("u4", "u2", names(szkoleniaU4))
  szkolenia = suppressWarnings(bind_rows(szkoleniaU2,
                                         szkoleniaU4))
  szkolenia = przywroc_etykiety(szkolenia, labWspolne)
  szkolenia = przywroc_etykiety(szkolenia, szkoleniaU2)
  rm(szkoleniaU2, szkoleniaU4)
  #|<-
  #|-> prace
  message("  pracy,")
  names(dane) = sub("^pg5(.*)$", "pg2\\1_99", names(dane))
  names(dane) = sub("^pg4$", "pg1b_99", names(dane))
  labTemp = dane %>%
    filter(.data$pg1b_97_1 %in% 0) %>%
    select("ID_RESP", matches("^pg2[cdefxghij].*_1$"))
  names(labTemp) =  sub("_1", "", names(labTemp))
  labTempPI = dane %>%
    filter(.data$pg1b_97_1 %in% 0) %>%
    select("ID_RESP", matches("^pi"))
  names(labTempPI) = sub("^pi([1234])", "pio\\1", names(labTempPI))
  labTempPO = dane %>%
    filter(.data$pg1b_97_1 %in% 0) %>%
    select("ID_RESP", matches("^po[[:digit:]]"))
  names(labTempPO) = sub("^po(1|[234].*)$", "pio\\1", names(labTempPO))
  prace = suppressWarnings(
    dane %>%
      select("ID_RESP", matches("^pg[12]"), "r5s2") %>%
      gather("zmienna", "wartosc", -"ID_RESP", -"r5s2")) %>%
    mutate(nr = as.numeric(sub("^.*_([[:digit:]]+)$", "\\1", .data$zmienna)),
           zmienna = sub("^(.*)_[[:digit:]]+$", "\\1", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    filter(.data$pg1b != "NIE DOTYCZY") %>%
    select(-"pg1b_97", -"pg2a") %>%
    mutate(pg1b = ifelse(grepl("^MP", .data$pg1b), .data$pg1b, "MP99"),
           pg2c = ifelse(.data$pg2c %in% 1:12, .data$pg2c, NA),
           pg2d = ifelse(.data$pg2d %in% 2000:2100, .data$pg2d, NA) %>% as.numeric(),
           pg2e = ifelse(.data$pg2e %in% 1:12, .data$pg2e, NA),
           czy_zakonczony = ifelse(.data$pg2f %in% 9995, 2, 1),
           pg2f = ifelse(.data$pg2f %in% 2000:2100, .data$pg2f, NA),
           pg2x = ifelse(.data$pg2x %in% 0:20, .data$pg2x, NA),
           pg2g = ifelse(.data$pg2g %in% 1:7, .data$pg2g, NA),
           # korekty pg2g i pg2h na podstawie pg2h_in
           pg2h_in = tolower(.data$pg2h_in),
           pg2h = case_when(grepl("bez umowy", .data$pg2h_in) ~ 6,
                            grepl("na czas okre[śs]lony", .data$pg2h_in) ~ 1,
                            grepl("dzia[łl]alno[sś][cć] gospodarcza|kontrakt", .data$pg2h_in) ~ 5,
                            grepl("sta[żz]|praktyka", .data$pg2h_in) ~ 7,
                            grepl("w[ła]asne gospodarstwo|pracuj[ęe] w polu|gospodarstwie rolnym rodzicom", .data$pg2h_in) ~ NA_real_,
                            TRUE ~ as.numeric(.data$pg2h)),
           pg2g = case_when(grepl("w[ła]asne gospodarstwo|pracuj[ęe] w polu|gospodarstwie rolnym rodzicom", .data$pg2h_in) ~ 7,
                            TRUE ~ as.numeric(.data$pg2g)),
           # koniec korekt
           pg2h = ifelse(.data$pg2h %in% 1:9, .data$pg2h, NA),
           pg2h_in = ifelse(.data$pg2h_in == "NIE DOTYCZY",
                            ".", enc2native(.data$pg2h_in)),
           pg2i = ifelse(.data$pg2i %in% 1:3, .data$pg2i, NA),
           pg2ia = ifelse(.data$pg2ia %in% 1:16, .data$pg2ia, NA),
           pg2ic = ifelse(.data$pg2ic %in% c(1:14, 98), .data$pg2ic, NA),
           pg2j = ifelse(.data$pg2j %in% 1:2, .data$pg2j, NA),
           typ_epizodu = "praca",
           czas_rozp = .data$pg2c - 6 + 12 * (.data$pg2d - 2017),
           czas_zakon = .data$pg2e - 6 + 12 * (.data$pg2f - 2017),
           # jeśli znamy moment zakończenia i długość, obliczmy na tej podstawie moment rozpoczęcia
           czas_rozp = case_when(is.na(.data$czas_rozp) & .data$czy_zakonczony %in% 1 ~
                                   .data$czas_zakon - .data$pg2x,
                                 is.na(.data$czas_rozp) ~
                                   6 + as.numeric(.data$r5s2) - .data$pg2x,
                                 TRUE ~ .data$czas_rozp),
           # jeśli znamy moment rozpoczęcia i długość, obliczmy na tej podstawie moment zakończenia
           czas_zakon = ifelse(is.na(.data$czas_zakon) & .data$czy_zakonczony %in% 1,
                               .data$czas_rozp + .data$pg2x, .data$czas_zakon),
           # oznaczmy, którym obserwacjom czasy będą imputowane
           czas_rozp_imput = ifelse(is.na(.data$czas_rozp), 1, 2),
           czas_zakon_imput = ifelse(is.na(.data$czas_zakon), 1, 2),
           # jeśli praca zaczeła się przed 2017 i nie znamy miesiąca, załóżmy, że był to lipiec
           czas_rozp = ifelse(is.na(.data$czas_rozp) & .data$pg2d < 2017,
                              (.data$pg2d - 2017) * 12 + 1, .data$czas_rozp),
           swiadectwo = NA) %>%
    select(-"r5s2")
  lPrac = prace %>%
    group_by(.data$ID_RESP) %>%
    summarise(nr_min = min(.data$nr),
              nr_max = max(.data$nr))
  pracePO = suppressWarnings(suppressMessages(bind_rows(
    dane %>%
      select("ID_RESP", matches("^pi")) %>%
      setNames(sub("^pi([1234])", "pio\\1",
                   names(select(dane, "ID_RESP", matches("^pi"))))) %>%
      left_join(lPrac %>%
                  select("ID_RESP", "nr_min") %>%
                  rename(nr = .data$nr_min)),
    dane %>%
      select("ID_RESP", matches("^po[[:digit:]]")) %>%
      setNames(sub("^po(1|[234].*)$", "pio\\1",
                   names(select(dane, "ID_RESP", matches("^po[[:digit:]]"))))) %>%
      left_join(lPrac %>%
                  select("ID_RESP", "nr_max") %>%
                  rename(nr = .data$nr_max))))) %>%
    filter(.data$pio1 %in% 1:4) %>%
    mutate(pi0 = ifelse(.data$pi0 %in% 0:94, .data$pi0, NA),
           pio1 = ifelse(.data$pio1 %in% 1:4, .data$pio1, NA),
           pio2 = ifelse(!(tolower(.data$pio2) %in% c("nie dotyczy", "odmowa")),
                         enc2native(tolower(.data$pio2)), "."),
           pio2_kod = as.integer(as.character(.data$pio2_kod)),
           pio3 = ifelse(!(tolower(.data$pio3) %in% c("nie dotyczy", "odmowa")),
                         enc2native(tolower(.data$pio3)), "."),
           pio3a = ifelse(.data$pio3a %in% 1:2, .data$pio3a, NA),
           pio3b = ifelse(.data$pio3b %in% 1:95, .data$pio3b, NA),
           pio4 = ifelse(.data$pio4 %in% 1:99995, .data$pio4, NA),
           pio4 = ifelse(.data$pio4 %in% c(997:999, 9997:9999), NA, .data$pio4),
           pio4a = ifelse(.data$pio4a %in% 1:5, .data$pio4a, NA),
           pi5 = ifelse(.data$pi5 %in% 1:3, .data$pi5, NA),
           po5 = ifelse(.data$po5 %in% 1:6, .data$po5, NA),
           po6_1 = ifelse(.data$po6_1 %in% 1:6, .data$po6_1, NA),
           po6_2 = ifelse(.data$po6_2 %in% 1:6, .data$po6_2, NA),
           po6_3 = ifelse(.data$po6_3 %in% 1:6, .data$po6_3, NA),
           po6_4 = ifelse(.data$po6_4 %in% 1:6, .data$po6_4, NA),
           po6_5 = ifelse(.data$po6_5 %in% 1:6, .data$po6_5, NA),
           po6_6 = ifelse(.data$po6_6 %in% 1:6, .data$po6_6, NA),
           po7 = ifelse(.data$po7 %in% 1:4, .data$po7, NA),
           po8 = ifelse(.data$po8 %in% 1:4, .data$po8, NA),
           po9 = ifelse(.data$po9 %in% 1:3, .data$po9, NA),
           po10 = ifelse(.data$po10 %in% 1:2, .data$po10, NA),
           po11_1 = ifelse(.data$po11_1 %in% 1:2, .data$po11_1, NA),
           po11_2 = ifelse(.data$po11_2 %in% 1:2, .data$po11_2, NA),
           po11_3 = ifelse(.data$po11_3 %in% 1:2, .data$po11_3, NA),
           po11_4 = ifelse(.data$po11_4 %in% 1:2, .data$po11_4, NA),
           po11_5 = ifelse(.data$po11_5 %in% 1:2, .data$po11_5, NA),
           po11_6 = ifelse(.data$po11_6 %in% 1:2, .data$po11_6, NA),
           po11_7 = ifelse(.data$po11_7 %in% 1:2, .data$po11_7, NA),
           po11_8 = ifelse(.data$po11_8 %in% 1:2, .data$po11_8, NA),
           po11_9 = ifelse(.data$po11_9 %in% 1:2, .data$po11_9, NA),
           po11_9_in = ifelse(!(tolower(.data$po11_9_in) %in% c("nie dotyczy", "odmowa")),
                              enc2native(tolower(.data$po11_9_in)), "."))
  prace = suppressWarnings(suppressMessages(
    prace %>%
      left_join(pracePO))) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           starts_with("pg"), starts_with("pi"), starts_with("pio"),
           starts_with("po")) %>%
    # czyszczenie odpowiedzi na pytania PO7-PO11 osobom, które w momencie wywiadu już nie pracowały
    mutate(po7 = ifelse(.data$czy_zakonczony %in% 2, .data$po7, NA),
           po8 = ifelse(.data$czy_zakonczony %in% 2, .data$po8, NA),
           po9 = ifelse(.data$czy_zakonczony %in% 2, .data$po9, NA),
           po10 = ifelse(.data$czy_zakonczony %in% 2, .data$po10, NA),
           po11_1 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_1, NA),
           po11_2 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_2, NA),
           po11_3 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_3, NA),
           po11_4 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_4, NA),
           po11_5 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_5, NA),
           po11_6 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_6, NA),
           po11_7 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_7, NA),
           po11_8 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_8, NA),
           po11_9 = ifelse(.data$czy_zakonczony %in% 2, .data$po11_9, NA),
           po11_9_in = ifelse(.data$czy_zakonczony %in% 2, .data$po11_9_in, "."))
  attributes(prace$pg1b)$label = "Identyfikator pracodawcy (unikalny w ramach osoby ale nie między badanymi)"
  prace = przywroc_etykiety(prace, labWspolne)
  prace = przywroc_etykiety(prace, labTemp)
  prace = przywroc_etykiety(prace, labTempPI)
  attributes(labTempPO$pio2_kod)$labels = attributes(labTempPO$pio2_kod)$labels %>%
    as.numeric() %>%
    setNames(names(attributes(labTempPO$pio2_kod)$labels))
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
  dane = dane %>%
    select(-starts_with("pb1xx"))
  labTemp = dane %>%
    filter(.data$pb0 %in% 1) %>%
    select("ID_RESP", matches("^pb1[bcdexfgh]_1$|pb1g_1_in"))
  names(labTemp) = sub("_1", "", names(labTemp))
  bezrobocie = suppressWarnings(
    dane %>%
      filter(.data$pb0 %in% 1) %>%
      select("ID_RESP", matches("^pb1[bcdexfgh]"), "r5s2") %>%
      gather("zmienna", "wartosc", -"ID_RESP", -"r5s2")) %>%
    mutate(nr = as.numeric(sub("^pb1[bcdexfgh]_([[:digit:]]+)(|_in)$",
                               "\\1", .data$zmienna)),
           zmienna = sub("^(pb1[bcdexfgh])_[[:digit:]]+(|_in)$",
                         "\\1\\2", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    mutate(pb1b = ifelse(.data$pb1b %in% 1:12, .data$pb1b, NA),
           pb1c = ifelse(.data$pb1c %in% 2000:2100, .data$pb1c, NA),
           pb1d = ifelse(.data$pb1d %in% 1:12, .data$pb1d, NA),
           czy_zakonczony = ifelse(.data$pb1e %in% 9995, 2, 1),
           pb1e = ifelse(.data$pb1e %in% 2000:2100, .data$pb1e, NA),
           pb1x = ifelse(.data$pb1x %in% 1:95, .data$pb1x, NA),
           pb1f = ifelse(.data$pb1f %in% 1:2, .data$pb1f, NA),
           pb1g = ifelse(.data$pb1g %in% 1:8, .data$pb1g, NA),
           pb1g_in = ifelse(!(tolower(.data$pb1g_in) %in% c("nie dotyczy", "odmowa")),
                            enc2native(tolower(.data$pb1g_in)), "."),
           pb1g = case_when(grepl("ci[ąa][żz][ay]|dziecko", .data$pb1g_in) ~ 2,
                            TRUE ~ as.numeric(.data$pb1g)),
           pb1h = ifelse(.data$pb1h %in% 1:2, .data$pb1h, NA),
           typ_epizodu = "bezrobocie",
           czas_rozp = .data$pb1b - 6 + 12 * (.data$pb1c - 2017),
           czas_zakon = .data$pb1d - 6 + 12 * (.data$pb1e - 2017),
           # jeśli znamy moment zakończenia i długość, obliczmy na tej podstawie moment rozpoczęcia
           czas_rozp =
             case_when(is.na(.data$czas_rozp) & .data$czy_zakonczony %in% 1 ~
                         .data$czas_zakon - .data$pb1x,
                       is.na(.data$czas_rozp) ~
                         6 + as.numeric(.data$r5s2) - .data$pb1x,
                       TRUE ~ .data$czas_rozp),
           # jeśli znamy moment rozpoczęcia i długość, obliczmy na tej podstawie moment zakończenia
           czas_zakon =
             ifelse(is.na(.data$czas_zakon) & .data$czy_zakonczony %in% 1,
                    .data$czas_rozp + .data$pb1x, .data$czas_zakon),
           swiadectwo = NA,
           czas_rozp_imput = ifelse(is.na(.data$czas_rozp), 1, 2),
           czas_zakon_imput = ifelse(is.na(.data$czas_zakon), 1, 2)) %>%
    filter(!is.na(.data$pb1f) | !is.na(.data$pb1h)) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           starts_with("pb"))
  bezrobocie = przywroc_etykiety(bezrobocie, labWspolne)
  bezrobocie = przywroc_etykiety(bezrobocie, labTemp)
  for (i in c("pb1b", "pb1c", "pb1d", "pb1e")) {
    attributes(bezrobocie[[i]])$labels = NULL
  }
  #|<-
  #|-> wszystkie epizody w jeden plik
  message("  Łączenie różnych typów epizodów.")
  epizody = suppressWarnings(bind_rows(naukaSzkAbs,
                                       naukaLOdD,
                                       studia,
                                       spolic,
                                       szkolenia,
                                       prace,
                                       bezrobocie)) %>%
    group_by(.data$ID_RESP) %>%
    mutate(czas_zakon_sz =
             .data$czas_zakon[.data$typ_epizodu == "szkoła objęta badaniem"]) %>%
    ungroup() %>%
    mutate(czas_rozp_imput = ifelse(is.na(.data$czas_rozp),
                                    NA, .data$czas_rozp_imput),
           czas_zakon_imput = ifelse(is.na(.data$czas_zakon),
                                     NA, .data$czas_zakon_imput)) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony", "swiadectwo", "czas_rozp_imput", "czas_zakon_imput",
           "czas_zakon_sz", everything())
  attributes(epizody$czas_zakon_sz)$label = "Czas zakończenia szkoły zawodowej, jako uczeń której resp. został zbadany"
  epizody = przywroc_etykiety(epizody, naukaLOdD) %>%
    przywroc_etykiety(studia) %>%
    przywroc_etykiety(spolic) %>%
    przywroc_etykiety(szkolenia) %>%
    przywroc_etykiety(prace) %>%
    przywroc_etykiety(bezrobocie)
  maska = !grepl("^(ID_RESP|typ_epizodu|nr|swiadectwo)$|^(czas|czy)_", names(epizody))
  names(epizody)[maska] = paste0("ABS_", names(epizody)[maska])
  #|<-
  #|-> członkowie gosp. dom.
  message("Wyłączanie zbioru członków gosp. dom.")
  labTemp = dane %>%
    select("ID_RESP", "m9", matches("^m10[bcdef].*_1$"))
  names(labTemp) = sub("_1", "", names(labTemp))
  gospDom = suppressWarnings(
    dane %>%
      select("ID_RESP", "m9", matches("^m10[bcdef]_")) %>%
      gather("zmienna", "wartosc", -"ID_RESP", -"m9")) %>%
    mutate(nr_osoby = as.numeric(sub("^.*_([[:digit:]]+)$",
                                     "\\1", .data$zmienna)),
           zmienna = sub("_[[:digit:]]+$", "", .data$zmienna)) %>%
    spread("zmienna", "wartosc", convert = TRUE) %>%
    filter(.data$nr_osoby < .data$m9) %>%
    mutate(nr_osoby = .data$nr_osoby + 1,
           m10b = ifelse(.data$m10b %in% 1:2, .data$m10b, NA),
           m10c = ifelse(.data$m10c > 9995, NA, .data$m10c),
           m10c_wiek = 2018 - .data$m10c,
           m10d = ifelse(.data$m10b %in% 1:10, .data$m10d, NA),
           m10d_in = ifelse(.data$m10d_in %in% c("NIE DOTYCZY", "odmowa"),
                            ".", tolower(.data$m10d_in)),
           m10e = ifelse(.data$m10e %in% 1:2, .data$m10e, NA),
           m10f = ifelse(.data$m10f %in% 1:3, .data$m10f, NA))
  attributes(gospDom$nr_osoby)$label = "Numer porządkowy osoby w ramach gosp. dom."
  attributes(gospDom$m10c_wiek)$label = "Wiek tej osoby"
  gospDom = przywroc_etykiety(gospDom, labTemp)
  attributes(gospDom$m10f)$label = "Czy ta osoba pracuje lub szuka pracy?"
  maska = !grepl("^(ID_RESP|typ_epizodu|nr|swiadectwo)$|^(czas|czy)_", names(gospDom))
  names(gospDom)[maska] = paste0("ABS_", names(gospDom)[maska])
  #|<-
  ################################################################################
  # zasadniczy zbiór
  ################################################################################
  #|->
  dane$f4_id = as.numeric(dane$f4_id)
  dane$pi2_kod = as.numeric(as.character(dane$pi2_kod))
  dane$po2_kod = as.numeric(as.character(dane$po2_kod))
  dane = dane %>%
    left_join(get("kzsb") %>% select("kod_zawodu", "branza_2019"),
              by = c("f4_id" = "kod_zawodu")) %>%
    rename(f4_branza_kzsb = "branza_2019") %>%
    left_join(get("kzsb") %>% select("kod_zawodu", "branza_2019"),
              by = c("pi2_kod" = "kod_zawodu")) %>%
    rename(pi2_branza_kzsb = "branza_2019") %>%
    left_join(get("kzsb") %>% select("kod_zawodu", "branza_2019"),
              by = c("po2_kod" = "kod_zawodu")) %>%
    rename(po2_branza_kzsb = "branza_2019") %>%
    select(-matches("^zp2")) %>%
    select(-matches("^sp[23456]")) %>%
    select(-matches("^pp[23456]")) %>%
    select(-matches("^u[24]")) %>%
    select(-matches("^pg[12]")) %>%
    select(-matches("^pb1")) %>%
    select(-matches("^m10[bcdef]_"))
  maska = !grepl("^(ID_RESP|typ_epizodu|nr|swiadectwo)$|^(czas|czy)_", names(dane))
  names(dane)[maska] = paste0("ABS_", names(dane)[maska])
  #|<-
  return(list(dane = dane,
              epizody = epizody,
              gospDom = gospDom))
}
