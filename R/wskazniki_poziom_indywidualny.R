#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące, czy badany
#' kiedykolwiek był zatrudniony w określonej formie
#' @param x ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @details
#' W kodowaniu wykorzystywane są zarówno odpowiedzi na pytanie PG2H, jak i na
#' pytanie PG2G, z wykorzystaniem następujących reguł:
#' \itemize{
#'   \item{\code{pg2gh.1 = any(pg2h \%in\% 1)},}
#'   \item{\code{pg2gh.2 = any(pg2h \%in\% 2)},}
#'   \item{\code{pg2gh.3 = any(pg2h \%in\% 3)},}
#'   \item{\code{pg2gh.4 = any(pg2h \%in\% 4)},}
#'   \item{\code{pg2gh.5 = any(pg2h \%in\% 5)},}
#'   \item{\code{pg2gh.6 = any(pg2g \%in\% 6)},}
#'   \item{\code{pg2gh.7 = any(pg2g \%in\% 7)},}
#'   \item{\code{pg2gh.8 = any(pg2h \%in\% (7:8))},}
#'   \item{\code{pg2gh.9 = any(pg2h \%in\% 9 | is.na(pg2h))}.}
#' }
#' Dodatkowo, jeśli zmienna \code{pg2h} przyjmuje wartość 6 (praca bez umowy),
#' wszystkie wynikowe zmienne są dla danego respondenta przekodowywane na brak
#' danych.
#' @return data frame
#' @seealso \code{\link{praca_miesiac}}, \code{\link{praca_pierwsza}},
#' \code{\link{praca_ostatnia}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% group_by summarise ungroup
praca_forma_ind = function(x, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(x),
            "pg2g" %in% names(x),
            "pg2h" %in% names(x))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(x))
  x %>%
    group_by(!!idAbsolwenta) %>%
    summarise(pg2h.6 = all(.data$pg2h %in% 6),
              pg2gh.1 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% 1)),
              pg2gh.2 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% 2)),
              pg2gh.3 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% 3)),
              pg2gh.4 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% 4)),
              pg2gh.5 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% 5)),
              pg2gh.6 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2g %in% 6)),
              pg2gh.7 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2g %in% 7)),
              pg2gh.8 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% (7:8))),
              pg2gh.9 = ifelse(.data$pg2h.6, NA,
                               any(.data$pg2h %in% 9 | is.na(.data$pg2h)))) %>%
    select(-"pg2h.6") %>%
    ungroup() %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące, czy badany
#' kiedykolwiek był zatrudniony w miejscu innym, niż miejscowość zamieszkania
#' @param x ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{praca_miesiac}}, \code{\link{praca_pierwsza}},
#' \code{\link{praca_ostatnia}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% group_by summarise ungroup
praca_zamieszkanie_ind = function(x, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(x),
            "pg2i" %in% names(x))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(x))
  x %>%
    group_by(!!idAbsolwenta) %>%
    summarise(pg2i.1 = any(.data$pg2i %in% 1),
              pg2i.2 = any(.data$pg2i %in% 2),
              pg2i.3 = any(.data$pg2i %in% 3),
              pg2i.9 = any(is.na(.data$pg2i))) %>%
    ungroup() %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące formę (formy)
#' zatrudnienia i miejsce (miejsca) pracy (w stosunku do miejsca zamieszkania)
#' badanego w określonym miesiącu od terminu planowego ukończenia szkoły. Nie
#' jest przy tym brana pod uwagę praca świadczona bez umowy.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param miesiac liczba - miesiąc od terminu planowego ukończenia szkoły
#' (porównywana z wartościami kolumn \code{czas_rozp} i \code{czas_zakon})
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{praca_forma}}, \code{\link{praca_forma2}},
#' \code{\link{praca_zamieszkanie}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join
praca_miesiac = function(epizody, miesiac, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "pg2g" %in% names(epizody),
            "pg2h" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            is.numeric(miesiac), length(miesiac) == 1)
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  epizody = epizody %>%
    filter(.data$typ_epizodu %in% "praca",
           !(.data$pg2h %in% 6), # wyłączamy pracę bez umowy,
           .data$czas_rozp <= miesiac & !is.na(.data$czas_rozp),
           .data$czas_zakon >= miesiac | is.na(.data$czas_zakon))
  epizody = suppressMessages(full_join(
    praca_forma_ind(epizody, !!idAbsolwenta),
    praca_zamieszkanie_ind(epizody, !!idAbsolwenta)))
  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                          paste0(names(epizody), "_", miesiac, "m"),
                          names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Prosta funkcja przeliczająca wysokość zarobków wyrażoną w kwocie
#' netto na kwotę brutto. Przyjęty został przybliżony przeliczniok o wartości
#' 0.71
#' @param x wysokość zarobków wyrażona w kwocie netto - zmienna
#' \strong{pio4pelen} z obiektu \code{epizody}
#' @param przelicznik przyjęty odgórnie przelicznik kwoty netto na kwotę brutto.
#' Posiada domyślną wartość 0.71
#' @export
placa_brutto = function(x, przelicznik = 0.71) {
  brutto = x / przelicznik
  return(brutto)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące pierwszą pracę
#' badanego wykonywaną po ukończeniu szkoły (również jeśli zaczęła się
#' wcześniej). Nie jest przy tym brana pod uwagę praca świadczona bez umowy.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{praca_forma_ind}}, \code{\link{praca_zamieszkanie_ind}},
#' \code{\link{praca_forma}}, \code{\link{praca_forma2}},
#' \code{\link{praca_przed_ukonczeniem_szkoly}},
#' \code{\link{praca_zamieszkanie}},
#' \code{\link{praca_zgodna_z_wyksztalceniem}},
#' \code{\link{praca_zarobki}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join mutate select
praca_pierwsza = function(epizody, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "pg2g" %in% names(epizody),
            "pg2h" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "nr" %in% names(epizody),
            "pio1" %in% names(epizody),
            "pio4" %in% names(epizody),
            "pi5" %in% names(epizody))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  miesiaceNauki = nauka_czas(epizody, 9, 1, !!idAbsolwenta)

  epizody = epizody %>%
    filter(.data$typ_epizodu %in% "praca",
           !(.data$pg2h %in% 6), # wyłączamy pracę bez umowy,
           .data$nr == 1, !is.na(.data$czas_rozp)) %>%
    mutate(praca_przed_ukonczeniem_szkoly = .data$czas_rozp < 0,
           wymiar = .data$pio3b / 40)
  if (any(duplicated(epizody[[as.character(idAbsolwenta)]]))) {
    stop("W zbiorze znajdują się badani, dla których występuje wiele epizodów pracy wyglądających na pierwsze.")
  }
  epizody = suppressMessages(
    epizody %>%
      select(as.character(idAbsolwenta), "pio1", "pio4", "pi5", "wymiar",
             praca_czas_rozp = .data$czas_rozp, "praca_przed_ukonczeniem_szkoly") %>%
      mutate(praca_czas_rozp = ifelse(.data$praca_czas_rozp < 0,
                                      NA, .data$praca_czas_rozp)) %>%
      full_join(praca_forma_ind(epizody %>%
                                  filter(!.data$praca_przed_ukonczeniem_szkoly),
                                !!idAbsolwenta)) %>%
      full_join(praca_zamieszkanie_ind(epizody %>%
                                         filter(!.data$praca_przed_ukonczeniem_szkoly),
                                       !!idAbsolwenta)) %>%
      left_join(miesiaceNauki) %>%
      group_by(!!idAbsolwenta) %>%
      mutate(laczenie_praca_nauka =
               .data$praca_czas_rozp %in% unlist(.data$nauka_czas) |
               .data$praca_czas_rozp < 1)) %>%
    group_by(!!idAbsolwenta) %>%
    mutate(pio4pelen = round(as.double(.data$pio4) / as.double(.data$wymiar)),
           pio4pelen_brutto = placa_brutto(.data$pio4pelen)) %>%
    ungroup() %>%
    select(-"nauka_czas")

  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                         paste0(names(epizody), "_pierwsza"),
                         names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące ostatnią pracę
#' badanego wykonywaną w analizowanym okresie. Nie jest przy tym brana pod uwagę
#' praca świadczona bez umowy.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @param limitG liczba - górna granica, do jakiej zostanie zawężony analizowany
#' okres, wyrażona jako liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_rozp})
#' @param limitD liczba - dolna granica, do jakiej zostanie zawężony analizowany
#' okres, wyrażona jako liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_zakon})
#' @return data frame
#' @seealso \code{\link{praca_forma_ind}}, \code{\link{praca_zamieszkanie_ind}},
#' \code{\link{praca_forma}}, \code{\link{praca_forma2}},
#' \code{\link{praca_zamieszkanie}},
#' \code{\link{praca_zgodna_z_wyksztalceniem}},
#' \code{\link{praca_zarobki}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join group_by mutate select ungroup
praca_ostatnia = function(epizody, idAbsolwenta = "ID_RESP",
                          limitG  = Inf, limitD = 0) {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "pg2g" %in% names(epizody),
            "pg2h" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            "nr" %in% names(epizody),
            "pio1" %in% names(epizody),
            "pio4" %in% names(epizody),
            "po5" %in% names(epizody),
            "po6_1" %in% names(epizody),
            "po6_2" %in% names(epizody),
            "po6_3" %in% names(epizody),
            "po6_4" %in% names(epizody),
            "po6_5" %in% names(epizody),
            "po6_6" %in% names(epizody),
            is.numeric(limitG), length(limitG) == 1,
            is.numeric(limitD), length(limitD) == 1)
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  miesiaceNauki = nauka_czas(epizody, 9, 1, !!idAbsolwenta)

  epizody = epizody %>%
    filter(.data$typ_epizodu %in% "praca",
           !(.data$pg2h %in% 6), # wyłączamy pracę bez umowy,
           .data$czas_rozp <= limitG & !is.na(.data$czas_rozp),
           .data$czas_zakon >= limitD | is.na(.data$czas_zakon)) %>%
    group_by(!!idAbsolwenta) %>%
    filter(.data$nr == max(.data$nr)) %>%
    ungroup() %>%
    mutate(wymiar = .data$pio3b / 40)
  if (any(duplicated(epizody[[as.character(idAbsolwenta)]]))) {
    stop("W zbiorze znajdują się badani, dla których występuje wiele epizodów pracy wyglądających na ostatni.")
  }
  epizody = suppressMessages(
    epizody %>%
      select(as.character(idAbsolwenta), "pio1", "pio4", "po5", "wymiar",
             praca_czas_rozp = .data$czas_rozp,
             matches("^po6_[123456]$")) %>%
      mutate(praca_czas_rozp = ifelse(.data$praca_czas_rozp < 0,
                                      NA, .data$praca_czas_rozp)) %>%
      full_join(praca_forma_ind(epizody, !!idAbsolwenta)) %>%
      full_join(praca_zamieszkanie_ind(epizody, !!idAbsolwenta)) %>%
      left_join(miesiaceNauki) %>%
      group_by(!!idAbsolwenta) %>%
      mutate(laczenie_praca_nauka =
               .data$praca_czas_rozp %in% unlist(.data$nauka_czas) |
               .data$praca_czas_rozp < 1)) %>%
    group_by(!!idAbsolwenta) %>%
    mutate(pio4pelen = round(as.double(.data$pio4) / as.double(.data$wymiar)),
           pio4pelen_brutto = placa_brutto(.data$pio4pelen)) %>%
    ungroup() %>%
    select(-"nauka_czas")

  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                          paste0(names(epizody), "_ostatnia"),
                          names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Na podstawie zbioru epizodów dla każdego badanego funkcja
#' zwraca, w których miesiącach się on uczył. Opisująca to w zwracanej ramce
#' danych zmienna \code{nauka_czas} jest kolumną-listą.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param limitG liczba - górna granica, do jakiej zostanie zawężony analizowany
#' okres, wyrażona jako liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_rozp})
#' @param limitD liczba - dolna granica, do jakiej zostanie zawężony analizowany
#' okres, wyrażona jako liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_zakon})
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @details Funkcja wykorzystywana jest wewnętrznie w ramach przekształceń
#' dokonywanych przez funkcje \code{\link{praca_czas}}
#' i \code{\link{bezrobocie_czas}}.
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join group_by mutate select ungroup
nauka_czas = function(epizody, limitG, limitD, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            "nr" %in% names(epizody),
            is.numeric(limitG), length(limitG) == 1,
            is.numeric(limitD), length(limitD) == 1)
  stopifnot(is.finite(limitG),
            is.finite(limitD))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  nauka = epizody %>%
    filter(.data$typ_epizodu %in% c("SPolic.", "studia", "LO dla dorosłych"),
           .data$czas_rozp <= limitG & !is.na(.data$czas_rozp),
           .data$czas_zakon >= limitD | is.na(.data$czas_zakon)) %>%
    mutate(czas_zakon = ifelse(is.na(.data$czas_zakon),
                               limitG, .data$czas_zakon),
           czas_rozp = ifelse(.data$czas_rozp < limitD,
                              limitD, .data$czas_rozp),
           czas_zakon = ifelse(.data$czas_zakon > limitG,
                               limitG, .data$czas_zakon)) %>%
    group_by(!!idAbsolwenta, .data$typ_epizodu, .data$nr) %>%
    mutate(nauka_czas = list(.data$czas_rozp:.data$czas_zakon)) %>%
    group_by(!!idAbsolwenta) %>%
    summarise(nauka_czas = list(unique(unlist(.data$nauka_czas)))) %>%
    ungroup()
  suppressMessages(full_join(nauka,
                             epizody %>%
                               select(as.character(idAbsolwenta)) %>%
                               distinct())) %>%
    mutate(nauka_czas = lapply(.data$nauka_czas,
                               function(x) {
                                 if (is.null(x)) {return(vector("integer", 0))
                                 } else {return(x)}})) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Na podstawie zbioru epizodów dla każdego badanego funkcja
#' oblicza zmienne-wskaźniki opisujące, przez jaką część zadanego okresu
#' badany pracował. Wskaźnik obliczany jest również oddzielnie w odniesieniu do
#' tych części zadanego okresu, kiedy badany się uczył i kiedy się nie uczył.
#' Formy zatrudnienia uwzględniane w obliczeniach pozwala ograniczyć argument
#' \code{pg2hWartosci}.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param pg2hWartosci opcjonalnie wektor liczb - wartości zmiennej \code{pg2h}
#' w ramce danych przekazanej argumentem \code{epizody}, opisujące te formy
#' zatrudnienia, które mają zostać uwzględnione przy zliczaniu (domyślnie
#' pod uwagę brane są pod uwagę wszystkie formy oprócz pracy bez umowy)
#' @param limitG liczba - górna granica analizowanego okresu, wyrażona jako
#' liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_rozp})
#' @param limitD liczba - dolna granica analizowanego okresu, wyrażona jako
#' liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_zakon})
#' @param sufiks ciąg znaków - sufiks, który zostanie dopisan na końcu nazw
#' zmiennych zawierających wartości wskaźników
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{czas_agregacja}}, \code{\link{czas_rozklad}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join group_by mutate n_distinct
#' select ungroup
praca_czas = function(epizody, pg2hWartosci = NULL, limitG  = 9, limitD = 1,
                      sufiks = "", idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            "nr" %in% names(epizody),
            "pg2h" %in% names(epizody),
            is.null(pg2hWartosci) | is.numeric(pg2hWartosci),
            is.numeric(limitG), length(limitG) == 1,
            is.numeric(limitD), length(limitD) == 1)
  stopifnot(is.finite(limitG),
            is.finite(limitD))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  prace = epizody %>%
    filter(.data$typ_epizodu %in% "praca",
           !(.data$pg2h %in% 6), # wyłączamy pracę bez umowy,
           .data$czas_rozp <= limitG & !is.na(.data$czas_rozp),
           .data$czas_zakon >= limitD | is.na(.data$czas_zakon)) %>%
    mutate(czas_zakon = ifelse(is.na(.data$czas_zakon),
                               limitG, .data$czas_zakon),
           czas_rozp = ifelse(.data$czas_rozp < limitD,
                              limitD, .data$czas_rozp),
           czas_zakon = ifelse(.data$czas_zakon > limitG,
                               limitG, .data$czas_zakon))
  if (!is.null(pg2hWartosci)) {
    prace = prace %>%
      filter(.data$pg2h %in% pg2hWartosci)
  }
  prace = prace %>%
    group_by(!!idAbsolwenta, .data$nr) %>%
    mutate(praca_czas = list(.data$czas_rozp:.data$czas_zakon)) %>%
    group_by(!!idAbsolwenta) %>%
    summarise(praca_czas = list(unique(unlist(.data$praca_czas)))) %>%
    ungroup()
  prace = suppressMessages(full_join(prace,
                                     nauka_czas(epizody, limitG, limitD,
                                                !!idAbsolwenta))) %>%
    group_by(!!idAbsolwenta) %>%
    mutate(praca_czas = lapply(praca_czas,
                               function(x) {
                                 if (is.null(x)) {return(vector("integer", 0))
                                 } else {return(x)}})) %>%
    summarise(praca_czas_gdy_bez_nauki =
                n_distinct(setdiff(unlist(.data$praca_czas),
                                   unlist(.data$nauka_czas))) /
                (limitG - limitD + 1 - n_distinct(unlist(.data$nauka_czas))),
              praca_czas_gdy_nauka =
                n_distinct(intersect(unlist(.data$praca_czas),
                                     unlist(.data$nauka_czas))) /
                n_distinct(unlist(.data$nauka_czas)),
              praca_czas =
                n_distinct(unlist(.data$praca_czas)) / (limitG - limitD + 1))
  names(prace) =  ifelse(!(names(prace) %in% as.character(idAbsolwenta)),
                         paste0(names(prace), sufiks),
                         names(prace))
  return(prace)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące czy badany
#' w określonym miesiącu od terminu planowego ukończenia szkoły był
#' zarejestrownym bezrobotnym.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param miesiac liczba - miesiąc od terminu planowego ukończenia szkoły
#' (porównywana z wartościami kolumn \code{czas_rozp} i \code{czas_zakon})
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{bezrobocie}},
#' \code{\link{czas_agregacja}}, \code{\link{czas_rozklad}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join
bezrobocie_miesiac = function(epizody, miesiac, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "pb1h" %in% names(epizody),
            "pg2h" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            is.numeric(miesiac), length(miesiac) == 1)
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  epizody = epizody %>%
    filter(.data$czas_rozp <= miesiac & !is.na(.data$czas_rozp),
           .data$czas_zakon >= miesiac | is.na(.data$czas_zakon)) %>%
    group_by(!!idAbsolwenta) %>%
    summarise(bezrobocie =
                case_when(any(.data$pb1h %in% 1) ~ 1,
                          any(.data$pg2h %in% c(1:5, 7:9) |
                                .data$typ_epizodu %in% c("LO dla dorosłych",
                                                         "SPolic.", "studia")) ~ 0)) %>%
    ungroup()
  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                          paste0(names(epizody), "_", miesiac, "m"),
                          names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Na podstawie zbioru epizodów dla każdego badanego funkcja
#' oblicza zmienne-wskaźniki opisujące, przez jaką część zadanego okresu
#' badany był zarejestrowanym bezrobotnym. Wskaźnik obliczany jest również
#' oddzielnie w odniesieniu do  tych części zadanego okresu, kiedy badany się
#' uczył i kiedy się nie uczył.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param limitG liczba - górna granica analizowanego okresu, wyrażona jako
#' liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_rozp})
#' @param limitD liczba - dolna granica analizowanego okresu, wyrażona jako
#' liczba miesięcy od momentu planowego ukończenia szkoły
#' (porównywana z wartością kolumny \code{czas_zakon})
#' @param sufiks ciąg znaków - sufiks, który zostanie dopisan na końcu nazw
#' zmiennych zawierających wartości wskaźników
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% filter full_join group_by mutate n_distinct
#' select ungroup
bezrobocie_czas = function(epizody, limitG  = 9, limitD = 1, sufiks = "",
                           idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            "nr" %in% names(epizody),
            "pb1h" %in% names(epizody),
            is.numeric(limitG), length(limitG) == 1,
            is.numeric(limitD), length(limitD) == 1)
  stopifnot(is.finite(limitG),
            is.finite(limitD))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  bezrobocie = epizody %>%
    filter(.data$typ_epizodu %in% "bezrobocie",
           .data$pb1h %in% 1, # tylko zarejestrowani w UP
           .data$czas_rozp <= limitG & !is.na(.data$czas_rozp),
           .data$czas_zakon >= limitD | is.na(.data$czas_zakon)) %>%
    mutate(czas_zakon = ifelse(is.na(.data$czas_zakon),
                               limitG, .data$czas_zakon),
           czas_rozp = ifelse(.data$czas_rozp < limitD,
                              limitD, .data$czas_rozp),
           czas_zakon = ifelse(.data$czas_zakon > limitG,
                               limitG, .data$czas_zakon)) %>%
    group_by(!!idAbsolwenta, .data$nr) %>%
    mutate(bezrobocie_czas = list(.data$czas_rozp:.data$czas_zakon)) %>%
    group_by(!!idAbsolwenta) %>%
    summarise(bezrobocie_czas = list(unique(unlist(.data$bezrobocie_czas)))) %>%
    ungroup()
  bezrobocie = suppressMessages(full_join(bezrobocie,
                                          nauka_czas(epizody, limitG, limitD,
                                                     !!idAbsolwenta))) %>%
    group_by(!!idAbsolwenta) %>%
    mutate(bezrobocie_czas = lapply(bezrobocie_czas,
                                    function(x) {
                                      if (is.null(x)) {return(vector("integer", 0))
                                      } else {return(x)}})) %>%
    summarise(bezrobocie_czas_gdy_bez_nauki =
                n_distinct(setdiff(unlist(.data$bezrobocie_czas),
                                   unlist(.data$nauka_czas))) /
                (limitG - limitD + 1 - n_distinct(unlist(.data$nauka_czas))),
              bezrobocie_czas_gdy_nauka =
                n_distinct(intersect(unlist(.data$bezrobocie_czas),
                                     unlist(.data$nauka_czas))) /
                n_distinct(unlist(.data$nauka_czas)),
              bezrobocie_czas =
                n_distinct(unlist(.data$bezrobocie_czas)) / (limitG - limitD + 1))
  names(bezrobocie) =  ifelse(!(names(bezrobocie) %in% as.character(idAbsolwenta)),
                              paste0(names(bezrobocie), sufiks),
                              names(bezrobocie))
  return(bezrobocie)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące czy badany
#' w określonym miesiącu od terminu planowego ukończenia szkoły uczył się,
#' a jeśli tak, to w jakiej formie. Jeśli w danym miesiącu badany uczył się
#' w kilku różnych formach, to wybierana jest jedna przy czym:
#' \itemize{
#'   \item{studia stacjonarne mają pierwszeństwo przed wszystkimi innymi formami,}
#'   \item{studia niestacjonarne mają pierwszeństwo przed szkołami policealnymi
#'         i LO dla dorosłych,}
#'   \item{szkoły policealne mają pierwszeństwo przed LO dla dorosłych.}
#' }
#' Jeśli badany uczył się w kilku szkołac policealnych (a jednocześnie nie
#' studiował), pierwszeństwo przyznawane jest tej, której branża była zgodna
#' z branżą zawodu, w jakim kształcił się w szkole, jako absolwent której
#' został objęty badaniem.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param dane ramka danych z odpowiedziami na ankietę absolwentów - typowo
#' element \code{dane} listy zwracanej przez funkcję
#' \code{\link{imputuj_miesiac_pk_1rm}}
#' @param miesiac liczba - miesiąc od terminu planowego ukończenia szkoły
#' (porównywana z wartościami kolumn \code{czas_rozp} i \code{czas_zakon})
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{nauka}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% arrange case_when desc filter group_by mutate n
#' slice summarise ungroup
nauka_miesiac = function(epizody, dane, miesiac, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            is.data.frame(dane),
            "typ_epizodu" %in% names(epizody),
            "sp6h_3" %in% names(epizody),
            "sp6f" %in% names(epizody),
            "pp6g" %in% names(epizody),
            "zp2i" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            is.numeric(miesiac), length(miesiac) == 1)
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))
  stopifnot(as.character(idAbsolwenta) %in% names(dane))

  dane = dane %>%
    select(!!idAbsolwenta, "ABS_f4_branza_kzsb")

  epizody = epizody %>%
    filter(.data$typ_epizodu %in% c("SPolic.", "studia", "LO dla dorosłych"),
           .data$czas_rozp <= miesiac & !is.na(.data$czas_rozp),
           .data$czas_zakon >= miesiac | is.na(.data$czas_zakon)) %>%
    mutate(nauka =
             case_when(.data$typ_epizodu %in% "studia" & !(.data$sp6h_3 %in% 1) ~ "studia stacjonarne",
                       .data$typ_epizodu %in% "studia" & .data$sp6h_3 %in% 1 ~ "studia niestacjonarne",
                       .data$typ_epizodu %in% "SPolic." ~ "szkoła policealna",
                       grepl("^LO ", .data$typ_epizodu) ~ "LO dla dorosłych") %>%
             factor(levels = c("studia stacjonarne", "studia niestacjonarne",
                               "szkoła policealna", "LO dla dorosłych"))) %>%
    left_join(dane, by = "ID_RESP") %>%
    mutate(spolic_kontynuacja_branza =
             .data$ABS_f4_branza_kzsb == .data$pp3_kierunek_branza_kzsb) %>%
    group_by(!!idAbsolwenta) %>%
    arrange(!!idAbsolwenta, .data$nauka, desc(.data$spolic_kontynuacja_branza)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(nauka = levels(.data$nauka)[.data$nauka],
           nauka_platna =
             case_when(.data$sp6f %in% 1 | .data$pp6g %in% 1 | .data$zp2i %in% 1 ~ 1,
                       !is.na(.data$nauka) ~ 0)) %>%
    select(!!idAbsolwenta, "nauka", "nauka_platna", "spolic_kontynuacja_branza")
  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                          paste0(names(epizody), "_", miesiac, "m"),
                          names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienną-wskaźnik opisującą status
#' edukacyjno-zawodowy respondenta w podanym miesiącu od momentu planowego
#' ukończenia szkoły.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param miesiac liczba - miesiąc od terminu planowego ukończenia szkoły
#' (porównywana z wartościami kolumn \code{czas_rozp} i \code{czas_zakon})
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{praca_nauka}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% arrange case_when filter group_by mutate
#' summarise ungroup
praca_nauka_miesiac = function(epizody, miesiac, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "pg2h" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "czas_zakon" %in% names(epizody),
            is.numeric(miesiac), length(miesiac) == 1)
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  epizody = epizody %>%
    filter(.data$typ_epizodu %in% c("praca", "SPolic.", "studia", "LO dla dorosłych"),
           !(.data$pg2h %in% 6), # wyłączamy pracę bez umowy
           .data$czas_rozp <= miesiac & !is.na(.data$czas_rozp),
           .data$czas_zakon >= miesiac | is.na(.data$czas_zakon)) %>%
    mutate(typ_epizodu = ifelse(.data$typ_epizodu == "praca",
                                "praca", "nauka")) %>%
    group_by(!!idAbsolwenta) %>%
    summarise(praca_nauka =
                case_when(any(.data$typ_epizodu %in% "praca") &
                            all(!(.data$typ_epizodu %in% "nauka")) ~ "tylko praca",
                          any(.data$typ_epizodu %in% "praca") &
                            any(.data$typ_epizodu %in% "nauka") ~ "praca i edukacja",
                          all(!(.data$typ_epizodu %in% "praca")) &
                            any(.data$typ_epizodu %in% "nauka") ~ "tylko edukacja")) %>%
    ungroup()
  epizody = suppressMessages(
    epizody %>%
      full_join(epizody %>% select(as.character(idAbsolwenta)) %>%
                  distinct())) %>%
    mutate(praca_nauka = ifelse(is.na(.data$praca_nauka),
                                "bez pracy i poza edukacją", .data$praca_nauka))
  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                          paste0(names(epizody), "_", miesiac, "m"),
                          names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki opisujące pierwszą studia
#' podjęte przez badanego po ukończeniu szkoły.
#' @param epizody ramka danych z epizodami - typowo element \code{epizody} listy
#' zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame
#' @seealso \code{\link{studia_gdzie}}, \code{\link{studia_odplatnosc}},
#' \code{\link{studia_tryb}}
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr .data %>% case_when filter group_by mutate select ungroup
studia_pierwsze = function(epizody, idAbsolwenta = "ID_RESP") {
  stopifnot(is.data.frame(epizody),
            "typ_epizodu" %in% names(epizody),
            "sp5" %in% names(epizody),
            "czas_rozp" %in% names(epizody),
            "nr" %in% names(epizody),
            "sp3_kierunek" %in% names(epizody),
            "sp3_uczelnia" %in% names(epizody),
            "sp6f" %in% names(epizody),
            "sp6h_1" %in% names(epizody),
            "sp6h_2" %in% names(epizody),
            "sp6h_3" %in% names(epizody))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(epizody))

  epizody = epizody %>%
    filter(.data$typ_epizodu %in% c("studia"),
           .data$sp5 %in% 1,
           .data$czas_rozp > 0 & !is.na(.data$czas_rozp)) %>%
    group_by(!!idAbsolwenta) %>%
    filter(.data$nr == min(.data$nr)) %>%
    ungroup() %>%
    mutate(studia_kierunek = .data$sp3_kierunek,
           studia_uczelnia = .data$sp3_uczelnia,
           studia_bezplatne = ifelse(.data$sp6f %in% 1, 1, 0),
           studia_tryb = case_when(.data$sp6h_1 %in% 1 ~ 1,
                                   .data$sp6h_2 %in% 1 ~ 2,
                                   .data$sp6h_3 %in% 1 ~ 3)) %>%
    select(as.character(idAbsolwenta), "studia_kierunek", "studia_uczelnia",
           "studia_bezplatne", "studia_tryb")
  names(epizody) = ifelse(!(names(epizody) %in% as.character(idAbsolwenta)),
                          paste0(names(epizody), "_pierwsze"),
                          names(epizody))
  return(epizody)
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki, które wymagają informacji
#' nie ze zbioru epizodów.
#' @param x ramka danych z odpowiedziami na ankietę absolwentów - typowo element
#' \code{dane} listy zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param maksRokEgz liczba - najpóźniejszy rok, w którym absolwent mógł zdać
#' egzamin, aby w wynikowym wskaźniku został oznaczony jako osoba, która ten
#' egzamin zdała
#' @return data frame zawierająca następujące kolumny:
#' \itemize{
#'   \item{wszystkie kolumny ramki danych przekazanej argumentem \code{x},
#'         których nazwy \strong{nie} zaczynają się od "ABS_",}
#'   \item{\code{SZK_teryt} - nr TERYT powiatu, przeliczony na podstawie
#'         znajdującego się wcześniej w tej zmiennej nr TERYT gminy, na terenie
#'         której znajduje się szkoła,}
#'   \item{\code{UCZ_plec} - płeć ucznia ("M" lub "K"),}
#'   \item{\code{matura_zdana} - wskaźnik opisujący, czy badany zdał maturę
#'         (liczba: 1 - zdał, 0 - nie zdał),}
#'   \item{\code{egz_zaw_zdany} - wskaźnik opisujący, czy badany zdał egzamin
#'         zawodowy, tj. zdał wszystkie egzaminy niezbędne do uzyskania
#'         dyplomu potwierdzającego kwalifikacje w zawodzie (liczba: 1 - zdał,
#'         0 - nie zdał).}
#' }
#' @seealso \code{\link{egz_zaw_zdawalnosc}}, \code{\link{matura_zdawalnosc}},
#' \code{\link{liczba_kobiet}}
#' @export
#' @importFrom haven is.labelled as_factor
#' @importFrom dplyr .data %>% case_when mutate select
wskazniki_nie_z_epizodow = function(x, maksRokEgz) {
  stopifnot(is.data.frame(x),
            "ABS_typ_szkoly" %in% names(x),
            "ABS_teryt_szkoly" %in% names(x),
            "ABS_m1" %in% names(x),
            "ABS_f8" %in% names(x),
            "ABS_f8a_rok" %in% names(x),
            "ABS_f10_rok" %in% names(x))
  stopifnot(is.labelled(x$ABS_typ_szkoly))
  x$ABS_typ_szkoly = as_factor(x$ABS_typ_szkoly)
  x$ABS_typ_szkoly = levels(x$ABS_typ_szkoly)[x$ABS_typ_szkoly]
  names(x) = ifelse(grepl("_szkoly", names(x)),
                    sub("_szkoly", "", sub("^ABS_", "SZK_", names(x))),
                    names(x))
  x %>%
    mutate(ABS_teryt_powiat = floor(.data$SZK_teryt / 100),
           ABS_teryt_woj = floor(.data$SZK_teryt / 10000),
           SZK_teryt = 100 * floor(as.numeric(.data$SZK_teryt) / 100),
           UCZ_plec = case_when(.data$ABS_m1 %in% 1 ~ "K",
                                .data$ABS_m1 %in% 2 ~ "M"),
           matura_zdana =
             case_when(grepl("technikum", .data$SZK_typ, ignore.case = TRUE) &
                         .data$ABS_f8 %in% 1 & .data$ABS_f8a_rok <= maksRokEgz &
                         !is.na(.data$ABS_f8a_rok) ~ 1,
                       grepl("technikum", .data$SZK_typ, ignore.case = TRUE) ~ 0),
           egz_zaw_zdany =
             case_when(.data$ABS_f9 %in% 1 & .data$ABS_f10_rok <= maksRokEgz &
                         !is.na(.data$ABS_f10_rok) ~ 1,
                       TRUE ~ 0)) %>%
    left_join(get("nazwy_jst"), by = c("ABS_teryt_powiat" = "teryt")) %>%
    select(-starts_with("ABS")) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie indywidualnym
#' @description Funkcja oblicza zmienne-wskaźniki, które wymagają informacji
#' nie ze zbioru epizodów.
#' @param x ramka danych zwracana przez funkcję
#' \code{\link{imputuj_miesiac_pk_1rm}}
#' @param idAbsolwenta nazwa zmiennej identyfikującej badanych (podana jako ciąg
#' znaków lub wyrażenie)
#' @return data frame zawierający następujące kolumny:
#' \itemize{
#'   \item{wszystkie kolumny ramki danych przekazanej argumentem \code{x},
#'         których nazwy \strong{nie} zaczynają się od "ABS_"; zwykle będą to:
#'         \itemize{
#'           \item{\code{ID_RESP},}
#'           \item{\code{SZK_typ},}
#'           \item{\code{SZK_kod},}
#'           \item{\code{SZK_pozycja_w_operacie},}
#'           \item{\code{SZK_regon},}
#'           \item{\code{SZK_id_sio},}
#'           \item{\code{UCZ_kod_zawodu},}
#'           \item{\code{UCZ_zawod},}
#'           \item{\code{UCZ_branza},}
#'           \item{\code{UCZ_obszar},}
#'         }}
#'   \item{\code{SZK_teryt},}
#'   \item{\code{UCZ_plec},}
#'   \item{\code{matura_zdana},}
#'   \item{\code{egz_zaw_zdany},}
#'   \item{\code{powiat_nazwa},}
#'   \item{\code{woj_nazwa},}
#'   \item{\code{pio1_pierwsza},}
#'   \item{\code{pio4_pierwsza},}
#'   \item{\code{pi5_pierwsza},}
#'   \item{\code{wymiar_pierwsza},}
#'   \item{\code{praca_czas_rozp_pierwsza},}
#'   \item{\code{praca_przed_ukonczeniem_szkoly_pierwsza},}
#'   \item{\code{pg2gh.1_pierwsza},}
#'   \item{\code{pg2gh.2_pierwsza},}
#'   \item{\code{pg2gh.3_pierwsza},}
#'   \item{\code{pg2gh.4_pierwsza},}
#'   \item{\code{pg2gh.5_pierwsza},}
#'   \item{\code{pg2gh.6_pierwsza},}
#'   \item{\code{pg2gh.7_pierwsza},}
#'   \item{\code{pg2gh.8_pierwsza},}
#'   \item{\code{pg2gh.9_pierwsza},}
#'   \item{\code{pg2i.1_pierwsza},}
#'   \item{\code{pg2i.2_pierwsza},}
#'   \item{\code{pg2i.3_pierwsza},}
#'   \item{\code{pg2i.9_pierwsza},}
#'   \item{\code{laczenie_praca_nauka_pierwsza},}
#'   \item{\code{pio4pelen_pierwsza},}
#'   \item{\code{pio4pelen_brutto_pierwsza},}
#'   \item{\code{pio1_ostatnia},}
#'   \item{\code{pio4_ostatnia},}
#'   \item{\code{po5_ostatnia},}
#'   \item{\code{wymiar_ostatnia},}
#'   \item{\code{praca_czas_rozp_ostatnia},}
#'   \item{\code{po6_1_ostatnia},}
#'   \item{\code{po6_2_ostatnia},}
#'   \item{\code{po6_3_ostatnia},}
#'   \item{\code{po6_4_ostatnia},}
#'   \item{\code{po6_5_ostatnia},}
#'   \item{\code{po6_6_ostatnia},}
#'   \item{\code{pg2gh.1_ostatnia},}
#'   \item{\code{pg2gh.2_ostatnia},}
#'   \item{\code{pg2gh.3_ostatnia},}
#'   \item{\code{pg2gh.4_ostatnia},}
#'   \item{\code{pg2gh.5_ostatnia},}
#'   \item{\code{pg2gh.6_ostatnia},}
#'   \item{\code{pg2gh.7_ostatnia},}
#'   \item{\code{pg2gh.8_ostatnia},}
#'   \item{\code{pg2gh.9_ostatnia},}
#'   \item{\code{pg2i.1_ostatnia},}
#'   \item{\code{pg2i.2_ostatnia},}
#'   \item{\code{pg2i.3_ostatnia},}
#'   \item{\code{pg2i.9_ostatnia},}
#'   \item{\code{laczenie_praca_nauka_ostatnia},}
#'   \item{\code{pio4pelen_ostatnia},}
#'   \item{\code{pio4pelen_brutto_ostatnia},}
#'   \item{\code{pg2gh.1_6m},}
#'   \item{\code{pg2gh.2_6m},}
#'   \item{\code{pg2gh.3_6m},}
#'   \item{\code{pg2gh.4_6m},}
#'   \item{\code{pg2gh.5_6m},}
#'   \item{\code{pg2gh.6_6m},}
#'   \item{\code{pg2gh.7_6m},}
#'   \item{\code{pg2gh.8_6m},}
#'   \item{\code{pg2gh.9_6m},}
#'   \item{\code{pg2i.1_6m},}
#'   \item{\code{pg2i.2_6m},}
#'   \item{\code{pg2i.3_6m},}
#'   \item{\code{pg2i.9_6m},}
#'   \item{\code{pg2gh.1_9m},}
#'   \item{\code{pg2gh.2_9m},}
#'   \item{\code{pg2gh.3_9m},}
#'   \item{\code{pg2gh.4_9m},}
#'   \item{\code{pg2gh.5_9m},}
#'   \item{\code{pg2gh.6_9m},}
#'   \item{\code{pg2gh.7_9m},}
#'   \item{\code{pg2gh.8_9m},}
#'   \item{\code{pg2gh.9_9m},}
#'   \item{\code{pg2i.1_9m},}
#'   \item{\code{pg2i.2_9m},}
#'   \item{\code{pg2i.3_9m},}
#'   \item{\code{pg2i.9_9m},}
#'   \item{\code{praca_czas_gdy_bez_nauki_p9m},}
#'   \item{\code{praca_czas_gdy_nauka_p9m},}
#'   \item{\code{praca_czas_p9m},}
#'   \item{\code{praca_czas_gdy_bez_nauki_uop_p9m},}
#'   \item{\code{praca_czas_gdy_nauka_uop_p9m},}
#'   \item{\code{praca_czas_uop_p9m},}
#'   \item{\code{bezrobocie_czas_gdy_bez_nauki_p9m},}
#'   \item{\code{bezrobocie_czas_gdy_nauka_p9m},}
#'   \item{\code{bezrobocie_czas_p9m},}
#'   \item{\code{bezrobocie_1m},}
#'   \item{\code{bezrobocie_2m},}
#'   \item{\code{bezrobocie_3m},}
#'   \item{\code{bezrobocie_4m},}
#'   \item{\code{bezrobocie_5m},}
#'   \item{\code{bezrobocie_6m},}
#'   \item{\code{bezrobocie_7m},}
#'   \item{\code{bezrobocie_8m},}
#'   \item{\code{bezrobocie_9m},}
#'   \item{\code{spolic_kontynuacja_branza_6m},}
#'   \item{\code{nauka_6m},}
#'   \item{\code{nauka_platna_6m},}
#'   \item{\code{spolic_kontynuacja_branza_9m},}
#'   \item{\code{nauka_9m},}
#'   \item{\code{nauka_platna_9m},}
#'   \item{\code{studia_kierunek_pierwsze},}
#'   \item{\code{studia_uczelnia_pierwsze},}
#'   \item{\code{studia_bezplatne_pierwsze},}
#'   \item{\code{studia_tryb_pierwsze},}
#'   \item{\code{praca_nauka_0m},}
#'   \item{\code{praca_nauka_1m},}
#'   \item{\code{praca_nauka_2m},}
#'   \item{\code{praca_nauka_3m},}
#'   \item{\code{praca_nauka_4m},}
#'   \item{\code{praca_nauka_5m},}
#'   \item{\code{praca_nauka_6m},}
#'   \item{\code{praca_nauka_7m},}
#'   \item{\code{praca_nauka_8m},}
#'   \item{\code{praca_nauka_9m}.}
#' }
#' @seealso Funkcje używane wewnętrznie do obliczania wskaźników zagregowanych:
#' \itemize{
#'   \item{\code{\link{wskazniki_nie_z_epizodow}},}
#'   \item{\code{\link{placa_brutto}},}
#'   \item{\code{\link{praca_pierwsza}},}
#'   \item{\code{\link{praca_ostatnia}},}
#'   \item{\code{\link{praca_miesiac}},}
#'   \item{\code{\link{praca_czas}},}
#'   \item{\code{\link{bezrobocie_czas}},}
#'   \item{\code{\link{bezrobocie_miesiac}},}
#'   \item{\code{\link{nauka_miesiac}},}
#'   \item{\code{\link{studia_pierwsze}},}
#'   \item{\code{\link{praca_nauka_miesiac}}.}
#' }
#' @export
#' @importFrom haven is.labelled as_factor
#' @importFrom dplyr %>% contains ends_with left_join select starts_with
oblicz_wskazniki_ind_1rm = function(x, idAbsolwenta = "ID_RESP") {
  stopifnot(is.list(x),
            "dane" %in% names(x),
            "epizody" %in% names(x))
  idAbsolwenta = ensym(idAbsolwenta)
  stopifnot(as.character(idAbsolwenta) %in% names(x$dane),
            as.character(idAbsolwenta) %in% names(x$epizody))
  names(x$epizody) = sub("^ABS_", "", names(x$epizody))

  message("Obliczanie wskaźników:\n- niewymagających odwołania do zbioru epizodów,")
  wskazniki = suppressWarnings(suppressMessages(
    x$dane %>%
      select(as.character(idAbsolwenta), contains("_szkoly"),
             "ABS_f4_branza_kzsb", "ABS_pi2_branza_kzsb", "ABS_po2_branza_kzsb",
             UCZ_kod_zawodu = .data$ABS_f4_id, UCZ_zawod = .data$ABS_f4,
             UCZ_branza = .data$ABS_f4_branza, UCZ_obszar = .data$ABS_f4_obszar,
             "ABS_m1", starts_with("ABS_f8"), "ABS_f9", starts_with("ABS_f10")) %>%
      wskazniki_nie_z_epizodow(maksRokEgz = 2017)))
  message("- dotyczących pracy,")
  wskazniki = suppressWarnings(suppressMessages(
    wskazniki %>%
      left_join(praca_pierwsza(x$epizody)) %>%
      left_join(praca_ostatnia(x$epizody)) %>% # ostatnia, ale po ukończeniu szkoły
      left_join(praca_miesiac(x$epizody, 6)) %>%
      left_join(praca_miesiac(x$epizody, 9)) %>%
      left_join(praca_czas(x$epizody, NULL, 9, 1, "_p9m")) %>%
      left_join(praca_czas(x$epizody, 1:2, 9, 1, "_uop_p9m"))))
  message("- dotyczących bezrobocia,")
  wskazniki = suppressWarnings(suppressMessages(
    wskazniki %>%
      left_join(bezrobocie_czas(x$epizody, 9, 1, "_p9m")) %>%
      left_join(bezrobocie_miesiac(x$epizody, 1)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 2)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 3)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 4)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 5)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 6)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 7)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 8)) %>%
      left_join(bezrobocie_miesiac(x$epizody, 9))))
  message("- dotyczących nauki,")
  wskazniki = suppressWarnings(suppressMessages(
    wskazniki %>%
      left_join(nauka_miesiac(x$epizody, x$dane, 6)) %>%
      left_join(nauka_miesiac(x$epizody, x$dane, 9)) %>%
      left_join(studia_pierwsze(x$epizody))))
  message("- opisujących status edukacyjno-zawodowy.")
  wskazniki = suppressWarnings(suppressMessages(
    wskazniki %>%
      left_join(praca_nauka_miesiac(x$epizody, 0)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 1)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 2)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 3)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 4)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 5)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 6)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 7)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 8)) %>%
      left_join(praca_nauka_miesiac(x$epizody, 9))))
  return(wskazniki)
}
