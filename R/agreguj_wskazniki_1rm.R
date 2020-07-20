#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych ze wskaźnikami z poziomu indywidualnego
#' (zwrócnej przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}).
#' @param wskazniki ramka danych ze wskaźnikami na poziomie idywidualnym
#' zwracana przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje,
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'   \item{\code{\link{liczba_zbadanych}},}
#'   \item{\code{\link{liczba_kobiet}},}
#'   \item{\code{\link{liczba_szkol}},}
#'   \item{\code{\link{zawody}},}
#'   \item{\code{\link{praca_nauka}},}
#'   \item{\code{\link{egz_zaw_zdawalnosc}},}
#'   \item{\code{\link{matura_zdawalnosc}},}
#'   \item{\code{\link{praca_przed_ukonczeniem_szkoly}},}
#'   \item{\code{\link{praca_czas_rozp}},}
#'   \item{\code{\link{praca_forma}},}
#'   \item{\code{\link{praca_forma2}},}
#'   \item{\code{\link{praca_zamieszkanie}},}
#'   \item{\code{\link{praca_zgodna_z_wyksztalceniem}},}
#'   \item{\code{\link{praca_zarobki}},}
#'   \item{\code{\link{praca_spelnienie_oczekiwan}},}
#'   \item{\code{\link{bezrobocie}},}
#'   \item{\code{\link{czas_agregacja}},}
#'   \item{\code{\link{czas_rozklad}},}
#'   \item{\code{\link{nauka}},}
#'   \item{\code{\link{studia_gdzie}},}
#'   \item{\code{\link{studia_odplatnosc}},}
#'   \item{\code{\link{studia_tryb}}.}
#' }
#' @export
#' @importFrom rlang ensyms
#' @importFrom dplyr .data %>% do group_by ungroup
agreguj_wskazniki_1rm = function(wskazniki, grupy) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy))
  nazwy = c("SZK_kod", 'SZK_powiat', 'UCZ_zawod', 'UCZ_plec',
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
            'praca_nauka_9m',
            'powiat_bezrobocie_1m', 'powiat_bezrobocie_2m',
            'powiat_bezrobocie_3m', 'powiat_bezrobocie_4m',
            'powiat_bezrobocie_5m', 'powiat_bezrobocie_6m',
            'powiat_bezrobocie_7m', 'powiat_bezrobocie_8m',
            'powiat_bezrobocie_9m', 'powiat_sr_wynagrodzenia_0r')
  sprawdz_nazwy(names(wskazniki), nazwy)

  wskazniki = agreguj_wskazniki(
    wskazniki, grupy,
    liczba_zbadanych = liczba_zbadanych(.data),
    liczba_zbadanych_kobiet = liczba_kobiet(.data),
    liczba_szkol = liczba_szkol(.data),
    zawody = zawody(.data),
    praca_nauka_0m = praca_nauka(.data, "_0m"),
    praca_nauka_1m = praca_nauka(.data, "_1m"),
    praca_nauka_2m = praca_nauka(.data, "_2m"),
    praca_nauka_3m = praca_nauka(.data, "_3m"),
    praca_nauka_4m = praca_nauka(.data, "_4m"),
    praca_nauka_5m = praca_nauka(.data, "_5m"),
    praca_nauka_6m = praca_nauka(.data, "_6m"),
    praca_nauka_7m = praca_nauka(.data, "_7m"),
    praca_nauka_8m = praca_nauka(.data, "_8m"),
    praca_nauka_9m = praca_nauka(.data, "_9m"),
    egz_zaw_zdawalnosc = egz_zaw_zdawalnosc(.data),
    matura_zdawalnosc = matura_zdawalnosc(.data),
    praca_przed_ukonczeniem_szkoly = praca_przed_ukonczeniem_szkoly(.data),
    praca_czas_rozp = praca_czas_rozp(.data),
    praca_forma_pierwsza = praca_forma(.data, "_pierwsza"),
    praca_forma_ostatnia = praca_forma(.data, "_ostatnia"),
    praca_forma2_pierwsza = praca_forma2(.data, "_pierwsza"),
    praca_forma_6m = praca_forma(.data, "_6m"),
    praca_forma_9m = praca_forma(.data, "_9m"),
    praca_forma2_6m = praca_forma2(.data, "_6m"),
    praca_forma2_9m = praca_forma2(.data, "_9m"),
    praca_forma2_bu_6m = praca_forma2(filter(.data, .data$praca_nauka_6m %in% "tylko praca"), "_6m"),
    praca_forma2_bu_9m = praca_forma2(filter(.data, .data$praca_nauka_9m %in% "tylko praca"), "_9m"),
    praca_zamieszkanie_pierwsza = praca_zamieszkanie(.data, "_pierwsza"),
    praca_zamieszkanie_ostatnia = praca_zamieszkanie(.data, "_ostatnia"),
    praca_zamieszkanie_6m = praca_zamieszkanie(.data, "_6m"),
    praca_zamieszkanie_9m = praca_zamieszkanie(.data, "_9m"),
    praca_zgodna_z_wyksztalceniem_pierwsza = praca_zgodna_z_wyksztalceniem(.data, "_pierwsza"),
    praca_zgodna_z_wyksztalceniem_ostatnia = praca_zgodna_z_wyksztalceniem(.data, "_ostatnia"),
    praca_zarobki_pierwsza = praca_zarobki(.data, "_pierwsza", "_0r"),
    praca_zarobki_ostatnia = praca_zarobki(.data, "_ostatnia", "_0r"),
    praca_spelnienie_oczekiwan_ostatnia = praca_spelnienie_oczekiwan(.data, "_ostatnia"),
    praca_czas_p9m_rozklad =
      czas_rozklad(.data, "praca_czas_p9m",
                   c(-Inf, seq(0, 1, by = 1 / 9) + 0.01),
                   paste0(0:9, c(" miesięcy", " miesiąc", rep(" miesięce", 3),
                                 rep(" miesięcy", 5)))),
    praca_czas_uop_p9m_rozklad =
      czas_rozklad(.data, "praca_czas_uop_p9m",
                   c(-Inf, seq(0, 1, by = 1 / 9) + 0.01),
                   paste0(0:9, c(" miesięcy", " miesiąc", rep(" miesięce", 3),
                                 rep(" miesięcy", 5)))),
    praca_czas_gdy_bez_nauki_p9m_rozklad =
      czas_rozklad(.data, "praca_czas_gdy_bez_nauki_p9m",
                   c(-Inf, seq(0, 1, by = 0.2)),
                   paste0(100 * seq(0, 1, by = 0.2), "% okresu")),
    praca_czas_gdy_bez_nauki_uop_p9m_rozklad =
      czas_rozklad(.data, "praca_czas_gdy_bez_nauki_uop_p9m",
                   c(-Inf, seq(0, 1, by = 0.2)),
                   paste0(100 * seq(0, 1, by = 0.2), "% okresu")),
    bezrobocie_czas_p9m_rozklad =
      czas_rozklad(.data, "bezrobocie_czas_p9m",
                   c(-Inf, seq(0, 1, by = 1 / 9) + 0.01),
                   paste0(0:9, c(" miesięcy", " miesiąc", rep(" miesięce", 3),
                                 rep(" miesięcy", 5)))),
    bezrobocie_czas_gdy_bez_nauki_p9m_rozklad =
      czas_rozklad(.data, "bezrobocie_czas_gdy_bez_nauki_p9m",
                   c(-Inf, seq(0, 1, by = 0.2)),
                   paste0(100 * seq(0, 1, by = 0.2), "% okresu")),
    praca_czas_p9m = czas_agregacja(.data, "praca_czas_p9m"),
    praca_czas_gdy_bez_nauki_p9m = czas_agregacja(.data, "praca_czas_gdy_bez_nauki_p9m"),
    praca_czas_gdy_nauka_p9m = czas_agregacja(.data, "praca_czas_gdy_nauka_p9m"),
    praca_czas_uop_p9m = czas_agregacja(.data, "praca_czas_uop_p9m"),
    praca_czas_gdy_bez_nauki_uop_p9m = czas_agregacja(.data, "praca_czas_gdy_bez_nauki_uop_p9m"),
    praca_czas_gdy_nauka_uop_p9m = czas_agregacja(.data, "praca_czas_gdy_nauka_uop_p9m"),
    bezrobocie_czas_p9m = czas_agregacja(.data, "bezrobocie_czas_p9m"),
    bezrobocie_czas_gdy_bez_nauki_p9m = czas_agregacja(.data, "bezrobocie_czas_gdy_bez_nauki_p9m"),
    bezrobocie_czas_gdy_nauka_p9m = czas_agregacja(.data, "bezrobocie_czas_gdy_nauka_p9m"),
    bezrobocie_1m = bezrobocie(.data, "_1m"),
    bezrobocie_2m = bezrobocie(.data, "_2m"),
    bezrobocie_3m = bezrobocie(.data, "_3m"),
    bezrobocie_4m = bezrobocie(.data, "_4m"),
    bezrobocie_5m = bezrobocie(.data, "_5m"),
    bezrobocie_6m = bezrobocie(.data, "_6m"),
    bezrobocie_7m = bezrobocie(.data, "_7m"),
    bezrobocie_8m = bezrobocie(.data, "_8m"),
    bezrobocie_9m = bezrobocie(.data, "_9m"),
    nauka_6m = nauka(.data, "_6m"),
    nauka_9m = nauka(.data, "_9m"),
    studia_gdzie_pierwsze = studia_gdzie(.data, "_pierwsze"),
    studia_odplatnosc_pierwsze = studia_odplatnosc(.data, "_pierwsze"),
    studia_tryb_pierwsze = studia_tryb(.data, "_pierwsze"))
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie szkoły
#' na podstawie ramki danych ze wskaźnikami z poziomu indywidualnego
#' (zwrócnej przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}).
#' @param wskazniki ramka danych ze wskaźnikami na poziomie idywidualnym
#' zwracana przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param progAnonimizacji liczba - wskazniki obliczone na podstawie
#' mniejszej liczby badanych zostaną zamienione na braki danych
#' (p. \code{\link{anonimizuj_wskazniki}})
#' @param wykluczGrupeZGrupyOdniesienia wartość logiczna - czy obserwacje
#' z analizowanej grupy powinny zostać wykluczone z grupy odniesienia
#' @return lista dwóch ramek danych:
#' \itemize{
#'   \item{\code{grupy} - ramka danych zawierająca wskaźniki obliczone dla
#'         poszczególnych grup,}
#'   \item{\code{grupyOdniesienia} - ramka danych zawierająca wskaźniki
#'         obliczone dla odpowiadających im grup odniesienia.}
#' }
#' @seealso \code{\link{agreguj_wskazniki_1rm}}
#' i code{\link{utworz_grupowanie_ze_zmiennej}}, które wykonują większość pracy
#' @importFrom dplyr %>% .data mutate
#' @export
agreguj_wskazniki_1rm_szk = function(wskazniki, progAnonimizacji = 10,
                                     wykluczGrupeZGrupyOdniesienia = FALSE) {
  stopifnot(is.data.frame(wskazniki),
            is.null(progAnonimizacji) | is.numeric(progAnonimizacji))
  sprawdz_nazwy(names(wskazniki),
                c("SZK_kod", "SZK_typ"))
  if (!is.null(progAnonimizacji)) {
    stopifnot(progAnonimizacji > 1)
  }

  wskazniki = wskazniki %>%
    agreguj_wskazniki_1rm(
      utworz_grupowanie_ze_zmiennej(wskazniki, "SZK_kod", "SZK_typ",
                                    wykluczGrupeZGrupyOdniesienia))
  wskazniki$grupy = wskazniki$grupy %>%
    mutate(GRUPA_kod = paste0("typ_szk_", as.numeric(factor(.data$SZK_typ))))

  wskazniki$grupyOdniesienia = wskazniki$grupyOdniesienia %>%
    mutate(GRUPA_kod = paste0("typ_szk_", as.numeric(factor(.data$SZK_typ))),
           GRUPA_nazwa =
             case_when(grepl("policealna", .data$SZK_typ, ignore.case = TRUE) ~
                         "uczniowie wszystkich zbadanych szkół policealnych (ponadgimnazjalnych)",
                       grepl("Technikum", .data$SZK_typ, ignore.case = TRUE) ~
                         "uczniowie wszystkich zbadanych techników",
                       grepl("Zasadnicza", .data$SZK_typ, ignore.case = TRUE) ~
                         "uczniowie wszystkich zbadanych zasadniczych szkół zawodowowych"))

  if (!is.null(progAnonimizacji)) {
    wskazniki$grupy = anonimizuj_wskazniki(wskazniki$grupy, progAnonimizacji)
    wskazniki$grupyOdniesienia = anonimizuj_wskazniki(wskazniki$grupyOdniesienia,
                                                      progAnonimizacji)
  }

  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie branży
#' w ramach szkoły na podstawie ramki danych ze wskaźnikami z poziomu
#' indywidualnego (zwrócnej przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}).
#' @param wskazniki ramka danych ze wskaźnikami na poziomie idywidualnym
#' zwracana przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param progAnonimizacji liczba - wskazniki obliczone na podstawie
#' mniejszej liczby badanych zostaną zamienione na braki danych
#' (p. \code{\link{anonimizuj_wskazniki}})
#' @param wykluczGrupeZGrupyOdniesienia wartość logiczna - czy obserwacje
#' z analizowanej grupy powinny zostać wykluczone z grupy odniesienia
#' @return lista dwóch ramek danych:
#' \itemize{
#'   \item{\code{grupy} - ramka danych zawierająca wskaźniki obliczone dla
#'         poszczególnych grup,}
#'   \item{\code{grupyOdniesienia} - ramka danych zawierająca wskaźniki
#'         obliczone dla odpowiadających im grup odniesienia.}
#' }
#' @seealso \code{\link{agreguj_wskazniki_1rm}}
#' i code{\link{utworz_grupowanie_ze_zmiennej}}, które wykonują większość pracy
#' @importFrom dplyr %>% .data mutate
#' @export
agreguj_wskazniki_1rm_szk_branza = function(wskazniki, progAnonimizacji = 10,
                                            wykluczGrupeZGrupyOdniesienia = FALSE) {
  stopifnot(is.data.frame(wskazniki),
            is.null(progAnonimizacji) | is.numeric(progAnonimizacji))
  sprawdz_nazwy(names(wskazniki),
                c("SZK_typ", "SZK_kod", "UCZ_branza"))
  if (!is.null(progAnonimizacji)) {
    stopifnot(progAnonimizacji > 1)
  }

  wskazniki = wskazniki %>%
    mutate(SZK_kod_branza = paste(.data$SZK_kod, .data$UCZ_branza),
           SZK_typ_branza = paste(.data$SZK_typ, .data$UCZ_branza))
  wskazniki = wskazniki %>%
    agreguj_wskazniki_1rm(
      utworz_grupowanie_ze_zmiennej(wskazniki, "SZK_kod_branza", "SZK_typ_branza",
                                    wykluczGrupeZGrupyOdniesienia,
                                    "SZK_kod", "SZK_typ", "UCZ_branza"))
  wskazniki$grupy = wskazniki$grupy %>%
    mutate(GRUPA_kod = paste0("typ_szk_", as.numeric(factor(.data$SZK_typ)),
                              "_b", as.numeric(factor(.data$UCZ_branza))),
           SZK_kod = paste0(.data$SZK_kod, "_b", as.numeric(factor(.data$UCZ_branza))))

  wskazniki$grupyOdniesienia = wskazniki$grupyOdniesienia %>%
    mutate(GRUPA_kod = paste0("typ_szk_", as.numeric(factor(.data$SZK_typ))),
           GRUPA_nazwa =
             case_when(grepl("policealna", .data$SZK_typ, ignore.case = TRUE) ~
                         "uczniowie wszystkich zbadanych szkół policealnych (ponadgimnazjalnych)",
                       grepl("Technikum", .data$SZK_typ, ignore.case = TRUE) ~
                         "uczniowie wszystkich zbadanych techników",
                       grepl("Zasadnicza", .data$SZK_typ, ignore.case = TRUE) ~
                         "uczniowie wszystkich zbadanych zasadniczych szkół zawodowowych"))

  if (!is.null(progAnonimizacji)) {
    wskazniki$grupy = anonimizuj_wskazniki(wskazniki$grupy, progAnonimizacji)
    wskazniki$grupyOdniesienia = anonimizuj_wskazniki(wskazniki$grupyOdniesienia,
                                                      progAnonimizacji)
  }

  return(wskazniki)
}
