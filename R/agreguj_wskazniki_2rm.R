#' @title Obliczanie wskaznikow z 2 rundy monitoringu na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych z wynikami ankiety CAWI.
#' @param wskazniki ramka danych z wynikami 2 rundy monitoringu
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'   \item{\code{\link{liczba_zbadanych}},}
#'   \item{\code{\link{dane_szkoly}},}
#'   \item{\code{\link{liczba_kobiet_2rm}},}
#'   \item{\code{\link{firma_badawcza}},}
#'   \item{\code{\link{formy}},}
#'   \item{\code{\link{zawod_liczebnosc}},}
#'   \item{\code{\link{zawod_przygotowanie_szkola}},}
#'   \item{\code{\link{uczestnictwo_pnz}},}
#'   \item{\code{\link{szkola_1_wyboru}},}
#'   \item{\code{\link{ponowny_wybor}},}
#'   \item{\code{\link{przygotowanie_do_zawodu}},}
#'   \item{\code{\link{przyg_zawodu_prakt_PL}},}
#'   \item{\code{\link{przyg_zawodu_prakt_niePL}},}
#'   \item{\code{\link{przyg_zaw_prakt_ANY}},}
#'   \item{\code{\link{przyg_zawodu_zaj_PL}},}
#'   \item{\code{\link{przyg_zawodu_zaj_niePL}},}
#'   \item{\code{\link{przyg_zawodu_zaj_szkola}},}
#'   \item{\code{\link{przyg_zawodu_zaj_ckp}},}
#'   \item{\code{\link{przyg_zaw_zaj_ANY}},}
#'   \item{\code{\link{nauka_zawod}},}
#'   \item{\code{\link{plany_6m}},}
#'   \item{\code{\link{czy_plany_eduk}},}
#'   \item{\code{\link{plany_eduk_tak}},}
#'   \item{\code{\link{plany_eduk_nie}},}
#'   \item{\code{\link{praca_zarobkowa}},}
#'   \item{\code{\link{praca_poza_wyuczonym}},}
#'   \item{\code{\link{brak_pracy}},}
#'   \item{\code{\link{mlodociani_praca}},}
#' }
#' @export
#' @importFrom dplyr .data
agreguj_cawi_ucz_2rm = function(wskazniki, grupy) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy))
  nazwy = c("ID_rspo", "szk_nazwa", "szk_adres", "M1", "Firma", "S1",
            "woj_nazwa", "S2_zawod", "W1", "W2", "W3", "PNZ3_1", "PNZ3_2",
            "PNZ5_1", "PNZ5_2", "PNZ5_3", "PNZ5_4", "PNZ9", "PL2_1", "PL2_2",
            "PL2_3", "PL2_4", "PL2_5", "PL2_6", "PL3", "PL4","PL5", "PL6",
            "PL9", "PL10", "PNZ8")
  sprawdz_nazwy(names(wskazniki), nazwy)

  wskazniki = agreguj_wskazniki(
    wskazniki, grupy,
    l_zbadanych = liczba_zbadanych(.data),
    dane_szkoly = dane_szkoly(.data),
    l_kobiet = liczba_kobiet_2rm(.data),
    firma = firma_badawcza(.data),
    formy_gramatyczne = formy(.data),
    l_zawod = zawod_liczebnosc(.data),
    l_zawod_przyg = zawod_przygotowanie_szkola(.data),
    uczestnictwo_pnz = uczestnictwo_pnz(.data),
    szk_1_wyb = szkola_1_wyboru(.data),
    ponowny_wybor = ponowny_wybor(.data),
    przyg_do_zaw = przygotowanie_do_zawodu(.data),
    przyg_zawodu_prakt_PL = przyg_zawodu_prakt_PL(.data),
    przyg_zawodu_prakt_niePL = przyg_zawodu_prakt_niePL(.data),
    przyg_zaw_prakt_ANY = przyg_zaw_prakt_ANY(.data),
    przyg_zawodu_zaj_PL = przyg_zawodu_zaj_PL(.data),
    przyg_zawodu_zaj_niePL = przyg_zawodu_zaj_niePL(.data),
    przyg_zawodu_zaj_szkola = przyg_zawodu_zaj_szkola(.data),
    przyg_zawodu_zaj_ckp = przyg_zawodu_zaj_ckp(.data),
    przyg_zaw_zaj_ANY = przyg_zaw_zaj_ANY(.data),
    nauka_zawod = nauka_zawod(.data),
    ocena_pnz = ocena_pnz(.data),
    plany_6m_bs1 = plany_6m(.data, "PL2_1",
                            "[%] W szkole branżowej drugiego stopnia"),
    plany_6m_licd = plany_6m(.data, "PL2_2", "[%] W liceum dla dorosłych"),
    plany_6m_stud = plany_6m(.data, "PL2_4", "[%] Na studiach"),
    czy_plany_edu = czy_plany_eduk(.data),
    plany_edu_tak = plany_eduk_tak(.data),
    plany_edu_nie = plany_eduk_nie(.data),
    praca_zarobkowa = praca_zarobkowa(.data),
    praca_poza_wyuczonym = praca_poza_wyuczonym(.data),
    brak_pracy = brak_pracy(.data),
    mlodociani_praca = mlodociani_praca(.data)
  )
  return(wskazniki)
}
