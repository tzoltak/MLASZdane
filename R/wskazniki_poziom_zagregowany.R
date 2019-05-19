#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return liczba
liczba_zbadanych = function(x) {
  stopifnot(is.data.frame(x))
  return(nrow(x))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return liczba
#' @seealso \code{\link{wskazniki_nie_z_epizodow}}
liczba_kobiet = function(x) {
  return(sum(x$UCZ_plec %in% "K"))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę szkół w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return liczba
liczba_szkol = function(x) {
  return(length(unique(x$SZK_kod)))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący wszystkie zawody, w których
#' uczyli się uczniowie w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return ciąg znaków - nazwy zawodów, oddzielone przecinkami, ułożone
#' w kolejności od najczęściej do najrzadziej wystepującego
zawody = function(x) {
  x$UCZ_zawod %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    names() %>%
    paste(collapse = ", ") %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład statusu
#' edukacyjno-zawodowego w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "praca_nauka\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{`n`} - liczba wszystkich badanych w grupie,}
#'   \item{\code{`tylko pracują`} - liczba [0, n],}
#'   \item{\code{`pracują i uczą się`} - liczba [0, n],}
#'   \item{\code{`tylko się uczą`} - liczba [0, n],}
#'   \item{\code{`nie pracują i nie uczą się`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_nauka_miesiac}}
#' @importFrom dplyr %>% one_of select
praca_nauka = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("praca_nauka", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = nrow(x),
       `tylko pracują` = sum(x$praca_nauka %in% "tylko praca"),
       `pracują i uczą się` = sum(x$praca_nauka %in% "praca i edukacja"),
       `tylko się uczą` = sum(x$praca_nauka %in% "tylko edukacja"),
       `nie pracują i nie uczą się` = sum(!(x$praca_nauka %in% c("tylko praca",
                                                                 "praca i edukacja",
                                                                 "tylko edukacja")))) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący średnią zdawalność egzaminu
#' zawodowego w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{`n`} - liczba badanych uwzględnionych w obliczeniach (tj. nie
#'         mających braku danych w zmiennej \code{egz_zaw_zdany}),}
#'   \item{\code{`zdawalność`} - liczba [0, 1].}
#' }
#' @seealso \code{\link{wskazniki_nie_z_epizodow}}
egz_zaw_zdawalnosc = function(x) {
  list(n = sum(!is.na(x$egz_zaw_zdany)),
       `zdawalność` = mean(x$egz_zaw_zdany, na.rm = TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący średnią zdawalność matury
#' w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{`n`} - liczba badanych uwzględnionych w obliczeniach (tj. nie
#'         mających braku danych w zmiennej \code{matura_zdana}),}
#'   \item{\code{`zdawalność`} - liczba [0, 1].}
#' }
#' @seealso \code{\link{wskazniki_nie_z_epizodow}}
matura_zdawalnosc = function(x) {
  list(n = sum(!is.na(x$matura_zdana)),
       `zdawalność` = mean(x$matura_zdana, na.rm = TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów w grupie,
#' którzy pracowali przed ukończeniem (badanej) szkoły.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{`n`} - liczba badanych w grupie,}
#'   \item{\code{`praca przed ukończeniem szkoły`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_pierwsza}}
#' @importFrom dplyr %>% one_of select
praca_przed_ukonczeniem_szkoly = function(x) {
  x = x %>%
    select(one_of("praca_przed_ukonczeniem_szkoly_pierwsza"))
  list(n = nrow(x),
       `praca przed ukończeniem szkoły` =
         sum(x$praca_przed_ukonczeniem_szkoly_pierwsza %in% TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący parametry rozkładu czasu
#' od momentu planowego ukończenia szkoły do podjęcia pierwszej pracy
#' (wśród badanych, którzy nie pracowali przed ukończeniem szkoły).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{`n`} - liczba badanych uwzględnionych w obliczeniach (tj. nie
#'         mających braku danych w zmiennej \code{praca_czas_rozp_pierwsza}),}
#'   \item{\code{`średnia`} - liczba [0, ),}
#'   \item{\code{`mediana`} - liczba [0, ),}
#'   \item{\code{`1.kwartyl`} - liczba [0, )}
#'   \item{\code{`3.kwartyl`} - liczba [0, ).}
#' }
#' @seealso \code{\link{praca_pierwsza}}
#' @importFrom stats median quantile
#' @importFrom dplyr %>% one_of select
praca_czas_rozp = function(x) {
  x = x %>%
    select(one_of("praca_czas_rozp_pierwsza"))
  list(n = sum(!is.na(x$praca_czas_rozp_pierwsza)),
       `średnia` = mean(x$praca_czas_rozp_pierwsza, na.rm = TRUE),
       `mediana` = median(x$praca_czas_rozp_pierwsza, na.rm = TRUE),
       `1.kwartyl` = quantile(x$praca_czas_rozp_pierwsza, 0.25, na.rm = TRUE),
       `3.kwartyl` = quantile(x$praca_czas_rozp_pierwsza, 0.75, na.rm = TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład formy zatrudnienia
#' w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazw zmiennych, na podstawie których ma
#' zostać obliczony wskaźnik (użyte zostaną zmienne o nazwach:
#' "pg2h([123456789]|10).\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.
#'         pracujących, z wyłączeniem pracujących bez umowy),}
#'   \item{\code{`umowa o pracę na czas nieokreślony`} - liczba [0, n],}
#'   \item{\code{`umowa o pracę na czas określony`} - liczba [0, n],}
#'   \item{\code{`umowa zlecenia/o dzieło`} - liczba [0, n],}
#'   \item{\code{`samozatrudnienie`} - liczba [0, n],}
#'   \item{\code{`rolnicy indywidualni`} - liczba [0, n],}
#'   \item{\code{`przez agencję pracy tymczasowej`} - liczba [0, n],}
#'   \item{\code{`staż lub praktyka`} - liczba [0, n],}
#'   \item{\code{`inna (lub nieznana)`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_miesiac}}, \code{\link{praca_pierwsza}},
#' \code{\link{praca_ostatnia}}, \code{\link{praca_forma2}}
#' @importFrom dplyr %>% one_of select
praca_forma = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("pg2gh.", 1:9, sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(!is.na(x$pg2gh.1)),
       `umowa o pracę na czas nieokreślony` = sum(x$pg2gh.2 %in% TRUE),
       `umowa o pracę na czas określony` = sum(x$pg2gh.1 %in% TRUE),
       `umowa zlecenia/o dzieło` = sum(x$pg2gh.4 %in% TRUE),
       `samozatrudnienie` = sum(x$pg2gh.5 %in% TRUE | x$pg2gh.6 %in% TRUE),
       `rolnicy indywidualni` = sum(x$pg2gh.7 %in% TRUE),
       `przez agencję pracy tymczasowej` = sum(x$pg2gh.3 %in% TRUE),
       `staż lub praktyka` = sum(x$pg2gh.8 %in% TRUE),
       `inna (lub nieznana)` = sum(x$pg2gh.9 %in% TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład formy zatrudnienia
#' w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazw zmiennych, na podstawie których ma
#' zostać obliczony wskaźnik (użyte zostaną zmienne o nazwach:
#' "pg2h([123456789]|10).\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.
#'         pracujących, z wyłączeniem pracujących bez umowy),}
#'   \item{\code{`umowa o pracę na czas nieokreślony`} - liczba [0, n],}
#'   \item{\code{`umowa o pracę na czas określony`} - liczba [0, n],}
#'   \item{\code{`umowa zlecenia/o dzieło`} - liczba [0, n],}
#'   \item{\code{`inna (lub nieznana)`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_miesiac}}, \code{\link{praca_pierwsza}},
#' \code{\link{praca_ostatnia}}, \code{\link{praca_forma}}
#' @importFrom dplyr %>% one_of select
praca_forma2 = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("pg2gh.", 1:9, sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(!is.na(x$pg2gh.1)),
       `umowa o pracę na czas nieokreślony` = sum(x$pg2gh.2 %in% TRUE),
       `umowa o pracę na czas określony` = sum(x$pg2gh.1 %in% TRUE),
       `umowa zlecenia/o dzieło` = sum(x$pg2gh.4 %in% TRUE),
       `inna (lub nieznana)` = sum(x$pg2gh.3 %in% TRUE | x$pg2gh.5 %in% TRUE |
                                     x$pg2gh.6 %in% TRUE | x$pg2gh.7 %in% TRUE |
                                     x$pg2gh.8 %in% TRUE | x$pg2gh.9 %in% TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład miejsca zamieszkania
#' (w kraju czy za granicą) badanych, którzy pracowali.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazw zmiennych, na podstawie których ma
#' zostać obliczony wskaźnik (użyte zostaną zmienne o nazwach:
#' "pg2i[123].\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.
#'         pracujących, z wyłączeniem pracujących bez umowy),}
#'   \item{\code{`w Polsce`} - liczba [0, n],}
#'   \item{\code{`za granicą`} - liczba [0, n],}
#'   \item{\code{`nieznane`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_miesiac}}, \code{\link{praca_pierwsza}},
#' \code{\link{praca_ostatnia}}
#' @importFrom dplyr %>% one_of select
praca_zamieszkanie = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("pg2i.", c(1:3, 9), sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(x$pg2i.1 %in% c(FALSE, TRUE)),
       `w Polsce` = sum(x$pg2i.1 %in% TRUE | x$pg2i.2 %in% TRUE),
       `za granicą` = sum(x$pg2i.3 %in% TRUE),
       `nieznane` = sum(x$pg2i.9 %in% TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład subiektywnej oceny
#' zgodności pierwszej lub ostatniej wykonywanej pracy z kierunkiem kształcenia.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazw zmiennych, na podstawie których ma
#' zostać obliczony wskaźnik (użyte zostaną zmienne o nazwach:
#' "pg2i[123].\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.
#'         pracujących, z wyłączeniem pracujących bez umowy),}
#'   \item{\code{`zgodna`} - liczba [0, n],}
#'   \item{\code{`niezgodna, ale wymaga podobnych kwalifikacji`} - liczba [0, n],}
#'   \item{\code{`niezgodna, wymaga innych kwalifikacji`} - liczba [0, n],}
#'   \item{\code{`praca, w której wykształcenie nie ma znaczenia`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_pierwsza}}, \code{\link{praca_ostatnia}}
#' @importFrom dplyr %>% one_of select
praca_zgodna_z_wyksztalceniem = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("pio1", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(x$pio1 %in% (1:4)),
       `zgodna` = sum(x$pio1 %in% 1),
       `niezgodna, ale wymaga podobnych kwalifikacji` = sum(x$pio1 %in% 2),
       `niezgodna, wymaga innych kwalifikacji` = sum(x$pio1 %in% 3),
       `praca, w której wykształcenie nie ma znaczenia` = sum(x$pio1 %in% 4)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład zarobków w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "pio4\code{sufiks}")
#' @param sufiksTlo opcjonalnie ciąg znaków - sufiks nazwy zmiennej
#' opisującej średnie zarobki w powiecie (użyta zostanie zmienna o nazwie:
#' "powiat_sr_wynagrodzenia\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "po5\code{sufiks}"),}
#'   \item{\code{`średnia`} - liczba [0, ),}
#'   \item{\code{`mediana`} - liczba [0, ),}
#'   \item{\code{`1.kwartyl`} - liczba [0, ),}
#'   \item{\code{`3.kwartyl`} - liczba [0, ),}
#'   \item{\code{`tlo`} - liczba [0, ).}
#' }
#' @seealso \code{\link{praca_pierwsza}}, \code{\link{praca_ostatnia}}
#' @importFrom stats median quantile
#' @importFrom dplyr %>% one_of select
praca_zarobki = function(x, sufiks, sufiksTlo = NULL) {
  if (!is.null(sufiksTlo)) {
    tlo = x %>%
      select(one_of(paste0("powiat_sr_wynagrodzenia", sufiksTlo), "SZK_powiat")) %>%
      distinct()
    if (nrow(tlo) > 1) {
      sufiksTlo = NULL
    } else {
      # poniżej przyjmuję zgrubne założenie, że przemnożenie przez 0.71 pozwala przejść z brutto do netto
      powiat = tlo$SZK_powiat
      tlo = list((0.71 * mean(tlo[[paste0("powiat_sr_wynagrodzenia", sufiksTlo)]],
                              na.rm = TRUE)) %>%
                   round(2))
      names(tlo) = paste("Przybliżone średnie zarobki netto w",
                         sub("powiat", "powiecie", powiat))
    }
  }
  x = x %>%
    select(one_of(paste0("pio4", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  x = list(n = sum(!is.na(x$pio4)),
           `średnia` = mean(x$pio4, na.rm = TRUE),
           `mediana` = median(x$pio4, na.rm = TRUE),
           `1.kwartyl` = quantile(x$pio4, 0.25, na.rm = TRUE),
           `3.kwartyl` = quantile(x$pio4, 0.75, na.rm = TRUE))
  if (!is.null(sufiksTlo)) {
    x = c(x, tlo)
  }
  return(x)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład zadowolenia
#' z ostatniej pracy w analizowanym okresie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "po5\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "po5\code{sufiks}"),}
#'   \item{\code{`niedostateczny`} - liczba [0, n],}
#'   \item{\code{`dopuszczający`} - liczba [0, n],}
#'   \item{\code{`dostateczny`} - liczba [0, n],}
#'   \item{\code{`dobry`} - liczba [0, n],}
#'   \item{\code{`bardzo dobry`} - liczba [0, n],}
#'   \item{\code{`celujący`} - liczba [0, n].}
#' }
#' @seealso \code{\link{praca_pierwsza}}, \code{\link{praca_ostatnia}}
#' @importFrom dplyr %>% one_of select
praca_spelnienie_oczekiwan = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("po5", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(x$po5 %in% (1:6)),
       `niedostateczny` = sum(x$po5 %in% 1),
       `dopuszczający` = sum(x$po5 %in% 2),
       `dostateczny` = sum(x$po5 %in% 3),
       `dobry` = sum(x$po5 %in% 4),
       `bardzo dobry` = sum(x$po5 %in% 5),
       `celujący` = sum(x$po5 %in% 6)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zarejestrowanych
#' bezrobotnych w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "bezrobocie\code{sufiks}")
#' @param sufiksTlo opcjonalnie ciąg znaków - sufiks nazwy zmiennej
#' opisującej stopę bezrobocia w powiecie (użyta zostanie zmienna o nazwie:
#' "powiat_bezrobocie\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "bezrobocie\code{sufiks}"),}
#'   \item{\code{`zarejestrowani bezrobotni`} - liczba [0, n),}
#'   \item{\code{`pracujący lub kontynuujący naukę`} - liczba [0, n).}
#' }
#' @seealso \code{\link{bezrobocie_miesiac}}
#' @importFrom dplyr %>% distinct one_of select
bezrobocie = function(x, sufiks, sufiksTlo = sufiks) {
  if (!is.null(sufiksTlo)) {
    tlo = x %>%
      select(one_of(paste0("powiat_bezrobocie", sufiks), "SZK_powiat")) %>%
      distinct()
    names(tlo) = sub(paste0(sufiks, "$"), "", names(tlo))
    if (nrow(tlo) == 1 & "SZK_powiat" %in% names(tlo)) {
      powiat = tlo$SZK_powiat
      tlo = list(tlo$powiat_bezrobocie)
      names(tlo) = paste("stopa bezrobocia w",
                         sub("powiat", "powiecie", powiat))
    } else {
      sufiksTlo = NULL
    }
  }
  x = x %>%
    select(one_of(paste0("bezrobocie", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  x = list(n = sum(!is.na(x$bezrobocie)),
           `zarejestrowani bezrobotni` = sum(x$bezrobocie %in% TRUE),
           `pracujący lub kontynuujący naukę` = sum(x$bezrobocie %in% FALSE))
  if (!is.null(sufiksTlo)) {
    x = c(x, tlo)
  }
  return(x)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład wskaźnika z poziomu
#' indywidualnego w ramach grupy - zwykle opisującego czas pracy lub bezrobocia
#' w badanym okresie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param zmienna ciąg znaków - nazwa zmiennej, na podstawie której ma być
#' obliczony wskaźnik
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej opisanej argumentem \code{zmienna}),}
#'   \item{\code{`średnia`} - liczba [0, ),}
#'   \item{\code{`mediana`} - liczba [0, ),}
#'   \item{\code{`1.kwartyl`} - liczba [0, ),}
#'   \item{\code{`3.kwartyl`} - liczba [0, ).}
#' }
#' @seealso \code{\link{praca_czas}}, \code{\link{bezrobocie_czas}}
#' @importFrom stats median quantile
#' @importFrom dplyr %>% one_of select
czas_agregacja = function(x, zmienna) {
  x = x %>%
    select(one_of(zmienna))
  names(x) = "czas"
  list(n = sum(!is.na(x$czas)),
       `średnia` = mean(x$czas, na.rm = TRUE),
       `mediana` = median(x$czas, na.rm = TRUE),
       `1.kwartyl` = quantile(x$czas, 0.25, na.rm = TRUE),
       `3.kwartyl` = quantile(x$czas, 0.75, na.rm = TRUE)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład wskaźnika z poziomu
#' indywidualnego w ramach grupy - zwykle opisującego czas pracy lub bezrobocia
#' w badanym okresie. Wartości wskaźnika z poziomu indywidualnego przekształcane
#' są na zmienną porządkową poprzez pocięcie na przedziały zdefiniowane przez
#' argument \code{punktyPodzialu}, a następnie dla każdej z wartości takiej
#' zmiennej porządkowej zwracana jest liczba jej wystąpień w ramach grupy.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param zmienna ciąg znaków - nazwa zmiennej, na podstawie której ma być
#' obliczony wskaźnik
#' @param punktyPodzialu wektor liczb - punkty wyznaczające granice przedziałów,
#' w ramach których zliczane będą wystąpienia badanych
#' @param etykiety wektor ciągów znaków - etykiety przedziałów wyznaczonych
#' przez \code{granicePrzedzialow}
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej opisanej argumentem \code{zmienna}),}
#'   \item{\code{`etykieta[1]`} - liczba [0, n),}
#'   \item{...}
#' }
#' @seealso \code{\link{praca_czas}}, \code{\link{bezrobocie_czas}}
#' @importFrom dplyr %>% one_of select
czas_rozklad = function(x, zmienna, punktyPodzialu, etykiety) {
  x = x %>%
    select(one_of(zmienna))
  names(x) = "czas"
  list(n = sum(!is.na(x$czas)),
       cut(x$czas, punktyPodzialu) %>%
         table() %>%
         as.list() %>%
         setNames(etykiety)) %>%
    unlist(recursive = FALSE) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący rozkład formy dalszej
#' edukacji w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "nauka\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "nauka\code{sufiks}"),}
#'   \item{\code{`studia stacjonarne`} - liczba [0, n],}
#'   \item{\code{`studia niestacjonarne`} - liczba [0, n],}
#'   \item{\code{`szkoła policealna`} - liczba [0, n],}
#'   \item{\code{`LO dla dorosłych`} - liczba [0, n].}
#' }
#' @seealso \code{\link{nauka_miesiac}}
#' @importFrom dplyr %>% one_of select
nauka = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("nauka", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(!is.na(x$nauka)),
       `studia stacjonarne` = sum(x$nauka %in% "studia stacjonarne"),
       `studia niestacjonarne` = sum(x$nauka %in% "studia niestacjonarne"),
       `szkoła policealna` = sum(x$nauka %in% "szkoła policealna"),
       `LO dla dorosłych` = sum(x$nauka %in% "LO dla dorosłych")) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący kierunki i uczelnie, na
#' których kontuowali kształcenie badani z grupy.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "studia_(kierunek|uczelnia)\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "studia_uczelnia\code{sufiks}"),}
#'   \item{\code{`najczęściej wybierane kierunki`} - ciąg znaków - nazwy
#'         kierunków, oddzielone przecinkami, ułożone w kolejności od
#'         najczęściej do najrzadziej wystepujących, z pominięciem tych,
#'         których częstość występowania była mniejsza niż 20\%,}
#'   \item{\code{`najczęściej wybierane uczelnie`} - ciąg znaków - nazwy
#'         uczelni, oddzielone przecinkami, ułożone w kolejności od
#'         najczęściej do najrzadziej wystepujących, z pominięciem tych,
#'         których częstość występowania była mniejsza niż 20\%.}
#' }
#' @seealso \code{\link{studia_pierwsze}}
#' @importFrom dplyr %>% one_of select
studia_gdzie = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("studia_", c("kierunek", "uczelnia"), sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  x = list(n = sum(!is.na(x$studia_uczelnia)),
           `najczęściej wybierane kierunki` = x$studia_kierunek %>%
             table() %>%
             prop.table() %>%
             sort(decreasing = TRUE),
           `najczęściej wybierane uczelnie` = x$studia_uczelnia %>%
             table() %>%
             prop.table() %>%
             sort(decreasing = TRUE))
  x[-1] = lapply(x[-1], function(x) {return(x[x > 0.2])})
  x$`najczęściej wybierane kierunki` = names(x$`najczęściej wybierane kierunki`)
  x$`najczęściej wybierane uczelnie` = names(x$`najczęściej wybierane uczelnie`) %>%
    toupper()
  return(x)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczby badanych z grupy,
#' którzy kontynuowali naukę na studiach płatnych i bezpłatnych.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "studia_bezplatne\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "studia_bezplatne\code{sufiks}"),}
#'   \item{\code{`studiujący na studiach bezpłatnych`} - liczba [0, n],}
#'   \item{\code{`studiujący na studiach płatnych`} - liczba [0, n].}
#' }
#' @seealso \code{\link{studia_pierwsze}}
#' @importFrom dplyr %>% one_of select
studia_odplatnosc = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("studia_bezplatne", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(!is.na(x$studia_bezplatne)),
       `studiujący na studiach bezpłatnych` = sum(x$studia_bezplatne %in% 1),
       `studiujący na studiach płatnych` = sum(x$studia_bezplatne %in% 0)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczby badanych z grupy,
#' którzy kontynuowali naukę na studiach w poszczególnych trybach kształcenia.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
#' grupa wyróżniona w ramach obiektu zwracanego przez funkcję
#' \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param sufiks ciąg znaków - sufiks nazwy zmiennej, na podstawie której ma
#' zostać obliczony wskaźnik (użyta zostanie zmienna o nazwie:
#' "studia_tryb\code{sufiks}")
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
#'         mających braku danych w zmiennej "studia_tryb\code{sufiks}"),}
#'   \item{\code{`studiujący na studiach dziennych`} - liczba [0, n],}
#'   \item{\code{`studiujący na studiach wieczorowych`} - liczba [0, n],}
#'   \item{\code{`studiujący na studiach zaocznych`} - liczba [0, n].}
#' }
#' @seealso \code{\link{studia_pierwsze}}
#' @importFrom dplyr %>% one_of select
studia_tryb = function(x, sufiks) {
  x = x %>%
    select(one_of(paste0("studia_tryb", sufiks)))
  names(x) = sub(paste0(sufiks, "$"), "", names(x))
  list(n = sum(!is.na(x$studia_tryb)),
       `studiujący na studiach dziennych` = sum(x$studia_tryb %in% 1),
       `studiujący na studiach wieczorowych` = sum(x$studia_tryb %in% 2),
       `studiujący na studiach zaocznych` = sum(x$studia_tryb %in% 3)) %>%
    return()
}
