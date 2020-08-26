#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
liczba_zbadanych = function(x) {
  stopifnot(is.data.frame(x))
  return(nrow(x))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca nazwę i adres szkoły jako listę.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
dane_szkoly = function(x) {
  list(
    `nazwa szkoły` = unique(x$szk_nazwa),
    `adres szkoły` = unique(x$szk_adres)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych kobiet w
#' grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
liczba_kobiet = function(x) {
  return(sum(x$M1 %in% 1))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca informację o nazwie firmy realizującej
#' badanie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return ciąg znaków
#' @importFrom dplyr %>% .data case_when summarise
firma_badawcza = function(x) {
  x = x %>%
    summarise(Firma_nazwa = case_when(n_distinct(.data$Firma) > 1 ~ "ndt.",
                                      all(.data$Firma %in% 1) ~ "PBS sp. z o.o.",
                                      all(.data$Firma %in% 2) ~ "Danae sp. z o.o."))
  return(x$Firma_nazwa)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca listę form gramatycznych różnych słów lub
#' wyrażeń, które pojawiają się w raporcie w zależności od typu szkoły.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% .data case_when distinct mutate select
formy = function(x) {
  x = x %>%
    select("S1","M1", "woj_nazwa") %>%
    distinct() %>%
    mutate(byla_lo = case_when(.data$S1 %in% c(1, 3) ~ "była",
                               .data$S1 %in% 2 ~ "było"),
           woj_nazwa_dop = paste0(tolower(.data$woj_nazwa), "go"),
           szkola_lm = case_when(.data$S1 %in% 1 ~ "szkół branżowych pierwszego stopnia",
                                 .data$S1 %in% 2 ~ "techników",
                                 .data$S1 %in% 3 ~ "szkół policealnych"),
           wybrana_ne = case_when(.data$S1 %in% c(1, 3) ~ "zostałaby wybrana",
                                  .data$S1 %in% 2 ~ "zostałoby wybrane"),
           przygotowala_o = case_when(.data$S1 %in% c(1, 3) ~ "przygotowała",
                                      .data$S1 %in% 2 ~ "przygotowało"),
           jego_jej = case_when(.data$S1 %in% c(1, 3) ~ "jej",
                                .data$S1 %in% 2 ~ "jego"),
           edukacja_plany = case_when(.data$S1 %in% 1 ~ "w szkole branżowej drugiego stopnia, liceum dla dorosłych, na kwalifikacyjnym kursie zawodowym lub na innym kursie",
                                      .data$S1 %in% c(2, 3) ~ "w szkole policealnej, na studiach lub na kwalifikacyjnym kursie zawodowym lub na innym kursie"),
           bylaby_lo = case_when(.data$S1 %in% c(1, 3) ~ "byłaby",
                                 .data$S1 %in% 2 ~ "byłoby"),
           typ_szkoly = case_when(
             .data$S1 == 1 ~ "szkoła branżowa pierwszego stopnia",
             .data$S1 == 2 ~ "technikum",
             .data$S1 == 3 ~ "szkoła policealna"
           )) %>%
    select(-c("S1","M1", "woj_nazwa")) %>%
    distinct() %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja obliczająca liczbę absolwentów danego zawodu.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
zawod_liczebnosc = function(x) {
  n = list(n = sum(!is.na(x$S2_zawod)))

  tabela = x$S2_zawod %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    as.list()

  return(c(n, tabela))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów oceniających
#' przygotowanie do wykonywania zawodu jako "doskonałe" lub "dobre" oraz
#' podstawę procentowania. Wynik funkcji jest wsadem do tabeli 1 w raporcie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
zawod_przygotowanie_szkola = function(x) {
  x %>%
    select("S2_zawod", "W3") %>%
    group_by(S2_zawod) %>%
    summarise(przygotowanie = round(sum(W3 %in% c(4, 5)) / sum(W3 %in% (1:7)) * 100),
              respN = sum(W3 %in% (1:7))) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja obliczająca liczbę absolwentów uczestniczących w danych
#' formach PNZ.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
uczestnictwo_pnz = function(x) {
  list(
    etykieta = "Formy PNZ - wsad do tabeli 2.",
    `praktyki zawodowe u pracodawcy w Polsce` = sum(x$PNZ2_1 == 1),
    `praktyki zawodowe u pracodawcy za granicą` = sum(x$PNZ2_2 == 1),
    `zajęcia praktyczne u pracodawcy w Polsce` = sum(x$PNZ4_1 == 1),
    `zajęcia praktyczne u pracodawcy za granicą` = sum(x$PNZ4_2 == 1),
    `zajęcia praktyczne w szkole` = sum(x$PNZ4_3 == 1),
    `zajęcia praktyczne w CKP, CKZ, CKU lub w innej szkole` = sum(x$PNZ4_4 == 1)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie W1 "Czy
#' obecna szkoła jest jedną z tych, do których najbardziej chciał(a) się Pan(i)
#' dostać?"
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
szkola_1_wyboru = function(x) {
  list(etykieta = "Czy obecna szkoła jest jedną z tych, do których najbardziej chciał się dostać?",
       n = sum(x$W1 %in% c(1:3, 7)),
       `Tak, to szkoła pierwszego wyboru` = sum(x$W1 %in% 1),
       `Tak, wolał(a)bym inną` = sum(x$W1 %in% 2),
       `Nie miał(am)em wyboru` = sum(x$W1 %in% 3),
       `Trudno powiedzieć` = sum(x$W1 %in% 7)) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie W2 "Gdyby
#' teraz mógł (mogła) Pani/Pan wybrać szkołę, to czy ponownie wybrał(a)by P. tę
#' samą szkołę, w której obecnie się P. uczy?" oraz odsetek sum odpowiedzi
#' "Raczej tak" i "Na pewno tak".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
ponowny_wybor = function(x) {
  list(etykieta = "[%] Czy wybrałby ponownie szkołę?",
       n = sum(x$W2 %in% (1:7)),
       `Na pewno nie` = sum(x$W2 %in% 1) / sum(x$W2 %in% (1:7)),
       `Raczej nie` = sum(x$W2 %in% 2) / sum(x$W2 %in% (1:7)),
       `Raczej tak` = sum(x$W2 %in% 4) / sum(x$W2 %in% (1:7)),
       `Na pewno tak` = sum(x$W2 %in% 5) / sum(x$W2 %in% (1:7)),
       `Trudno powiedzieć` = sum(x$W2 %in% 7) / sum(x$W2 %in% (1:7)),
       `Raczej tak + Na pewno tak` = sum(x$W2 %in% c(4, 5)) / sum(x$W2 %in% (1:7))
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie W3 "Jak P.
#' uważa, na ile szkoła przygotowała Panią/Pana do wykonywania zawodu, którego
#' się P. w niej uczy?" oraz sumy odsetków odpowiedzi: "Przygotowała doskonale"
#' i "Przygotowała dobrze" oraz "Nie przygotowała w ogóle" i "Przygotowała
#' słabo".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przygotowanie_do_zawodu = function(x) {
  list(etykieta = "[%] Na ile szkoła przygotowała do wykonywania zawodu?",
       n = sum(x$W3 %in% (1:7)),
       `Nie przygotowała w ogóle` = sum(x$W3 %in% 1) / sum(x$W3 %in% (1:7)),
       `Przygotowała słabo` = sum(x$W3 %in% 2) / sum(x$W3 %in% (1:7)),
       `Przygotowała średnio` = sum(x$W3 %in% 3) / sum(x$W3 %in% (1:7)),
       `Przygotowała dobrze` = sum(x$W3 %in% 4) / sum(x$W3 %in% (1:7)),
       `Przygotowała doskonale` = sum(x$W3 %in% 5) / sum(x$W3 %in% (1:7)),
       `Trudno powiedzieć` = sum(x$W3 %in% 7) / sum(x$W3 %in% (1:7)),
       `Nie przygotowała w ogóle + słabo` = sum(x$W3 %in% c(1, 2)) / sum(x$W3 %in% (1:7)),
       `Przygotowała doskonale + dobrze` = sum(x$W3 %in% c(4, 5)) / sum(x$W3 %in% (1:7))
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ3 "Jak
#' Pan(i) ocenia, czy praktyki przygotowały Panią/Pana do wykonywania pracy w
#' zawodzie, którego się P. uczył(a)? - U pracodawcy w Polsce" oraz sumę
#' odsetków wskazań odpowiedzi "Przygotowały bardzo dobrze" i "Przygotowały
#' dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zawodu_prakt_PL = function(x) {
  list(etykieta = "[%] Praktyki u pracodawcy w Polsce",
       n = sum(x$PNZ3_1 %in% (1:5)),
       `Nie przygotowały w ogóle` = round(sum(x$PNZ3_1 %in% 1) / sum(x$PNZ3_1 %in% (1:5)) * 100),
       `Przygotowały słabo` = round(sum(x$PNZ3_1 %in% 2) / sum(x$PNZ3_1 %in% (1:5)) * 100),
       `Przygotowały średnio` = round(sum(x$PNZ3_1 %in% 3) / sum(x$PNZ3_1 %in% (1:5)) * 100),
       `Przygotowały dobrze` = round(sum(x$PNZ3_1 %in% 4) / sum(x$PNZ3_1 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze` = round(sum(x$PNZ3_1 %in% 5) / sum(x$PNZ3_1 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze oraz dobrze` = round(sum(x$PNZ3_1 %in% c(4, 5)) / sum(x$PNZ3_1 %in% (1:5)) * 100)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ3 "Jak
#' Pan(i) ocenia, czy praktyki przygotowały Panią/Pana do wykonywania pracy w
#' zawodzie, którego się P. uczył(a)? - U pracodawcy za granicą" oraz sumę
#' odsetków wskazań odpowiedzi
#' "Przygotowały bardzo dobrze" i "Przygotowały dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zawodu_prakt_niePL = function(x) {
  list(etykieta = "[%] Praktyki u pracodawcy za granicą",
       n = sum(x$PNZ3_2 %in% (1:5)),
       `Nie przygotowały w ogóle` = round(sum(x$PNZ3_2 %in% 1) / sum(x$PNZ3_2 %in% (1:5)) * 100),
       `Przygotowały słabo` = round(sum(x$PNZ3_2 %in% 2) / sum(x$PNZ3_2 %in% (1:5)) * 100),
       `Przygotowały średnio` = round(sum(x$PNZ3_2 %in% 3) / sum(x$PNZ3_2 %in% (1:5)) * 100),
       `Przygotowały dobrze` = round(sum(x$PNZ3_2 %in% 4) / sum(x$PNZ3_2 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze` = round(sum(x$PNZ3_2 %in% 5) / sum(x$PNZ3_2 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze oraz dobrze` = round(sum(x$PNZ3_2 %in% c(4, 5)) / sum(x$PNZ3_2 %in% (1:5)) * 100)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca odsetek absolwentów, których praktyki
#' przygotowały "Przygotowały bardzo dobrze" i "Przygotowały dobrze" (pytanie
#' PNZ3 "Jak Pan(i) ocenia, czy praktyki przygotowały Panią/Pana do wykonywania
#' pracy w zawodzie, którego się P. uczył(a)?").
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zaw_prakt_ANY = function(x) {
  list(etykieta = "PNZ3_1 lub PNZ3_2 kody doskonale i dobrze (4 i 5)",
       n = sum(x$PNZ3_1 %in% (1:5) | x$PNZ3_2 %in% (1:5)),
       `Przygotowały bardzo dobrze oraz dobrze` = round(
         sum(x$PNZ3_1 %in% c(4, 5) | x$PNZ3_2 %in% c(4, 5)) /
           sum(x$PNZ3_1 %in% 1:5 | x$PNZ3_2 %in% 1:5) * 100
         )
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ5 "Jak P.
#' ocenia, czy zajęcia praktyczne przygotowały P. do wykonywania pracy w
#' zawodzie, którego się Pani/Pan uczył(a)? - U pracodawcy w Polsce" oraz sumę
#' odsetków wskazań odpowiedzi"Przygotowały bardzo dobrze" i "Przygotowały
#' dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zawodu_zaj_PL = function(x) {
  list(etykieta = "[%] Zajęcia u pracodawcy w Polsce",
       n = sum(x$PNZ5_1 %in% (1:5)),
       `Nie przygotowały w ogóle` = round(sum(x$PNZ5_1 %in% 1) / sum(x$PNZ5_1 %in% (1:5)) * 100),
       `Przygotowały słabo` = round(sum(x$PNZ5_1 %in% 2) / sum(x$PNZ5_1 %in% (1:5)) * 100),
       `Przygotowały średnio` = round(sum(x$PNZ5_1 %in% 3) / sum(x$PNZ5_1 %in% (1:5)) * 100),
       `Przygotowały dobrze` = round(sum(x$PNZ5_1 %in% 4) / sum(x$PNZ5_1 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze` = round(sum(x$PNZ5_1 %in% 5) / sum(x$PNZ5_1 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze oraz dobrze` = round(sum(x$PNZ5_1 %in% c(4, 5)) / sum(x$PNZ5_1 %in% (1:5)) * 100)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ5 "Jak P.
#' ocenia, czy zajęcia praktyczne przygotowały P. do wykonywania pracy w
#' zawodzie, którego się Pani/Pan uczył(a)? - U pracodawcy za granicą" oraz sumę
#' odsetków wskazań odpowiedzi"Przygotowały bardzo dobrze" i "Przygotowały
#' dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zawodu_zaj_niePL = function(x) {
  list(etykieta = "[%] Zajęcia u pracodawcy za granicą",
       n = sum(x$PNZ5_2 %in% (1:5)),
       `Nie przygotowały w ogóle` = round(sum(x$PNZ5_2 %in% 1) / sum(x$PNZ5_2 %in% (1:5)) * 100),
       `Przygotowały słabo` = round(sum(x$PNZ5_2 %in% 2) / sum(x$PNZ5_2 %in% (1:5)) * 100),
       `Przygotowały średnio` = round(sum(x$PNZ5_2 %in% 3) / sum(x$PNZ5_2 %in% (1:5)) * 100),
       `Przygotowały dobrze` = round(sum(x$PNZ5_2 %in% 4) / sum(x$PNZ5_2 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze` = round(sum(x$PNZ5_2 %in% 5) / sum(x$PNZ5_2 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze oraz dobrze` = round(sum(x$PNZ5_2 %in% c(4, 5)) / sum(x$PNZ5_2 %in% (1:5)) * 100)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ5 "Jak P.
#' ocenia, czy zajęcia praktyczne przygotowały P. do wykonywania pracy w
#' zawodzie, którego się Pani/Pan uczył(a)? - w szkole" oraz sumę odsetków
#' wskazań odpowiedzi"Przygotowały bardzo dobrze" i "Przygotowały dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zawodu_zaj_szkola = function(x) {
  list(etykieta = "[%] Zajęcia w szkole",
       n = sum(x$PNZ5_3 %in% (1:5)),
       `Nie przygotowały w ogóle` = round(sum(x$PNZ5_3 %in% 1) / sum(x$PNZ5_3 %in% (1:5)) * 100),
       `Przygotowały słabo` = round(sum(x$PNZ5_3 %in% 2) / sum(x$PNZ5_3 %in% (1:5)) * 100),
       `Przygotowały średnio` = round(sum(x$PNZ5_3 %in% 3) / sum(x$PNZ5_3 %in% (1:5)) * 100),
       `Przygotowały dobrze` = round(sum(x$PNZ5_3 %in% 4) / sum(x$PNZ5_3 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze` = round(sum(x$PNZ5_3 %in% 5) / sum(x$PNZ5_3 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze oraz dobrze` = round(sum(x$PNZ5_3 %in% c(4, 5)) / sum(x$PNZ5_3 %in% (1:5)) * 100)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ5 "Jak P.
#' ocenia, czy zajęcia praktyczne przygotowały P. do wykonywania pracy w
#' zawodzie, którego się Pani/Pan uczył(a)? - W Centrum Kształcenia
#' Praktycznego/Zawodowego" oraz sumę odsetków wskazań odpowiedzi"Przygotowały
#' bardzo dobrze" i "Przygotowały dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zawodu_zaj_ckp = function(x) {
  list(etykieta = "[%] Zajęcia w CKP / CKZ",
       n = sum(x$PNZ5_4 %in% (1:5)),
       `Nie przygotowały w ogóle` = round(sum(x$PNZ5_4 %in% 1) / sum(x$PNZ5_4 %in% (1:5)) * 100),
       `Przygotowały słabo` = round(sum(x$PNZ5_4 %in% 2) / sum(x$PNZ5_4 %in% (1:5)) * 100),
       `Przygotowały średnio` = round(sum(x$PNZ5_4 %in% 3) / sum(x$PNZ5_4 %in% (1:5)) * 100),
       `Przygotowały dobrze` = round(sum(x$PNZ5_4 %in% 4) / sum(x$PNZ5_4 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze` = round(sum(x$PNZ5_4 %in% 5) / sum(x$PNZ5_4 %in% (1:5)) * 100),
       `Przygotowały bardzo dobrze oraz dobrze` = round(sum(x$PNZ5_4 %in% c(4, 5)) / sum(x$PNZ5_4 %in% (1:5)) * 100)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca odsetek absolwentów, których zajęcia
#' praktyczne przygotowały "Przygotowały bardzo dobrze" i "Przygotowały dobrze"
#' (pytanie PNZ5 Jak P. ocenia, czy zajęcia praktyczne przygotowały P. do
#' wykonywania pracy w zawodzie, którego się Pani/Pan uczył(a)?").
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
przyg_zaw_zaj_ANY = function(x) {
  list(etykieta = "PNZ5_1 - PNZ5_4 kody doskonale i dobrze (4 i 5)",
       n = sum(x$PNZ5_1 %in% (1:5) |
                 x$PNZ5_2 %in% (1:5) |
                 x$PNZ5_3 %in% (1:5) |
                 x$PNZ5_4 %in% (1:5)),
       `Przygotowały bardzo dobrze oraz dobrze` = round(
         sum(x$PNZ5_1 %in% c(4, 5) |
               x$PNZ5_2 %in% c(4, 5) |
               x$PNZ5_3 %in% c(4, 5) |
               x$PNZ5_4 %in% c(4, 5)) /
           sum(x$PNZ5_1 %in% (1:5) |
                 x$PNZ5_2 %in% (1:5) |
                 x$PNZ5_3 %in% (1:5) |
                 x$PNZ5_4 %in% (1:5)) * 100
       )
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie PNZ9 "Jak
#' Pani/Pan ocenia, czy nauka u tego pracodawcy przygotowała P. do wykonywania
#' pracy w zawodzie, którego się Pan(i) uczył(a)?" oraz sumę odsetków
#' odpowiedzi: "Przygotowała doskonale" i "Przygotowała dobrze".
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
nauka_zawod = function(x) {
  list(etykieta = "[%] Ocena nauki w kontekście wykonywanego zawodu",
       n = sum(x$PNZ9 %in% (1:5)),
       `Nie przygotowała w ogóle` = sum(x$PNZ9 %in% 1) / sum(x$PNZ9 %in% (1:5)),
       `Przygotowała słabo` = sum(x$PNZ9 %in% 2) / sum(x$PNZ9 %in% (1:5)),
       `Przygotowała średnio` = sum(x$PNZ9 %in% 3) / sum(x$PNZ9 %in% (1:5)),
       `Przygotowała dobrze` = sum(x$PNZ9 %in% 4) / sum(x$PNZ9 %in% (1:5)),
       `Przygotowała doskonale` = sum(x$PNZ9 %in% 5) / sum(x$PNZ9 %in% (1:5)),
       `Przygotowała doskonale + dobrze` = sum(x$PNZ9 %in% c(4, 5)) / sum(x$PNZ9 %in% (1:5))
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca informację o zadowoleniu z praktycznej
#' nauki zawodu (PNZ). Definicja wskaźnika: % PNZ3, PNZ5, PNZ7, PNZ9 - co
#' najmniej jedna odpowiedź “przygotowała dobrze” lub “przygotowała bardzo
#' dobrze”
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
ocena_pnz = function(x) {
  list(etykieta = "[%] Min. 1 fomra PNZ przygotowała dobrze lub b. dobrze",
       n = sum(x$PNZ3_1 %in% 1:5 |
                 x$PNZ3_2 %in% 1:5 |
                 x$PNZ5_1 %in% 1:5 |
                 x$PNZ5_2 %in% 1:5 |
                 x$PNZ5_3 %in% 1:5 |
                 x$PNZ5_4 %in% 1:5 |
                 x$PNZ7_1 %in% 1:5 |
                 x$PNZ7_2 %in% 1:5 |
                 x$PNZ7_3 %in% 1:5 |
                 x$PNZ7_4 %in% 1:5 |
                 x$PNZ9 %in% 1:5),
       `Dobrze lub b dobrze` = (sum(x$PNZ3_1 %in% 4:5 |
                                     x$PNZ3_2 %in% 4:5 |
                                     x$PNZ5_1 %in% 4:5 |
                                     x$PNZ5_2 %in% 4:5 |
                                     x$PNZ5_3 %in% 4:5 |
                                     x$PNZ5_4 %in% 4:5 |
                                     x$PNZ7_1 %in% 4:5 |
                                     x$PNZ7_2 %in% 4:5 |
                                     x$PNZ7_3 %in% 4:5 |
                                     x$PNZ7_4 %in% 4:5 |
                                     x$PNZ9 %in% 4:5) /
         sum(x$PNZ3_1 %in% 1:5 |
               x$PNZ3_2 %in% 1:5 |
               x$PNZ5_1 %in% 1:5 |
               x$PNZ5_2 %in% 1:5 |
               x$PNZ5_3 %in% 1:5 |
               x$PNZ5_4 %in% 1:5 |
               x$PNZ7_1 %in% 1:5 |
               x$PNZ7_2 %in% 1:5 |
               x$PNZ7_3 %in% 1:5 |
               x$PNZ7_4 %in% 1:5 |
               x$PNZ9 %in% 1:5) * 100) %>% round()
       )
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca sumę wskazań odpowiedzi "Raczej tak" oraz
#' "Zdecydowanie tak" w pytaniu PL2 "Czy w ciągu 6 miesięcy po zakończeniu nauki
#' w obecnej szkole planuje Pani/Pan rozpocząć naukę" dla 1., 2. i 4. pozycji w
#' kafeterii.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @param zmienna zmienna znajdująca się w ramce danych \code{x}. Funkcja służy
#' do wyliczania wskaźników na zmiennych \code{PL2_1}, \code{PL2_2} oraz
#' \code{PL2_4}.
#' @param opis opis zwykle odnoszący się do argumentu przekazanego w
#' \code{zmienna}
#' @return lista
#' @importFrom dplyr %>%
#' @importFrom rlang ensym
plany_6m = function(x, zmienna, opis) {
  zmienna = ensym(zmienna)
  list(etykieta = opis,
       n = sum(x[[zmienna]] %in% c(1:4, 7)),
      `Zdecydowanie tak + Raczej tak` = round(sum(x[[zmienna]] %in% c(3, 4)) / sum(x[[zmienna]] %in% c(1:4, 7)) * 100)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja obliczająca procent absolwentów, którzy w ciągu 6
#' miesięcy od ukończenia szkoły planują rozpocząć dalszą naukę.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czy_plany_eduk = function(x) {
  list(etykieta = "[%] Plany edukacyjne - 6 m. od ukończenia szkoły",
       n = sum(x$PL2_1 %in% c(1:4, 7) |
                 x$PL2_2 %in% c(1:4, 7) |
                 x$PL2_3 %in% c(1:4, 7) |
                 x$PL2_4 %in% c(1:4, 7) |
                 x$PL2_5 %in% c(1:4, 7) |
                 x$PL2_6 %in% c(1:4, 7)),
       `% PL2_1-6 min. 1 odp. kod 3 lub 4` =
         (sum(x$PL2_1 %in% c(3, 4) |
                x$PL2_2 %in% c(3, 4) |
                x$PL2_3 %in% c(3, 4) |
                x$PL2_4 %in% c(3, 4) |
                x$PL2_5 %in% c(3, 4) |
                x$PL2_6 %in% c(3, 4)
         ) / sum(x$PL2_1 %in% c(1:4, 7) |
                   x$PL2_2 %in% c(1:4, 7) |
                   x$PL2_3 %in% c(1:4, 7) |
                   x$PL2_4 %in% c(1:4, 7) |
                   x$PL2_5 %in% c(1:4, 7) |
                   x$PL2_6 %in% c(1:4, 7)
         ) * 100) %>% round()
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja obliczająca procent absolwentów, którzy albo pracują w
#' wyuczonym zawodzie albo planują w ciągu najbliższych 12 miesięcy rozpocząć
#' pracę w wyuczonym zawodzie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
plany_eduk_tak = function(x) {
  list(etykieta = "[%] W ciągu N12M zamierza podjąć lub podjął pracę w wyuczonym zawodzie",
       n = sum(x$PL4 != 97 | x$PL6 != 97),
       `% PL4 = 1 lub PL6 = 1` = (sum(x$PL4 == 1 | x$PL6 == 1) /
                                    sum(x$PL4 != 97 | x$PL6 != 97) * 100) %>%
         round()
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja obliczająca procent absolwentów, którzy albo nie pracują
#' w wyuczonym zawodzie albo nie planują w ciągu najbliższych 12 miesięcy
#' rozpoczynać pracy w wyuczonym zawodzie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
plany_eduk_nie = function(x) {
  list(etykieta = "[%] Nie pracuje w wyuczonym zawodzi i w ciągu N12M nie zamierza podjąć pracy w wyuczonym zawodzie",
       n = sum(x$PL2_1 %in% c(1:4, 7) |
                 x$PL2_2 %in% c(1:4, 7) |
                 x$PL2_3 %in% c(1:4, 7) |
                 x$PL2_4 %in% c(1:4, 7) |
                 x$PL2_5 %in% c(1:4, 7) |
                 x$PL2_6 %in% c(1:4, 7)),
       `% PL2_1-6 brak odp. 3 | 4 & PL3=2 & PL6=2` =
         (sum((!(x$PL2_1 %in% c(3, 4)) |
                 !(x$PL2_2 %in% c(3, 4)) |
                 !(x$PL2_3 %in% c(3, 4)) |
                 !(x$PL2_4 %in% c(3, 4)) |
                 !(x$PL2_5 %in% c(3, 4)) |
                 !(x$PL2_6 %in% c(3, 4))) & x$PL3 == 2 & x$PL6 == 2
         ) / sum(x$PL2_1 %in% c(1:4, 7) |
                   x$PL2_2 %in% c(1:4, 7) |
                   x$PL2_3 %in% c(1:4, 7) |
                   x$PL2_4 %in% c(1:4, 7) |
                   x$PL2_5 %in% c(1:4, 7) |
                   x$PL2_6 %in% c(1:4, 7)
         ) * 100) %>% round()
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca liczebności absolwentów, którzy pracowali
#' zawodowo w momencie badania, w tym pracujących w wyuczonym zawodzie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarobkowa = function(x) {
  list(
    etykieta = "Pracujący zarobkowo",
    `Uczniowie pracujący zarobkowo w momencie badania` = sum(x$PL3 == 1),
    `w tym: pracujący w wyuczonym zawodzie` = sum(x$PL3 == 1 & x$PL4 == 1)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca liczebności absolwentów, którzy pracują
#' poza wyuczonym zawodem, w tym zamierzących podjąć pracę w wyuczonym zawodzie
#' w ciągu następnych 12 miesięcy.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_poza_wyuczonym = function(x) {
  list(
    etykieta = "Pracujący poza wyuczonym zawodem",
    `Uczniowie pracujący poza zawodem wyuczonym` = sum(x$PL3 == 1 & x$PL4 == 2),
    `w tym zamierzający podjąć pracę w zawodzie wyuczonym w ciągu N12M` =
      sum(x$PL3 == 1 & x$PL4 == 2 & x$PL6 == 1)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca liczebności absolwentów, którzy nie
#' pracowali w momencie badania, w tym zamierzających podjąć pracę w ciągu
#' następnych 12 miesięcy, w tym planujących podjęcie pracy w wyuczonym
#' zawodzie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
brak_pracy = function(x) {
  list(
    etykieta = "Nie pracujący zarobkowo",
    `Uczniowie nie pracujący w momencie badania` = sum(x$PL3 == 2),
    `w tym: zamierzający podjąć pracę w ciągu N12M` = sum(x$PL5 == 1),
    `w tym: planujący pracę w wyuczonym zawodzie` =
      sum(x$PL3 == 2 & x$PL5 == 1 & x$PL6 == 1)
  ) %>% return()
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja przechowująca liczebności absolwentów będących
#' młodocianymi pracownikami, w tym zamierzjącymi podjąć pracę oraz
#' przechowująca liczbę młodocianych pracowników planujących pracę w wyuczonym
#' zawodzie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
mlodociani_praca = function(x) {
  list(
    etykieta = "Młodociani pracownicy",
    `Uczniowie będący młodocianymi pracownikami` = sum(x$PNZ8 == 1),
    `w tym zamierzający podjąć pracę` = sum(x$PL9 == 1),
    `Młodociani planujący pracę w wyuczonym zawodzie` = sum(x$PL10 == 1)
  ) %>% return()
}
