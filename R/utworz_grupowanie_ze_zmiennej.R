#' @title Obliczanie wskaznikow na poziomie zagregowanym - definiowanie grupowania
#' @description Funkcja pozwala przygotować ramkę danych opisującą podział na
#' grupy, dla których mają następnie zostać obliczone wskaźniki zagregowane
#' przy pomocy funkcji \code{\link{agreguj_wskazniki}}, oraz odpowiadające im
#' grupy odniesienia w prostej, aczy typowej sytuacji, kiedy taki podział
#' definiują różne wartości jednej zmiennej w zbiorze (np. identyfikator szkoły),
#' a grupa odniesienia daje się określić przez taką samą (jak w analizowanej
#' grupie) wartość innej zmiennej (np. typ szkoły). Dodatkowy argument pozwala
#' określić czy obserwacje należące do analizowanej grupy powinny zostać,
#' wykluczone, czy też włączone do grupy odniesienia.
#' @param x ramka danych (zwykle zbiór wskaźników indywidualnych)
#' @param zmGrupujaca nazwa zmiennej, której wartości definiują podział na
#' grupy, podana jako wyrażenie lub ciąg znaków
#' @param zmGrupaOdniesienia nazwa zmiennej, której wartości definiują grupę
#' odniesienia, podana jako wyrażenie lub ciąg znaków
#' @param wykluczGrupeZGrupyOdniesienia wartość logiczna - czy obserwacje
#' z analizowanej grupy powinny zostać wykluczone z grupy odniesienia
#' @param ... opcjonalnie specyfikacja dodatkowych kolumn z ramki danych
#' podanej argumentem \code{x}, które mają zostać dołączone do zwracanej ramki
#' danych (podane w dowolny sposób akceptowany przez funkcję
#' \code{\link[dplyr]{select}})
#' @return ramka danych, która może zostać użyta jako argument \code{grupy}
#' w wywołaniu funkcji \code{\link{agreguj_wskazniki}}.
#' @export
#' @importFrom dplyr %>% .data count distinct mutate select
utworz_grupowanie_ze_zmiennej = function(x, zmGrupujaca, zmGrupaOdniesienia,
                                         wykluczGrupeZGrupyOdniesienia = TRUE,
                                         ...) {
  stopifnot(is.data.frame(x),
            is.logical(wykluczGrupeZGrupyOdniesienia),
            length(wykluczGrupeZGrupyOdniesienia) == 1)
  stopifnot(wykluczGrupeZGrupyOdniesienia %in% c(TRUE, FALSE))
  zmGrupujaca = ensym(zmGrupujaca)
  zmGrupaOdniesienia = ensym(zmGrupaOdniesienia)
  stopifnot(as.character(zmGrupujaca) %in% names(x),
            as.character(zmGrupaOdniesienia) %in% names(x))

  x = x %>%
    select(!!zmGrupujaca, !!zmGrupaOdniesienia, ...) %>%
    distinct()
  powtorzenia = x %>%
    count(!!zmGrupujaca) %>%
    filter(n > 1) %>%
    select(!!zmGrupujaca)
  if (nrow(powtorzenia) > 0) {
    blad = paste("W ramach niektórych grup występują różne wartości zmiennnej, która powinna definiować grupę odniesienia lub innych zmiennych, które mają zostać dołączone do zwracanej ramki danych. Uniemożliwia to przygotowanie definicji grup odniesienia. Grupy, w których wystąpił problem:\n\n",
                 pokaz_ramke_danych(powtorzenia), "\n\n")
    stop(blad)
  }
  x = x %>%
    mutate(grupa = paste0(as.character(zmGrupujaca), " %in% ",
                          ifelse(is.character(!!zmGrupujaca), '"', ''),
                          !!zmGrupujaca,
                          ifelse(is.character(!!zmGrupujaca), '"', '')),
           odniesienie = paste0(as.character(zmGrupaOdniesienia), " %in% ",
                                ifelse(is.character(!!zmGrupaOdniesienia), '"', ''),
                                !!zmGrupaOdniesienia,
                                ifelse(is.character(!!zmGrupaOdniesienia), '"', '')))
  if (wykluczGrupeZGrupyOdniesienia) {
    x = x %>%
      mutate(odniesienie = paste0("(", .data$odniesienie, ") & !(",
                                  as.character(zmGrupujaca), " %in% ",
                                  ifelse(is.character(!!zmGrupujaca), '"', ''),
                                  !!zmGrupujaca,
                                  ifelse(is.character(!!zmGrupujaca), '"', ''),
                                  ")"))
  }
  return(x)
}
