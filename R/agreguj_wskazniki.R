#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych ze wskaźnikami z poziomu indywidualnego
#' (zwrócnej przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}).
#' @param wskazniki ramka danych ze wskaźnikami na poziomie idywidualnym
#' zwracana przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param grupy ramka danych zawierająca definicje podziałów na grupy (oraz
#' ewentualnie inne zmienne, które zostan dołączone do zwracanych zbiorów) -
#' zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}} lub
#' przygotowana samodzielnie; musi zawierać kolumny \emph{grupa}
#' i \emph{odniesienie}, które zawierają wyrażenia wybierające jednostki
#' obserwacji należące do danej grupy lub grupy odniesienia (mogą być one
#' zapisane \emph{stricte} w formie niezewaluowanych wyrażeń języka, ale także
#' formuł lub ciągów znaków)
#' @param ... wyrażenia postaci
#' \code{nazwa_wskaznika = funkcja_obliczajaca_wskaznik(.data, ew_inne_argumenty)}
#' @return lista dwóch ramek danych:
#' \itemize{
#'   \item{\code{grupy} - ramka danych zawierająca wskaźniki obliczone dla
#'         poszczególnych grup,}
#'   \item{\code{grupyOdniesienia} - ramka danych zawierająca wskaźniki
#'         obliczone dla odpowiadających im grup odniesienia.}
#' }
#' @seealso Funkcje, które wywołują \code{agreguj_wskazniki}:
#' \itemize{
#'   \item{\code{\link{agreguj_wskazniki_1rm}}.}
#' }
#' @export
#' @importFrom dplyr mutate_all
#' @importFrom rlang := !! enexprs
agreguj_wskazniki = function(wskazniki, grupy, ...) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy),
            "grupa" %in% names(grupy), "odniesienie" %in% names(grupy))
  funkcje = enexprs(...)
  # zanim zostanie odpalony potencjalnie czasochłonny proces obliczania
  # zagregowanych wskaźników sprawdźmy dla wszystkich grup, czy wyrażenia,
  # które je definiują, są poprawne
  problemyGrupa = sprawdz_definicje_grup(wskazniki, grupy$grupa)
  problemyOdniesienie = sprawdz_definicje_grup(wskazniki, grupy$odniesienie)
  if (!all(problemyGrupa %in% "") | !all(problemyOdniesienie %in% "")) {
    blad = "\n"
    if (!all(problemyGrupa %in% "")) {
      blad = paste0(blad,
                    "Wystąpiły problemy z definicjami grup, dla których mają zostać obliczone wskaźniki:\n\n",
                    pokaz_ramke_danych(data.frame(
                      wiersz = 1:nrow(grupy),
                      `definicja grupy` = as.character(grupy$grupa),
                      `błąd` = problemyGrupa,
                      check.names = FALSE)[!(problemyGrupa %in% ""), ]),
                    "\n\n")
    }
    if (!all(problemyOdniesienie %in% "")) {
      blad = paste0(blad,
                    "Wystąpiły problemy z definicjami grup odniesienia, dla których mają zostać obliczone wskaźniki:\n\n",
                    pokaz_ramke_danych(data.frame(
                      wiersz = 1:nrow(grupy),
                      `definicja grupy odniesienia` = as.character(grupy$odniesienie),
                      `błąd` = problemyOdniesienie,
                      check.names = FALSE)[!(problemyOdniesienie %in% ""), ]),
                    "\n\n")
    }
    stop(blad)
  }

  # tworzenie zmiennych do przechowywania obliczonych wskaźników
  for (z in names(funkcje)) {
    grupy = mutate(grupy, !!z := vector(mode = "list", length = nrow(grupy)))
  }
  odniesienia = grupy
  # sama agregacja
  for (i in 1:nrow(grupy)) {
    grupaEnv = new.env()
    assign(".data",
           wskazniki[eval(zwroc_wywolanie_grupy(grupy$grupa[i]), wskazniki), ],
           envir = grupaEnv)
    odniesienieEnv = new.env()
    assign(".data",
           wskazniki[eval(zwroc_wywolanie_grupy(grupy$odniesienie[i]), wskazniki), ],
           envir = odniesienieEnv)

    for (f in 1:length(funkcje)) {
      grupy[[names(funkcje)[f]]][[i]] = eval(funkcje[[f]], grupaEnv)
      odniesienia[[names(funkcje)[f]]][[i]] = eval(funkcje[[f]], odniesienieEnv)
    }
  }

  grupy = grupy %>%
    mutate_all(list(proste_wskazniki_na_wektor))
  odniesienia = odniesienia %>%
    mutate_all(list(proste_wskazniki_na_wektor))
  return(list(grupy = grupy,
              grupyOdniesienia = odniesienia))
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym - funkcje pomocnicze
#' @description Nieeksportowana funkcja używana w ramach
#' \code{\link{agreguj_wskazniki}} do weryfikacji, czy wyrażenia definiujące
#' grupy, dla których mają zostać obliczone (zagregowane) wskaźniki, dają się
#' poprawnie zewaluować (w kontekście ramki danych ze wskaźnikami z poziomu
#' indywidualnego).
#' @param wskazniki ramka danych ze wskaźnikami na poziomie idywidualnym
#' @param definicje wektor lub lista zawierająca definicje podziałów na grupy
#' @return wektor tekstowy o długości równej długości \code{definicje}:
#' zawiera komunikaty o błędach (jeśli wystąpiły) lub puste ciągi znaków (jeśli
#' definicja zewaluowała się bez problemu)
#' @seealso \code{\link{agreguj_wskazniki}}
sprawdz_definicje_grup = function(wskazniki, definicje) {
  problemy = rep("", length(definicje))
  for (i in 1:length(definicje)) {
    grupa = zwroc_wywolanie_grupy(definicje[i])
    if (!is.call(grupa)) {
      problemy[i] = "nie udało się skonwertować na 'wywołanie' ('call')"
      next
    }
    grupa = tryCatch(eval(grupa, wskazniki), error = function(e) {return(e)})
    if (inherits(grupa, "error")) {
      problemy[i] =  grupa$message
    }
    if (is.logical(grupa)) {
      if (!any(grupa)) {
        problemy[i] = "zdefiniowana grupa nie zawiera żadnych absolwentów"
      }
      if (length(grupa) != nrow(wskazniki)) {
        problemy[i] = "definicja grupy zwraca wektor logiczny, którego długość nie pasuje do liczby wierszy w zbiorze wskaźników"
      }
    } else {
      problemy[i] = "definicja grupy nie zwraca wektora logicznego"
    }
  }
  return(problemy)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym - funkcje pomocnicze
#' @description Nieeksportowana funkcja używana w ramach
#' \code{\link{agreguj_wskazniki}} i \code{\link{sprawdz_definicje_grup}} do
#' konwersji różnych form, w jakich mogą być zapisane wyrażenia definiujące
#' grupy na \emph{call}.
#' @param x wektor (lub lista) jednoelementowy
#' @return call
#' @seealso \code{\link{agreguj_wskazniki}},
#' \code{\link{sprawdz_definicje_grup}}
zwroc_wywolanie_grupy = function(x) {
  stopifnot(length(x) == 1)
  x = x[[1]]
  if (inherits(x, "formula")) {
    x = as.character(x[length(x)])
  }
  if (is.character(x)) {
    x = str2lang(x)
  }
  return(x)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym - funkcje pomocnicze
#' @description Nieeksportowana funkcja używana w ramach
#' \code{\link{agreguj_wskazniki}} do zamiany kolumn zawierających wskaźniki
#' które są (dla każdej grupy) pojedynczą liczbą z kolumn-list na wektory
#' @param x ramka danych (zawierająca wskaźniki na poziomie zagregowanym)
#' @return ramka danych
#' @seealso \code{\link{agreguj_wskazniki}}
proste_wskazniki_na_wektor = function(x) {
  if (all(sapply(x, mode) != "list")) {
    return(unlist(x))
  } else {
    return(x)
  }
}
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
