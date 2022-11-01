#' @title Obliczanie wskaznikow na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych ze wskaźnikami z poziomu indywidualnego
#' (zwrócnej przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}).
#' @param wskazniki ramka danych ze wskaźnikami na poziomie idywidualnym
#' zwracana przez funkcję \code{\link{oblicz_wskazniki_ind_1rm}}
#' @param grupy ramka danych zawierająca definicje podziałów na grupy (oraz
#' ewentualnie inne zmienne, które zostaną dołączone do zwracanych zbiorów) -
#' zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}} lub
#' przygotowana samodzielnie; musi zawierać kolumny \emph{grupa}
#' i \emph{odniesienie}, które zawierają wyrażenia wybierające jednostki
#' obserwacji należące do danej grupy lub grupy odniesienia (mogą być one
#' zapisane \emph{stricte} w formie niezewaluowanych wyrażeń języka, ale także
#' formuł lub ciągów znaków)
#' @param ... wyrażenia postaci
#' \code{nazwa_wskaznika = funkcja_obliczajaca_wskaznik(.data, ew_inne_argumenty)}
#' @param wielowatkowo ciąg znaków - czy obliczenia powinny zostać wykonane
#' wielowątkowo, a jeśli tak, to przy pomocy jakiego podejścia
#' @param uzywajPakietow wektor ciągów znaków z nazwami pakietów, które musza
#' zostac załadowane, aby móc wykonać wyrażenia podane w {...} (z wyjątkiem
#' ładowanych domyślnie do sesji R oraz \emph{MLASZdane}, który zostanie
#' załadowany zawsze) - potrzebne tylko przy używaniu wielowątkowości
#' w systemach operacyjnych Windows
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
#' @importFrom utils installed.packages
#' @importFrom dplyr bind_cols bind_rows mutate_all
#' @importFrom rlang := !! enexprs
#' @importFrom parallel clusterEvalQ clusterExport clusterMap mcMap stopCluster
#' @importFrom parallelly availableCores makeClusterPSOCK supportsMulticore
agreguj_wskazniki = function(wskazniki, grupy, ...,
                             wielowatkowo = c("brak", "socket", "fork"),
                             uzywajPakietow = vector(mode = "character",
                                                     length = 0)) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy),
            "grupa" %in% names(grupy), "odniesienie" %in% names(grupy),
            is.character(uzywajPakietow))
  wielowatkowo = match.arg(wielowatkowo)
  if (length(uzywajPakietow) > 0 && wielowatkowo %in% c("brak", "fork")) {
    warning("Argument 'uzywajPakietow' zostanie zignorowany, gdyż argument 'wielowatkowo' ma wartość 'brak' lub 'fork'.")
  } else if (length(uzywajPakietow) > 0) {
    stopifnot(all(uzywajPakietow %in% installed.packages()[, "Package"]))
  }
  if (wielowatkowo == "fork" && !supportsMulticore()) {
    warning("System operacyjny nie umożliwia tworzenia 'forków' procesów, do wielowątkowości zostanie wykorzystane podejście 'socket'.")
    wielowatkowo = "socket"
  }
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

  if (wielowatkowo == "fork") {
    odniesienia = bind_cols(
      grupy,
      bind_rows(mcMap(oblicz_wskazniki_w_forku, grupy$grupa,
                      MoreArgs = list(.data = wskazniki, funkcje = funkcje),
                      mc.cores = availableCores() - 1)))
    grupy = bind_cols(
      grupy,
      bind_rows(mcMap(oblicz_wskazniki_w_forku, grupy$odniesienie,
                      MoreArgs = list(.data = wskazniki, funkcje = funkcje),
                      mc.cores = availableCores() - 1)))
  } else if (wielowatkowo == "socket") {
    # wydzielanie danych odpowiednich dla każdej grupy
    wskaznikiG = wskaznikiO = vector(mode = "list", length = nrow(grupy))
    for (i in 1:nrow(grupy)) {
      wskaznikiG[[i]] = wskazniki[eval(zwroc_wywolanie_grupy(grupy$grupa[i]), wskazniki), ]
      wskaznikiO[[i]] = wskazniki[eval(zwroc_wywolanie_grupy(grupy$odniesienie[i]), wskazniki), ]
    }
    # obliczenia na klastrze
    cl = makeClusterPSOCK(availableCores() - 1, autoStop = TRUE)
    uzywajPakietow = union("MLASZdane", uzywajPakietow)
    clusterExport(cl, varlist = "uzywajPakietow", envir = environment())
    clusterEvalQ(cl,
                 for (p in uzywajPakietow) {library(p, character.only = TRUE)})
    odniesienia = bind_cols(
      grupy,
      bind_rows(clusterMap(cl, oblicz_wskazniki_w_watku, wskaznikiO,
                           MoreArgs = list(funkcje = funkcje))))
    grupy = bind_cols(
      grupy,
      bind_rows(clusterMap(cl, oblicz_wskazniki_w_watku, wskaznikiG,
                           MoreArgs = list(funkcje = funkcje))))
    stopCluster(cl)
  } else {
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
#' \code{\link{agreguj_wskazniki}} do obliczania wsartości wskaźników, kiedy
#' wykorzystywana jest wielowątkowość w systemach innych niż Windows (tj.,
#' kiedy możliwe jest tworzenie \emph{forków} procesów)
#' @param grupa wyrażenie zawierające definicję grupy (lub grupy odniesienia)
#' @param .data ramka danych (zawierająca wskaźniki na poziomie indywidualnym)
#' @param funkcje lista wyrażeń przekazanych w wywołaniu
#' \code{\link{agreguj_wskazniki}} poprzez \code{...}
#' @return ramka danych
#' @seealso \code{\link{agreguj_wskazniki}}
oblicz_wskazniki_w_forku = function(grupa, .data, funkcje) {
  .data = .data[eval(zwroc_wywolanie_grupy(grupa), .data), ]
  wyniki = rep(vector(mode = "list", length = 1),
               length(funkcje))
  names(wyniki) = names(funkcje)
  for (f in 1:length(funkcje)) {
    wyniki[[names(funkcje)[f]]][[1]] = eval(funkcje[[f]])
  }
  return(wyniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym - funkcje pomocnicze
#' @description Nieeksportowana funkcja używana w ramach
#' \code{\link{agreguj_wskazniki}} do obliczania wsartości wskaźników, kiedy
#' wykorzystywana jest wielowątkowość w systemach Windows (tj. z użyciem
#' \emph{socket connections})
#' @param .data ramka danych (zawierająca wskaźniki na poziomie indywidualnym)
#' @param funkcje lista wyrażeń przekazanych w wywołaniu
#' \code{\link{agreguj_wskazniki}} poprzez \code{...}
#' @return ramka danych
#' @seealso \code{\link{agreguj_wskazniki}}
oblicz_wskazniki_w_watku = function(.data, funkcje) {
  wyniki = rep(vector(mode = "list", length = 1),
               length(funkcje))
  names(wyniki) = names(funkcje)
  for (f in 1:length(funkcje)) {
    wyniki[[names(funkcje)[f]]][[1]] = eval(funkcje[[f]])
  }
  return(wyniki)
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
