#' @title Anonimizacja wskaznikow na poziomie zagregowanym
#' @description Funkcja anonimizuje zestawienie wskaźników na poziomie
#' zagregowanym poprzez przypisanie wartości \code{NA} wszystkim elementom
#' każdego wskaźnika w danej grupie, dla którego wartość elementu \code{n} jest
#' mniejsza niż \code{prog}. Kolumny nie będące listami (oraz kolumny-listy,
#' których elementy nie są listami zawierającymi element o nazwie \code{n})
#' pozostaną w zwróconej ramce danych niezmienione.
#' @param wskazniki ramka danych ze wskaźnikami na poziomie zagregowanym
#' zwracana przez funkcję \code{\link{agreguj_wskazniki_1rm}}
#' @param prog liczba - minimalna liczba badanych niewymagająca anonimizacji
#' @return data frame
#' @export
anonimizuj_wskazniki = function(wskazniki, prog = 10) {
  stopifnot(is.data.frame(wskazniki),
            is.numeric(prog), length(prog) == 1)
  stopifnot(prog > 0)

  for (i in 1:ncol(wskazniki)) {
    if (is.list(wskazniki[[i]])) {
      wskazniki[[i]] = lapply(wskazniki[[i]], function(x, prog) {
        if (!is.list(x)) {
          return(x)
        } else if (!("n" %in% names(x))) {
          return(x)
        } else {
          if (x$n < prog) {
            x[!(names(x) %in% "n")] =
              lapply(x[!(names(x) %in% "n")], function(x) {return(NA)})
            return(x)
          }
          else {
            return(x)
          }
        }
      }, prog = prog)
    }
  }
  return(wskazniki)
}
