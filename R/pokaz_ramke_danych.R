#' @title Funkcje pomocnicze
#' @description Nieeksportowana funkcja zwracająca zawartość ramki danych
#' w formie ciągu znaków, który można wydrykować przy pomocy np. funkcji
#' \code{\link[base]{message}}, zamiast używać \code{print}
#' @param x ramka danych
#' @param sep ciąg znaków - separator poziomy pól
#' @return ciąg znaków
#' @seealso \code{\link{agreguj_wskazniki}}
pokaz_ramke_danych = function(x, sep = " ") {
  stopifnot(is.data.frame(x),
            is.character(sep), length(sep) == 1)
  x = rbind(names(x),
            sapply(x, as.character))
  x = apply(x, 2, format)
  x = apply(x, 1, paste, collapse = sep)
  x = paste(x, collapse = "\n")
  return(x)
}
