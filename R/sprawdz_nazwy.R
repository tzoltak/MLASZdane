#' @title Kontrola kompletnosci zbioru wskaznikow
#' @description Funkcja sprawdza, czy w podanym zestawie nazw występują
#' wszystkie wymagane nazwy.
#' @param nazwy wektor tekstowy - nazwy, których kompletność ma być sprawdzona
#' @param wzorzec wektor tekstowy - nazwy, z których każda powinna wystąpić
#' w \code{nazwy}
#' @export
sprawdz_nazwy = function(nazwy, wzorzec) {
  stopifnot(is.character(nazwy),
            is.character(wzorzec))
  powtorzone = nazwy[duplicated(nazwy)]
  if (length(powtorzone) > 0) {
    stop(paste0("W zbiorze danych następujące nazwy zmiennych są zduplikowane:\n- '",
                paste(powtorzone, collapse = "',\n- '"),
                "'."))
  }
  brakujace = setdiff(wzorzec, nazwy)
  if (length(brakujace) > 0) {
    stop(paste0("W zbiorze danych brakuje zmiennych:\n- '",
                paste(brakujace, collapse = "',\n- '"),
                "'."))
  }
  invisible(NULL)
}
