#' @title Przeksztalca zbior wksaznikow z poziomu zagregowanego na plaska tabele
#' @description
#' Funkcja przekształca zbiór danych ze wskaźnikami na poziomie zagregowanym,
#' w którym poszczególne wskaźniki mają postać kolumn-list na płaską ramkę
#' danych, w której każdy wskaźnik (który miał złożoną strukturę) reprezentowany
#' jest przez kilka różnych kolumn.
#' @param wskazniki ramka danych ze wskaźnikami na poziomie zagregowanym
#' @details Większość pracy wykonuje pod spodem funkcja
#' \code{\link{przygotuj_wskaznik_do_splaszczenia}}. Następnie kolumny
#' konwertowane są na wektory typu "labelled", aby można im było przypisać
#' etykiety w bardzie opisowy sposób (niż poprawna leksykalnie nazwa zmiennej)
#' zdawały sprawę ze znaczenia zmiennych i aby etykiety takie bezproblemowo
#' zapisywały się przy eksporcie spłaszczonego zbioru danych do plików SPSS lub
#' Staty.
#' @return ramka danych
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr %>% .data bind_cols do group_by select ungroup
#' @importFrom haven labelled
splaszcz_wskazniki_zagregowane = function(wskazniki) {
  stopifnot(is.data.frame(wskazniki))

  wynik = select(wskazniki)
  for (i in names(wskazniki)) {
    # właściwie należałoby zawartośc tej pętli przepisać tak, aby wykorzystać
    # funkcję `rowwise()` z dplyra po ostatniej dużej zmianie API tego pakietu
    temp = wskazniki %>%
      select(i) %>%
      mutate(`___tmpGrpVar___` = 1:nrow(wskazniki)) %>%
      group_by(.data$`___tmpGrpVar___`)
    temp = suppressWarnings(
      temp %>%
        do(przygotuj_wskaznik_do_splaszczenia(.data[[i]]))) %>%
      ungroup() %>%
      unnest(cols = c()) %>%
      select(-"___tmpGrpVar___")
    for (j in 1:ncol(temp)) {
      if (!is.character(temp[[j]])) {
        temp[[j]] = as.numeric(temp[[j]])
      }
      temp[[j]] = labelled(temp[[j]],
                           setNames(vector(mode = mode(temp[[j]]), length = 0),
                                    vector(mode = "character", length = 0)),
                           ifelse(names(temp)[j] == "temp",
                                  i,
                                  enc2utf8(paste0(i, ": ", names(temp)[j]))))
      names(temp)[j] = ifelse(names(temp)[j] == "temp",
                              i,
                              paste0(i, "_",
                                     ifelse(names(temp)[j] == "n",
                                            "n",
                                            paste0("zm", j - 1))))
    }
    wynik = bind_cols(wynik, temp)
  }
  return(wynik)
}
#' @title Przeksztalca zbior wksaznikow z poziomu zagregowanego na plaska tabele
#' @description
#' Funkcja przekształca kolumnę-listę ramki danych w ramkę danych z wieloma
#' kolumnami będącymi zwykłymi wektorami (o liczbie wierszy równej długości
#' przekazanej listy).
#' @param x lista (zwykle: lista list o tej samej strukturze)
#' @return ramka danych
#' @export
#' @importFrom dplyr %>%
przygotuj_wskaznik_do_splaszczenia = function(x) {
  x = lapply(x, function(x) {
    if (!is.list(x)) {
      return(list(temp = x))
    } else {
      return(lapply(x, function(x) {
        if (is.null(x)) {
          return(NA_character_)
        }
        if (length(x) == 0) {
          y = NA
          mode(y) = mode(x)
          return(y)
        } else if (length(x) > 1) {
          return(paste(x, collapse = ", "))
        } else {
          return(x)
        }}))
    }
  }) %>%
    as.data.frame(stringsAsFactors = FALSE,
                  check.names = FALSE)
  # brutalny hack na wskaźniki opisujące "tło", tj. wartość podobnego
  # wskaźnika z BDL GUS dla JST
  names(x) = sub("^(.* w )(gminie|powiecie|województwie|) .*$", "\\1\\2", names(x))
  return(x)
}
