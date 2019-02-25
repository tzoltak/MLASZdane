#' @title Tworzenie zbioru osobo-miesiecy
#' @description
#' Funkcja odpowiada za rozstrzyganie konfliktów pomiędzy statusami
#' nauki/pracy/bezrobocia w poszczególnych miesiącach wynikającymi z deklaracji
#' respondentów. Jest wykorzystywana wewnątrz wywołań funkcji
#' \code{\link{przygotuj_zbior_osobo_miesiecy_pilrm}}
#' i \code{\link{przygotuj_zbior_osobo_miesiecy_1rm}}.
#' @param x ramka danych obemująca opis statusów \strong{jednego} badanego
#' @param zmiennaID ciąg znaków - nazwa zmiennej przechowującej unikalne ID badanego
#' @param kodyPracyNaCzarno wektor liczbowy - wartości zmiennej \code{praca},
#' które opisują pracę na czarno (czyli takie, które nie wchodzą w formalny
#' konflikt z posiadaniem statusu zarejestrwoanego bezrobotnego)
#' @return ramka danych ze skorygowanymi statusami i dopisanymi kolumnami
#' opisującymi, jakie zmiany zostały dokonane
#' @importFrom dplyr .data case_when mutate right_join select
koryguj_statusy = function(x, zmiennaID,
                           kodyPracyNaCzarno = vector(mode = "numeric", length = 0)) {
  stopifnot(is.character(zmiennaID), length(zmiennaID) == 1,
            is.numeric(kodyPracyNaCzarno))
  stopifnot(zmiennaID %in% names(x),
            zmiennaID == "ID_RESP" | !("ID_RESP" %in% names(x)))
  names(x)[names(x) == zmiennaID] = "ID_RESP"

  x = suppressMessages(
    x %>%
      right_join(data.frame(czas = min(x$czas):max(x$czas))) %>%
      mutate(praca_a_bezrobocie =
               case_when(is.na(.data$praca) | is.na(.data$bezrobocie) ~ "ndt.",
                         (.data$praca %in% kodyPracyNaCzarno) &
                           !is.na(.data$bezrobocie) ~ "praca na czarno na bezrobociu",
                         !(.data$praca %in% c(kodyPracyNaCzarno, NA) &
                             !is.na(.data$bezrobocie)) ~ "konflikt"),
             korekta_ciaglosc_nauki = "ndt."))
  # jeśli mamy tylko jeden epizod, niewątpliwie nie ma konfliktów (a kod poniżej się wykrzaczy)
  if (nrow(x) == 1) {
    return(select(x, -"ID_RESP"))
  }
  # korekta "konfliktów" pracy i bezrobocia
  for (i in 1:(nrow(x) - 1)) {
    if (all(!(x$praca[i:(i + 1)] %in% c(kodyPracyNaCzarno, NA)) &
            !is.na(x$bezrobocie[i:(i + 1)]))) {
      if (i == 1) {
        x$praca_a_bezrobocie[i] = "miesiąc graniczny"
      } else if (x$imput_praca[i] %in% 0 & x$imput_bezrobocie[i] %in% 1) {
        x$bezrobocie[i] = NA
        x$praca_a_bezrobocie[i] = "skorygowano bezrobocie"
      } else if (x$imput_praca[i] %in% 1 & x$imput_bezrobocie[i] %in% 0) {
        x$praca[i] = NA
        x$praca_a_bezrobocie[i] = "skorygowano pracę"
      } else if (x$imput_praca[i] %in% 0 & x$imput_bezrobocie[i] %in% 0) {
        x$praca_a_bezrobocie[i] = "sprzeczne deklaracje resp."
      } else if (x$imput_praca[i] %in% 1 & x$imput_bezrobocie[i] %in% 1) {
        x$praca_a_bezrobocie[i] = "sprzeczne wyniki imputacji"
      }
      if ((i + 1) == nrow(x)) {
        if (x$imput_praca[i + 1] %in% 0 & x$imput_bezrobocie[i + 1] %in% 1) {
          x$bezrobocie[i + 1] = NA
          x$praca_a_bezrobocie[i + 1] = "skorygowano bezrobocie"
        } else if (x$imput_praca[i + 1] %in% 1 & x$imput_bezrobocie[i + 1] %in% 0) {
          x$praca[i + 1] = NA
          x$praca_a_bezrobocie[i + 1] = "skorygowano pracę"
        } else if (x$imput_praca[i + 1] %in% 0 & x$imput_bezrobocie[i + 1] %in% 0) {
          x$praca_a_bezrobocie[i + 1] = "sprzeczne deklaracje resp."
        } else if (x$imput_praca[i + 1] %in% 1 & x$imput_bezrobocie[i + 1] %in% 1) {
          x$praca_a_bezrobocie[i + 1] = "sprzeczne wyniki imputacji"
        }
      }
    } else if (!(x$praca[i] %in% c(kodyPracyNaCzarno, NA)) &
               !is.na(x$bezrobocie[i])) {
      x$praca_a_bezrobocie[i] = "miesiąc graniczny"
    } else if (!(x$praca[i + 1] %in% c(kodyPracyNaCzarno, NA)) &
               !is.na(x$bezrobocie[i + 1])) {
      x$praca_a_bezrobocie[i + 1] = "miesiąc graniczny"
    }
  }
  # korekta wakacji, gdy ciągłość nauki
  for (i in setdiff(which(x$nauka %in% 2 & (x$czas %% 12) %in% 4), 1:5)) {
    if (!is.na(x$nauka[i - 5])) {
      x$korekta_ciaglosc_nauki[intersect((i - 4):(i - 1),
                                         which(is.na(x$nauka)))] = "skorygowane"
      x$nauka[(i - 4):(i - 1)] =
        ifelse(is.na(x$nauka[(i - 4):(i - 1)]),
               2, x$nauka[(i - 4):(i - 1)])
    }
  }
  for (i in setdiff(which(x$nauka %in% 3 & (x$czas %% 12) %in% 3), 1:4)) {
    if (!is.na(x$nauka[i - 4])) {
      x$korekta_ciaglosc_nauki[intersect((i - 3):(i - 1),
                                         which(is.na(x$nauka)))] = "skorygowane"
      x$nauka[(i - 3):(i - 1)] =
        ifelse(is.na(x$nauka[(i - 3):(i - 1)]),
               3, x$nauka[(i - 3):(i - 1)])
    }
  }
  # koniec
  return(select(x, -"ID_RESP"))
}
