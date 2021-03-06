% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wskazniki_poziom_indywidualny.R
\name{praca_ostatnia}
\alias{praca_ostatnia}
\title{Obliczanie wskaznikow na poziomie indywidualnym}
\usage{
praca_ostatnia(epizody, idAbsolwenta = "ID_RESP", limitG = Inf, limitD = 0)
}
\arguments{
\item{epizody}{ramka danych z epizodami - typowo element \code{epizody} listy
zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}}

\item{idAbsolwenta}{nazwa zmiennej identyfikującej badanych (podana jako ciąg
znaków lub wyrażenie)}

\item{limitG}{liczba - górna granica, do jakiej zostanie zawężony analizowany
okres, wyrażona jako liczba miesięcy od momentu planowego ukończenia szkoły
(porównywana z wartością kolumny \code{czas_rozp})}

\item{limitD}{liczba - dolna granica, do jakiej zostanie zawężony analizowany
okres, wyrażona jako liczba miesięcy od momentu planowego ukończenia szkoły
(porównywana z wartością kolumny \code{czas_zakon})}
}
\value{
data frame
}
\description{
Funkcja oblicza zmienne-wskaźniki opisujące ostatnią pracę
badanego wykonywaną w analizowanym okresie. Nie jest przy tym brana pod uwagę
praca świadczona bez umowy.
}
\seealso{
\code{\link{praca_forma_ind}}, \code{\link{praca_zamieszkanie_ind}},
\code{\link{praca_forma}}, \code{\link{praca_forma2}},
\code{\link{praca_zamieszkanie}},
\code{\link{praca_zgodna_z_wyksztalceniem}},
\code{\link{praca_zarobki}}
}
