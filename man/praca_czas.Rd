% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wskazniki_poziom_indywidualny.R
\name{praca_czas}
\alias{praca_czas}
\title{Obliczanie wskaznikow na poziomie indywidualnym}
\usage{
praca_czas(
  epizody,
  pg2hWartosci = NULL,
  limitG = 9,
  limitD = 1,
  sufiks = "",
  idAbsolwenta = "ID_RESP"
)
}
\arguments{
\item{epizody}{ramka danych z epizodami - typowo element \code{epizody} listy
zwracanej przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}}

\item{pg2hWartosci}{opcjonalnie wektor liczb - wartości zmiennej \code{pg2h}
w ramce danych przekazanej argumentem \code{epizody}, opisujące te formy
zatrudnienia, które mają zostać uwzględnione przy zliczaniu (domyślnie
pod uwagę brane są pod uwagę wszystkie formy oprócz pracy bez umowy)}

\item{limitG}{liczba - górna granica analizowanego okresu, wyrażona jako
liczba miesięcy od momentu planowego ukończenia szkoły
(porównywana z wartością kolumny \code{czas_rozp})}

\item{limitD}{liczba - dolna granica analizowanego okresu, wyrażona jako
liczba miesięcy od momentu planowego ukończenia szkoły
(porównywana z wartością kolumny \code{czas_zakon})}

\item{sufiks}{ciąg znaków - sufiks, który zostanie dopisan na końcu nazw
zmiennych zawierających wartości wskaźników}

\item{idAbsolwenta}{nazwa zmiennej identyfikującej badanych (podana jako ciąg
znaków lub wyrażenie)}
}
\value{
data frame
}
\description{
Na podstawie zbioru epizodów dla każdego badanego funkcja
oblicza zmienne-wskaźniki opisujące, przez jaką część zadanego okresu
badany pracował. Wskaźnik obliczany jest również oddzielnie w odniesieniu do
tych części zadanego okresu, kiedy badany się uczył i kiedy się nie uczył.
Formy zatrudnienia uwzględniane w obliczeniach pozwala ograniczyć argument
\code{pg2hWartosci}.
}
\seealso{
\code{\link{czas_agregacja}}, \code{\link{czas_rozklad}}
}
