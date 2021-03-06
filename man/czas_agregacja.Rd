% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wskazniki_poziom_zagregowany.R
\name{czas_agregacja}
\alias{czas_agregacja}
\title{Obliczanie wskaznikow na poziomie zagregowanym}
\usage{
czas_agregacja(x, zmienna)
}
\arguments{
\item{x}{ramka danych ze wskaźnikami na poziomie indywidualnym - typowo
grupa wyróżniona w ramach obiektu zwracanego przez funkcję
\code{\link{oblicz_wskazniki_ind_1rm}}}

\item{zmienna}{ciąg znaków - nazwa zmiennej, na podstawie której ma być
obliczony wskaźnik}
}
\value{
lista o elementach:
\itemize{
  \item{\code{n} - liczba badanych uwzględnionych w obliczeniach (tj.nie
        mających braku danych w zmiennej opisanej argumentem \code{zmienna}),}
  \item{\code{`średnia`} - liczba [0, ),}
  \item{\code{`mediana`} - liczba [0, ),}
  \item{\code{`1.kwartyl`} - liczba [0, ),}
  \item{\code{`3.kwartyl`} - liczba [0, ).}
}
}
\description{
Funkcja oblicza wskaźnik opisujący rozkład wskaźnika z poziomu
indywidualnego w ramach grupy - zwykle opisującego czas pracy lub bezrobocia
w badanym okresie.
}
\seealso{
\code{\link{praca_czas}}, \code{\link{bezrobocie_czas}}
}
