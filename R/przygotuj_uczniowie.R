#' @title Przygotowuje plik uczniowie
#' @description
#' uczniowie to plik wyeksportowany z systemu SIO2, który zawiera informacje
#' o monitorowanych absolwentach, którzy kontynuują naukę w szkołach
#' (niebędących uczelniami wyższymi)
#' @return ramka danych (klasy 'uczniowie_df')
#' @export
#' @importFrom utils read.csv2
przygotuj_uczniowie = function(){
  uczniowie = read.csv2(list.files(pattern = "^uczniowie[.].csv$"))
  colnames(uczniowie) = tolower(colnames(uczniowie))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(uczniowie) = c('uczniowie_df', class(uczniowie))
  return(uczniowie)
}
