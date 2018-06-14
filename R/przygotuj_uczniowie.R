#' @title Przygotowuje plik uczniowie
#' @description
#' uczniowie to plik wyeksportowany z systemu SIO2, który zawiera informacje
#' o monitorowanych absolwentach, którzy kontynuują naukę w szkołach
#' (niebędących uczelniami wyższymi)
#' @return ramka danych (klasy 'uczniowie_df')
#' @export
#' @importFrom rio import
przygotuj_uczniowie = function(){
  uczniowie = import(list.files(pattern = "^uczniowie[.].{3,4}$"))
  colnames(uczniowie) = tolower(colnames(uczniowie))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(uczniowie) = c('uczniowie_df', class(uczniowie))
  return(uczniowie)
}
