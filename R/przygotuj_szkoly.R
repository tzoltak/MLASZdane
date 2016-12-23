#' @title Przygotowuje plik szkoly
#' @description
#' szkoly to plik wyeksporotwany z SIO, zawierający informacje o szkołach,
#' dla których mają zostać obliczone wartości wskaźników
#' @return ramka danych (klasy 'szkoly_df')
#' @export
#' @importFrom rio import
przygotuj_szkoly = function(){
  szkoly = import(list.files(pattern = "^szkoły[.].{3,4}$"))
  colnames(szkoly) = tolower(colnames(szkoly))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(szkoly) = c('szkoly_df', class(szkoly))
  return(szkoly)
}
