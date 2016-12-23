#' @title Przygotowuje plik studenci
#' @description
#' studenci to plik wyeksportowany z systemu POL-on, zawierający informacje
#' o absolwentach podlegających monitorowaniu, którzy kontunuują naukę na
#' uczelniach wyższych
#' @return ramka danych (klasy 'studenci_df')
#' @export
#' @importFrom rio import
przygotuj_studenci = function(){
  studenci = import(list.files(pattern = "^studenci[.].{3,4}$"))
  colnames(studenci) = tolower(colnames(studenci))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(studenci) = c('studenci_df', class(studenci))
  return(studenci)
}
