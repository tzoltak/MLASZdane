#' @title Przygotowuje plik studenci
#' @description
#' studenci to plik wyeksportowany z systemu POL-on, zawierający informacje
#' o absolwentach podlegających monitorowaniu, którzy kontunuują naukę na
#' uczelniach wyższych
#' @return ramka danych (klasy 'studenci_df')
#' @export
#' @importFrom utils read.csv2
przygotuj_studenci = function(){
  studenci = read.csv2(list.files(pattern = "^studenci[.].csv$"))
  colnames(studenci) = tolower(colnames(studenci))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(studenci) = c('studenci_df', class(studenci))
  return(studenci)
}
