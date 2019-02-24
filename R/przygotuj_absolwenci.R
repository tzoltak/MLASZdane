#' @title Przygotowuje plik absolwenci
#' @description
#' absolwenci to plik zawierający informacje o absolwentach szkół poddanych
#' badaniu monitorującemu - w szczególności zawierający informacje o tym, czy
#' wyrazili oni zgodę na wykorzystanie ich nr. PESEL do łączenia danych
#' (i ew. ten PESEL, jub jego zpseudonimizowane przekształcenie)
#' @return ramka danych (klasy 'absolwenci_df')
#' @export
#' @importFrom utils read.csv2
przygotuj_absolwenci = function(){
  absolwenci = read.csv2(list.files(pattern = "^absolwenci[.].csv$"))
  colnames(absolwenci) = tolower(colnames(absolwenci))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(absolwenci) = c('absolwenci_df', class(absolwenci))
  return(absolwenci)
}
