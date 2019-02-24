#' @title Przygotowuje plik zdajacy_egzaminy
#' @description
#' zdajacy_egzaminy to plik z CKE/OKE, zawierający wyniki egzaminów zawodowych
#' monitorowanych absolwentów
#' @return ramka danych (klasy 'zdajacy_egzaminy_df')
#' @export
#' @importFrom utils read.csv2
przygotuj_zdajacy = function(){
  zE = read.csv2(list.files(pattern = "^zadający_egzaminy[.].csv$"))
  colnames(zE) = tolower(colnames(zE))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(zE) = c('zdajacy_egzaminy_df', class(zE))
  return(zE)
}
