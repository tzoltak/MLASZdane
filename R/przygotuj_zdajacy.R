#' @title Przygotowuje plik zdajacy_egzaminy
#' @description
#' zdajacy_egzaminy to plik z CKE/OKE, zawierający wyniki egzaminów zawodowych
#' monitorowanych absolwentów
#' @return ramka danych (klasy 'zdajacy_egzaminy_df')
#' @export
#' @importFrom rio import
przygotuj_zdajacy = function(){
  zE = import(list.files(pattern = "^zadający_egzaminy[.].{3,4}$"))
  colnames(zE) = tolower(colnames(zE))
  # tu będą się znajdować przekształcenia struktury pliku
  #   jak już będzie wiadomo, jak on dokładnie wygląda
  class(zE) = c('zdajacy_egzaminy_df', class(zE))
  return(zE)
}
