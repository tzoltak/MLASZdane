#' @title Obrobka danych etykietowanych
#' @description
#' Funkcja pozwala szybko i wygodnie przywracać etykiety zmiennych i wartości,
#' które "gubią się" w niektórych operacjach na ramkach danych (o ile wcześniej
#' zostały one zapisane).
#' @param x ramka danych, której zmiennym mają zostać przypisane etykiety
#' @param labTemp ramka danych (lub lista) z której mają zostać skopiowane
#' etykiety zmiennych i wartości zmiennych
#' @return ramka danych
przywroc_etykiety = function(x, labTemp) {
  nieMaZm = setdiff(names(labTemp), names(x))
  if (length(nieMaZm) > 0) {
    warning("W ramce z danymi nie ma zmiennych: '", paste(nieMaZm, collapse = "', '"), "'.")
    labTemp = labTemp[, names(labTemp) %in% names(x)]
  }
  for (i in names(labTemp)) {
    attributes(x[[i]])$label = enc2native(attributes(labTemp[[i]])$label)
    attributes(x[[i]])$labels = attributes(labTemp[[i]])$labels
    if (!is.null(attributes(x[[i]])$labels)) {
      names(attributes(x[[i]])$labels) =
        enc2native(names(attributes(x[[i]])$labels))
      class(x[[i]]) = "haven_labelled"
    }
  }
  return(x)
}
