#' @title Pobieranie danych z Banku Danych Lokalnych GUS
#' @description
#' Korzystając z API BDL funkcja zwraca informacje o wskaźnikach, których
#' nazwa zawiera podany ciąg znaków.
#' @param nazwa ciąg znaków - fragment nazwy wskaźnika, który ma być znaleziony
#' @return ramka danych z przypisaną klasą "wskaznikiBDL"
#' @details
#' \strong{Uwaga!} Nie wszystkie wskaźniki BDL GUS da się rozsądnie znaleźć
#' w ten sposób, gdyż niektóre mają przypisane niezwykle opisowe nazwy w rodzaju
#' "ogółem" - te trzeba by wcześniej znajdować poprzez nazwę grupy i podgrupy
#' wskaźników, jednak możliwości takiego przeszukiwania nie zaimplementowano
#' w pakiecie MLASZdane. Wskaźniki takie (dokładie ich `id`) trzeba znaleźć
#' \emph{ręcznie} w API BDL, a następnie pobrać ich dane identyfikacyjne
#' korzysając z funkcji \code{\link{wskaznik_bdl}}.
#' @seealso \code{\link{pobierz_dane_bdl}}, \code{\link{wskaznik_bdl}}
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows last
znajdz_wskazniki_bdl = function(nazwa) { # nocov start
  stopifnot(is.character(nazwa), length(nazwa) == 1)

  nazwa = URLencode(enc2utf8(nazwa))
  temp = fromJSON(paste0("https://bdl.stat.gov.pl/api/v1/variables/search?name=",
                         nazwa, "&page-size=100&lang=pl&format=json"))
  wskazniki = temp$results
  while ("next" %in% names(temp$links)) {
    temp = fromJSON(temp$links$`next`)
    wskazniki = bind_rows(wskazniki, temp$results)
  }
  kolumnaWskaznik = last(grep("^n[[:digit:]]+$", names(wskazniki)))
  if (length(unique(wskazniki[[kolumnaWskaznik]])) > 1) {
    warning("Znaleziono więcej niż jeden wskaźnik pasujący do podanej nazwy!",
            immediate. = TRUE)
  }
  class(wskazniki) = c("wskaznikiBDL", class(wskazniki))
  return(wskazniki)
} # nocov end
#' @title Pobieranie danych z Banku Danych Lokalnych GUS
#' @description
#' Funkcja pozwala przy pomocy API BDL pobrać dane dot. wskaźnika, którego nie
#' da się zidentyfikować na podstawie samej jego nazwy (używając funkcji
#' \code{\link{znajdz_wskazniki_bdl}}).
#' @param id liczba całkowita - id wskaźnika w API BDL
#' @param zamienNazwe opcjonlanie ciąg znaków - nazwa, która ma zostać
#' przypisana wskaźnikowi zamiast tej, którą ma w API BDL
#' @return ramka danych z przypisaną klasą "wskaznikiBDL"
#' @details
#' Niektóre wskaźniki mają w BDL GUS przypisane niezwykle opisowe nazwy
#' w rodzaju "ogółem".  Wskaźniki takie (dokładie ich `id`) trzeba znaleźć
#' \emph{ręcznie} w API BDL, a następnie pobrać ich dane identyfikacyjne
#' korzysając właśnie z tej funkcji, aby można je wykorzystać w analogiczny
#' sposób, jak te, informacje o których pobiera się przy pomocy funkcji
#' \code{\link{znajdz_wskazniki_bdl}}.
#' @seealso \code{\link{pobierz_dane_bdl}}, \code{\link{znajdz_wskazniki_bdl}}
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr distinct select
wskaznik_bdl = function(id, zamienNazwe = NULL) { # nocov start
  stopifnot(is.numeric(id), length(id) == 1,
            is.null(zamienNazwe) | is.character(zamienNazwe))
  stopifnot(all(as.integer(id) == id))
  if (!is.null(zamienNazwe)) {
    stopifnot(length(zamienNazwe) == 1)
  }

  temp = fromJSON(paste0("https://bdl.stat.gov.pl/api/v1/variables/",
                         id, "?lang=pl&format=json"))
  wskaznik = temp %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    select(-"years") %>%
    distinct()
  if (!is.null(zamienNazwe)) {
    wskaznik$n1 = zamienNazwe
  }
  class(wskaznik) = c("wskaznikiBDL", class(wskaznik))
  return(wskaznik)
} # nocov end
#' @title Pobieranie danych z Banku Danych Lokalnych GUS
#' @description
#' Funkcja przy pomocy API BDL pobiera zestawienie wartości podanych wskaźników
#' w podanych latach na podanym poziomie agregacji.
#' @param wskazniki data frame zwrócony przez funkcję
#' \code{\link{znajdz_wskazniki_bdl}} lub \code{\link{wskaznik_bdl}}
#' @param lata wektor liczbowy - lata, dla których mają zostać zwrócone
#' wartości wskaźników
#' @param poziom opcjonalnie poziom agregacji wskaźników: "makroregiony",
#' "województwa", "regiony", "podregiony", "powiaty" lub "gminy"
#' @return ramka danych z przypisaną klasą "daneBDL"
#' @details
#' \strong{Uwaga!} API BDL ma absurdalnie niskie limity na dopuszczalną
#' w przedziale czasu liczbę zapytań (a przy tym ogranicza liczbę zwracanych
#' wierszy zestawienia do maksimum 100), więc pobieranie wskaźników przy pomocy
#' tej funkcji należy sobie dobrze rozplanować w czasie. W związku z tym zasadne
#' jest też zapisanie ściągniętych zestawień lokalnie do wykorzystania
#' w przyszłości, bez konieczności używania API.
#' @seealso \code{\link{przeksztalc_dane_bdl}}
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @importFrom dplyr .data bind_rows left_join mutate rename
pobierz_dane_bdl = function(wskazniki, lata, poziom = "powiaty") { # nocov start
  stopifnot("wskaznikiBDL" %in% class(wskazniki),
            is.data.frame(wskazniki),
            "id" %in% names(wskazniki),
            is.numeric(lata),
            is.character(poziom), length(poziom) == 1)
  stopifnot(is.integer(wskazniki$id),
            all(as.integer(lata) == lata),
            lata %in% (2010:as.integer(format(Sys.Date(), "%Y"))),
            poziom %in% c("makroregiony", "województwa", "regiony",
                          "podregiony", "powiaty", "gminy", ""))

  dodajTeryt = !grepl("region", poziom)
  lata = paste0("year=", lata, collapse = "&")
  poziom = paste0("&unit-level=",
                  which(poziom == c("makroregiony", "województwa", "regiony",
                                    "podregiony", "powiaty", "gminy")))

  brakiDanych = vector(mode = "integer", length = 0)
  dane = data.frame(idWsk = vector(mode = "integer", length = 0))
  for (i in wskazniki$id) {
    temp = fromJSON(paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/",
                           i, "?", lata, poziom, "&page-size=100&lang=pl&format=json"))
    if (temp$totalRecords > 0) {
      dane = bind_rows(dane, unnest(temp$results, everything()))
    } else {
      brakiDanych = c(brakiDanych, i)
    }
    while ("next" %in% names(temp$links)) {
      temp = fromJSON(temp$links$`next`)
      dane = bind_rows(dane, unnest(temp$results, everything()))
    }
    dane = dane %>%
      mutate(idWsk = ifelse(is.na(.data$idWsk), i, .data$idWsk))
  }
  if (length(brakiDanych) > 0) {
    warning("W BDL brak danych dla wskaźników o id: ",
            paste0(brakiDanych, collapse = ", "),
            ".", immediate. = TRUE)
    print(filter(wskazniki, .data$id %in% brakiDanych))
    cat("\n")
  }
  dane = suppressMessages(wskazniki %>%
                            rename(idWsk = "id") %>%
                            left_join(dane)) %>%
    rename(idJst = "id") %>%
    mutate(year = as.integer(.data$year))
  if (dodajTeryt) {
    dane = dane %>%
      mutate(teryt = as.integer(sub("^..(..)...(....).$", "\\1\\2\\3", .data$idJst)))
  }
  class(dane) = c("daneBDL", class(dane))
  return(dane)
} # nocov end
#' @title Pobieranie danych z Banku Danych Lokalnych GUS
#' @description
#' Funkcja przetwarza dane pobrane z API BDL w formie \emph{długiej} (jedna
#' JST-wiele wierszy) na formę \emph{szeroką} (jedna JST-jeden wiersz), w której
#' są zdatne do wykorzystania przy obliczaniu wskaźników szkół dla konkretnego
#' rocznika absolwentów.
#' @param daneBdl data frame zwrócony przez funkcję \code{\link{pobierz_dane_bdl}}
#' @param rocznik liczba - rok ukończenia szkoły, w stosunku do którego mają
#' zostać skonstruowane nazwy wskaźników
#' @param prefiks opcjonalnie prefiks dopisywany na początku nazw (nazw,
#' nie wartości!) zmiennych opisujących JST (teryt i nazwę) w zwracanej ramce
#' danych
#' @return ramka danych
#' @details
#' \strong{Uwaga!} W tej chwili zmieniając nazwy wskaźników na nazwy zmiennych
#' funkcja stosuje następujące reguły:
#' \itemize{
#'   \item{nazwy wszystkich wskaźników zawierające ciąg znaków "bezrobocia"
#'         traktowane są jako jeden wskaźnik, w nazwach wynikowych zmiennych
#'         opisywany jako "bezrobocie";}
#'   \item{nazwy wszystkich wskaźników zawierające ciąg znaków "wynagrodzenia"
#'         traktowane są jako jeden wskaźnik, w nazwach wynikowych zmiennych
#'         opisywany jako "sr_wynagrodzenia";}
#'   \item{nazwy wszystkich pozostałych wskaźników pozostają bez zmian, z tym
#'         że znaki spacji zamieniane są w nich na znak "_".}
#' }
#' \strong{Nie ma gwarancji, że ww. reguły nie ulegną zmianie w przyszłości!}
#'
#' Nazwy zmiennych zawierających wartości wskaźników w zwracanym zestawieniu są
#' postaci: \code{jst_wskaznik_[[:digit:]]+[mr]}, gdzie:
#' \itemize{
#'   \item{\code{wskaznik} to nazwa wskaźnika (p. wyżej);}
#'   \item{\code{[[:digit:]]+} to liczba (miesięcy lub lat od momentu planowego
#'         ukończenia szkoły przez uczniów rocznika podanego w wywołaniu funkcji
#'         argumentem  \code{rocznik});}
#'   \item{\code{[mr]+} to litera "m" lub litera "r" w zależności od tego, czy
#'         dany wskaźnik raportowany jest przez GUS z częstotliwością miesięczną,
#'         czy roczną.}
#' }
#' Jeśli wszystkie wskaźniki w zestawieniu pobranym z API BDL przekazanym do
#' funkcji są określone na tym samym poziomie agregacji, to w nazwach zmiennych
#' ciąg znaków "jst" jest zamieniany na nazwę danego poziomu (np. "powiat").
#' @export
#' @importFrom stats setNames
#' @importFrom tidyr spread
#' @importFrom dplyr .data case_when mutate select
przeksztalc_dane_bdl = function(daneBdl, rocznik, prefiks = "") {
  stopifnot("daneBDL" %in% class(daneBdl),
            is.data.frame(daneBdl),
            is.numeric(rocznik), length(rocznik) == 1,
            all(as.integer(rocznik) == rocznik),
            is.character(prefiks), length(prefiks) == 1)

  poziomy = sub("^([^ ]+) .*$", "\\1", daneBdl$name) %>%
    tolower() %>%
    unique()
  miesiace = as.list(1:13) %>%
    setNames(c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec",
               "lipiec", "sierpień", "wrzesień", "październik", "listopad",
               "grudzień", "ndt."))
  daneBdl = daneBdl %>%
    mutate(miesiac = ifelse(.data$n1 %in% names(miesiace), .data$n1, "ndt."),
           czas = ifelse(.data$n1 %in% names(miesiace),
                         paste0(12 * (.data$year - rocznik) +
                                  unlist(miesiace[.data$miesiac]) - 6, "m"),
                         paste0(.data$year - rocznik, "r")),
           n1 = ifelse(.data$n1 %in% names(miesiace), .data$n2, .data$n1),
           n1 = case_when(grepl("bezrobocia",
                                .data$n1) ~ paste0("jst_bezrobocie_", .data$czas),
                          grepl("wynagrodzenia",
                                .data$n1) ~ paste0("jst_sr_wynagrodzenia_", .data$czas),
                          TRUE ~ paste0("jst_", gsub("[[:blank:]]", "_", .data$n1),
                                        "_", .data$czas)),
           teryt = as.numeric(.data$teryt),
           name = sub("^Powiat", "powiat", .data$name),
           name = sub("^Gmina", "gmina", .data$name)) %>%
    filter(!grepl("^-", .data$czas)) %>%
    select(wskaznik = "n1", wartosc = "val", teryt = "teryt", nazwaJst = "name") %>%
    spread("wskaznik", "wartosc")
  if (prefiks != "") {
    names(daneBdl)[names(daneBdl) %in% c("teryt", "nazwaJst")] =
      paste0(prefiks, names(daneBdl)[names(daneBdl) %in% c("teryt", "nazwaJst")])
  }
  if (length(poziomy) == 1) {
    names(daneBdl) = sub("nazwaJst$", poziomy, names(daneBdl))
    names(daneBdl) = sub("^jst_", paste0(poziomy, "_"), names(daneBdl))
  }
  return(daneBdl)
}
