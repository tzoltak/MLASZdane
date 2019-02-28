---
title: "Przygotowywanie wskaźników na podstawie Banku Danych Lokalnych GUS"
author: "Tomasz Żółtak"
date: "28 lutego 2019"
output: rmarkdown::html_vignette
lang: pl
vignette: >
  %\VignetteIndexEntry{Przygotowywanie wskaźników na podstawie BDL GUS}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# 1. Pobieranie wskaźników z API BDL

Pobieranie wskaźników z API BDL z wykorzystaniem funkcji pakietu MLASZdane przebiega w dwóch krokach:

  1. Wyszukanie wskaźników, które chce się pobrać przy pomocy funkcji `znajdz_wskazniki_bdl()` lub `wskaznik_bdl()`.
  2. Pobranie wartości wyszukanych wcześniej wskaźników przy pomocy funkcji `pobierz_dane_bdl()`.
  
## 1.1. Wyszukiwanie wskaźników

Wyszukiwanie wskaźników BDL przy użyciu API nie jest łatwe, ze względu na przyjęty przez GUS nie do końca spójny (a przynajmniej niezbyt wygodny) schemat nazywania wskaźników. Wskaźniki można bowiem podzielić na dwie grupy: te, które posiadają unikalne i *informatywne* nazwy oraz te, których nazwy są *nieinformatywne* i nie są unikalne (w szczególności wskaźniki o nazwie "ogółem"). Te pierwsze można łatwo wyszukiwać po nazwach (korzystając z odpowiedniej funkcji API), te drugie trzeba znajdować przeszukując krok po kroku w głąb drzewiastą strukturę grup i podgrup wskaźników. Ponieważ to drugie podejście jest dosyć skomplikowane i słabo poddające się automatyzacji, w ramach pakietu zdecydowano się przyjąć następujące podejście:

  + Wskaźniki, które da się w API BDL znaleźć po nazwie (np. *stopa bezrobocia rejestrowanego*) można wyszukać przy pomocy funkcji `znajdz_wskazniki_bdl()`.
    + w przypadku wskaźników, których wartości GUS raportuje z częstotliwością większą niż roczna (kwartalną, miesięczną) nazwa wskaźnika opisywana jest w zwracanych wynikach dwoma kolumnami: `n1` opisuje okres sprawozdawczy w ramach roku (kwartał lub miesiąc), a `n2 ` jako taką nazwę wskaźnika.
  + w przypadku wskaźników, których nie da się w API BDL wyszukać po nazwie (np. *przeciętne miesięczne wynagrodzenia brutto*), trzeba samodzielnie (*ręcznie*) zidentyfikować ich `id` w API BDL, a następnie użyć funkcji `wskaznik_bdl()`, aby pobrać informacje o danym wskaźniku zwrócone w analogicznej formie jak ta, w jakiej zwraca informacje o wyszukanych wskaźnikach funkcja `znajdz_wskazniki_bdl()`.
    + Funkcja `wskaznik_bdl()` umożliwia przy tym nadpisanie *nieinformatywnej* nazwy wskaźnika pobranej z API BDL (i zwracaną w kolumnie `n1`) nazwą, której chcielibyśmy używać na potrzeby dalszych operacji na danych.
  
Rozróżnienie wskaźników jednego i drugiego rodzaju w praktyce polega na sprawdzeniu, czy dany wskaźnik daje się znaleźć po nazwie, co do której spodziewamy się, że ją ma - jeśli się to nie udaje, najprawdopodobniej jest to wskaźnik drugiego rodzaju.

**Przykładowe użycia:**

```
library(MLASZdane)
znajdz_wskazniki_bdl("stopa bezrobocia rejestrowanego")
wskaznik_bdl(64428, "przeciętne miesięczne wynagrodzenia brutto")
```

## 1.2. Pobieranie zestawień wartości wskaźników

Do pobierania zestawień wartości wskaźników z API BDL służy funkcja `pobierz_dane_bdl()`, która przyjmuje następujące argumenty:

  + `wskazniki` - obiekt zwrócony przez funkcję `znajdz_wskazniki_bdl()` lub `wskaznik_bdl()` (lub dowolne połączenie wyników wywołania tych funkcji przy pomocy funkcji `rbind()` lub `bind_rows()`);
  + `lata` lata, dla których mają zostać pobrane wartości wskaźników (można podać wektor liczb, aby pobrać wartości z wielu lat);
  + `poziom` - poziom agregacji, na którym mają zostać pobrane wartości wskaźników: "makroregiony", "województwa", "regiony", "podregiony", "powiaty" lub "gminy" (domyślnie "powiaty").

Funkcja zwraca ramkę danych o kolumnach:

 + `idWsk` - id wskaźnika w API BDL;
 + `subjectId` - id tematu (grupy wskaźników) w API BDL;
 + `n1` i ew. `n2` - kolumny opisujące nazwy wskaźnków w API BDL (por. sekcja 1.1.);
 + `level` - najniższy poziom NTS, na którym raportowany jest wskaźnik;
 + `measureUnitId`, `measureUnitName` - id i nazwa jednostki, w jakiej wyrażone są wartości wskaźnika;
 + `idJst` - kod NTS jednostki terytorialnej, do której odnosi się wartość wskaźnika;
 + `name` - nazwa jednostki terytorialnej, do której odnosi się wartość wskaźnika;
 + `year` - rok, do którego odnosi się wartość wskaźnika;
 + `val` - wartość wskaźnika;
 + `attrId` - identyfikator atrybutu (co do zasady nieistotny);
 + `teryt` kod TERYT przeliczony na podstawie `idJst`.

**Przykładowe użycia:**

**Uwaga!** API BDL ma absurdalnie niskie limity na dopuszczalną w przedziale czasu liczbę zapytań, więc zapytanie takie jak poniżej można w praktyce wykonać tylko raz na 15 minut. w związku z tym zasadne jest zrobienie tego raz i zapisanie wyników lokalnie do wykorzystania w przyszłości (co w kodzie poniżej realizuje wywołanie funkcji `save()`).

```
library(MLASZdane)
wskaznikiBdl =
  bind_rows(znajdz_wskazniki_bdl("stopa bezrobocia rejestrowanego") %>%
              pobierz_dane_bdl(2017:2018, "powiaty"),
            wskaznik_bdl(64428, "przeciętne miesięczne wynagrodzenia brutto") %>%
              pobierz_dane_bdl(2017, "powiaty"))
save(wskaznikiBdl, file = "wskazniki BDL.RData")
```

# 2. Przekształcanie zestawień pobranych z API BDL na zestawienia wskaźników wykorzystywanych w monitorowaniu losów absolwentów

Przekształcenie zestawień wskaźników pobranych z API BDL na zestawienia wskaźników wykorzystywanych w monitorowaniu losów absolwentów polega na zamienie formy zestawienia z *długiej* (jedna JST-wiele wierszy) na *szeroką* (jedna JST-jeden wiersz) oraz przypisaniu kolumnom takiego zestawienia w formie *szerokiej* adekwatnych nazw. Przekształcenia te wykonuje funkcja `przeksztalc_dane_bdl()`, przy czym w obecnej postaci ma ona kilka **ważnych ograniczeń**:

  + nazwy wszystkich wskaźników zawierające ciąg znaków "bezrobocia" traktowane są jako jeden wskaźnik, w nazwach wynikowych zmiennych opisywany jako "bezrobocie";
  + nazwy wszystkich wskaźników zawierające ciąg znaków "wynagrodzenia" traktowane są jako jeden wskaźnik, w nazwach wynikowych zmiennych opisywany jako "sr_wynagrodzenia";
  + nazwy wszystkich pozostałych wskaźników pozostają bez zmian, z tym że znaki spacji zamieniane są w nich na znak "_".

**Nie ma gwarancji, że ww. reguły nie ulegną zmianie w przyszłości!**

Nazwy zmiennych zawierających wartości wskaźników w zwracanym zestawieniu są postaci: `jst_wskaznik_[[:digit:]]+[mr]`, gdzie:

  + `wskaznik` to nazwa wskaźnika (p. wyżej);
  + `[[:digit:]]+` to liczba (miesięcy lub lat od momentu planowego ukończenia szkoły przez uczniów rocznika podanego w wywołaniu funkcji argumentem rocznik);
  + `[mr]+` to litera "m" lub litera "r" w zależności od tego, czy dany wskaźnik raportowany jest przez GUS z częstotliwością miesięczną, czy roczną.

Jeśli wszystkie wskaźniki w zestawieniu pobranym z API BDL przekazanym do funkcji są określone na tym samym poziomie agregacji, to w nazwach zmiennych ciąg znaków "jst" jest zamieniany na nazwę danego poziomu (np. "powiat").

Przy wywołaniu funkcji konieczne jest podanie rocznika, w odniesieniu do którego przygotowane zostaną nazwy zmiennych (przy czym funkcja **arbitralnie zakłada, że abolwenci powinni planowo kończyć szkołę w czerwcu**).

Przy pomocy opcjonalnego argumentu `prefiks` możliwe jest też dopisanie do nazw zmiennych identyfikujących w wynikowym zestawieniu JST (`teryt` i `nazwaJst`) prefiksu, tak aby odpowiadały one nazwom zmiennych w zbiorze, do którego ma być przyłączane zestawienie.

**Przykładowe użycie:**

```
library(MLASZdane)
load("wskazniki BDL.RData")
przeksztalc_dane_bdl(wskaznikiBdl, 2017)
```
