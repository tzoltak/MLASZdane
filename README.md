![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

[![Travis build status](https://travis-ci.org/tzoltak/MLASZdane.svg?branch=master)](https://travis-ci.org/tzoltak/MLASZdane)
[![Coverage status](https://codecov.io/gh/tzoltak/MLASZdane/branch/master/graph/badge.svg)](https://codecov.io/github/tzoltak/MLASZdane?branch=master)

# MLASZdane

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój

Pakiet służy do złączania danych z rejestrów, baz danych administracyjnych, baz danych wyników egzaminów i wyników badań sondażowych na potrzeby projektu MLEZAiMD.

# Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN, więc trzeba instalować go ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalację najproście przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/MLASZdane', build_opts = c("--no-resave-data"))
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

# Użycie

## 1. runda monitoringu

Do przetwarzania wyników badań sondażowych przeprowadzonych w projekcie MLEZAiMD w ramach 1. rundy monitoringu (zbiór danych: *MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav*) służą następujące funkcje:

  + `wczytaj_wyniki_1rm()` - wczytuje zbiór danych z wynikami badania CAPI absolwentów (zapisany w formacie .sav programu SPSS) i tworzy na jego podstawie:
    + zbiór wymienionych przez respondentów *epizodów* nauki/pracy/bezrobocia (o które w czasie wywiadu pytano w pętlach, prosząc o wymienienie po kolei wszystkich *epizodów* danego rodzaju) w formie *długiej* (tj. wiele wierszy opisuje jednego respondenta - po jednym wierszu na każdy wymieniony przez niego *epizod*),
    + zbiór członków gospodarstw domowych w formie *długiej* (tj. wiele wierszy może opisywać gospodarstwo domowe tego samego respondenta - po jednym wierszu na każdego członka gospodarstwa domowego),
    + zbiór z pozostałymi zmiennymi;
  + `imputuj_miesiac_pk_1rm()` - imputuje wartości zmiennych opisujących czas rozpoczęcie i zakończenia się poszczególnych *epizodów* nauki/pracy/bezrobocia, jeśli respondent odpowiadając na pytania nie określił ich w sposób precyzyjny;
  + `przygotuj_zbior_osobo_miesiecy_1rm()` - tworzy zbiór, w którym jednostką obserwacji jest status nauki/zatrudnienia respondenta w danym miesiącu od ukończenia szkoły, użyteczny do analizy i ilustrowania zmiany statusu nauki/zatrudnienia absolwentów w czasie.

Wszystkie ww. funkcje przygotowują zbiory danych w taki sposób, że możliwe jest łatwe zapisanie ich do plików w formacie .sav, korzystając z funkcji pakietu [*haven*](https://haven.tidyverse.org).

Więcej informacji na temat struktury i zawartości tworzonych zbiorów danych zawiera [dokumentacja dot. 1. rundy monitoringu](./inst/doc/runda_1-dokumentacja.md).

Schemat wykorzystania ww. funkcji wygląda następująco:

```r
library(MLASZdane)

# wczytanie i przetworzenie zbioru z wynikami sondażu
dane1RM = wczytaj_wyniki_pilrm("MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav")
## wynikiem działania jest lista zbiorów
str(dane1RM)
head(dane1RM$epizody)
head(dane1RM$gospDom)

# imputowanie czasów rozpoczęcia i zakończenia epizodów
dane1RM = imputuj_miesiac_pk_1rm(dane1RM)

# zapis utworzonych zbiorów do plików w formacie .sav
library(haven)
write_sav(dane1RM$epizody, "MLEZAiMD_runda1_epizody.sav")
write_sav(dane1RM$gospDom, "MLEZAiMD_runda1_gosp_dom.sav")
write_sav(dane1RM$dane, "MLEZAiMD_runda1_bez_petli.sav")

# tworzenie i zapis pliku osobo-miesięcy
osMies1RM = przygotuj_zbior_osobo_miesiecy_pilrm(dane1RM)
head(osMies1RM)
write_sav(osMies1RM, "MLEZAiMD_runda1_osobo-miesiace.sav")
```

## Pilotażowa runda monitoringu

Do przetwarzania wyników badań sondażowych przeprowadzonych w projekcie MLEZAiMD w ramach pilotażowej rundy monitoringu (zbiór danych: *MLEZAMiD_absolwent_n2959_20171013.sav*) służą następujące funkcje:

  + `wczytaj_wyniki_pilrm()` - wczytuje zbiór danych z wynikami badania CAPI absolwentów (zapisany w formacie .sav programu SPSS) i tworzy na jego podstawie:
    + zbiór wymienionych przez respondentów *epizodów* nauki/pracy/bezrobocia (o które w czasie wywiadu pytano w pętlach, prosząc o wymienienie po kolei wszystkich *epizodów* danego rodzaju) w formie *długiej* (tj. wiele wierszy opisuje jednego respondenta - po jednym wierszu na każdy wymieniony przez niego *epizod*),
    + zbiór członków gospodarstw domowych w formie *długiej* (tj. wiele wierszy może opisywać gospodarstwo domowe tego samego respondenta - po jednym wierszu na każdego członka gospodarstwa domowego),
    + zbiór czasów odpowiedzi na pytania,
    + zbiór z pozostałymi zmiennymi;
  + `imputuj_miesiac_pk_pilrm()` - imputuje wartości zmiennych opisujących czas rozpoczęcie i zakończenia się poszczególnych *epizodów* nauki/pracy/bezrobocia, jeśli respondent odpowiadając na pytania nie określił ich w sposób precyzyjny;
  + `przygotuj_zbior_osobo_miesiecy_pilrm()` - tworzy zbiór, w którym jednostką obserwacji jest status nauki/zatrudnienia respondenta w danym miesiącu od ukończenia szkoły, użyteczny do analizy i ilustrowania zmiany statusu nauki/zatrudnienia absolwentów w czasie.

Wszystkie ww. funkcje przygotowują zbiory danych w taki sposób, że możliwe jest łatwe zapisanie ich do plików w formacie .sav, korzystając z funkcji pakietu [*haven*](https://haven.tidyverse.org).

Więcej informacji na temat struktury i zawartości tworzonych zbiorów danych zawiera [dokumentacja dot. pilotażowej rundy monitoringu](./inst/doc/runda_pilotazowa-dokumentacja.md).

Schemat wykorzystania ww. funkcji wygląda następująco:

```r
library(MLASZdane)

# wczytanie i przetworzenie zbioru z wynikami sondażu
danePilRM = wczytaj_wyniki_pilrm("MLEZAMiD_absolwent_n2959_20171013.sav")
## wynikiem działania jest lista zbiorów
str(danePilRM)
head(danePilRM$epizody)
head(danePilRM$gospDom)

# imputowanie czasów rozpoczęcia i zakończenia epizodów
danePilRM = imputuj_miesiac_pk_pilrm(danePilRM)

# zapis utworzonych zbiorów do plików w formacie .sav
library(haven)
write_sav(danePilRM$epizody, "MLEZAiMD_runda_pilot_epizody.sav")
write_sav(danePilRM$gospDom, "MLEZAiMD_runda_pilot_gosp_dom.sav")
write_sav(danePilRM$dane, "MLEZAiMD_runda_pilot_bez_petli.sav")

# tworzenie i zapis pliku osobo-miesięcy
osMiesPilRM = przygotuj_zbior_osobo_miesiecy_pilrm(danePilRM)
head(osMiesPilRM)
write_sav(osMiesPilRM, "MLEZAiMD_runda_pilot_osobo-miesiace.sav")
```
