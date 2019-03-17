![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

[![Travis build status](https://travis-ci.org/tzoltak/MLASZdane.svg?branch=master)](https://travis-ci.org/tzoltak/MLASZdane)
[![Coverage status](https://codecov.io/gh/tzoltak/MLASZdane/branch/master/graph/badge.svg)](https://codecov.io/github/tzoltak/MLASZdane?branch=master)

# MLASZdane

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój

Pakiet służy do złączania danych z rejestrów, baz danych administracyjnych, baz danych wyników egzaminów i wyników badań sondażowych na potrzeby projektu MLEZAiMD.

# Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN, więc trzeba instalować go ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/MLASZdane', build_opts = c("--no-resave-data"))
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

# Użycie

## Przygotowanie zbiorów z 1. rundy monitoringu

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
dane1RM = wczytaj_wyniki_1rm("MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav")
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

## Obliczenie wskaźników na poziomie indywidualnym na podstawie zbiorów z 1. rundy monitoringu

Aby przygotować zbiór danych ze wskaźnikami opisującymi sytuację edukacyjno-zawodową poszczególnych absolwentów na podstawie wyników 1. rundy monitoringu należy użyć funkcji `oblicz_wskazniki_ind_1rm()`.

Zakładając, że wcześniej wykonane zostały czynności opisane w poprzedniej sekcji i przetworzone zbiory danych z 1. rundy monitoringu (z zaimputowanymi brakami danych czasu rozpoczęcia i zakończenia *epizodów*) znajdują się w obiekcie `dane1RM` wystarczy wywołać kod:

```r
wskaznikiInd = oblicz_wskazniki_ind_1rm(dane1RM)
```

Szczegółowy opis sposobu obliczania wskaźników zawiera [dokumentacja procedur obliczania wskaźników na podstawie danych z 1. rundy monitoringu](./inst/doc/runda_1-wskazniki.md).

## Obliczenie wskaźników na poziomie szkół i grup porównawczych na podstawie zbiorów z 1. rundy monitoringu

Aby obliczyć wskaźniki na poziomie szkół oprócz zbioru danych ze wskaźnikami na poziomie indywidualnym potrzebny jest również zbiór ze wskaźnikami średnich wynagrodzeń i bezrobocia w powiatach pobranych z Banku Danych Lokalnych GUS. Sposób przygotowania takiego zbioru opisany został w specjalnej dokumentacji: [wskaźniki z danych BDL](./inst/doc/wskazniki_z_danych_BDL.md). Poniżej założymy, że zbiór danych z wartościami wskaźników z BDL (pobrany przy pomocy funkcji `pobierz_dane_bdl()`) został, tak jak w przykładzie z ww. dokumentacji, przypisany do obiektu o nazwie `wskaznikiBdl` i zapisany w pliku *wskazniki_BDL.RData*. Aby przyłączyć wskaźniki z BDL do zbioru wskaźników indywidualnych, należy w tej sytuacji wykonać kod:

```r
load("wskazniki_BDL.RData")
wskaznikiBdl = przeksztalc_dane_bdl(wskaznikiBdl, 2017, "SZK_")

wskaznikiInd = left_join(wskaznikiInd, wskaznikiBdl)
```

Teraz można przystąpić do przygotowania zestawień wskaźników zagregowanych na potrzeby późniejszego generowania raportów szkół z wykorzystaniem pakietu [MLASZraporty](https://github.com/tzoltak/MLASZraporty):

  + na poziomie szkół,
  + na poziomie typów szkół, które przy tworzeniu raportów posłużą jako punkt odniesienia dla ww. wskaźników na poziomie szkół.
  
Służą do tego odpowiednio funkcje `agreguj_wskazniki_szk()` i `agreguj_wskazniki_typ_szk()`. Przygotowane przy ich pomocy zbiory można następnie zapisać, w celu późniejszego wykorzystania do przygotowania raportów szkół.

```r
wskaznikiSzk = agreguj_wskazniki_szk(wskaznikiInd)
wskaznikiTypSzk = agreguj_wskazniki_typ_szk(wskaznikiInd)
save(wskaznikiSzk, wskaznikiTypSzk, file = "wskazniki_szkol.RData")
```

Szczegółowy opis sposobu obliczania wskaźników zawiera [dokumentacja procedur obliczania wskaźników na podstawie danych z 1. rundy monitoringu](./inst/doc/runda_1-wskazniki.md)

W perspektywie tworzenia raportów na podstawie przygotowanego w wyżej opisany sposób zbioru danych, należy jeszcze zwrócić uwagę, że szablon 'raport_szkoly.Rmd' zaimplementowany w pakiecie [MLASZraporty](https://github.com/tzoltak/MLASZraporty) wymaga dołączenia do zbioru wskaźników na poziomie szkół trzech dodatkowych zmiennych, których nie daje się wygenerować na podstawie zbiorów z wynikami sondaży z 1. rundy monitoringu (konieczne jest odwołanie się w tym celu do danych z operatu losowania próby do badania):

  + `SZK_nazwa` - nazwa szkoły,
  + `SZK_adres` - adres szkoły,
  + `SZK_l_uczn_pop` - liczba uczniów w szkole należących do badanej populacji (w odróżnieniu zarówno od liczby uczniów wylosowanych do badania, jak i od liczby uczniów, których udało się zbadać).

### Eksport zbiorów wskaźników na poziomie zagregowanym

Przy pomocy funkcji `splaszcz_wskazniki_zagregowane()` możliwe jest też przekształcenie przygotowanych zbiorów wskaźników na poziomie zagregowanym do postaci *płaskich* ramek danych (tzn. niezawierających kolumn-list), co umożliwia zapisanie ich w formacie SPSS lub Staty:

```r
wskaznikiSzkEksport = splaszcz_wskazniki_zagregowane(wskaznikiSzk)
library(haven)
write_dta(wskaznikiSzkEksport, "wskazniki_szkol.dta")
write_sav(wskaznikiSzkEksport, "wskazniki_szkol.sav")
```

## Przygotowanie zbiorów z pilotażowej rundy monitoringu

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
