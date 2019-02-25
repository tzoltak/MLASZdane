![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

[![Travis build status](https://travis-ci.org/tzoltak/MLASZraporty.svg?branch=master)](https://travis-ci.org/tzoltak/MLASZraporty)
[![Coverage status](https://codecov.io/gh/tzoltak/MLASZraporty/branch/master/graph/badge.svg)](https://codecov.io/github/tzoltak/MLASZraporty?branch=master)

# MLASZdane

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój

Pakiet służy do złączania danych z rejestrów, baz danych administracyjnych, baz danych wyników egzaminów i wyników badań sondażowych na potrzeby projektu MLEZAiMD.

# Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN, więc trzeba instalować go ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalację najproście przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/MLASZdane')
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.
