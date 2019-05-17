---
title: "Przetwarzanie zbiorów z 1. rundy monitoringu"
author: "Tomasz Żółtak"
date: "17 maja 2019"
output: rmarkdown::html_vignette
lang: pl
vignette: >
  %\VignetteIndexEntry{Przetwarzanie zbiorów z 1. rundy monitoringu}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# 1. Wprowadzenie

### 1.1. Funkcja wczytaj_wyniki_1rm()

Na podstawie zbioru z badania absolwentów (*MLEZAiMD_I_runda_CAPI_absolwent_n7713_20180924_z_wagami_z_kodowaniem.sav*) funkcja `wczytaj_wyniki_1rm()` zwraca listę ramek danych (zbiorów) zawierającą następujące elementy:

  - `dane` - zawiera odpowiedzi na pytania niedotyczące *epizodów* (por. niżej);
  - `epizody` - zawiera informacje dotyczące wymienionych przez respondentów *epizodów*:
    + nauki w szkole, jako uczeń której badany został zrekrutowany do badania (ważny ze względu na informację o momencie zakończenia tej nauki);
    + nauki w LO dla dorosłych,
    + zdawania do szkół policealnych i nauki w szkołach policealnych,
    + zdawania na studia i studiowania,
    + odbywania szkoleń i zdobywania certyfikatów (niezwiązanych bezpośrednio z nauką w ww. formach),
    + pracy,
    + bezrobocia;
  - `gospDom` - zawiera informacje o członkach gospodarstw domowych respondentów.

### 1.2. Funkcja imputuj_miesiac_pk_1rm()

Funkcja `imputuj_miesiac_pk_1rm()` przetwarza listę zbiorów danych, zwróconą przez `wczytaj_wyniki_1rm()` i zwraca listę zbiorów danych o dokładnie takiej samej strukturze (p. wyżej).

Metody imputacji wartości zmiennych opisujących moment rozpoczęcia i moment zakończenia *epizodów* opisane zostały szczegółowo w sekcji 3. tego dokumentu.

### 1.3. Funkcja przygotuj_zbior_osobo_miesiecy_1rm()

Funkcja `przygotuj_zbior_osobo_miesiecy_1rm()` przetwarza listę zbiorów danych, zwróconą przez `imputuj_miesiac_pk_1rm()` i zwraca ramkę danych (zbiór) zawierający dane o *epizodach* nauki, pracy i bezrobocia przekształcone do postaci osobo-miesięcy (obserwacją jest osoba w danym miesiącu).

Struktura zbioru i sposób jego tworzenia zostały szczegółowo opisane w sekcji 6. tego dokumentu.

## 1.4. Identyfikator respondenta

Identyfikatorem respondenta w zbiorach jest zmienna `ID_RESP`.

# 2. Struktura zbioru danych *epizodów*

## 2.1. Zmienne specjalne w zbiorze *epizodów*

Aby możliwe było łatwiejsze identyfikowanie *epizodów*, w zbiorze utworzone zostały specjalne zmienne (znajdują się na początku zbioru):

  - `ID_RESP` - identyfikator respondenta, umożliwia łączenie z innymi zbiorami danych z badania;
  - `typ_epizodu` - typ *epizodu*, przyjmuje jedną z dziesięciu wartości:
     1. 'szkoła objęta badaniem';
     2. 'LO dla dorosłych';
     3. 'zdawanie do SPolic.';
     4. 'SPolic.';
     5. 'zdawanie na studia';
     6. 'studia';
     7. 'szkolenia u2';
     8. 'szkolenia u4';
     9. 'praca';
     10. 'bezrobocie'.
     + **Uwaga!** Epizody 'studia' i 'SPolic.' są specyficzne, gdyż zawierają również informacje dotyczące zdawania na te kierunki studiów/do tych szkół policealnych, na/w których respondent podjął naukę. W związku z tym, jeśli przedmiotem analizy mają być kierunki/szkoły na/do których respondent zdawał, należy w niej uwzględnić odpowiednio zarówno *epizody* typu 'zdawanie na studia', jak i *epizody* typu 'studia' albo zarówno *epizody* typu 'zdawanie do SPolic.', jak i *epizody* typu 'SPolic.'.
  - `nr` - wartość zmiennej wskazuje, jako który z kolei *epizod* (inaczej mówiąc, w którym *obiegu pętli skryptu*, czy też, w którym *wierszu tabeli reprezentującej odpowiedzi na blok pytań o „epizody” danego typu w „papierowej” wersji kwestionariusza*) określonego typu respondent wymienił dany *epizod*;
     + **Uwaga!** Respondenci niekoniecznie wymieniali *epizody* w zgodzie z ich chronologiczną kolejnością. Jeśli chce się mieć pewność, że *epizody* uszeregowane są w kolejności chronologicznej, należy posłużyć się w pierwszej kolejności zmiennymi `czas_rozp` lub `czas_zakon`, a dopiero w dalszej zmienną `nr` (przy czym należy mieć na uwadze, że w zmiennych `czas_rozp` i `czas_zakon` występują braki danych, a w przypadku zmiennej `czas_zakon` należy też obsłużyć sytuacje, gdy *epizod* się nie zakończył).
     + Zmienna `nr` odnosi się łącznie do *epizodów* typu 'zdawanie do SPolic.' i 'SPolic.' (tj. epizody obu tych typów w ramach tej samej osoby posiadają jedną, ciągłą *numerację*) oraz łącznie do *epizodów* typu 'zdawanie na studia' i 'studia', co wiąże się z opisaną wyżej specyfiką *epizodów* typów 'studia' i 'SPolic.'.
     + Dla *epizodów* typu 'LO dla dorosłych' jako wartość zmiennej `nr` występuje tylko 1, gdyż kwestionariusz nie dopuszczał wymienienia kilku *epizodów* tego typu.
     + W przypadku *epizodów* typu 'praca' **wartość zmiennej `nr` równa 99** oznacza, że dany *epizod* pracy został *dopowiedziany* w wyniku udzielenia przez respondenta odpowiedzi na pytania PG5.
  - `czas_rozp` - czas rozpoczęcia trwania danego *epizodu*, wyrażony jako **liczba miesięcy od czerwca 2017 r.**;
    + zmienna może przyjmować wartości ujemne (typowo zdarza się to w odniesieniu do *epizodów* pracy);
    + nie ma zastosowania dla *epizodów* typu 'szkolenia u2' ani 'szkolenia u4';
    + **jeśli respondent nie podał miesiąca**, w jakim rozpoczął się dany *epizod*, dla epizodów typu 'praca' i 'bezrobocie' dokonano imputacji wartości zmiennej `czas_rozp` (p. odpowiednia sekcja); dla epizodów pozostałych typów zawsze arbitralnie przyjmowano, że był to czerwiec;
    + należy mieć na uwadze, że mimo to zdarzają się *epizody* (do których ma zastosowanie data rozpoczęcia) z brakami danych w tej zmiennej (respondenci czasem nie byli w stanie podać nawet roku);
  - `czas_zakon` - czas zakończenia trwania danego *epizodu*, wyrażony jako **liczba miesięcy od czerwca 2017 r.**;
    + zmienna może przyjmować wartości ujemne (typowo zdarza się to w odniesieniu do *epizodów* pracy);
    + nie ma zastosowania dla *epizodów* typu 'zdawanie na studia' i 'zdawanie do SPolic.';
    + **jeśli respondent nie podał miesiąca**, w jakim zakończył się dany *epizod*, dla epizodów typu 'praca' i 'bezrobocie' dokonano imputacji wartości zmiennej `czas_zakon` (p. odpowiednia sekcja); dla epizodów pozostałych typów zawsze arbitralnie przyjmowano, że był to lipiec;
    + należy mieć na uwadze, że mimo to zdarzało się, że respondenci nie byli w stanie podać nawet roku i jako wartość zmiennej występuje brak danych;
    + **brak danych w zmiennej `czas_zakon` może też wskazywać na to, że dany epizod wciąż trwa** - aby sprawdzić, czy mamy z tym do czynienia, należy posłużyć się zmienną `czy_zakonczony`.
  - `czy_zakonczony` - dychotomiczna zmienna wskazująca, czy dany *epizod* się zakończył, czy w momencie realizacji wywiadu z respondentem wciąż trwał:
    + dla *epizodów* typu 'LO dla dorosłych', 'studia', 'SPolic.', 'szkolenia u2' i 'szkolenia u4' wartość zmiennej została przekodowana ze zmiennych odpowiednio: `zp2c`, `sp6d`, `pp6d` (jako wskazujące na zakończenie *epizodu* traktowano odpowiedzi mówiące o uzyskaniu dyplomu, jak i o przerwaniu nauki) oraz `u2d`;
    + dla *epizodów* typu 'praca' i 'bezrobocie' wartość zmiennej została zakodowana na podstawie zmiennych odpowiednio: `pg2f`, i `pb1e`;
    + **przy kodowaniu wartości dla *epizodów* wszystkich ww. typów, jeśli respondent nie udzielił odpowiedzi na odpowiednie pytanie, przyjmowano, że *epizod* się zakończył**; 
    + dla *epizodów* typu 'zdawanie na studia' i 'zdawanie do SPolic.' wartością zmiennej jest brak danych.
  - `swiadectwo` - dychotomiczna zmienna, w przypadku *epizodów* typu 'LO dla dorosłych', 'studia', 'SPolic.', 'szkolenia u2' i 'szkolenia u4' wskazująca, czy dany *epizod* zakończył się uzyskaniem świadectwa (w odniesieniu do trzech pierwszych typów można to utożsamiać z *zakończeniem się sukcesem*, przypadku szkoleń respondent mógł wymienić kursy, które nie dawały możliwości uzyskania formalnego potwierdzenia ich ukończenia); dla epizodów typu `zdawanie na studia` i `zdawanie do SPolic.` zawsze przyjmuje wartość 2 ('nie'), dla epizodów 'pracy' i 'bezrobocia' wartością jest brak danych;
  - `czas_rozp_imput` - dychotomiczna zmienna opisująca, czy wartość zmiennej `czas_rozp` została zaimputowana  (p. sekcja nt. imputacji); jeśli `czas_rozp` jest brakiem danych, również `czas_rozp_imput` jest brakiem danych;
  - `czas_zakon_imput` - dychotomiczna zmienna opisująca, czy wartość zmiennej `czas_zakon` została zaimputowana  (p. sekcja nt. imputacji); jeśli `czas_zakon` jest brakiem danych, również `czas_zakon_imput` jest brakiem danych;
  - `czas_zakon_sz` - zmienna opisująca czas zakończenia przez respondenta nauki w szkole, jako uczeń której trafił on do badania - wyrażona na tej samej skali co zmienne `czas_rozp` i `czas_zakon`;
    + zmienna ta pozwala w łatwy sposób przejść od skali czasu zaczepionej w miesiącu kalendarzowym, w jakiej wyrażone są domyślnie zmienne `czas_rozp` i `czas_zakon` do skali zaczepionej w momencie zakończenia nauki w szkole zawodowej (jako absolwent której respondent ma być badany).
     
## 2.2. Różnice w nazwach i etykietach względem oryginalnego zbioru SPSS

  - Nazwy wszystkich zmiennych w zbiorach przekształconych do postaci *długiej*, z jedynym wyjątkiem zmiennej `ID_RESP`, przekształcone zostały na pisane małymi literami.
  - Nazwy zmiennych w tworzonych zbiorach zasadniczo odpowiadają nazwom zmiennych z oryginalnego pliku w formacie SPSS, z tym że pozbawione zostały elementów wskazujących na *nr pętli* (w zależności od zmiennej przyrostek tego rodzaju mógł występować albo na końcu nazwy zmiennej, albo nieco wcześniej).
     + Wyjątkiem są zmienne związane z pytaniami o pierwszą i ostatnią pracę oraz zmienne opisujące odpowiedzi na pytania z tabeli U4 (p. niżej).
  - Ze względu na ograniczenia techniczne (biblioteki służącej do zapisu z R plików w formacie Staty) wartości zmiennych, które mają mieć przypisane etykiety wartości, muszą stanowić (nieprzerwany) ciąg kolejnych liczb całkowitych, począwszy od 1. W związku z tym **wartości powiązane z niektórymi etykietami w tworzonych zbiorach mogą różnić się od tych, jakie są im przypisane w oryginalnym pliku w formacie SPSS**.
     + Typowo dotyczy to kodów specjalnych opisujących tzw. *braki danych użytkownika*.
     + W praktyce ma to jednak niewielkie znaczenie, gdyż wartości *braków danych użytkownika* przy przekształcaniu do postaci długiej były przekodowywane na *systemowe* braki danych.
     + Wszystkie zmienne, które mają etykiety wartości "tak"-"nie" zakodowane są w zbiorach w postaci długiej wg schematu: 1-tak, 2-nie (w zmiennych `sp6h_1` - `sp6h_3` i `pp6i_1` - `pp6i_3` w oryginalnym zbiorze SPSS wartości kodowane były wg schematu 0-nie, 1-tak).
  - Braki danych w zmiennych tekstowych oznaczone są ciągiem znaków składającym się z pojedynczej kropki: '.' (Stata nie akceptuje pustych ciągów znaków).

## 2.3. Epizody dotyczące zdawania do szkół policealnych lub na studia

  - Specyfika podziału na *epizody* typu 'studia' i 'SPolic.' oraz epizody typu 'zdawanie na studia' i 'zdawanie do SPolic.' została omówiona w sekcji *Zmienne specjalne w zbiorach w postaci „długiej”*.
  - Na podstawie zmiennych odpowiednio `pp3a` i `sp3a` utworzona została zmienna `czy_preferowany` opisująca to, czy szkoła lub kierunek był preferowany przez respondenta (spośród potencjalnie kilku, do których/na które zdawał) przy pomocy wartości: 1-tak, 2-nie.
    + Jeśli respondent zdawał tylko do jednej szkoły czy na jedne studia, w zmiennej `czy_preferowany` przypisana została do danej szkoły/kierunku wartość 1 (tak).
    + Jeśli respondent nie wskazał preferowanego kierunku/szkoły, wartością zmiennej jest brak danych.

## 2.4. Informacje o odbytych szkoleniach i zdobytych uprawnieniach

Pytania o zdobyte uprawnienia i odbyte szkolenia zadawane były w kwestionariuszu w dwóch blokach: U2 i U4, z których pierwszy w założeniach odnosił się do szkoleń kończących się uzyskaniem oficjalnych certyfikatów (względnie uzyskania takich certyfikatów bez dedykowanego szkolenia), a drugi do pozostałych szkoleń. Trudno jednak powiedzieć, w jakim stopniu respondenci odpowiadając trzymali się tego podziału.

Pytania w obu blokach były analogiczne, poza tym, że w bloku U2 wystąpiły dwa pytania (o sam egzamin), których nie było w bloku U4.
W związku z tym nazwy zmiennych opisujących odpowiedzi na pytania z bloku U4 zostały zmienione na zaczynające się od *u2* (zamiast *u4*), tak aby epizody obu tych typów (tj. *szkolenia u2* i *szkolenia u4*) można było analizować łącznie. Dodatkowo nazwę zmiennej `u4g` zmieniono na `u2h`, gdyż w przypadku tych dwóch zmiennych w kwestionariuszu przypisano inne sufiksy literowych do analogicznych pytań.

Blok pytań, w którym opisany został dany epizod, można zidentyfikować na podstawie zmiennej `typ_epizodu`.

## 2.5. Epizody pracy

  - Zmienna `pg1b` podaje kod pracodawcy, który w sytuacji, gdy respondent miał wiele *epizodów* pracy, pozwala stwierdzić, czy była to praca u różnych, czy u tego samego pracodawcy. Kody (numery) pracodawców pozwalają ich identyfikować tylko w ramach rekordów opisujących tego samego respondenta.
    + **W zbiorze utworzonym na podstawie wyników 1. fali badania** (*uczniowie_CAPI-prace.dta*) występuje tu pewne ograniczenie: dla *epizodów* pracy, które zostały *dopowiedziane* (w odpowiedzi na pytanie, czy respondent nie przypomniał sobie żadnych dodatkowych *epizodów* pracy), nazwa pracodawcy nie została przez PBS zakodowana w sposób analogiczny do tego, jak postąpiono w zasadniczej pętli pytań o pracę. W związku z tym konieczne było przyjęcie arbitralnego założenia, że *epizod* ten dotyczy innego pracodawcy, niż wcześniej wymienieni. W takim przypadku jako kod pracodawcy wpisana została wartość 99.
  - Zakres informacji o *epizodach* pracy różni się w zależności od tego, czy był to pierwszy, ostatni, czy któryś inny *epizod* pracy. W przypadku pierwszych i ostatnich epizodów dołączono bowiem do zbioru informacje o odpowiedziach na pytania odpowiednio PI0-PI5 oraz PO1-PO11.
    + W przypadku pytań P1-P5 i PO1-PO5, które zadawane były w tej samej formie, utworzone zostały w zbiorze wspólne zmienne opisujące udzielone na nie odpowiedzi, zastępując w& nazwach oryginalnych zmiennych przedrostki "pi" i "po" wspólnym przedrostkiem "pio".
  - Chociaż pytania PO7-PO11 były zadawane wszystkim respondentom, którzy wymienili choć jeden *epizod* pracy, to jednak nie miały one sensu dla tych, którzy w momencie realizacji wywiadu nie pracowali (przez pomyłkę nie dodano filtru w ankiecie). Dla tych osób odpowiedzi udzielone na te pytania zostały zastąpione brakami danych.
  - Dokonane zostało *domknięcie* odpowiedzi na pytanie o formę zatrudnienia, podanych przez respondentów w formie opisowej (oryginalne odpowiedzi znajdują się  zmiennej `ABS_pg2h_in`):
    + odpowiedzi dające się zidentyfikować jako pasujące wprost do podanej w pytaniu kafeterii przekodowano odpowiednio;
    + "kontrakt" został potraktowany jako samozatrudnienie (zmiennej `ABS_pg2h` przypisano wartość 5);
    + różne typy staży zostały dołączone do stażu z UP (zmiennej `ABS_pg2h` przypisano wartość 7);
    + pracę u rodziców na polu potraktowano jako pracę *u siebie w gospodarstwie* (zmiennej `ABS_pg2g` przypisano wartość 7, zmiennej `ABS_pg2h` brak danych);

## 2.6. Epizody bezrobocia

  - W odpowiedziach na pytanie o przyczynę niepodejmowania pracy ciążę wymienianą w części "inne" przekodowano na "opiekę nad dzieckiem" (zmiennej `pb1g` przypisano wartość 2).

# 3. Imputacja wartości zmiennych czas_rozp i czas_zakon

## 3.1. Arbitralne przypisanie wartości

W przypadku wszystkich epizodów innych typów niż 'praca' i 'bezrobocie', jeśli respondent podał rok, ale nie podał miesiąca rozpoczęcia/zakończenia *epizodu*, arbitralnie przyjmowano, że rozpoczął się on w lipcu lub skończył w czerwcu.

  - W przypadku *epizodów* nauki sprawdzano, czy tak przypisana wartość zmiennej `czas_zakon` nie okazuje się mniejsza, niż wartość zmiennej `czas_rozp` (może do tego dojść, jeśli respondent *wypadł* ze szkoły jeszcze w tym samym roku, w którym rozpoczął w niej naukę). Jeśli tak było jako wartość `czas_zakon` arbitralnie przypisywano liczbę o 1 większą od wartości `czas_rozp`.

Tak samo postąpiono z datami rozpoczęcia/zakończenia epizodów pracy i bezrobocia, co do których respondenci zadeklarowali, że były one w innych latach niż 2017 lub 2018 (ze względu na niewielką ogólną liczbę *epizodów* obejmujących taki wcześniejszy okres nie ma możliwości sensownie zaimputować tych czasów na podstawie modelu regresji).

  - Nie zdarzyło się, aby epizod o przypisanej w ten sposób wartości zmiennej `czas_rozp` miał niepełną informację dotyczącą czasu zakończenia epizodu (tzn. tak przypisane wartości zmiennej `czas_rozp` nie zostały ani razu wykorzystane w procedurze opisanej w następnej sekcji).

Tak przypisane wartości oznaczone są w zmiennych `czas_rozp_imput` i `czas_zakon_imput` jako zaimputowane.

## 3.2. Uzupełnianie wartości zmiennych `czas_rozp` i `czas_zakon` w epizodach pracy i bezrobocia nieoznaczane jako imputacja

Jeśli respondent dla danego *epizodu*:

  - nie podał informacji koniecznych do obliczenia wartości zmiennej `czas_rozp`, ale dało się ustalić wartość zmiennej `czas_zakon` lub
  - nie podał informacji koniecznych do obliczenia wartości zmiennej `czas_zakon`, ale dało się ustalić wartość zmiennej `czas_rozp`,

a także udzielił odpowiedzi na pytanie o szacunkową długość trwania epizodu (zmienne `pg2x`, `pb1x`), to wartość *brakującej* zmiennej zmiennej obliczano, odpowiednio dodając lub odejmując długość trwania epizodu do/od znanego momentu rozpoczęcia, lub zakończenia.

Obliczone w ten sposób wartości **nie są** w zmiennych 'czas_rozp_imput` i `czas_zakon_imput` oznaczane jako zaimputowane.

## 3.3. Imputacja na podstawie modeli regresji

Imputacji brakujących wartości zmiennych `czas_rozp` i `czas_zakon` dla *epizodów* typu 'praca' i 'bezrobocie' dokonano przy użyciu modeli regresji liniowej.

  - Imputację przeprowadzono tylko w sytuacji, gdy brakowało informacji o miesiącu, ale była podana informacja o roku rozpoczęcia/zakończenia *epizodu*.
  - Modele estymowane były oddzielnie dla każdego z tych dwóch typów *epizodów*.
  - W procedurze imputacji (zarówno przy estymacji modeli, jak i przy samym imputowaniu braków danych) brano pod uwagę wyłącznie epizody, które rozpoczęły się w 2017 r. lub później.

### 3.3.1. Zmienna czas_rozp

  - Zmienną zależną w modelu była `czas_rozp`.
  - Jako predyktory wykorzystano:
    1) typ szkoły (dwie zmienne *dummy* zakodowane na podstawie zmiennej `typ_szkoly`);
    2) płeć respondenta (zmienna `m1`);
    3) formę pracy lub rozróżnienie na bezrobotnych (poszukujących pracy) i biernych zawodowo - p. opis zmiennych `praca` i `bezrobocie` w sekcji *Zbiór z danymi w postaci osobo-miesięcy* (przekodowane na zestaw zmiennych *dummy*, przy czym brak danych uwzględniono przy przekodowaniu jako *legalną* wartość);
    4) rok, w którym rozpoczął się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennej *dummy*);
    5) liczbę wszystkich *epizodów* pracy danego respondenta;
    6) liczbę wszystkich *epizodów* bezrobocia (lub nieaktywności zawodowej) danego respondenta;
    7) informację o tym, czy w (dowolnym momencie) 2017 roku respondent brał udział w edukacji formalnej, a jeśli tak, to w jakiej formie (4 zmienne *dummy*);
    8) informację o tym, czy w (dowolnym momencie) 2018 roku respondent brał udział w edukacji formalnej, a jeśli tak, to w jakiej formie (4 zmienne *dummy*);
    9) informację o tym, w którym miesiącu został przeprowadzony wywiad z respondentem (zmienna `r5s2` traktowana jako zmienna ciągła);
    10) interakcje pomiędzy 1) a 2), 4), 5), 6) i 9);
    11) interakcje pomiędzy 2) a 3), 4), 5), i 6).
  - Model osiągał następujące statystyki dopasowania (na zbiorze epizodów, dla których znane były wartości zmiennej `czas_rozp`):
    + Dla *epizodów* typu 'praca': R^2^ = 0,68; odsetek poprawnych klasyfikacji = 16,6%.
    + Dla *epizodów* typu 'bezrobocie': R^2^ = 0,74; odsetek poprawnych klasyfikacji = 38,7%.
  - Przy dokonywaniu imputacji przewidywania wynikające z modelu zaokrąglano do najbliższych liczb całkowitych (tak aby zmienna `czas_rozp` również po imputacji przyjmowała tylko wartości całkowite).
    + Ww. odsetki poprawnych klasyfikacji zostały obliczone na podstawie tak zaokrąglonych wartości (ale wartość R^2^ podano dla niezaokrąglonych przewidywań).
  - Po dokonaniu imputacji dokonano sprawdzenia:
    + Czy zaimputowana wartość zmiennej zgadza się ze znanym rokiem rozpoczęcia *epizodu*.
    + Czy zaimputowana wartość zmiennej nie wskazuje, że *epizod* rozpoczął się później, niż się zakończył (dla *epizodów*, dla których znana była wartość zmiennej `czas_zakon`, ale wartość zmiennej `czas_rozp` była imputowana).
    + Nie stwierdzono jednak wystąpienia żadnego z ww. problemów. 

Jak widać, skuteczność modelu imputacji, zwłaszcza w odniesieniu do epizodów pracy, pozostawia nieco do życzenia. W praktyce mamy do czynienia z pewnym *zbijaniem* imputowanych czasów rozpoczęcia w okolicach wartości 2-3 oraz 8-9 (w zależności od roku, w którym zaczął się epizod). Jednakże biorąc pod uwagę, że możemy w ten sposób *odratować* blisko 100 *epizodów* pracy, niedokładność ta wydaje się możliwa do zaakceptowania.

### 3.3.2. Zmienna czas_zakon

  - Zmienną zależną w modelu była długość trwania *epizodu* (tj. różnica pomiędzy `czas_zakon` a `czas_rozp`).
    + Przy imputacji wartość zmiennej `czas_zakon` określano, dodając do wartości zmiennej `czas_rozp` wartość przewidywania wynikającą z modelu (zaokrągloną do najbliższej liczby całkowitej).
  - Jako predyktory wykorzystano:
    1) typ szkoły (dwie zmienne *dummy* zakodowane na podstawie zmiennej `typ_szkoly`);
    2) płeć respondenta (zmienna `m1`);
    3) formę pracy lub rozróżnienie na bezrobotnych (poszukujących pracy) i biernych zawodowo - p. opis zmiennych `praca` i `bezrobocie` w sekcji *Zbiór z danymi w postaci osobo-miesięcy* (przekodowane na zestaw zmiennych *dummy*, przy czym brak danych uwzględniono przy przekodowaniu jako *legalną* wartość);
    4) rok, w którym rozpoczął się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennej *dummy*);
    5) rok, w którym zakończył się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennej *dummy*);
    6) interakcję 4) i 5);
    7) liczbę wszystkich *epizodów* pracy danego respondenta;
    8) liczbę wszystkich *epizodów* bezrobocia (lub nieaktywności zawodowej) danego respondenta;
    9) informację o tym, w którym miesiącu został przeprowadzony wywiad z respondentem (zmienna `r5s2` traktowana jako zmienna ciągła);
    10) interakcje pomiędzy 1) a 2), 4), 7, i 8);
    11) interakcje pomiędzy 2) a 3)-8);
    12) interakcje pomiędzy 3) a 5);
    13) interakcje pomiędzy 4) a 7) i 8);
    13) interakcje pomiędzy 5) a 7), 8) i 9).
  - Model osiągał następujące statystyki dopasowania (na zbiorze epizodów, dla których znane były wartości zmiennej `czas_zakon`):
    + Dla *epizodów* typu 'praca': R^2^ = 0,66; odsetek poprawnych klasyfikacji = 27,4%.
    + Dla *epizodów* typu 'bezrobocie': R^2^ = 0,71; odsetek poprawnych klasyfikacji = 27,5%.
  - Przy dokonywaniu imputacji przewidywania wynikające z modelu zaokrąglano do najbliższych liczb całkowitych (tak aby zmienna `czas_zakon` również po imputacji przyjmowała tylko wartości całkowite).
    + Ww. odsetki poprawnych klasyfikacji zostały obliczone na podstawie tak zaokrąglonych wartości (ale wartość R^2^ podano dla niezaokrąglonych przewidywań).
  - Po dokonaniu imputacji dokonano sprawdzenia:
    + Czy zaimputowana wartość zmiennej zgadza się ze znanym rokiem zakończenia *epizodu*.
    + Czy zaimputowana wartość zmiennej nie wskazuje, że *epizod* zakończył się wcześniej, niż się rozpoczął.
    + Nie stwierdzono jednak wystąpienia żadnego z ww. problemów.

Także tutaj skuteczność modelu imputacji pozostawia nieco do życzenia. W praktyce model imputacji nie jest w stanie skutecznie przewidywać bardzo długich (można by powiedzieć *nietypowo długich*) *epizodów*. W odniesieniu do tych trwających od 0 do 9 miesięcy (a więc typowych, które mogą się pojawić w zbiorze) jest co prawda nie bardzo dokładny, ale jednak dający sensowne przewidywania.

## 3.4. Podsumowanie liczby braków danych dot. czasu rozpoczęcia i zakończenia epizodów w danych z badania absolwentów

**Bez dokonania opisanych powyżej przypisań i imputacji**

|typ_epizodu       | ogółem epizodów| b.d. czas_rozp|    %| b.d. czas_zakon|    %|
|:-----------------|---------------:|--------------:|----:|---------------:|----:|
|bezrobocie        |            2490|            105|  4,2|              48|  1,9|
|LO dla dorosłych  |             978|             15|  1,5|               8|  0,8|
|praca             |            6987|            328|  4,7|             125|  1,8|
|SPolic.           |             386|              9|  2,3|               1|  0,3|
|studia            |            1564|             15|  1,0|              13|  0,8|
|szkolenia u2      |            3222|               |     |             956| 29,7|
|szkolenia u4      |             267|               |     |              67| 25,1|

**Po dokonaniu opisanych powyżej przypisań i imputacji**

|typ_epizodu       | ogółem epizodów| b.d. czas_rozp|    %| b.d. czas_zakon|    %|
|:-----------------|---------------:|--------------:|----:|---------------:|----:|
|bezrobocie        |            2490|             83|  3,3|              39|  1,6|
|LO dla dorosłych  |             978|              6|  0,6|               5|  0,5|
|praca             |            6987|            147|  2,1|              78|  1,1|
|SPolic.           |             386|              2|  0,5|               1|  0,3|
|studia            |            1564|             12|  0,8|              11|  0,7|
|szkolenia u2      |            3222|               |     |             192|  6,0|
|szkolenia u4      |             267|               |     |              18|  6,7|

Jak widać, przeprowadzone działania nie doprowadziły do dużych zmian w zbiorze, z wyjątkiem *epizodów* typu 'szkolenia u2' i 'szkolenia u4', w odniesieniu do których respondenci mieli bardzo duże problemy z precyzyjnym podaniem daty ich zakończenia. Warto jednak odnotować, że o ponad połowę udało się zmniejszyć liczbę braków danych czasu rozpoczęcia w przypadku epizodów pracy.

# 4. Zbiory z informacjami o członkach gospodarstw domowych

  - Do zbioru dołączono zmienną `m10c_wiek`, zawierająca wiek przeliczony z daty urodzenia.

# 5. Uwagi o analizie zbioru *epizodów*

Typowo zbiór *epizodów* wykorzystywane będzie w analizie w ten sposób, że:

  - najpierw dokonane zostanie zawężenie zbioru do *epizodów* określonego typu (*epizody* innych typów zostaną usunięte);
  - następnie na tak zawężonym zbiorze wykonane zostaną pewne procedury agregacji w ramach respondentów (aby uzyskać interesujące wskaźniki);
  - następnie, tak zagregowane wyniki będą dołączane do oryginalnego zbioru danych (elementu `dane` listy zwracanej przez funkcję `wczytaj_wyniki_1rm()`, w którym jednemu respondentowi odpowiada jeden wiersz) w celu przeprowadzenia dalszych analiz.
  
Należy mieć przy tym na uwadze, że znaczna część respondentów mogła nie mieć żadnego *epizodu* właśnie analizowanego typu. W szczególności oznacza to, że:

  - Typowo przy dołączaniu wyników zagregowanych do poziomu respondentów na podstawie zbioru *epizodów* do oryginalnego zbioru (w którym jednostką obserwacji jest respondent) wielu osobom w oryginalnym zbiorze, obejmującym wszystkich respondentów, przypisane zostaną braki danych.
    + Istotne może być więc zakodowanie tych braków danych zaraz po dokonaniu łączenia w sposób, który pozwoli potem na wygodne (i przede wszystkim nieprowadzące do pomyłek, w szczególności związanych z przyjęciem złej podstawy procentowania) obsłużenie ich w prowadzonych dalej analizach.
    + Należy przy tym mieć na uwadze, że w zbiorze *epizodów* wartości *braków danych użytkownika* zostały już typowo zamienione na *systemowe* braki danych (a więc *systemowy* brak danych w połączonym zbiorze może dla wielu zmiennych równie dobrze wskazywać na to, że respondent nie miał żadnego *epizodu* danego typu, co na to, że taki *epizod* miał, ale nie był w stanie podać o nim interesującej nas informacji).
    + W związku z tym **dobrą praktyką przy agregacji zbioru *epizodów* do poziomu respondentów jest utworzenie w nim takiej zmiennej (innej niż `ID_RESP`), co do której będziemy mieć pewność, że dla wszystkich w zagregowanym zbiorze przyjmie ona wartość niebędącą brakiem danych** i będzie ją można potem wygodnie wykorzystać do stwierdzenia, którzy respondenci w oryginalnym zbiorze danych mieli choć jeden *epizod* interesującego nas typu.
  - Jeśli zagregowany do poziomu respondentów zbiór *epizodów* będzie analizowany bez przyłączania do oryginalnego zbioru, w kontekście podstaw procentowania należy mieć na uwadze, że zawiera on typowo tylko podzbiór spośród wszystkich badanych.

# 6. Zbiór z danymi w postaci osobo-miesięcy

## 6.1. Struktura zbioru

W zbiorze zawarte zostały następujące zmienne opisujące cechy respondentów stałe w czasie:

  - `ID_RESP` - identyfikator respondenta, umożliwia łączenie z innymi zbiorami danych z badania;
  - `typ_szkoly` - typ szkoły, jako uczeń której respondent został zakwalifikowany do badania;
  - `r5s2` - miesiąc, w którym został przeprowadzony wywiad z respondentem;
  - `f6` - zmienna opisująca powód nieuzyskania świadectwa szkoły, jako uczeń której respondent został zakwalifikowany do badania;
    + jeśli respondent ukończył tę szkołę, zmiennej przypisany jest brak danych;
  - `f7` - rok uzyskania świadectwa szkoły, jako uczeń której respondent został zakwalifikowany do badania;
    + jeśli respondent nie uzyskał świadectwa tej szkoły, zmiennej przypisany jest brak danych;
  - `m1` - płeć respondenta;
  - `m2` - rok urodzenia respondenta;
    + w tej rundzie nie mamy tu ani braków, ani osób starszych, niż powinny się znaleźć w badaniu;
  - `m3` - klasa wielkości miejscowości zamieszkania w momencie przeprowadzania wywiadu z absolwentem.
  
W zbiorze zawarte zostały następujące zmienne opisujące statusy respondentów w poszczególnych miesiącach:

  - `data` - identyfikator miesiąca w formie *czytelnej*;
  - `czas` - identyfikator miesiąca w formie liczby - liczba miesięcy, jaka upłynęła od czerwca 2017 r. (a więc *domyślnego* momentu ukończenia szkoły, jako absolwent której respondent został zakwalifikowany do badania) do miesiąca, który opisuje dany rekord w zbiorze;
  - `status` - zmienna kodująca w syntetycznej formie status respondenta: wartości zmiennych (kolejno) `praca`, `nauka` i `bezrobocie` *sklejone* ze sobą w jeden ciąg znaków (składający się z trzech cyfr: cyfra setek koduje pracę, dziesiątek naukę, a jedności bezrobocie);
    + wartość zmiennej status równa '999' ma specjalne znaczenie - p. następna sekcja;
  - `praca` - zmienna kodująca status zatrudnienia;
  - `nauka` - zmienna kodująca status uczestnictwa w edukacji formalnej;
  - `bezrobocie` - zmienna kodująca, czy respondent deklarował się jako pozostający bez pracy;
  - `praca_a_bezrobocie` - zmienna opisująca ew. występowanie w danym miesiącu (dla danego respondenta) *konfliktów* pomiędzy statusem zatrudnienia a statusem bezrobocia, fakt skorygowania takiego *konfliktu* (p. sekcja *Sposób kodowania statusów*) lub jego przyczynę;
  - `korekta_ciaglosc_nauki` - zmienna pozwala zidentyfikować rekordy, w których status uczestnictwa w nauce formalnej został skorygowany ze względu na stwierdzenie ciągłości nauki (p. sekcja *Sposób kodowania statusów*);
  - `imput_praca` - zmienna wskazująca, że status zatrudnienia w danym miesiącu (dla danego respondenta) został zakodowany w oparciu o zaimputowaną wartość zmiennej `czas_rozp` lub zmiennej `czas_zakon`;
  - `imput_nauka` - zmienna wskazująca, że status uczestnictwa w nauce formalnej w danym miesiącu (dla danego respondenta) został zakodowany w oparciu o zaimputowaną wartość zmiennej `czas_rozp` lub zmiennej `czas_zakon`;
  - `imput_bezrobocie` - zmienna wskazująca, że status bezrobocia w danym miesiącu (dla danego respondenta) został zakodowany w oparciu o zaimputowaną wartość zmiennej `czas_rozp` lub zmiennej `czas_zakon`.

## 6.2. Okres objęty obserwacjami w zbiorze

  - Pierwszy miesiąc występujący w zbiorze dla danego respondenta to wrzesień 2016 r.
  - Ostatnim miesiącem występującym w zbiorze jest dla danego respondenta miesiąc poprzedzający ten, w którym nastąpiła realizacja wywiadu (z absolwentem).
  - Dla każdego respondenta w zbiorze zawarte są rekordy opisujące każdy kolejny miesiąc od pierwszego do ostatniego uwzględnionego dla niego w zbiorze (zgodnie z ww. regułami).
  - Okres miesięcy objęty zbiorem można oczywiście łatwo zawęzić, odfiltrowując obserwacje na podstawie zmiennej `czas`.

## 6.3. Sposób kodowania statusów

### 6.3.1. Reguły klasyfikacji na podstawie *epizodów* obejmujących poszczególne miesiące

  - Przy kodowaniu pominięte zostały *epizody*, dla których nieznany (już po imputacji) był czas rozpoczęcia *epizodu* lub *epizod* był zakończony (`czy_zakonczony` równe 1) i nieznany (już po imputacji) był czas zakończenia *epizodu*.
    + W związku z tym nie wzięto pod uwagę 278 *epizodów* (1,4% wszystkich odnotowanych epizodów).
    + Należy mieć na uwadze, że dla części *epizodów* respondenci nie byli w stanie podać miesiąca rozpoczęcia lub zakończenia *epizodu* i wartości zmiennych `czas_rozp` i `czas_zakon` były w takich przypadkach imputowane (p. sekcje *Zmienne specjalne w zbiorach w postaci „długiej”* i *Imputacja wartości zmiennych `czas_rozp` i `czas_zakon`*).
  - `praca` - zmienną zakodowano na podstawie zmiennych `ABS_pg2g` i `ABS_pg2h`:
    + `ABS_pg2h` równe 1 (`ABS_pg2g` równe 1, 2 lub 3) -> 1 (*zatrudniony na umowę o pracę na czas określony*);
    + `ABS_pg2h` równe 2 (`ABS_pg2g` równe 1, 2 lub 3) -> 2 (*zatrudniony na umowę o pracę na czas nieokreślony*);
    + `ABS_pg2h` równe 3 (`ABS_pg2g` równe 1, 2 lub 3) -> 3 (*zatrudniony przez agencję pracy tymczasowej*);
    + `ABS_pg2h` równe 4 (`ABS_pg2g` równe 1, 2 lub 3) -> 4 (*zatrudniony na umowie cywilnoprawnej*);
    + `ABS_pg2h` równe 5 (`ABS_pg2g` równe 1, 2 lub 3) -> 5 (*samozatrudniony (praca 'u kogoś')*);
    + `ABS_pg2g` równe 6 -> 6 (*prowadzi własną działalność ('praca 'u siebie')*);
    + `ABS_pg2g` równe 7 -> 7 (*prowadzi własne gosp. rolne*);
    + `ABS_pg2h` równe 7 lub 8 (`ABS_pg2g` równe 1, 2 lub 3) -> 8 (*odbywa staż lub praktykę absolwencką*);
    + `ABS_pg2h` równe 6 (`ABS_pg2g` równe 1, 2 lub 3) -> 9 (*zatrudniony bez umowy (na czarno)*);
    + wszystkie pozostałe przypadki, gdy wystąpił epizod pracy -> 10;
  - `nauka` - zmienną zakodowano na podstawie zmiennej `typ_epizodu`:
    + `typ_epizodu` równe 'studia' -> 2;
    + `typ_epizodu` równe 'SPolic.' -> 3;
    + wszystkim miesiącom od września 2016 r. do czerwca 2017 r. (wartości zmiennej `czas` od -9 do 0) przypisano wartość 1, która oznacza, że respondent uczył się w tym czasie w szkole, jako uczeń której został zakwalifikowany do badania; jeśli nie ukończył jej w czerwcu 2017 r., status ten mógł występować również w następnych miesiącach;
  - `bezrobocie` - zmienną zakodowano na podstawie zmiennej `pb1f`:
    + `pb1f` równe 1 -> 1 (*bezrobotny, poszukuje pracy*);
    + `pb1f` równe 2 -> 2 (*bierny zawodowo*);
    + wszystkie pozostałe przypadki, gdy wystąpił epizod bezrobocia -> 3;

### 6.3.2. Uwzględnienie ciągłości nauki (przez okres wakacji)

Jeśli stwierdzono, że respondent:

  - studiował w październiku danego roku i jednocześnie uczył się w szkole dowolnego typu w maju tego samego roku,
  - lub uczył się w szkole policealnej we wrześniu danego roku i jednocześnie uczył się w szkole dowolnego typu w maju tego samego roku,

wszystkie miesiące (włącznie) od czerwca do odpowiednio września lub sierpnia, w których respondent nie zadeklarował, że się uczył, oznaczane zostały jako okres odpowiednio studiowania (`nauka` równe 2) lub nauki w szkole policealnej (`nauka` równe 3).

### 6.3.3. Kodowanie na podstawie niewystępowania *epizodów* obejmujących dany miesiąc

  - `praca` - jeśli respondent nie wymienił żadnych *epizodów* pracy obejmujących dany miesiąc, zmiennej `praca` przypisywana była wartość 0;
  - `nauka` - jeśli respondent nie wymienił żadnych *epizodów* nauki obejmujących dany miesiąc, zmiennej `nauka` przypisywana była wartość 0 (z wyłączeniem miesięcy, dla których status ten zmieniono w wyniku opisanej powyżej procedury uwzględniania ciągłości nauki);
  - `bezrobocie`:
    + jeśli respondent nie wymienił żadnego *epizodu* bezrobocia obejmującego dany miesiąc i jednocześnie choć jedna zmienna spośród `praca` i `nauka` przyjmowała w tym miesiącu dla respondenta wartość inną niż 0, zmiennej `bezrobocie` przypisywano wartość 0 (*nie bezrobotny*);
    + jeśli respondent nie wymienił żadnego *epizodu* bezrobocia obejmującego dany miesiąc i jednocześnie zarówno zmienna `praca`, jak i zmienna `nauka` przyjmowały wartość 0 (tj. respondent nie pracował, ani się nie uczył), przyjmowano, że respondent był w danym miesiącu bierny zawodowo i zmiennej `bezrobocie` przypisywano wartość 2;
      + wystąpienie takiej sytuacji kodowane jest specjalną wartością zmiennej `status`: '999';
  
## 6.4. Relacje pomiędzy statusami pracy i nauki a statusem bezrobocia

W ramach przyjętego schematu kodowania nie narzucano wzajemnego wykluczania się *niezerowych* (tj. wskazującymi na aktywność zawodową lub edukacyjną) wartości zmiennych `praca` i `nauka` z *niezerowymi* wartościami zmiennej `bezrobocie` (wskazującymi na pozostawaniu bez zatrudnienia). W związku z występowaniem w zbiorze rekordów, w których statusy wskazują jednocześnie na bezrobocie (lub nieaktywność zawodową) i aktywność zawodową lub naukę, należy rozważyć, że:

  1. Po dużej części są to sytuacje, które mogły mieć miejsce:
     + Jeśli przejście od bezrobocia do pracy lub odwrotnie odbyło się w środku miesiąca, respondent istotnie był w danym miesiącu zarówno pracujący, jak i bezrobotny (wiersze w zbiorze, w których najprawdopodobniej mamy do czynienia z taką sytuacją wyróżnione są przez wartość 'miesiąc graniczny' zmiennej `praca_a_bezrobocie`).
     + Jeśli respondent pracował *na czarno*, mógł jednocześnie być formalnie bezrobotny (choć w zbiorze nie odnotowujemy tego typu deklaracji).
     + Postrzeganie się w kategoriach osoby bezrobotnej może być dla części respondentów oderwane od faktu kontynuowania lub nie nauki.
  2. W sytuacjach, gdy zidentyfikowano *konflikt* statusu pracy i bezrobocia, niedający się zinterpretować w kategoriach *miesiąca granicznego*, starano się je jeszcze rozstrzygnąć, biorąc pod uwagę, że niektóre statusy zostały przypisane na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_zakon` i w związku z tym można je uznać za mniej wiarygodne.
     + Jeśli w danym miesiącu (dla danego respondenta) status pracy wskazujący na aktywność zawodową został zakodowany na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_zakon`, a status bezrobocia wskazujący na brak pracy został zakodowany w oparciu o deklaracje respondenta dot. miesiąca rozpoczęcia lub zakończenia *epizodu* (bezrobocia), status zatrudnienia był zmieniany na brak zatrudnienia. Sytuacje takie oznaczone są w zbiorze wartością 'skorygowano pracę' zmiennej `praca_a_bezrobocie`.
     + Jeśli w danym miesiącu (dla danego respondenta) status bezrobocia wskazujący na brak pracy został zakodowany na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_zakon`, a status zatrudnienia wskazujący aktywność zawodową został zakodowany w oparciu o deklaracje respondenta dot. miesiąca rozpoczęcia lub zakończenia *epizodu* (pracy), status bezrobocia był zmieniany na 'nie bezrobotny'. Sytuacje takie oznaczone są w zbiorze wartością 'skorygowano bezrobocie' zmiennej `praca_a_bezrobocie`.
     + W innych przypadkach nie dało się dokonać korekt. Są one oznaczone w zbiorze wartościami 'sprzeczne deklaracje resp.' lub 'sprzeczne wyniki imputacji' zmiennej `praca_a_bezrobocie`, w zależności od tego, czy oba statusy zostały określone na podstawie informacji o miesiącu rozpoczęcia lub zakończenia *epizodu* podanych przez respondenta, czy też oba zostały przypisane na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_zakon`.
       + Przypadków takich jest na szczęście w zbiorze zaledwie 36 (na blisko 15 tys.).
  3. W ogólności nie mamy niestety gwarancji, że respondenci odpowiadali w sposób spójny. W ramach wywiadu nie mieliśmy bowiem możliwości wyłapywania ew. konfliktów w deklaracjach na bieżąco i zmuszania respondentów do ich rozstrzygnięcia.

## 6.5. Warto przemyśleć przed przystąpieniem do analiz

  - Jak przekodować trzy statusy opisane zmiennymi `praca`, `nauka` i `bezrobocie` na jeden status, biorąc pod uwagę, że (z powodów opisanych we wcześniejszej sekcji) respondent może w tym samym miesiącu być opisany jako pracujący, uczący się i bezrobotny.
  - Do jakiego okresu ograniczyć analizę.
    + Czy powinien to być ten sam okres dla każdego respondenta, czy chcemy skorzystać z faktu, że dla tych, z którymi wywiad został przeprowadzony później, dysponujemy dłuższą historią.
    + Kiedy wyznaczyć początek analizowanego okresu. Wydaje się, że najsensowniejsze cezury plasują się pomiędzy styczniem a lipcem 2017 r.
  - W danych występują (nieliczni) respondenci, którzy nie uzyskali świadectwa ukończenia szkoły, jako uczniowie której zostali zakwalifikowani do badania (można ich zidentyfikować po tym, że zmienna `f6` nie jest brakiem danych) lub które w momencie realizacji wywiadu wciąż kontynuowały w niej naukę. Te drugie jeszcze nie są jej absolwentami, te pierwsze nie całkiem (przynajmniej *nie w tym samym stopniu* co osoby, które świadectwo otrzymały). Być może wolelibyśmy wykluczyć ich z analizy?
