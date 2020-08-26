---
title: "Przetwarzanie zbiorów z pilotażowej rundy monitoringu"
author: "Tomasz Żółtak"
date: "27 października 2017"
output: rmarkdown::html_vignette
lang: pl
vignette: >
  %\VignetteIndexEntry{Przetwarzanie zbiorów z pilotażowej rundy monitoringu}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# 1. Wprowadzenie

### 1.1. Funkcja wczytaj_wyniki_pilrm()

Na podstawie zbioru z badania absolwentów (*MLEZAMiD_absolwent_n2959_20171013.sav*) funkcja `wczytaj_wyniki_pilrm()` zwraca listę ramek danych (zbiorów) zawierającą następujące elementy:

  - `dane` - zawiera odpowiedzi na pytania niedotyczące *epizodów* (por. niżej);
  - `epizody` - zawiera informacje dotyczące wymienionych przez respondentów *epizodów*:
    + nauki w szkole, jako uczeń której badany został zrekrutowany do badania (ważny ze względu na informację o momencie zakończenia tej nauki);
    + nauki w LO dla dorosłych,
    + zdawania do szkół policealnych i nauki w szkołach policealnych,
    + zdawania na studia i studiowania,
    + odbywania szkoleń i zdobywania certyfikatów (niezwiązanych bezpośrednio z nauką w ww. formach),
    + pracy,
    + bezrobocia;
  - `gospDom` - zawiera informacje o członkach gospodarstw domowych respondentów;
  - `czasy` - zawiera informacje o czasach udzielania odpowiedzi na pytania.

### 1.2. Funkcja imputuj_miesiac_pk_pilrm()

Funkcja `imputuj_miesiac_pk_pilrm()` przetwarza listę zbiorów danych, zwróconą przez `wczytaj_wyniki_pilrm()` i zwraca listę zbiorów danych o dokładnie takiej samej strukturze (p. wyżej).

Metody imputacji wartości zmiennych opisujących moment rozpoczęcia i moment zakończenia *epizodów* opisane zostały szczegółowo w sekcji 3. tego dokumentu.

### 1.3. Funkcja przygotuj_zbior_osobo_miesiecy_pilrm()

Funkcja `przygotuj_zbior_osobo_miesiecy_pilrm()` przetwarza listę zbiorów danych, zwróconą przez `imputuj_miesiac_pk_pilrm()` i zwraca ramkę danych (zbiór) zawierający dane o *epizodach* nauki, pracy i bezrobocia przekształcone do postaci osobo-miesięcy (obserwacją jest osoba w danym miesiącu).

Struktura zbioru i sposób jego tworzenia zostały szczegółowo opisane w sekcji 6. tego dokumentu.

## 1.4. Identyfikator respondenta

Identyfikatorem respondenta w zbiorach jest zmienna `ID` (W oryginalnym zbiorze danych z wynikami badania absolwentów zmienna ta nazywa się `ID_IBE`).

# 2. Struktura zbioru danych *epizodów*

## 2.1. Zmienne specjalne w zbiorze *epizodów*

Aby możliwe było łatwiejsze identyfikowanie interesujących nas w analizie *epizodów*, w zbiorze utworzone zostały specjalne zmienne (znajdują się na początku zbioru):

  - `ID` - identyfikator respondenta, umożliwia łączenie z innymi zbiorami danych z badania;
  - `typ_epizodu` - typ *epizodu*, przyjmuje jedną z ośmiu wartości:
     1. 'LO dla dorosłych';
     2. 'zdawanie do SPolic.';
     3. 'SPolic.';
     4. 'zdawanie na studia';
     5. 'studia';
     6. 'uprawnienia';
     7. 'praca';
     8. 'bezrobocie'.
     + **Uwaga!** Epizody 'studia' i 'SPolic.' są specyficzne, gdyż zawierają również informacje dotyczące zdawania na te kierunki studiów/do tych szkół policealnych, na/w których respondent podjął naukę. W związku z tym, jeśli przedmiotem analizy mają być kierunki/szkoły na/do których respondent zdawał, należy w niej uwzględnić odpowiednio zarówno *epizody* typu 'zdawanie na studia', jak i *epizody* typu 'studia' albo zarówno *epizody* typu 'zdawanie do SPolic.', jak i *epizody* typu 'SPolic.'.
  - `nr` - wartość zmiennej wskazuje, jako który z kolei (inaczej mówiąc, w którym *obiegu pętli skryptu*, czy też, w którym *wierszu tabeli reprezentującej odpowiedzi na blok pytań o „epizody” danego typu w „papierowej” wersji kwestionariusza*) *epizod* określonego typu respondent wymienił dany *epizod*;
     + **Uwaga!** Respondenci niekoniecznie wymieniali *epizody* w zgodzie z ich chronologiczną kolejnością. Jeśli chce się mieć pewność, że *epizody* uszeregowane są w kolejności chronologicznej, należy posłużyć się w pierwszej kolejności zmiennymi `czas_rozp` lub `czas_kon`, a dopiero w dalszej zmienną `nr`.
     + Zmienna `nr` odnosi się łącznie do *epizodów* typu 'zdawanie do SPolic.' i 'SPolic.' (tj. epizody obu tych typów w ramach tej samej osoby posiadają jedną, ciągłą *numerację*) oraz łącznie do *epizodów* typu 'zdawanie na studia' i 'studia', co wiąże się z opisaną wyżej specyfiką *epizodów* typów 'studia' i 'SPolic.'.
     + Dla *epizodów* typu 'LO dla dorosłych' jako wartość zmiennej `nr` występuje tylko 1, gdyż kwestionariusz nie dopuszczał wymienienia kilku *epizodów* tego typu.
     + W przypadku *epizodów* typu 'praca' **wartość zmiennej `nr` równa 99** oznacza, że dany *epizod* pracy został *dopowiedziany* w wyniku udzielenia przez respondenta odpowiedzi na pytania PG5.
  - `czas_rozp` - czas rozpoczęcia trwania danego *epizodu*, wyrażony jako liczba miesięcy od czerwca 2015 r. (a więc *domyślnego* momentu ukończenia szkoły, jako absolwent której respondent został zakwalifikowany do badania);
    + zmienna może przyjmować wartości ujemne (typowo zdarza się to w odniesieniu do *epizodów* pracy);
    + nie ma zastosowania dla *epizodów* typu 'uprawnienia';
    + **jeśli respondent nie podał miesiąca**, w jakim rozpoczął się dany *epizod*, dla epizodów typu 'praca' i 'bezrobocie' dokonano imputacji wartości zmiennej `czas_rozp` (p. odpowiednia sekcja); dla epizodów pozostałych typów zawsze arbitralnie przyjmowano, że był to czerwiec;
    + należy mieć na uwadze, że mimo to zdarzają się *epizody* (do których ma zastosowanie data rozpoczęcia) z brakami danych w tej zmiennej (respondenci czasem nie byli w stanie podać nawet roku);
  - `czas_kon` - czas zakończenia trwania danego *epizodu*, wyrażony jako liczba miesięcy od czerwca 2015 r. (a więc *domyślnego* momentu ukończenia szkoły, jako absolwent której respondent został zakwalifikowany do badania);
    + zmienna może przyjmować wartości ujemne (typowo zdarza się to w odniesieniu do *epizodów* pracy);
    + nie ma zastosowania dla *epizodów* typu 'zdawanie na studia' i 'zdawanie do SPolic.';
    + **jeśli respondent nie podał miesiąca**, w jakim zakończył się dany *epizod*, dla epizodów typu 'praca' i 'bezrobocie' dokonano imputacji wartości zmiennej `czas_rozp` (p. odpowiednia sekcja); dla epizodów pozostałych typów zawsze arbitralnie przyjmowano, że był to lipiec;
    + należy mieć na uwadze, że mimo to zdarzało się, że respondenci nie byli w stanie podać nawet roku i jako wartość zmiennej występuje brak danych;
    + **brak danych w zmiennej `czas_kon` może też wskazywać na to, że dany epizod wciąż trwa** - aby sprawdzić, czy mamy z tym do czynienia, należy posłużyć się zmienną `czy_zakonczony`.
  - `czy_zakonczony` - dychotomiczna zmienna wskazująca, czy dany *epizod* się zakończył, czy w momencie realizacji wywiadu z respondentem wciąż trwał:
    + dla *epizodów* typu 'LO dla dorosłych', 'studia', 'SPolic.' i 'uprawnienia' wartość zmiennej została przekodowana ze zmiennych odpowiednio: `zp2c`, `sp6d`, `pp6d` (jako wskazujące na zakończenie *epizodu* traktowano odpowiedzi mówiące o uzyskaniu dyplomu, jak i o przerwaniu nauki) oraz `u2d`;
    + dla *epizodów* typu 'praca' i 'bezrobocie' wartość zmiennej została zakodowana na podstawie zmiennych odpowiednio: `pg2f`, i `pb1e`;
    + **przy kodowaniu wartości dla *epizodów* wszystkich ww. typów, jeśli respondent nie udzielił odpowiedzi na odpowiednie pytanie, przyjmowano, że *epizod* się zakończył**; 
    + dla *epizodów* typu 'zdawanie na studia' i 'zdawanie do SPolic.' wartością zmiennej jest brak danych.
     
## 2.2. Różnice w nazwach i etykietach względem oryginalnego zbioru SPSS

  - Nazwy wszystkich zmiennych w zbiorach przekształconych do postaci *długiej*, z jedynym wyjątkiem zmiennej `ID`, przekształcone zostały na pisane małymi literami.
  - Nazwy zmiennych w zbiorze *epizodów* zasadniczo odpowiadają nazwom zmiennych z oryginalnego pliku w formacie SPSS, z tym że pozbawione zostały elementów wskazujących na *nr pętli* (w zależności od zmiennej przyrostek tego rodzaju mógł występować albo na końcu nazwy zmiennej, albo nieco wcześniej).
     + Wyjątkiem są zmienne związane z pytaniami o pierwszą i ostatnią pracę (p. niżej).
  - Ze względu na ograniczenia techniczne (biblioteki służącej do zapisu z R plików w formacie Staty) wartości zmiennych, które mają mieć przypisane etykiety wartości, muszą stanowić (nieprzerwany) ciąg kolejnych liczb całkowitych, począwszy od 1. W związku z tym **wartości powiązane z niektórymi etykietami w zbiorze *epizodów* mogą różnić się od tych, jakie są im przypisane w oryginalnym pliku w formacie SPSS**.
     + Typowo dotyczy to kodów specjalnych opisujących tzw. *braki danych użytkownika*.
     + Wszystkie zmienne, które mają etykiety wartości "tak"-"nie" zakodowane są w zbiorach w postaci długiej wg schematu: 1-tak, 2-nie (w zmiennych `sp6h_1` - `sp6h_3` i `pp6i_1` - `pp6i_3` w oryginalnym zbiorze SPSS wartości kodowane były wg schematu 0-nie, 1-tak).
     + W praktyce ma to jednak niewielkie znaczenie, gdyż wartości *braków danych użytkownika* przy przekształcaniu do postaci długiej były przekodowywane na *systemowe* braki danych.
  - Braki danych w zmiennych tekstowych oznaczone są ciągiem znaków składającym się z pojedynczej kropki: '.' (Stata nie akceptuje pustych ciągów znaków).

## 2.3. Epizody dotyczące zdawania do szkół policealnych lub na studia

  - Specyfika podziału na *epizody* typu 'studia' i 'SPolic.' oraz epizody typu 'zdawanie na studia' i 'zdawanie do SPolic.' została omówiona w sekcji *Zmienne specjalne w zbiorach w postaci „długiej”*.
  - Na podstawie zmiennych odpowiednio `pp3a` i `sp3a` utworzona została zmienna `czy_preferowany` opisująca to, czy szkoła lub kierunek był preferowany przez respondenta (spośród potencjalnie kilku, do których/na które zdawał) przy pomocy wartości: 1-tak, 2-nie.
    + Jeśli respondent zdawał tylko do jednej szkoły czy na jedne studia, w zmiennej `czy_preferowany` przypisana została do danej szkoły/kierunku wartość 1 (tak).
    + Jeśli respondent nie wskazał preferowanego kierunku/szkoły, wartością zmiennej jest brak danych.

## 2.4. Epizody pracy

  - Zmienna `pg1b` podaje kod pracodawcy, który w sytuacji, gdy respondent miał wiele *epizodów* pracy, pozwala stwierdzić, czy była to praca u różnych, czy u tego samego pracodawcy. Zmienna ta ma następujące własności i ograniczenia:
    + Kody (numery) pracodawców pozwalają ich identyfikować tylko w ramach rekordów opisujących tego samego respondenta.
    + Dla *epizodów* pracy, które zostały *dopowiedziane* w ramach odpowiedzi na pytania PG4, nazwa pracodawcy nie została przez PBS zakodowana w sposób analogiczny do tego, jak postąpiono w zasadniczej pętli pytań o pracę. W związku z tym konieczne było przyjęcie arbitralnego założenia, że *epizod* ten dotyczy innego pracodawcy, niż wcześniej wymienieni. W takim przypadku jako kod pracodawcy wpisana została wartość 99.
  - Zakres informacji o *epizodach* pracy różni się w zależności od tego, czy był to pierwszy, ostatni, czy któryś inny *epizod* pracy. W przypadku pierwszych i ostatnich epizodów dołączono bowiem do zbioru informacje o odpowiedziach na pytania odpowiednio PI0-PI6 oraz PO0-PO6.
    + Przedrostki nazw zmiennych opisujących pierwszą i ostatnią pracę zostały zmienione na wspólny: *pio*.
    + Nazwę zmiennej opisującej odpowiedzi na pytanie PI5 zmieniono na `pio7'.

# 3. Imputacja wartości zmiennych czas_rozp i czas_kon

Imputacji brakujących wartości zmiennych `czas_rozp` i `czas_kon` dla *epizodów* typu 'praca' i 'bezrobocie' dokonano przy użyciu modeli regresji liniowej.

  + Modele estymowane były oddzielnie dla każdego z tych dwóch typów *epizodów*.
  + W procedurze imputacji (zarówno przy estymacji modeli, jak i przy samym imputowaniu braków danych) brano pod uwagę wyłącznie epizody, które rozpoczęły się w 2015 r. lub później.

## 3.1. Zmienna czas_rozp

  + Zmienną zależną w modelu była `czas_rozp`.
  + Jako predyktory wykorzystano:
    1) typ szkoły (dwie zmienne *dummy* zakodowane na podstawie zmiennej `typ_szkoly`);
    2) płeć respondenta (zmienna `m1`);
    3) formę pracy lub rozróżnienie na bezrobotnych (poszukujących pracy) i biernych zawodowo - p. opis zmiennych `praca` i `bezrobocie` w sekcji *Zbiór z danymi w postaci osobo-miesięcy* (przekodowane na zestaw zmiennych *dummy*, przy czym brak danych uwzględniono przy przekodowaniu jako *legalną* wartość);
    4) rok, w którym rozpoczął się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennych *dummy*);
    5) rok, w którym zakończył się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennych *dummy*);
    6) liczbę wszystkich *epizodów* pracy danego respondenta;
    7) liczbę *epizodów* pracy respondenta, o których wiadomo, że obejmowały rok, w którym rozpoczął się dany *epizod*;
    8) liczbę *epizodów* pracy respondenta, o których wiadomo, że obejmowały rok, w którym zakończył się dany *epizod*;
    9) liczbę wszystkich *epizodów* bezrobocia (lub nieaktywności zawodowej) danego respondenta;
    10) liczbę *epizodów* bezrobocia respondenta (lub nieaktywności zawodowej), o których wiadomo, że obejmowały rok, w którym rozpoczął się dany *epizod*;
    11) liczbę *epizodów* bezrobocia respondenta (lub nieaktywności zawodowej), o których wiadomo, że obejmowały rok, w którym zakończył się dany *epizod*;
    12) informację o tym, czy w (dowolnym momencie) roku rozpoczęcia danego *epizodu* respondent brał udział w edukacji formalnej, a jeśli tak, to w jakiej formie (4 zmienne *dummy*);
    13) informację o tym, czy w (dowolnym momencie) roku zakończenia danego *epizodu* respondent brał udział w edukacji formalnej, a jeśli tak, to w jakiej formie (4 zmienne *dummy*);
    14) interakcje pomiędzy 1) a 6)-13);
    15) interakcje pomiędzy 2) a 6)-13);
    16) interakcje pomiędzy 3) a 6)-13);
    17) interakcje pomiędzy 4) a 6)-13);
    18) interakcje pomiędzy 5) a 6)-13);
    19) miesiąc przeprowadzenia wywiadu (zmienna `r5s2`) i jego interakcję z 4).
  + Model osiągał następujące statystyki dopasowania (na zbiorze epizodów, dla których znane były wartości zmiennej `czas_rozp`):
    + Dla *epizodów* typu 'praca': R^2^ = 0,92; odsetek poprawnych klasyfikacji = 19,5%.
    + Dla *epizodów* typu 'bezrobocie': R^2^ = 0,95; odsetek poprawnych klasyfikacji = 47,3%.
  + Przy dokonywaniu imputacji przewidywania wynikające z modelu zaokrąglano do najbliższych liczb całkowitych (tak aby zmienna `czas_rozp` również po imputacji przyjmowała tylko wartości całkowite).
    + Ww. odsetki poprawnych klasyfikacji zostały obliczone na podstawie tak zaokrąglonych wartości (ale wartość R^2^ podano dla niezaokrąglonych przewidywań).
  + Po dokonaniu imputacji dokonano sprawdzenia:
    + Czy zaimputowana wartość zmiennej zgadza się ze znanym rokiem rozpoczęcia *epizodu*.
    + Czy zaimputowana wartość zmiennej nie wskazuje, że *epizod* rozpoczął się później, niż się zakończył (dla *epizodów*, dla których znana była wartość zmiennej `czas_kon`, ale wartość zmiennej `czas_rozp` była imputowana).
    + W pojedynczych przypadkach, gdy stwierdzono naruszenia ww. warunków, zmieniano zaimputowaną wartość zmiennej `czas_rozp` na najbliższą mającą sens.

## 3.2. Zmienna czas_kon

  + Zmienną zależną w modelu była długość trwania *epizodu* (tj. różnica pomiędzy `czas_kon` a `czas_rozp`).
    + Przy imputacji wartość zmiennej `czas_kon` określano, dodając do wartości zmiennej `czas_rozp` wartość przewidywania wynikającą z modelu (zaokrągloną do najbliższej liczby całkowitej).
  + Jako predyktory wykorzystano:
    1) typ szkoły (dwie zmienne *dummy* zakodowane na podstawie zmiennej `typ_szkoly`);
    2) płeć respondenta (zmienna `m1`);
    3) formę pracy lub rozróżnienie na bezrobotnych (poszukujących pracy) i biernych zawodowo - p. opis zmiennych `praca` i `bezrobocie` w sekcji *Zbiór z danymi w postaci osobo-miesięcy* (przekodowane na zestaw zmiennych *dummy*, przy czym brak danych uwzględniono przy przekodowaniu jako *legalną* wartość);
    4) rok, w którym rozpoczął się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennych *dummy*);
    5) rok, w którym zakończył się *epizod* (traktowany jako zmienna kategorialna - zakodowany przy pomocy zmiennych *dummy*);
    6) liczbę wszystkich *epizodów* pracy danego respondenta;
    7) liczbę *epizodów* pracy respondenta, o których wiadomo, że obejmowały rok, w którym rozpoczął się dany *epizod*;
    8) liczbę *epizodów* pracy respondenta, o których wiadomo, że obejmowały rok, w którym zakończył się dany *epizod*;
    9) liczbę wszystkich *epizodów* bezrobocia (lub nieaktywności zawodowej) danego respondenta;
    10) liczbę *epizodów* bezrobocia respondenta (lub nieaktywności zawodowej), o których wiadomo, że obejmowały rok, w którym rozpoczął się dany *epizod*;
    11) liczbę *epizodów* bezrobocia respondenta (lub nieaktywności zawodowej), o których wiadomo, że obejmowały rok, w którym zakończył się dany *epizod*;
    12) informację o tym, czy w (dowolnym momencie) roku rozpoczęcia danego *epizodu* respondent brał udział w edukacji formalnej, a jeśli tak, to w jakiej formie (4 zmienne *dummy*);
    13) informację o tym, czy w (dowolnym momencie) roku zakończenia danego *epizodu* respondent brał udział w edukacji formalnej, a jeśli tak, to w jakiej formie (4 zmienne *dummy*);
    14) interakcje pomiędzy 1) a 6)-13);
    15) interakcje pomiędzy 2) a 6)-13);
    16) interakcje pomiędzy 3) a 6)-13);
    17) interakcje pomiędzy 4) a 6)-13);
    18) interakcje pomiędzy 5) a 6)-13);
    19) miesiąc przeprowadzenia wywiadu (zmienna `r5s2`) i jego interakcję z 5);
    20) datę (miesięczną) rozpoczęcia danego epizodu (zmienna `czas_rozp`) - uwzględniano przy tym również wartości tej zmiennej, które zostały zaimputowane.
  + Model osiągał następujące statystyki dopasowania (na zbiorze epizodów, dla których znane były wartości zmiennej `czas_kon`):
    + Dla *epizodów* typu 'praca': R^2^ = 0,85; odsetek poprawnych klasyfikacji = 21,1%.
    + Dla *epizodów* typu 'bezrobocie': R^2^ = 0,86; odsetek poprawnych klasyfikacji = 19,6%.
  + Przy dokonywaniu imputacji przewidywania wynikające z modelu zaokrąglano do najbliższych liczb całkowitych (tak aby zmienna `czas_kon` również po imputacji przyjmowała tylko wartości całkowite).
    + Ww. odsetki poprawnych klasyfikacji zostały obliczone na podstawie tak zaokrąglonych wartości (ale wartość R^2^ podano dla niezaokrąglonych przewidywań).
  + Po dokonaniu imputacji dokonano sprawdzenia:
    + Czy zaimputowana wartość zmiennej zgadza się ze znanym rokiem zakończenia *epizodu*.
    + Czy zaimputowana wartość zmiennej nie wskazuje, że *epizod* zakończył się wcześniej, niż się rozpoczął.
    + W pojedynczych przypadkach, gdy stwierdzono naruszenia ww. warunków, zmieniano zaimputowaną wartość zmiennej `czas_kon` na najbliższą mającą sens.

# 4. Zbiór z informacjami o członkach gospodarstw domowych

  - Informacje zawarte w zmiennej `m10d_in` zostały domknięte i zespolone z *typowymi* odpowiedziami na pytanie M10 w ramach zmiennej `m10d_rekod`.
  - Do zbioru dołączona została zmienna `m10c_wiek`, zawierająca wiek przeliczony z daty urodzenia.

# 5. Uwagi o analizie zbioru *epizodów*

Typowo zbiór *epizodów* wykorzystywane będzie w analizie w ten sposób, że:

  - najpierw dokonane zostanie zawężenie zbioru do *epizodów* określonego typu (*epizody* innych typów zostaną usunięte);
  - następnie na tak zawężonym zbiorze wykonane zostaną pewne procedury agregacji w ramach respondentów (aby uzyskać interesujące wskaźniki);
  - następnie tak zagregowane wyniki będą dołączane do oryginalnego zbioru danych (elementu `dane` listy zwracanej przez funkcję `wczytaj_wyniki_pilrm()`, w którym jednemu respondentowi odpowiada jeden wiersz) w celu przeprowadzenia dalszych analiz.
  
Należy mieć przy tym na uwadze, że znaczna część respondentów mogła nie mieć żadnego *epizodu* właśnie analizowanego typu. W szczególności oznacza to, że:

  - Typowo przy dołączaniu wyników zagregowanych do poziomu respondentów na podstawie zbioru *epizodów* do oryginalnego zbioru (w którym jednostką obserwacji jest respondent) wielu osobom w oryginalnym zbiorze, obejmującym wszystkich respondentów, przypisane zostaną braki danych.
    + Istotne może być więc zakodowanie tych braków danych zaraz po dokonaniu łączenia w sposób, który pozwoli potem na wygodne (i przede wszystkim nieprowadzące do pomyłek, w szczególności związanych z przyjęciem złej podstawy procentowania) obsłużenie ich w prowadzonych dalej analizach.
    + Należy przy tym mieć na uwadze, że w zbiorze *epizodów* wartości *braków danych użytkownika* zostały już typowo zamienione na *systemowe* braki danych (a więc *systemowy* brak danych w połączonym zbiorze może dla wielu zmiennych równie dobrze wskazywać na to, że respondent nie miał żadnego *epizodu* danego typu, co na to, że taki *epizod* miał, ale nie był w stanie podać o nim interesującej nas informacji).
    + W związku z tym **dobrą praktyką przy agregacji zbioru danych w postaci *długiej* do poziomu respondentów jest utworzenie w nim takiej zmiennej (innej niż `ID`), co do której będziemy mieć pewność, że dla wszystkich w zagregowanym zbiorze przyjmie ona wartość niebędącą brakiem danych** i będzie ją można potem wygodnie wykorzystać do stwierdzenia, którzy respondenci w oryginalnym zbiorze danych mieli choć jeden *epizod* interesującego nas typu.
  - Jeśli zagregowany do poziomu respondentów zbiór danych w postaci *długiej* będzie analizowany bez przyłączania do oryginalnego zbioru, w kontekście podstaw procentowania należy mieć na uwadze, że zawiera on typowo tylko podzbiór spośród wszystkich badanych.

# 6. Zbiór z danymi w postaci osobo-miesięcy

## 6.1. Struktura zbioru

W zbiorze zawarte zostały następujące zmienne opisujące cechy respondentów stałe w czasie:

  - `ID` - identyfikator respondenta, umożliwia łączenie z innymi zbiorami danych z badania;
  - `typ_szkoly` - typ szkoły, jako uczeń której respondent został zakwalifikowany do badania;
  - `r5s2` - miesiąc, w którym został przeprowadzony wywiad z respondentem;
  - `f6` - zmienna opisująca powód nieuzyskania świadectwa szkoły, jako uczeń której respondent został zakwalifikowany do badania;
    + na podstawie zebranych danych nie da się określić daty, kiedy uczeń przerwał naukę w tej szkole;
    + jeśli respondent ukończył tę szkołę, zmiennej przypisany jest brak danych;
  - `f7` - rok uzyskania świadectwa szkoły, jako uczeń której respondent został zakwalifikowany do badania;
    + jeśli respondent nie uzyskał świadectwa tej szkoły, zmiennej przypisany jest brak danych;
  - `t_laczny_czas` - łączny czas trwania wywiadu, obliczony poprzez zsumowanie zmiennych opisujących czasy odpowiedzi na poszczególne pytania;
    + niestety **w przypadku znacznej części respondentów zmienna ta podaje zafałszowany (zbyt krótki) czas trwania wywiadu**;
  - `m1` - płeć respondenta;
  - `m2` - rok urodzenia respondenta;
    + w zmiennej występują (nieliczne) braki danych;
    + **w zbiorze występują osoby starsze (rekordzista to rocznik '55)**;
  - `m3` - klasa wielkości miejscowości zamieszkania w momencie przeprowadzania wywiadu z absolwentem.
  
W zbiorze zawarte zostały następujące zmienne opisujące statusy respondentów w poszczególnych miesiącach:

  - `data` - identyfikator miesiąca w formie *czytelnej*;
  - `czas` - identyfikator miesiąca w formie liczby - liczba miesięcy, jaka upłynęła od czerwca 2015 r. (a więc *domyślnego* momentu ukończenia szkoły, jako absolwent której respondent został zakwalifikowany do badania) do miesiąca, który opisuje dany rekord w zbiorze;
  - `status` - zmienna kodująca w syntetycznej formie status respondenta: wartości zmiennych (kolejno) `praca`, `nauka` i `bezrobocie` *sklejone* ze sobą w jeden ciąg znaków (składający się z trzech cyfr: cyfra setek koduje pracę, dziesiątek naukę, a jedności bezrobocie);
    + wartość zmiennej status równa '999' ma specjalne znaczenie - p. następna sekcja;
  - `praca` - zmienna kodująca status zatrudnienia;
  - `nauka` - zmienna kodująca status uczestnictwa w edukacji formalnej;
  - `bezrobocie` - zmienna kodująca, czy respondent deklarował się jako pozostający bez pracy;
  - `praca_a_bezrobocie` - zmienna opisująca ew. występowanie w danym miesiącu (dla danego respondenta) *konfliktów* pomiędzy statusem zatrudnienia a statusem bezrobocia, fakt skorygowania takiego *konfliktu* (p. sekcja *Sposób kodowania statusów*) lub jego przyczynę;
  - `korekta_ciaglosc_nauki` - zmienna pozwala zidentyfikować rekordy, w których status uczestnictwa w nauce formalnej został skorygowany ze względu na stwierdzenie ciągłości nauki (p. sekcja *Sposób kodowania statusów*);
  - `imput_praca` - zmienna wskazująca, że status zatrudnienia w danym miesiącu (dla danego respondenta) został zakodowany w oparciu o zaimputowaną wartość zmiennej `czas_rozp` lub zmiennej `czas_kon`;
  - `imput_nauka` - zmienna wskazująca, że status uczestnictwa w nauce formalnej w danym miesiącu (dla danego respondenta) został zakodowany w oparciu o zaimputowaną wartość zmiennej `czas_rozp` lub zmiennej `czas_kon`;;
  - `imput_bezrobocie` - zmienna wskazująca, że status bezrobocia w danym miesiącu (dla danego respondenta) został zakodowany w oparciu o zaimputowaną wartość zmiennej `czas_rozp` lub zmiennej `czas_kon`;.

## 6.2. Okres objęty obserwacjami w zbiorze

  + Pierwszy miesiąc występujący w zbiorze dla danego respondenta to:
    + Wrzesień 2014 r. - dla respondentów, którzy nie wymienili żadnego epizodu, który rozpoczynałby się wcześniej.
    + Miesiąc rozpoczęcia najwcześniejszego z wymienionych epizodów, jeśli takowy jest wcześniejszy niż wrzesień 2014 r.
  + Ostatnim miesiącem występującym w zbiorze jest dla danego respondenta miesiąc poprzedzający ten, w którym nastąpiła realizacja wywiadu (z absolwentem).
  + Dla każdego respondenta w zbiorze zawarte są rekordy opisujące każdy kolejny miesiąc od pierwszego do ostatniego uwzględnionego dla niego w zbiorze (zgodnie z ww. regułami).
  + Okres miesięcy objęty zbiorem można oczywiście łatwo zawęzić, odfiltrowując obserwacje na podstawie zmiennej `czas`.
    + Należy mieć na uwadze, że **w ogólności analiza okresu przed 2015 r. (a na pewno przed wrześniem 2014 r.) nie ma sensu**, chyba że w szczególnych przypadkach, np. gdy chcemy utworzyć zmienną opisującą w zbiorczy sposób uprzednie doświadczenie zawodowe respondenta.

## 6.3. Sposób kodowania statusów

### 6.3.1. Reguły klasyfikacji na podstawie *epizodów* obejmujących poszczególne miesiące

  - Przy kodowaniu pominięte zostały *epizody*, dla których nieznany był rok rozpoczęcia *epizodu* (tj. już po przeprowadzeniu imputacji wartością zmiennej `czas_rozp` był brak danych) lub *epizod* był zakończony (`czy_zakonczony` równe 1) i nieznany był rok zakończenia *epizodu* (tj. już po przeprowadzeniu imputacji wartością zmiennej `czas_kon` był brak danych).
    + Należy mieć na uwadze, że dla części *epizodów* respondenci nie byli w stanie podać miesiąca rozpoczęcia lub zakończenia *epizodu* i wartości zmiennych `czas_rozp` i `czas_kon` były w takich przypadkach imputowane (p. sekcje *Zmienne specjalne w zbiorach w postaci „długiej”* i *Imputacja wartości zmiennych `czas_rozp` i `czas_kon`*).
  - `praca` - zmienną zakodowano na podstawie zmiennych `pg2g` i `pg2h`:
    + `pg2h` równe 1 (`pg2g` równe 1, 2 lub 3) -> 1 (*zatrudniony na umowę o pracę*);
    + `pg2h` równe 2 (`pg2g` równe 1, 2 lub 3) -> 2 (*zatrudniony przez agencję pracy tymczasowej*);
    + `pg2h` równe 3 (`pg2g` równe 1, 2 lub 3) -> 3 (*zatrudniony na umowie cywilnoprawnej*);
    + `pg2h` równe 4 (`pg2g` równe 1, 2 lub 3) -> 4 (*samozatrudniony ('praca u kogoś')*);
    + `pg2g` równe 4 -> 5 (*prowadzi własną działalność ('praca u siebie')*);
    + `pg2g` równe 5 -> 6 (*prowadzi własne gosp. rolne*);
    + `pg2h` równe 6 lub 7 (`pg2g` równe 1, 2 lub 3) -> 7 (*odbywa staż lub praktykę absolwencką*);
    + `pg2h` równe 5 (`pg2g` równe 1, 2 lub 3) -> 8 (*zatrudniony bez umowy (na czarno)*);
    + wszystkie pozostałe przypadki, gdy wystąpił epizod pracy -> 9;
  - `nauka` - zmienną zakodowano na podstawie zmiennej `typ_epizodu`:
    + `typ_epizodu` równe 'studia' -> 2;
    + `typ_epizodu` równe 'SPolic.' -> 3;
    + wszystkim miesiącom od września 2014 r. do czerwca 2015 r. (wartości zmiennej `czas` od -9 do 1) przypisano wartość 1, która oznacza, że respondent uczył się w tym czasie w szkole, jako uczeń której został zakwalifikowany do badania;
      + Ponieważ na podstawie zebranych danych występują trudności z określeniem, kiedy respondent zakończył naukę w szkole, jako uczeń której został zakwalifikowany do badania, dla wszystkich badanych arbitralnie założono, że ukończyli naukę w tej szkole w czerwcu 2015 r. (por. sekcja *Warto przemyśleć przed przystąpieniem do analiz*).
  - `bezrobocie` - zmienną zakodowano na podstawie zmiennej `pb1f`:
    + `pb1f` równe 1 -> 1 (*bezrobotny, poszukuje pracy*);
    + `pb1f` równe 2 -> 2 (*bierny zawodowo*);
    + wszystkie pozostałe przypadki, gdy wystąpił epizod bezrobocia -> 3;

### 6.3.2. Uwzględnienie ciągłości nauki (przez okres wakacji)

Jeśli stwierdzono, że respondent:

  - studiował w październiku danego roku i jednocześnie uczył się w szkole dowolnego typu w maju tego samego roku,
  - lub uczył się w szkole policealnej we wrześniu danego roku i jednocześnie uczył się w szkole dowolnego typu w maju tego samego roku,

wszystkie miesiące (włącznie) od czerwca do odpowiednio września lub sierpnia, w których respondent nie zadeklarował, że się uczył, oznaczane były jako okres odpowiednio studiowania (`nauka` równe 2) lub nauki w szkole policealnej (`nauka` równe 3).

**Kodowanie na podstawie niewystępowania *epizodów* obejmujących dany miesiąc**

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
  2. W sytuacjach, gdy zidentyfikowano *konflikt* statusu pracy i bezrobocia, niedający się zinterpretować w kategoriach *miesiąca granicznego*, starano się je jeszcze rozstrzygnąć, biorąc pod uwagę, że niektóre statusy zostały przypisane na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_kon` i w związku z tym można je uznać za mniej wiarygodne.
     + Jeśli w danym miesiącu (dla danego respondenta) status pracy wskazujący na aktywność zawodową został zakodowany na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_kon`, a status bezrobocia wskazujący na brak pracy został zakodowany w oparciu o deklaracje respondenta dot. miesiąca rozpoczęcia lub zakończenia *epizodu* (bezrobocia), status zatrudnienia był zmieniany na brak zatrudnienia. Sytuacje takie oznaczone są w zbiorze wartością 'skorygowano pracę' zmiennej `praca_a_bezrobocie`.
     + Jeśli w danym miesiącu (dla danego respondenta) status bezrobocia wskazujący na brak pracy został zakodowany na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_kon`, a status zatrudnienia wskazujący aktywność zawodową został zakodowany w oparciu o deklaracje respondenta dot. miesiąca rozpoczęcia lub zakończenia *epizodu* (pracy), status bezrobocia był zmieniany na 'nie bezrobotny'. Sytuacje takie oznaczone są w zbiorze wartością 'skorygowano bezrobocie' zmiennej `praca_a_bezrobocie`.
     + W innych przypadkach nie dało się dokonać korekt. Są one oznaczone w zbiorze wartościami 'sprzeczne deklaracje resp.' lub 'sprzeczne wyniki imputacji' zmiennej `praca_a_bezrobocie`, w zależności od tego, czy oba statusy zostały określone na podstawie informacji o miesiącu rozpoczęcia lub zakończenia *epizodu* podanych przez respondenta, czy też oba zostały przypisane na podstawie imputowanych wartości zmiennych `czas_rozp` i `czas_kon`.
  3. W ogólności nie mamy niestety gwarancji, że respondenci odpowiadali w sposób spójny. W ramach wywiadu nie mieliśmy bowiem możliwości wyłapywania ew. konfliktów w deklaracjach na bieżąco i zmuszania respondentów do ich rozstrzygnięcia.
     + Być może w przyszłości chcąc badać historie życia respondentów, należałoby pomyśleć o zagwarantowaniu sobie, że firma realizująca je w terenie będzie używać bardziej wyspecjalizowanego pod tym kątem oprogramowania do realizacji.

## 6.5. Warto przemyśleć przed przystąpieniem do analiz

  + Jak przekodować trzy statusy opisane zmiennymi `praca`, `nauka` i `bezrobocie` na jeden status, biorąc pod uwagę, że (z powodów opisanych we wcześniejszej sekcji) respondent może w tym samym miesiącu być opisany jako pracujący, uczący się i bezrobotny.
  + W danych występują również bardzo starzy respondenci (najstarszy to rocznik '55). Jeśli chcieć, zgodnie z pierwotnymi założeniami metodologicznymi badania BLASZ, ograniczyć się do analizy osób, które w momencie badania w szkołach miały nie więcej niż 29 lat, należałoby usunąć osoby urodzone przed 1986 r. (na podstawie zmiennej `m2` - trzeba przy tym jeszcze podjąć decyzję, co z respondentami - na szczęście nielicznymi - których roku urodzenia nie znamy).
  + Do jakie okresu ograniczyć analizę.
    + Czy powinien to być ten sam okres dla każdego respondenta, czy chcemy skorzystać z faktu, że dla tych, z którymi wywiad został przeprowadzony później, dysponujemy dłuższą historią.
    + Kiedy wyznaczyć początek analizowanego okresu. Wydaje się, że najsensowniejsze cezury plasują się pomiędzy styczniem a lipcem 2015 r. Absolutnie nie należy analizować sekwencji zdarzeń w okresie przed wrześniem 2014 r.
  + W danych występują - szczęśliwie nieliczni - respondenci, którzy nie uzyskali świadectwa ukończenia szkoły, jako uczniowie której zostali zakwalifikowani do badania (można ich zidentyfikować po tym, że zmienna `f6` nie jest brakiem danych). Być może należałoby wykluczyć ich z analizy, gdyż po pierwsze, nie do końca są absolwentami (a przynajmniej *nie w takim samym stopniu*, jak wszyscy pozostali), a po drugie (i może ważniejsze), na podstawie zebranych danych nie jesteśmy w stanie ustalić, jak długo kontynuowali oni jeszcze naukę (na potrzeby kodowania zupełnie arbitralnie przyjąłem, że *jak wszyscy* opuścili mury szkoły w czerwcu 2015 r.).
  + W danych występują osoby, które deklarują, że świadectwo ukończenia szkoły, jako uczniowie której zostali zakwalifikowani do badania, uzyskały w innym roku, niż 2015. Można mieć jednak mieć wątpliwości, czy informacja ta jest rzetelna, biorąc pod uwagę, że 10 osób wskazuje, że dyplom ten uzyskało już w 2014 r. (realizacja wywiadów w BLASZu rozpoczęła się 26 listopada 2014 r.; osoby, które składają takie deklaracje, były uczniami ZSZ lub techników). Nie jest też jasne, jaka dokładnie była sytuacja osób, które zadeklarowały inny niż 2015 rok ukończenia takiej szkoły i jak w związku z tym najlepiej byłoby zakodować ich statusy edukacyjne. Biorąc pod uwagę niewielką liczbę takich osób (oprócz 10 deklarujących rok 2014 jeszcze 29 deklarujących rok 2016 i 5  deklarujących rok 2017), uznałem, że najlepiej (najprościej) będzie problem zignorować i konsekwentnie dla wszystkich stosować arbitralne założenie, że szkołę, jako uczniowie której trafili do badania, ukończyli w czerwcu 2015 r. Można jednak chcieć dokonać w tej kwestii innych rozstrzygnięć i odpowiednio przekodować wartości zmiennej `nauka` w oparciu o wartość zmiennej `f7` (oraz `czas`).
