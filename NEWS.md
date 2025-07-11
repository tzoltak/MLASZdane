# MLASZdane 0.5.0 (23.08.2023)

-   Do funkcji `agreguj_wskazniki()` dodano wstępną obsługę wielowątkowości (z użyciem pakietu *parallelly*). Stabilność implementacji i ew. zyski z używania wielowątkowości (na różnych systemach operacyjnych) wymagają dalszych testów.
-   Funkcja `agreguj_wskazniki()` akceptuje wskazanie, że dla danej grupy ma nie być używana żadna grupa odniesienia. W tym celu jako definicję grupy odniesienia w kolumnie `odniesienie` argumentu `grupy` należy podać brak danych (`NA`). Funkcja akceptuje też teraz wywołania, w których agrument `grupy` w ogóle nie zawiera kolumny `odniesienie`.

    -   Uwaga, w przypadku, gdy brak grupy odniesienia dotyczy tylko niektórych grup i wykorzystywane są wskaźniki, których wartości dla poszczególnych grup mają postać inną, niż skalar, struktura wartości wskaźników zwracanych dla poszczególnych grup jest niestabilna! Dla grup, w przypadku których nie zdefiniowano grupy odniesienia wskaźnik będzie jednoelementowym wektorem logicznym zawierającym brak danych, podczas gdy dla pozostałych grup będzie miał bardziej złożoną, właściwą sobie strukturę.

-   Poprawiono wywołania funkcji `rename` na zgodne z konwencją obowiązującą od wersji 1.0.0 pakietu *tidyselect*, w której preferowane jest wybieranie kolumn po ich nazwie (bez prefiksu `.data$`) oraz zamieniono wywołania `mutate_all(., ...)` na `mutate(., across(everything(), ...))`.

# MLASZdane 0.4.2 (9.08.2021)

-   Funkcja `przygotuj_wskaznik_do_splaszczenia()` konwertuje wszystkie zmienne wskaźników na ciągi znaków (zmienne logiczne są uprzednio konwertowane na liczby), aby uniknąć sytuacji, kiedy w niektórych grupach zachowują one oryginalny format, a w innych są ciągami znaków (por. wskaźnik *l_zawod_przyg* w zbiorze z 2. RM). Dla równowagi `splaszcz_wskazniki_zagregowane()` konwertuje z powrotem na liczby zmienne, dla których da się to zrobić.
-   Do testów dodane zostały te sprawdzające (co prawda bardzo pobieżnie), czy `splaszcz_wskazniki_zagregowane()` działa na zbiorach wskaźników zagregowanych z 2. RM.
-   Dostosowano sposób wywoływania funkcji `unnest()` w ramach `pobierz_dane_bdl()` do zmian w API tej pierwszej.

# MLASZdane 0.4.1 (27.08.2020)

-   Wprowadzono nowe funkcje do obliczania wskaźników na poziomie zagregowanym z 2. rundy monitoringu, które są kompatybilne ze zmianą sposobu obliczania wskaźników wprowadzoną w wersji 0.4.0 pakietu.

    -   Patrz funkcja agreguj_cawi_ucz_2rm().

# MLASZdane 0.4.0 (5.06.2020)

-   Zmieniony został sposób obliczania wskaźników na poziomie zagregowanym:

    -   definicja grupy odnienia powiązana jest bezpośrednio z analizowaną grupą, co umożliwia wykorzystanie grup odniesienia tworzonych w różny sposób dla poszczególnych spośród analizowanych grup (w szczególności wykluczanie obserwacji należących do analizowanej grupy z odpowiadającej jej grupy odniesienia);

    -   funkcje obliczające wskaźniki na poziomie zagregowanym zwracają teraz listę dwóch ramek danych (poprzednio po prostu ramkę danych): jedna zawiera wartości wskaźników w analizowanych grupach, druga w odpowiadających im grupom odniesienia;

    -   agreguj_wskazniki() została całkowicie przepisana i ma zmienione API;

    -   utworz_grupowanie_ze_zmiennej() to nowa funkcja, która pozwala wygodnie utworzyć ramkę danych opisującą podział na analizowane grupy i odpowiadające im grupy odniesienia, jeśli analizowane grupy definiują wartości jednej ze zmiennych w zbiorze, a przypisanie grupy odniesienia odbywa się na podstawie wartości pewnej drugiej zmiennej;

    -   zmodyfikowane zostały funkcje służące obliczaniu wskaźników na poziomie zagregowanym na podstawie wyników 1. rundy monitoringu:

        -   agreguj_wskazniki_1rm() jest "koniem roboczym", w którym definiowany jest zakres zwracanych wskaźników (i wywoływana jest funkcja agreguj_wskazniki(), aby przeprowadzić zadane obliczenia);
        -   agreguj_wskazniki_1rm_szk() zastąpiła agreguj_wskazniki_szk() i agreguj_wskazniki_typ_szk();
        -   agreguj_wskazniki_1rm_szk_branza() zastąpiła agreguj_wskazniki_szk_branza() i agreguj_wskazniki_typ_szk_branza();
        -   nowe funkcje wymienione w dwóch powyższych punktach dają możliwość zadania, że obserwacje należących do analizowanej grupy mają zostać wykluczone z odpowiadającej jej grupy odniesienia, ale domyślnie jest to wyłączone (dla zachowania wstecznej kompatybilności);
        -   format zwracanych ramek danych z wartościami wskaźników na poziomie zagregowanym został zachowany poza tym, że nowe funkcje zwracają kilka dodatkowych kolumn w stosunku do wcześniejszych rozwiązań (co jednak nie powinno wpływać na kompatybilność);

# MLASZdane 0.3.4 (5.06.2020)

-   Poprawki w kodzie zapewniające kompatybilność z dplyr 1.0.0.

# MLASZdane 0.3.3 (2.04.2020)

-   Dodanie mapowania nazw branż według przyporządkowania z 2019 roku dla zmiennych: "ABS_pp3_kierunek_kod", "ABS_f4_id", "ABS_pi2_kod" oraz "ABS_po2_kod".

-   Dodanie obliczania wskaźników indywidualnych opisujących, czy nauka jest kontynuowana w tej samej branży.

-   Etykiety zmiennych `czas_rozp` i `czas_kon` w zbiorach przygotowywanych przez funkcję `wczytaj_wyniki_1rm()` zawierają poprawny rok (2017 zamiast 2015).

-   Aktualizacja sposobu wywołania funkcji `unnest()` z pakietu tidyr po zmianach wprowadzonych w wersji 1.0.0 tego pakietu.

    -   Podniesienie wersji pakietów tidyr, rlang i dplyr w zależnościach.

# MLASZdane 0.3.2 (19.05.2019)

-   Uzupełnienie i poprawki w dokumentacji funkcji.

# MLASZdane 0.3.1 (17.05.2019)

-   Drobne poprawki w winietce dot. 1. rundy monitoringu.

# MLASZdane 0.3.0 (17.03.2019)

-   Nowe funkcja splaszcz_wskazniki_zagregowane() pozwala przekształcić zbiór wskaźników na poziomie zagregowanym do postaci płaskiej ramki danych, która może zostać łatwo zapisana do pliku SPSS lub Staty.

# MLASZdane 0.2.0 (16.03.2019)

-   Naprawiono błędy w sposobie obliczania wskaźników opisujących formę pracy:

    -   nie są już pomijani absolwenci prowadzący własne firmy lub gospodarstwa rolne, którzy zadeklarowali to w pytaniu PG2G (i w związku z tym nie zadano im pytania PG2H),
    -   wskaźniki indywidualne opisujące pracę w formie umowy o pracę na czas określony i na czas nieokreślony nie są używane "na odwrót" przy obliczaniu wskaźników na poziomie zagregowanym.

-   Dokumentacja procedur obliczania wskaźników na podstawie wyników 1. rundy monitoringu.

-   Drobne poprawki w dokumentacji funkcji.

# MLASZdane 0.1.0 (03.03.2019)

-   Pierwsza w pełni funkcjonalna wersja pakietu.
