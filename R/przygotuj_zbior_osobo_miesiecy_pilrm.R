#' @title Tworzenie zbioru osobo-miesiecy
#' @description
#' Funkcja przekształca dane o epizodach nauki, pracy i bezrobocia, zebrane
#' w ramach pilotażowej rundy monitoringu na zbiór, w którym obserwacją jest konkretny
#' badany w konkretnym miesiącu, a zmienne opisują jego status zatrudnienia/nauki.
#' @param x lista zwracana przez funkcję \code{\link{imputuj_miesiac_pk_pilrm}}
#' @return ramka danych
#' @details Patrz dokumentacja dot. struktury i zawartości zbiorów danych
#' z \href{../doc/runda_pilotazowa-dokumentacja.html}{rundy pilotażowej}
#' tworzonych przy pomocy funkcji pakietu MLASZdane.
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr .data arrange bind_rows case_when filter first last
#' left_join mutate n select summarise
przygotuj_zbior_osobo_miesiecy_pilrm = function(x) {
  stopifnot(is.list(x),
            all(c("dane", "epizody") %in% names(x)))
  dane = x$dane
  epizody = x$epizody
  rm(x)

  message("Przygotowywanie danych.")
  names(dane) = names(dane) %>% tolower()
  names(dane) = sub("^(id_ibe|id)$", "ID", names(dane))
  dane$ID = as.numeric(dane$ID)
  labTemp = attributes(dane$m3)
  dane$m3 = as.numeric(dane$m3)
  attributes(dane$m3) = labTemp
  dane$f6[dane$f6 == 7] = NA
  dane$m2[!(dane$m2 %in% 1950:2000)] = NA
  dane$m3[!(dane$m3 %in% (1:6))] = NA

  epizody = suppressWarnings(suppressMessages(
    epizody %>%
      filter(.data$typ_epizodu %in% c("bezrobocie", "LO dla dorosłych", "praca",
                                      "SPolic.", "studia")) %>%
      left_join(dane %>% select("ID", "r5s2"))))
  epizody = epizody %>%
    mutate(praca = case_when(.data$pg2h %in% 1 ~ 1,
                             .data$pg2h %in% 2 ~ 2,
                             .data$pg2h %in% 3 ~ 3,
                             .data$pg2h %in% 4 ~ 4,
                             .data$pg2g %in% 4 ~ 5,
                             .data$pg2g %in% 5 ~ 6,
                             .data$pg2h %in% (6:7) ~ 7,
                             .data$pg2h %in% 5 ~ 8,
                             .data$typ_epizodu == "praca" ~ 9,
                             TRUE ~ NA_real_),
           imput_mies_rozp = epizody %>%
             select("zp2a", "sp6c1", "pp6c1", "pg2c", "pb1b") %>%
             is.na() %>%
             apply(1, all),
           imput_mies_kon = epizody %>%
             select("zp2f", "sp6e1", "pp6f1", "pg2e", "pb1d") %>%
             is.na() %>%
             apply(1, all) & .data$czy_zakonczony %in% 1,
           nauka = case_when(.data$typ_epizodu %in% "studia" ~ 2,
                             .data$typ_epizodu %in% c("LO dla dorosłych",
                                                      "SPolic.") ~ 3,
                             TRUE ~ NA_real_),
           bezrobocie = case_when(.data$pb1f %in% 1 ~ 1,
                                  .data$pb1f %in% 2 ~ 2,
                                  .data$typ_epizodu == "bezrobocie" ~ 3,
                                  TRUE ~ NA_real_),
           czas_kon = ifelse(.data$czy_zakonczony %in% 1,
                             .data$czas_kon,
                             17 + as.numeric(.data$r5s2))) %>%
    select("ID", "typ_epizodu", "czas_rozp", "czas_kon", "czy_zakonczony",
           "imput_mies_rozp", "imput_mies_kon", "praca", "nauka", "bezrobocie")
  epizody = suppressWarnings(
    epizody %>%
      bind_rows(data.frame(ID = dane$ID, typ_epizodu = "szkoła", czas_rozp = -9,
                           czas_kon = 0, czy_zakonczony = 1,
                           imput_mies_rozp = FALSE, imput_mies_kon = FALSE,
                           praca = NA, nauka = 1, bezrobocie = NA,
                           stringsAsFactors = FALSE)))
  message("Moment przeprowadzenia wywiadu (w 2017 r.):")
  table(`dzień` = factor(dane$r5s3, 1:31), `miesiąc` = dane$r5s2) %>% print()

  # przekształcanie
  lBD = sum(is.na(epizody$czas_rozp) | is.na(epizody$czas_kon))
  message("Do odrzucenia jest ", lBD, " rekordów (",
          round(100 * lBD / nrow(epizody), 1),
          "% odnotowanych epizodów),\n",
          "odnośnie których respondenci nie byli w stanie podać nawet roku ich rozpoczęcia lub zakończenia.")
  message("\nPrzekształcanie danych.")
  message("  Tworzenie zbioru epizodo-miesięcy.")
  epizodoMiesiace = epizody %>%
    filter(!is.na(.data$czas_rozp) & !is.na(.data$czas_kon)) %>%
    mutate(status = ifelse(!is.na(.data$praca),
                           .data$praca,
                           ifelse(!is.na(.data$nauka),
                                  .data$nauka,
                                  .data$bezrobocie)),
           lp = 1:n(),
           typ_epizodu = ifelse(.data$typ_epizodu %in% c("studia", "SPolic.",
                                                         "LO dla dorosłych",
                                                         "szkoła"),
                                "nauka", .data$typ_epizodu)) %>%
    group_by(.data$ID, .data$lp, .data$typ_epizodu, .data$status,
             .data$imput_mies_rozp, .data$imput_mies_kon) %>%
    do(data.frame(czas = .data$czas_rozp:.data$czas_kon)) %>% # zapewne nie super wydajne, ale wygodne
    arrange(.data$ID, .data$typ_epizodu, .data$lp, .data$czas) %>%
    group_by(.data$ID, .data$typ_epizodu, .data$lp) %>%
    mutate(imput = case_when(n() == 1 ~ .data$imput_mies_rozp & .data$imput_mies_kon,
                             czas == first(.data$czas) ~ .data$imput_mies_rozp,
                             czas == last(.data$czas) ~ .data$imput_mies_kon,
                             TRUE ~ .data$imput_mies_rozp | .data$imput_mies_kon) %>%
             as.numeric())
  epizodoTypMiesiace = epizodoMiesiace %>%
    arrange(.data$ID, .data$typ_epizodu, .data$czas, .data$imput, .data$status) %>%
    group_by(.data$ID, .data$typ_epizodu, .data$czas) %>%
    summarise(status = first(.data$status),
              imput = first(.data$imput))
  message("\n  Tworzenie zbioru osobo-miesięcy.")
  osoboMiesiace = epizodoTypMiesiace %>%
    group_by(.data$ID, .data$czas) %>%
    summarise(praca = ifelse(any(.data$typ_epizodu == "praca"),
                             .data$status[.data$typ_epizodu == "praca"], NA),
              nauka = ifelse(any(.data$typ_epizodu == "nauka"),
                             .data$status[.data$typ_epizodu == "nauka"], NA),
              bezrobocie =  ifelse(any(.data$typ_epizodu == "bezrobocie"),
                                   .data$status[.data$typ_epizodu == "bezrobocie"], NA),
              imput_praca = ifelse(any(.data$typ_epizodu == "praca"),
                                   .data$imput[.data$typ_epizodu == "praca"], NA),
              imput_nauka = ifelse(any(.data$typ_epizodu == "nauka"),
                                   .data$imput[.data$typ_epizodu == "nauka"], NA),
              imput_bezrobocie =  ifelse(any(.data$typ_epizodu == "bezrobocie"),
                                         .data$imput[.data$typ_epizodu == "bezrobocie"], NA)) %>%
    arrange(.data$ID, .data$czas) %>%
    group_by(.data$ID) %>%
    do(status = koryguj_statusy(.data, "ID", 8:9)) %>%
    unnest() %>%
    mutate(status = ifelse(is.na(.data$praca) & is.na(.data$nauka) & is.na(.data$bezrobocie),
                           "999", ""),
           praca = ifelse(is.na(.data$praca), 0, .data$praca),
           nauka = ifelse(is.na(.data$nauka), 0, .data$nauka),
           bezrobocie = ifelse(is.na(.data$bezrobocie), 0, .data$bezrobocie),
           bezrobocie = ifelse(.data$status == "999", 2, .data$bezrobocie),
           status = ifelse(.data$status == "999", .data$status,
                           paste0(.data$praca, .data$nauka, .data$bezrobocie)),
           data = paste0(2015 + (.data$czas + 5) %/% 12, "_",
                         sub(" ", "0", format(1 + (.data$czas %% 12), width = 2))),
           imput_praca = 2 - .data$imput_praca,
           imput_nauka = 2 - .data$imput_nauka,
           imput_bezrobocie = 2 - .data$imput_bezrobocie,
           praca_a_bezrobocie = factor(.data$praca_a_bezrobocie,
                                       levels = c("ndt.",
                                                  "praca na czarno na bezrobociu",
                                                  "miesiąc graniczny",
                                                  "skorygowano pracę",
                                                  "skorygowano bezrobocie",
                                                  "sprzeczne deklaracje resp.",
                                                  "sprzeczne wyniki imputacji")),
           korekta_ciaglosc_nauki = factor(.data$korekta_ciaglosc_nauki,
                                           levels = c("ndt.", "skorygowane")))
  osoboMiesiace = suppressWarnings(suppressMessages(
    osoboMiesiace %>%
      left_join(dane %>%
                  select("ID", "typ_szkoly", "f6", "f7", "m1", "m2", "m3", "r5s2")))) %>%
    mutate(r5s2 = as.numeric(.data$r5s2)) %>%
    filter(.data$czas < (18 + .data$r5s2)) %>%
    select("ID", "typ_szkoly", "r5s2", "f6", "f7", "m1", "m2", "m3",
           "data", "czas", "status", "praca", "nauka", "bezrobocie",
           "praca_a_bezrobocie", "korekta_ciaglosc_nauki", starts_with("imput_"))
  message("\nStatystyki korekt statusów:")
  message("  praca a bezrobocie")
  table(osoboMiesiace$praca_a_bezrobocie) %>%
    as.data.frame() %>%
    setNames(c("sytuacja", "n")) %>%
    print(row.names = FALSE, right = FALSE)
  message("  korekta ciągłości nauki")
  table(osoboMiesiace$korekta_ciaglosc_nauki) %>%
    as.data.frame() %>%
    setNames(c("sytuacja", "n")) %>%
    print(row.names = FALSE, right = FALSE)

  # upiększanie (etykietowanie)
  message("Dodawanie etykiet.")
  attributes(osoboMiesiace$ID)$label = attributes(dane$ID)$label
  attributes(osoboMiesiace$r5s2)$label = attributes(dane$r5s2)$label
  attributes(osoboMiesiace$data)$label = "Miesiąc i rok"
  attributes(osoboMiesiace$status)$label = "Zbiorcza informacja o statusie resp. w danym miesiącu"
  attributes(osoboMiesiace$czas)$label = "Miesiąc i rok wyrażone jako liczba miesięcy od czerwca 2015"
  attributes(osoboMiesiace$praca)$label = "Status zatrudnienia resp. w danym miesiącu"
  attributes(osoboMiesiace$praca)$labels = c(
    "zatrudniony na umowę o pracę" = 1,
    "zatrudniony przez agencję pracy tymczasowej" = 2,
    "zatrudniony na umowie cywilnoprawnej" = 3,
    "samozatrudniony (praca 'u kogoś')" = 4,
    "prowadzi własną działalność ('praca 'u siebie')" = 5,
    "prowadzi własne gosp. rolne" = 6,
    "odbywa staż lub praktykę absolwencką" = 7,
    "zatrudniony bez umowy (na czarno)" = 8,
    "zatrudniony ale nie wiadomo w jakiej formie" = 9,
    "niezatrudniony" = 0)
  attributes(osoboMiesiace$nauka)$label = "Status uczestnictwa w edukacji formalnej resp. w danym miesiącu"
  attributes(osoboMiesiace$nauka)$labels = c(
    "uczy się w szkole, jako uczeń której był badany" = 1,
    "studiuje" = 2,
    "uczy się w LO dla dorosłych lub SPolic." = 3,
    "nie uczy się" = 0)
  attributes(osoboMiesiace$bezrobocie)$label = "Status poszukiwania pracy przez resp. w danym miesiącu"
  attributes(osoboMiesiace$bezrobocie)$labels = c(
    "bezrobotny, poszukuje pracy" = 1,
    "bierny zawodowo" = 2,
    "bez pracy, nie wiadomo, czy bierny, czy poszukuje pracy" = 3,
    "nie jest bezrobotny" = 0)
  attributes(osoboMiesiace$imput_praca)$label = "Czy status zatrudnienia był imputowany?"
  attributes(osoboMiesiace$imput_praca)$labels = c("Tak" = 1, "Nie" = 2)
  attributes(osoboMiesiace$imput_nauka)$label = "Czy status uczestnictwa w edukacji formalnej był imputowany?"
  attributes(osoboMiesiace$imput_nauka)$labels = c("Tak" = 1, "Nie" = 2)
  attributes(osoboMiesiace$imput_bezrobocie)$label = "Czy status poszukiwania pracy był imputowany?"
  attributes(osoboMiesiace$imput_bezrobocie)$labels = c("Tak" = 1, "Nie" = 2)
  attributes(osoboMiesiace$praca_a_bezrobocie)$label = "Informacja o 'konfliktach' pomiędzy statusem zatrudnienia a poszukiwania pracy"
  attributes(osoboMiesiace$korekta_ciaglosc_nauki)$label = "Czy status uczestnictwa w edukacji formalnej był korygowany z powodu ciągłości nauki?"

  for (i in 1:ncol(osoboMiesiace)) {
    if ("label" %in% names(attributes(osoboMiesiace[[i]]))) {
      attributes(osoboMiesiace[[i]])$label = enc2native(attributes(osoboMiesiace[[i]])$label)
    }
    if ("labels" %in% names(attributes(osoboMiesiace[[i]]))) {
      names(attributes(osoboMiesiace[[i]])$labels) =
        enc2native(names(attributes(osoboMiesiace[[i]])$labels))
    }
  }

  return(osoboMiesiace)
}
