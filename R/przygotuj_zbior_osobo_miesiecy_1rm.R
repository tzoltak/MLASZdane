#' @title Tworzenie zbioru osobo-miesiecy
#' @description
#' Funkcja przekształca dane o epizodach nauki, pracy i bezrobocia, zebrane
#' w ramach 1. rundy monitoringu na zbiór, w którym obserwacją jest konkretny
#' badany w konkretnym miesiącu, a zmienne opisują jego status zatrudnienia/nauki.
#' @param x lista zwracana przez funkcję \code{\link{imputuj_miesiac_pk_1rm}}
#' @param print wartość logiczna - czy drukować na konsolę tabele
#' z podsumowaniami przekształcanych danych? (warto wyłączyć przy uruchamianiu
#' w ramach testów)
#' @return ramka danych
#' @details Patrz dokumentacja dot. struktury i zawartości zbiorów danych
#' z \href{../doc/runda_1-dokumentacja.html}{1. rundy monitoringu}
#' tworzonych przy pomocy funkcji pakietu MLASZdane.
#' @export
#' @importFrom tidyr unnest
#' @importFrom dplyr .data arrange case_when filter first last left_join mutate
#' n select summarise
przygotuj_zbior_osobo_miesiecy_1rm = function(x, print = TRUE) {
  stopifnot(is.list(x),
            all(c("dane", "epizody") %in% names(x)),
            is.logical(print),
            length(print) == 1,
            print %in% c(TRUE, FALSE))
  dane = x$dane
  epizody = x$epizody
  rm(x)

  message("Przygotowywanie danych.")
  names(dane) = sub("ABS_", "", names(dane))
  names(dane) = names(dane) %>% tolower()
  names(dane) = sub("^id_resp$", "ID_RESP", names(dane))
  labTemp = attributes(dane$m3)
  dane$m3 = as.numeric(dane$m3)
  attributes(dane$m3) = labTemp
  dane$f6[dane$f6 == 7] = NA
  dane$m2[!(dane$m2 %in% 1950:2000)] = NA
  dane$m3[!(dane$m3 %in% (1:6))] = NA

  names(epizody) = sub("ABS_", "", names(epizody))
  epizody = suppressWarnings(suppressMessages(
    epizody %>%
      filter(.data$typ_epizodu %in% c("bezrobocie", "LO dla dorosłych", "praca",
                                      "SPolic.", "studia", "szkoła objęta badaniem")) %>%
      left_join(dane %>%
                  select("ID_RESP", "r5s2")))) %>%
    mutate(typ_epizodu = ifelse(.data$typ_epizodu == "szkoła objęta badaniem",
                                "szkoła", .data$typ_epizodu),
           praca = case_when(.data$pg2h %in% 1 ~ 1,
                             .data$pg2h %in% 2 ~ 2,
                             .data$pg2h %in% 3 ~ 3,
                             .data$pg2h %in% 4 ~ 4,
                             .data$pg2h %in% 5 ~ 5,
                             .data$pg2g %in% 6 ~ 6,
                             .data$pg2g %in% 7 ~ 7,
                             .data$pg2h %in% (7:8) ~ 8,
                             .data$pg2h %in% 6 ~ 9,
                             .data$typ_epizodu == "praca" ~ 10,
                             TRUE ~ NA_real_),
           nauka = case_when(.data$typ_epizodu == "szkoła" ~ 1,
                             .data$typ_epizodu == "studia" ~ 2,
                             .data$typ_epizodu %in% c("LO dla dorosłych", "SPolic.") ~ 3,
                             TRUE ~ NA_real_),
           bezrobocie = case_when(.data$pb1f %in% 1 ~ 1,
                                  .data$pb1f %in% 2 ~ 2,
                                  .data$typ_epizodu == "bezrobocie" ~ 3,
                                  TRUE ~ NA_real_),
           czas_zakon = ifelse(.data$czy_zakonczony %in% 1,
                               .data$czas_zakon, 5 + as.numeric(.data$r5s2)),
           czas_rozp = ifelse(.data$typ_epizodu == "szkoła",
                              -9, .data$czas_rozp),
           czas_rozp_imput = .data$czas_rozp_imput == 1,
           czas_zakon_imput = .data$czas_zakon_imput == 1) %>%
    select("ID_RESP", "typ_epizodu", "czas_rozp", "czas_zakon", "czy_zakonczony",
           "czas_rozp_imput", "czas_zakon_imput", "praca", "nauka", "bezrobocie")
  if (print) {
    message("Moment przeprowadzenia wywiadu (w 2018 r.):")
    table(`dzień` = factor(dane$r5s3, 1:31), `miesiąc` = dane$r5s2) %>% print()
  }

  # przekształcanie
  lBD = sum(is.na(epizody$czas_rozp) | is.na(epizody$czas_zakon))
  message("Do odrzucenia jest ", lBD, " rekordów (",
          round(100 * lBD / nrow(epizody), 1),
          "% odnotowanych epizodów),\n",
          "odnośnie których respondenci nie byli w stanie podać nawet roku ich rozpoczęcia lub zakończenia.")
  message("\nPrzekształcanie danych.")
  message("  Tworzenie zbioru epizodo-miesięcy.")
  epizodoMiesiace = epizody %>%
    filter(!is.na(.data$czas_rozp) & !is.na(.data$czas_zakon)) %>%
    filter(.data$czas_zakon >= -9) %>%
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
    group_by(.data$ID_RESP, .data$lp, .data$typ_epizodu, .data$status,
             .data$czas_rozp_imput, .data$czas_zakon_imput) %>%
    do(data.frame(czas = .data$czas_rozp:.data$czas_zakon)) %>% # zapewne nie super wydajne, ale wygodne
    arrange(.data$ID_RESP, .data$typ_epizodu, .data$lp, .data$czas) %>%
    group_by(.data$ID_RESP, .data$typ_epizodu, .data$lp) %>%
    mutate(imput =
             case_when(n() == 1 ~ .data$czas_rozp_imput & .data$czas_zakon_imput,
                       czas == first(.data$czas) ~ .data$czas_rozp_imput,
                       czas == last(.data$czas) ~ .data$czas_zakon_imput,
                       TRUE ~ .data$czas_rozp_imput | .data$czas_zakon_imput) %>%
             as.numeric())
  epizodoTypMiesiace = epizodoMiesiace %>%
    arrange(.data$ID_RESP, .data$typ_epizodu, .data$czas, .data$imput,
            .data$status) %>%
    group_by(.data$ID_RESP, .data$typ_epizodu, .data$czas) %>%
    summarise(status = first(.data$status),
              imput = first(.data$imput))
  message("\n  Tworzenie zbioru osobo-miesięcy.")
  osoboMiesiace = epizodoTypMiesiace %>%
    group_by(.data$ID_RESP, .data$czas) %>%
    summarise(praca = ifelse(any(.data$typ_epizodu == "praca"),
                             .data$status[.data$typ_epizodu == "praca"], NA),
              nauka = ifelse(any(.data$typ_epizodu == "nauka"),
                             .data$status[.data$typ_epizodu == "nauka"], NA),
              bezrobocie = ifelse(any(.data$typ_epizodu == "bezrobocie"),
                                  .data$status[.data$typ_epizodu == "bezrobocie"], NA),
              imput_praca = ifelse(any(.data$typ_epizodu == "praca"),
                                   .data$imput[.data$typ_epizodu == "praca"], NA),
              imput_nauka = ifelse(any(.data$typ_epizodu == "nauka"),
                                   .data$imput[.data$typ_epizodu == "nauka"], NA),
              imput_bezrobocie = ifelse(any(.data$typ_epizodu == "bezrobocie"),
                                        .data$imput[.data$typ_epizodu == "bezrobocie"], NA)) %>%
    arrange(.data$ID_RESP, .data$czas) %>%
    group_by(.data$ID_RESP) %>%
    do(status = koryguj_statusy(.data, "ID_RESP", 9:10)) %>%
    unnest(cols = "status") %>%
    mutate(status = ifelse(is.na(.data$praca) & is.na(.data$nauka) & is.na(.data$bezrobocie),
                           "999", ""),
           praca = ifelse(is.na(.data$praca), 0, .data$praca),
           nauka = ifelse(is.na(.data$nauka), 0, .data$nauka),
           bezrobocie = ifelse(is.na(.data$bezrobocie), 0, .data$bezrobocie),
           bezrobocie = ifelse(.data$status == "999", 2, .data$bezrobocie),
           status = ifelse(.data$status == "999", .data$status,
                           paste0(.data$praca, .data$nauka, .data$bezrobocie)),
           data = paste0(2017 + (.data$czas + 5) %/% 12, "_",
                         sub(" ", "0",
                             format(1 + ((.data$czas - 7) %% 12), width = 2))),
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
                  select("ID_RESP", "typ_szkoly", "f4", "f6", "f7",
                         "m1", "m2", "m3", "r5s2")))) %>%
    mutate(r5s2 = as.numeric(.data$r5s2)) %>%
    filter(.data$czas < (6 + .data$r5s2), .data$czas >= -9) %>%
    select("ID_RESP", "typ_szkoly", "r5s2", "f4", "f6", "f7",
           "m1", "m2", "m3", "data", "czas",
           "status", "praca", "nauka", "bezrobocie", "praca_a_bezrobocie",
           "korekta_ciaglosc_nauki", starts_with("imput_"))
  if (print) {
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
  }

  # upiększanie (etykietowanie)
  message("Dodawanie etykiet.")
  attributes(osoboMiesiace$ID_RESP)$label = attributes(dane$ID_RESP)$label
  attributes(osoboMiesiace$r5s2)$label = attributes(dane$r5s2)$label
  osoboMiesiace$f4 = factor(osoboMiesiace$f4)
  attributes(osoboMiesiace$f4)$label = attributes(dane$f4)$label
  attributes(osoboMiesiace$data)$label = "Miesiąc i rok"
  attributes(osoboMiesiace$status)$label = "Zbiorcza informacja o statusie resp. w danym miesiącu"
  attributes(osoboMiesiace$czas)$label = "Miesiąc i rok wyrażone jako liczba miesięcy od czerwca 2017"
  attributes(osoboMiesiace$praca)$label = "Status zatrudnienia resp. w danym miesiącu"
  attributes(osoboMiesiace$praca)$labels = c(
    "zatrudniony na umowę o pracę na czas określony" = 1,
    "zatrudniony na umowę o pracę na czas nieokreślony" = 2,
    "zatrudniony przez agencję pracy tymczasowej" = 3,
    "zatrudniony na umowie cywilnoprawnej" = 4,
    "samozatrudniony (praca 'u kogoś')" = 5,
    "prowadzi własną działalność ('praca 'u siebie')" = 6,
    "prowadzi własne gosp. rolne" = 7,
    "odbywa staż lub praktykę absolwencką" = 8,
    "zatrudniony bez umowy (na czarno)" = 9,
    "zatrudniony ale nie wiadomo w jakiej formie" = 10,
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
      class(osoboMiesiace[[i]]) = "haven_labelled"
    }
  }

  return(osoboMiesiace)
}
