#' @title Imputacja miesiacow rozpoczecia i zakonczenia epizodow w danych sondazowych
#' @description
#' Funkcja imputuje braki danych w zmiennych opisujących miesiąc rozpoczęcia
#' i miesiąc zakończenia epizodów pracy nauki i bezrobocia w danych sondażowych
#' z pilotażowej rundy monitringu.
#' @param x lista zwracana przez funkcję \code{\link{wczytaj_wyniki_1rm}}
#' @param print wartość logiczna - czy drukować na konsolę tabele z wynikami
#' imputacji? (warto wyłączyć przy uruchamianiu w ramach testów)
#' @return lista ramek danych o takiej samej strukturze, jak zwracana przez
#' \code{\link{wczytaj_wyniki_1rm}}
#' @details Patrz dokumentacja dot. struktury i zawartości zbiorów danych
#' z \href{../doc/runda_1-dokumentacja.html}{1. rundy monitoringu}
#' tworzonych przy pomocy funkcji pakietu MLASZdane.
#' @export
#' @importFrom stats lm model.frame predict relevel
#' @importFrom dplyr .data bind_rows case_when do filter group_by left_join
#' mutate rename summarise ungroup
imputuj_miesiac_pk_1rm = function(x, print = TRUE) {
  stopifnot(is.list(x),
            all(c("dane", "epizody") %in% names(x)),
            is.logical(print),
            length(print) == 1,
            print %in% c(TRUE, FALSE))
  mp = options()$max.print
  options(max.print = 99999)
  on.exit(options(max.print = mp))

  names(x$epizody) = sub("ABS_", "", names(x$epizody))
  names(x$dane) = sub("ABS_", "", names(x$dane))
  message("Przygotowywanie danych.")
  epizodyImput = x$epizody %>%
    filter((.data$czas_rozp <= 12 & .data$czas_rozp >= -5) | is.na(.data$czas_rozp))
  epizodyImput = suppressWarnings(suppressMessages(
    epizodyImput %>%
      mutate(rok_rozp = 2017 + (.data$czas_rozp + 5) %/% 12,
             rok_rozp =
               ifelse(is.na(.data$rok_rozp),
                      case_when(!is.na(.data$zp2b) ~ as.numeric(.data$zp2b),
                                !is.na(.data$sp6c2) ~ as.numeric(.data$sp6c2),
                                !is.na(.data$pp6c2) ~ as.numeric(.data$pp6c2),
                                !is.na(.data$pg2d) ~ as.numeric(.data$pg2d),
                                !is.na(.data$pb1c) ~ as.numeric(.data$pb1c)),
                      .data$rok_rozp),
             rok_zakon = 2017 + (.data$czas_zakon + 5) %/% 12,
             rok_zakon =
               ifelse(is.na(.data$rok_zakon) & .data$czy_zakonczony %in% 1,
                      case_when(!is.na(.data$zp2g) ~ as.numeric(.data$zp2g),
                                !is.na(.data$sp6e2) ~ as.numeric(.data$sp6e2),
                                !is.na(.data$pp6f2) ~ as.numeric(.data$pp6f2),
                                !is.na(.data$pg2f) ~ as.numeric(.data$pg2f),
                                !is.na(.data$pb1e) ~ as.numeric(.data$pb1e)),
                      .data$rok_zakon),
             mies_rozp = epizodyImput %>%
               select("zp2a", "sp6c1", "pp6c1", "pg2c", "pb1b") %>%
               apply(1, function(x) {return(x[!is.na(x)])}) %>%
               as.numeric(),
             mies_zakon = epizodyImput %>%
               select("zp2f", "sp6e1", "pp6f1", "pg2e", "pb1d") %>%
               apply(1, function(x) {return(x[!is.na(x)])}) %>%
               as.numeric(),
             forma_zatrudnienia = case_when(.data$pg2h %in% 1 ~ 1,
                                            .data$pg2h %in% 2 ~ 2,
                                            .data$pg2h %in% 3 ~ 3,
                                            .data$pg2h %in% 4 ~ 4,
                                            .data$pg2h %in% 5 ~ 5,
                                            .data$pg2g %in% 6 ~ 6,
                                            .data$pg2g %in% 7 ~ 7,
                                            .data$pg2h %in% (7:8) ~ 8,
                                            .data$pg2h %in% 6 ~ 9,
                                            .data$typ_epizodu == "praca" ~ 10,
                                            TRUE ~ 0),
             bezrobocie = case_when(.data$pb1f %in% 1 ~ 1,
                                    .data$pb1f %in% 2 ~ 2,
                                    .data$typ_epizodu == "bezrobocie" ~ 9,
                                    TRUE ~ 0),
             dlugosc_trwania =
               case_when(.data$typ_epizodu == "praca" ~ as.integer(.data$pg2x),
                         .data$typ_epizodu == "bezrobocie" ~ as.integer(.data$pb1x))) %>%
      group_by(.data$ID_RESP) %>%
      mutate(lEpP = sum(.data$typ_epizodu %in% "praca"),
             lEpB = sum(.data$typ_epizodu %in% "bezrobocie")) %>%
      mutate(nauka2017 =
               case_when(any(.data$typ_epizodu %in% "studia" & .data$rok_rozp %in% 2017) ~ "studia",
                         any(.data$typ_epizodu %in% "SPolic." & .data$rok_rozp %in% 2017) ~ "SPolic.",
                         any(.data$typ_epizodu %in% "LOdD" & .data$rok_rozp %in% 2017) ~ "LOdD",
                         any(.data$typ_epizodu %in% "szkoła objęta badaniem" & .data$rok_rozp %in% 2017) ~ "SZ",
                         TRUE ~ "nie") %>%
               factor(levels = c("nie", "studia", "SPolic.", "LOdD", "SZ")),
             nauka2018 =
               case_when(any(.data$typ_epizodu %in% "studia" & .data$rok_rozp %in% 2018) ~ "studia",
                         any(.data$typ_epizodu %in% "SPolic." & .data$rok_rozp %in% 2018) ~ "SPolic.",
                         any(.data$typ_epizodu %in% "LOdD" & .data$rok_rozp %in% 2018) ~ "LOdD",
                         TRUE ~ "nie") %>%
               factor(levels = c("nie", "studia", "SPolic.", "LOdD", "SZ"))) %>%
      ungroup() %>%
      left_join(x$dane %>% select("ID_RESP", "typ_szkoly", "m1", "r5s2")))) %>%
    mutate(m1 = factor(.data$m1, levels = attributes(x$dane$m1)$labels,
                       labels = names(attributes(x$dane$m1)$labels)),
           typ_szkoly = factor(.data$typ_szkoly,
                               levels = attributes(x$dane$typ_szkoly)$labels,
                               labels = names(attributes(x$dane$typ_szkoly)$labels)) %>%
             relevel("Technikum"),
           forma = case_when(.data$typ_epizodu == "praca" ~ .data$forma_zatrudnienia,
                             .data$typ_epizodu == "bezrobocie" ~ .data$bezrobocie) %>%
             factor(),
           r5s2 = as.numeric(.data$r5s2),
           rok_rozp_f = factor(.data$rok_rozp),
           rok_zakon_f = factor(ifelse(.data$czy_zakonczony %in% 2,
                                       2100, .data$rok_zakon)))
  ## modelowanie
  message("Modelowanie:")
  epizodyImputM = epizodyImput %>%
    filter(.data$typ_epizodu %in% c("praca", "bezrobocie"))
  epizodyImputM = split(epizodyImputM, epizodyImputM$typ_epizodu)
  ### czas rozpoczęcia
  message("  czas rozpoczęcia")
  modeleCzas = lapply(epizodyImputM, function(x) {
    return(lm(czas_rozp ~
                m1 * typ_szkoly +
                m1 * forma +
                m1 * (lEpP + lEpB) +
                m1 * rok_rozp_f +
                typ_szkoly * (lEpP + lEpB) +
                typ_szkoly * rok_rozp_f +
                nauka2017 + nauka2018 +
                typ_szkoly * r5s2,
              data = x))})
  tabeleCzas = lapply(modeleCzas, function(x) {
    l = range(c(round(predict(x), 0), model.frame(x)[, 1]))
    return(table(przewidywanie = factor(round(predict(x), 0), levels = l[1]:l[2]),
                 czas_rozp = factor(model.frame(x)[, 1], levels = l[1]:l[2])))
  })
  if (print) {
    cat("    statystyki dopasowania\n")
    data.frame(zmienna = names(modeleCzas),
               R2 = lapply(modeleCzas, function(x) {return(summary(x)$r.squared)}) %>%
                 unlist() %>%
                 round(3),
               `odsetek poprawn. klasyf.` =
                 lapply(tabeleCzas, function(x) {return(sum(diag(x)) / sum(x))}) %>%
                 unlist() %>%
                 round(3),
               check.names = FALSE) %>%
      print(row.names = FALSE)
  }
  epizodyImputM = mapply(function(x, m) {
    x = suppressWarnings(
      x %>%
        mutate(czas_rozp_przew = round(predict(m, newdata = x), 0),
               czas_rozp = ifelse(is.na(.data$czas_rozp),
                                  .data$czas_rozp_przew, .data$czas_rozp),
               czas_rozp = ifelse(.data$czas_rozp > (6 + .data$r5s2),
                                  6 + .data$r5s2, .data$czas_rozp),
               check = sign((2017 + (.data$czas_rozp + 5) %/% 12) - .data$rok_rozp),
               czas_rozp =
                 case_when(.data$check == 1 ~ (.data$rok_rozp - 2016) * 12 - 6,
                           .data$check == -1 ~ (.data$rok_rozp - 2017) * 12 - 6,
                           TRUE ~ .data$czas_rozp),
               czas_rozp =
                 ifelse(.data$czas_rozp > .data$czas_zakon & !is.na(.data$czas_zakon),
                        .data$czas_zakon, .data$czas_rozp)))
    return(x)
  }, epizodyImputM, modeleCzas, SIMPLIFY = FALSE)
  #lapply(epizodyImputM, function(x) {return(summary(x$czas_rozp))})
  ### długość trwania epizodu
  message("  długość trwania epizodu")
  modeleDl = lapply(epizodyImputM, function(x) {
    return(lm(I(czas_zakon - czas_rozp) ~ czas_rozp +
                m1 * typ_szkoly +
                m1 * forma +
                m1 * rok_rozp_f * rok_zakon_f +
                m1 * (lEpP + lEpB) +
                forma * rok_zakon_f +
                typ_szkoly * (lEpP + lEpB) +
                typ_szkoly * rok_rozp_f +
                rok_rozp_f * (lEpP + lEpB) +
                rok_zakon_f * (lEpP + lEpB) +
                rok_zakon_f * r5s2,
              data = x))})
  tabeleDl = lapply(modeleDl, function(x) {
    l = range(c(round(predict(x), 0), model.frame(x)[, 1]))
    return(table(przewidywanie = factor(round(predict(x), 0), levels = l[1]:l[2]),
                 dl = factor(model.frame(x)[, 1], levels = l[1]:l[2])))
  })
  if (print) {
    cat("    statystyki dopasowania\n")
    data.frame(zmienna = names(modeleDl),
               R2 = lapply(modeleDl, function(x) {return(summary(x)$r.squared)}) %>%
                 unlist() %>%
                 round(3),
               `odsetek poprawn. klasyf.` =
                 lapply(tabeleDl, function(x) {return(sum(diag(x)) / sum(x))}) %>%
                 unlist() %>%
                 round(3),
               check.names = FALSE) %>%
      print(row.names = FALSE)
  }
  epizodyImputM = mapply(function(x, m) {
    x = x %>%
      mutate(rok_zakon_f =
               factor(.data$rok_zakon_f,
                      levels = setdiff(levels(.data$rok_zakon_f), "2100")))
    x = suppressWarnings(
      x %>%
        mutate(czas_zakon_przew = round(predict(m, newdata = x), 0),
               czas_zakon_przew = ifelse(.data$czas_zakon_przew < 0,
                                         0, .data$czas_zakon_przew),
               czas_zakon_przew = .data$czas_rozp + .data$czas_zakon_przew,
               czas_zakon = ifelse(is.na(.data$czas_zakon),
                                   .data$czas_zakon_przew, .data$czas_zakon),
               czas_zakon = ifelse(.data$czas_zakon > (6 + .data$r5s2),
                                   6 + .data$r5s2, .data$czas_zakon),
               check = sign((2017 + (.data$czas_zakon + 5) %/% 12) - .data$rok_zakon),
               czas_zakon =
                 case_when(.data$check == 1 ~ (.data$rok_zakon - 2016) * 12 - 6,
                           .data$check == -1 ~ (.data$rok_zakon - 2017) * 12 - 6,
                           TRUE ~ .data$czas_zakon),
               czas_zakon =
                 ifelse(.data$czas_zakon < .data$czas_rozp & !is.na(.data$czas_rozp),
                        .data$czas_rozp, .data$czas_zakon)))
      return(x)
  }, epizodyImputM, modeleDl, SIMPLIFY = FALSE)
  #lapply(epizodyImputM, function(x) {return(summary(x$czas_zakon))})
  # łączenie
  epizodyImput = suppressWarnings(bind_rows(epizodyImputM)) %>%
    select("ID_RESP", "typ_epizodu", "nr", "czas_rozp", "czas_zakon",
           "czy_zakonczony") %>%
    rename(imput_czas_rozp = .data$czas_rozp, imput_czas_zakon = .data$czas_zakon)
  if (print) {
    message("Wyniki imputacji:")
    table(imput_czas_rozp = epizodyImput$imput_czas_rozp,
          imput_czas_zakon = epizodyImput$imput_czas_zakon, exclude = NULL) %>%
      print()
    table(imput_czas_zakon = epizodyImput$imput_czas_zakon,
          czy_zakonczony = epizodyImput$czy_zakonczony, exclude = NULL) %>%
      print()
  }

  labWspolne = filter(x$epizody, FALSE)
  x$epizody = suppressWarnings(suppressMessages(
    x$epizody %>%
      left_join(epizodyImput))) %>%
      mutate(czas_rozp = ifelse(is.na(.data$czas_rozp),
                                .data$imput_czas_rozp, .data$czas_rozp),
             czas_zakon = ifelse(is.na(.data$czas_zakon),
                                 .data$imput_czas_zakon, .data$czas_zakon),
             czas_rozp_imput = ifelse(is.na(.data$czas_rozp),
                                      NA, .data$czas_rozp_imput),
             czas_zakon_imput = ifelse(is.na(.data$czas_zakon),
                                       NA, .data$czas_zakon_imput)) %>%
      select(-"imput_czas_rozp", -"imput_czas_zakon")
  x$epizody = przywroc_etykiety(x$epizody, labWspolne)
  x$epizody$typ_epizodu = enc2native(x$epizody$typ_epizodu)
  maska = !grepl("^(ID_RESP|typ_epizodu|nr|swiadectwo)$|^(czas|czy)_",
                 names(x$epizody))
  names(x$epizody)[maska] = paste0("ABS_", names(x$epizody)[maska])
  maska = !grepl("^(ID_RESP|typ_epizodu|nr|swiadectwo)$|^(czas|czy)_",
                 names(x$dane))
  names(x$dane)[maska] = paste0("ABS_", names(x$dane)[maska])

  return(x)
}
