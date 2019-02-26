#' @title Imputacja miesiacow rozpoczecia i zakonczenia epizodow w danych sondazowych
#' @description
#' Funkcja imputuje braki danych w zmiennych opisujących miesiąc rozpoczęcia
#' i miesiąc zakończenia epizodów pracy nauki i bezrobocia w danych sondażowych
#' z pilotażowej rundy monitringu.
#' @param x lista zwracana przez funkcję \code{\link{wczytaj_wyniki_pilrm}}
#' @return lista ramek danych o takiej samej strukturze, jak zwracana przez
#' \code{\link{wczytaj_wyniki_pilrm}}
#' @details Patrz dokumentacja dot. struktury i zawartości zbiorów danych
#' z \href{../doc/runda_pilotazowa-dokumentacja.html}{rundy pilotażowej}
#' tworzonych przy pomocy funkcji pakietu MLASZdane.
#' @export
#' @importFrom stats lm model.frame predict relevel
#' @importFrom tidyr gather spread
#' @importFrom dplyr .data bind_rows case_when do filter group_by left_join
#' mutate rename summarise ungroup
imputuj_miesiac_pk_pilrm = function(x) {
  stopifnot(is.list(x),
            all(c("dane", "epizody") %in% names(x)))
  mp = options()$max.print
  options(max.print = 99999)
  on.exit(options(max.print = mp))

  message("Przygotowywanie danych.")
  epizodyImput = x$epizody %>%
    filter(.data$czas_rozp >= -5 & !is.na(.data$czas_rozp),
           (!is.na(.data$czas_kon) | .data$czy_zakonczony %in% 2))
    epizodyImput = suppressWarnings(suppressMessages(
      epizodyImput %>%
      mutate(rok_rozp = 2015 + (.data$czas_rozp + 5) %/% 12,
             rok_kon = 2015 + (.data$czas_kon + 5) %/% 12,
             rok_kon = ifelse(is.na(.data$rok_kon), 2017, .data$rok_kon),
             mies_rozp = epizodyImput %>%
               select("zp2a", "sp6c1", "pp6c1", "pg2c", "pb1b") %>%
               apply(1, function(x) {return(x[!is.na(x)])}) %>% as.numeric(),
             mies_kon = epizodyImput %>%
               select("zp2f", "sp6e1", "pp6f1", "pg2e", "pb1d") %>%
               apply(1, function(x) {return(x[!is.na(x)])}) %>% as.numeric(),
             forma_zatrudnienia = case_when(.data$pg2h %in% 1 ~ 1,
                                            .data$pg2h %in% 2 ~ 2,
                                            .data$pg2h %in% 3 ~ 3,
                                            .data$pg2h %in% 4 ~ 4,
                                            .data$pg2g %in% 4 ~ 5,
                                            .data$pg2g %in% 5 ~ 6,
                                            .data$pg2h %in% (6:7) ~ 7,
                                            .data$pg2h %in% 5 ~ 8,
                                            .data$typ_epizodu == "praca" ~ 9,
                                            TRUE ~ 0),
             bezrobocie = case_when(.data$pb1f %in% 1 ~ 1,
                                    .data$pb1f %in% 2 ~ 2,
                                    .data$typ_epizodu == "bezrobocie" ~ 9,
                                    TRUE ~ 0)) %>%
      group_by(.data$ID) %>%
      mutate(lEpP = sum(.data$typ_epizodu %in% "praca"),
             lEpB = sum(.data$typ_epizodu %in% "bezrobocie")) %>%
      group_by(.data$ID, .data$typ_epizodu, .data$nr,
               .data$czas_rozp, .data$czas_kon, .data$czy_zakonczony,
               .data$mies_rozp, .data$rok_rozp, .data$mies_kon, .data$rok_kon,
               .data$lEpP, .data$lEpB, .data$forma_zatrudnienia, .data$bezrobocie) %>%
      do(data.frame(rok = .data$rok_rozp:.data$rok_kon)) %>% # to by pewnie dało się zrobić efektywniej...
      left_join(x$dane %>% select("ID", "typ_szkoly", "m1", "r5s2")) %>%
      group_by(.data$ID, .data$rok) %>%
      mutate(
        lEpPRR = ifelse(.data$rok == .data$rok_rozp,
                        sum(.data$typ_epizodu %in% "praca"), 0),
        lEpPRK = ifelse(.data$rok == .data$rok_kon,
                        sum(.data$typ_epizodu %in% "praca"), 0),
        lEpBRR = ifelse(.data$rok == .data$rok_rozp,
                        sum(.data$typ_epizodu %in% "bezrobocie"), 0),
        lEpBRK = ifelse(.data$rok == .data$rok_kon,
                        sum(.data$typ_epizodu %in% "bezrobocie"), 0),
        naukaRR = ifelse(.data$rok == .data$rok_rozp,
                         case_when(any(.data$typ_epizodu %in% "studia") ~ "studia",
                                   any(.data$typ_epizodu %in% "SPolic.") ~ "SPolic.",
                                   any(.data$typ_epizodu %in% "LO dla dorosłych") ~ "LOdD",
                                   TRUE ~ "nie"),
                         "nie"),
        naukaRK = ifelse(.data$rok == .data$rok_kon,
                         case_when(any(.data$typ_epizodu %in% "studia") ~ "studia",
                                   any(.data$typ_epizodu %in% "SPolic.") ~ "SPolic.",
                                   any(.data$typ_epizodu %in% "LO dla dorosłych") ~ "LOdD",
                                   TRUE ~ "nie"),
                         "nie")) %>%
      group_by(.data$ID, .data$typ_szkoly, .data$m1, .data$r5s2,
               .data$typ_epizodu, .data$nr, .data$czas_rozp, .data$czas_kon,
               .data$czy_zakonczony, .data$mies_rozp, .data$rok_rozp,
               .data$mies_kon, .data$rok_kon, .data$lEpP, .data$lEpB,
               .data$forma_zatrudnienia, .data$bezrobocie) %>%
      summarise(lEpPRR = sum(.data$lEpPRR),
                lEpPRK = sum(.data$lEpPRK),
                lEpBRR = sum(.data$lEpBRR),
                lEpBRK = sum(.data$lEpBRK),
                naukaRR = case_when(any(.data$naukaRR %in% "studia") ~ "studia",
                                    any(.data$naukaRR %in% "SPolic.") ~ "SPolic.",
                                    any(.data$naukaRR %in% "LOdD") ~ "LOdD",
                                    TRUE ~ "nie") %>%
                  factor(levels = c("nie", "studia", "SPolic.", "LOdD")),
                naukaRK = case_when(any(.data$naukaRK %in% "studia") ~ "studia",
                                    any(.data$naukaRK %in% "SPolic.") ~ "SPolic.",
                                    any(.data$naukaRK %in% "LOdD") ~ "LOdD",
                                    TRUE ~ "nie") %>%
                  factor(levels = c("nie", "studia", "SPolic.", "LOdD"))) %>%
      ungroup() %>%
      mutate(rok_kon = ifelse(.data$czy_zakonczony %in% 2, NA, .data$rok_kon),
             forma = factor(case_when(.data$typ_epizodu == "praca" ~ .data$forma_zatrudnienia,
                                      .data$typ_epizodu == "bezrobocie" ~ .data$bezrobocie)),
             r5s2 = as.numeric(.data$r5s2))))
  epizodyImput$m1 = factor(epizodyImput$m1,
                           levels = attributes(x$dane$m1)$labels,
                           labels = names(attributes(x$dane$m1)$labels))
  epizodyImput$typ_szkoly =
    factor(epizodyImput$typ_szkoly,
           levels = attributes(x$dane$typ_szkoly)$labels,
           labels = names(attributes(x$dane$typ_szkoly)$labels)) %>%
    relevel("Technikum")
  cat("\n")
  ## modelowanie
  message("Modelowanie:")
  epizodyImputM = epizodyImput %>%
    filter(.data$typ_epizodu %in% c("praca", "bezrobocie")) %>%
    mutate(czas_rozp = ifelse(is.na(.data$mies_rozp), NA, .data$czas_rozp),
           czas_kon = ifelse(is.na(.data$mies_kon), NA, .data$czas_kon),
           rok_rozp_f = factor(.data$rok_rozp),
           rok_kon_f = factor(ifelse(.data$czy_zakonczony %in% 2,
                                     2100, .data$rok_kon)))
  epizodyImputM = split(epizodyImputM, epizodyImputM$typ_epizodu)
  ### czas rozpoczęcia
  message("  czas rozpoczęcia:")
  modeleCzas = lapply(epizodyImputM, function(x) {
    return(lm(czas_rozp ~
                m1 * rok_rozp_f * rok_kon_f + forma + typ_szkoly +
                m1 * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                typ_szkoly * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                forma * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                rok_rozp_f * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                rok_kon_f * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                m1 * (naukaRR + naukaRK) +
                typ_szkoly * (naukaRR + naukaRK) +
                forma * (naukaRR + naukaRK) +
                rok_rozp_f * (naukaRR + naukaRK) +
                rok_kon_f * (naukaRR + naukaRK) +
                rok_rozp_f * r5s2,
              data = x))})
  tabeleCzas = lapply(modeleCzas, function(x) {
    l = range(c(round(predict(x), 0), model.frame(x)[, 1]))
    return(table(przewidywanie = factor(round(predict(x), 0), levels = l[1]:l[2]),
                 dl = factor(model.frame(x)[, 1], levels = l[1]:l[2])))
  })
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
  epizodyImputM = mapply(function(x, m) {
    x = suppressWarnings(
      x %>%
        mutate(czas_rozp_przew = round(predict(m, newdata = x), 0),
               czas_rozp = ifelse(is.na(.data$czas_rozp),
                                  .data$czas_rozp_przew, .data$czas_rozp),
               czas_rozp = ifelse(.data$czas_rozp > (18 + .data$r5s2),
                                  18 + .data$r5s2, .data$czas_rozp),
               check = sign((2015 + (.data$czas_rozp + 5) %/% 12) - .data$rok_rozp),
               czas_rozp = case_when(.data$check == 1 ~ (.data$rok_rozp - 2014) * 12 - 6,
                                     .data$check == -1 ~ (.data$rok_rozp - 2015) * 12 - 6,
                                     TRUE ~ .data$czas_rozp),
               czas_rozp = ifelse(.data$czas_rozp > .data$czas_kon & !is.na(.data$czas_kon),
                                  .data$czas_kon, .data$czas_rozp)))
      return(x)
  }, epizodyImputM, modeleCzas, SIMPLIFY = FALSE)
  #lapply(epizodyImputM, function(x) {return(summary(x$czas_rozp))})
  ### długość trwania epizodu
  message("  długość trwania epizodu:")
  modeleDl = lapply(epizodyImputM, function(x) {
    return(lm(I(czas_kon - czas_rozp) ~ czas_rozp +
                m1 * rok_rozp_f * rok_kon_f + forma + typ_szkoly +
                m1 * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                typ_szkoly * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                forma * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                rok_rozp_f * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                rok_kon_f * (lEpP + lEpPRR + lEpPRK + lEpB + lEpBRR + lEpBRK) +
                m1 * (naukaRR + naukaRK) +
                typ_szkoly * (naukaRR + naukaRK) +
                forma * (naukaRR + naukaRK) +
                rok_rozp_f * (naukaRR + naukaRK) +
                rok_kon_f * (naukaRR + naukaRK) +
                rok_kon_f * r5s2,
              data = x))})
  tabeleDl = lapply(modeleDl, function(x) {
    l = range(c(round(predict(x), 0), model.frame(x)[, 1]))
    return(table(przewidywanie = factor(round(predict(x), 0), levels = l[1]:l[2]),
                 dl = factor(model.frame(x)[, 1], levels = l[1]:l[2])))
  })
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
  epizodyImputM = mapply(function(x, m) {
    x = x %>%
      mutate(rok_kon_f =
               factor(.data$rok_kon_f,
                      levels = setdiff(levels(.data$rok_kon_f), "2100")))
    x = suppressWarnings(
      x %>%
        mutate(czas_kon_przew = .data$czas_rozp + round(predict(m, newdata = x), 0),
               czas_kon = ifelse(is.na(.data$czas_kon),
                                 .data$czas_kon_przew, .data$czas_kon),
               czas_kon = ifelse(.data$czas_kon > (18 + .data$r5s2),
                                 18 + .data$r5s2, .data$czas_kon),
               check = sign((2015 + (.data$czas_kon + 5) %/% 12) - .data$rok_kon),
               czas_kon = case_when(.data$check == 1 ~ (.data$rok_kon - 2014) * 12 - 6,
                                    .data$check == -1 ~ (.data$rok_kon - 2015) * 12 - 6,
                                    TRUE ~ .data$czas_kon),
               czas_kon = ifelse(.data$czas_kon < .data$czas_rozp & !is.na(.data$czas_rozp),
                                 .data$czas_rozp, .data$czas_kon)))
    return(x)
  }, epizodyImputM, modeleDl, SIMPLIFY = FALSE)
  #lapply(epizodyImputM, function(x) {return(summary(x$czas_kon))})
  # łączenie
  epizodyImput = suppressWarnings(bind_rows(epizodyImputM)) %>%
    select("ID", "typ_epizodu", "nr", "czas_rozp", "czas_kon", "czy_zakonczony") %>%
    rename(czas_rozp_imput = .data$czas_rozp, czas_kon_imput = .data$czas_kon)
  message("Wyniki imputacji:")
  table(czas_rozp_imput = epizodyImput$czas_rozp_imput,
        czas_kon_imput = epizodyImput$czas_kon_imput, exclude = NULL) %>%
    print()
  table(czas_kon_imput = epizodyImput$czas_kon_imput,
        czy_zakonczony = epizodyImput$czy_zakonczony, exclude = NULL) %>%
    print()

  labWspolne = filter(x$epizody, FALSE)
  x$epizody = suppressWarnings(suppressMessages(
    x$epizody %>%
    left_join(epizodyImput))) %>%
    mutate(czas_rozp = ifelse(!is.na(.data$czas_rozp_imput),
                              .data$czas_rozp_imput, .data$czas_rozp),
           czas_kon = ifelse(!is.na(.data$czas_kon_imput),
                             .data$czas_kon_imput, .data$czas_kon)) %>%
    select(-"czas_rozp_imput", -"czas_kon_imput")
  x$epizody = przywroc_etykiety(x$epizody, labWspolne)
  x$epizody$typ_epizodu = enc2native(x$epizody$typ_epizodu)

  return(x)
}
