#' @title Imputacja miesiacow rozpoczecia i konca epizodow w danych sondazowych
#' @description
#' Funkcja imputuje braki danych w zmiennych opisujących miesiąc rozpoczęcia
#' i miesiąc zakończenia epizodów pracy nauki i bezrobocia w danych sondażowych
#' z pilotażowej rundy monitringu.
#' @param x lista zwracana przez funkcję \code{\link{wczytaj_wyniki_pilrm}}
#' @return ramka danych
#' @export
#' @importFrom stats lm model.frame predict relevel setNames
#' @importFrom tidyr gather spread
#' @importFrom dplyr bind_rows case_when do filter filter_ group_by_ left_join
#' mutate_ rename_ summarise_ ungroup
imputuj_mies_pk_pilrm = function(x) {
  stopifnot(is.list(x), length(x) == 4,
            all(c("dane", "epizody") %in% names(x)))
  mp = options()$max.print
  options(max.print = 99999)
  on.exit(options(max.print = mp))

  message("Przygotowywanie danych.")
  epizodyImput = suppressWarnings(suppressMessages(
    x$epizody %>%
      filter_(~czas_rozp >= -5 & !is.na(czas_rozp),
              ~(!is.na(czas_kon) | czy_zakonczony %in% 2)) %>%
      mutate_(.dots = list(
        rok_rozp = ~2015 + (czas_rozp + 5) %/% 12,
        rok_kon = ~2015 + (czas_kon + 5) %/% 12,
        rok_kon = ~ifelse(is.na(rok_kon), 2017, rok_kon),
        mies_rozp = ~(.) %>% select(zp2a, sp6c1, pp6c1, pg2c, pb1b) %>%
          apply(1, function(x) {return(x[!is.na(x)])}) %>% as.numeric(),
        mies_kon = ~(.) %>% select(zp2f, sp6e1, pp6f1, pg2e, pb1d) %>%
          apply(1, function(x) {return(x[!is.na(x)])}) %>% as.numeric(),
        forma_zatrudnienia = ~case_when(pg2h %in% 1 ~ 1,
                                        pg2h %in% 2 ~ 2,
                                        pg2h %in% 3 ~ 3,
                                        pg2h %in% 4 ~ 4,
                                        pg2g %in% 4 ~ 5,
                                        pg2g %in% 5 ~ 6,
                                        pg2h %in% (6:7) ~ 7,
                                        pg2h %in% 5 ~ 8,
                                        typ_epizodu == "praca" ~ 9,
                                        TRUE ~ 0),
        bezrobocie = ~case_when(pb1f %in% 1 ~ 1,
                                pb1f %in% 2 ~ 2,
                                typ_epizodu == "bezrobocie" ~ 9,
                                TRUE ~ 0))) %>%
      group_by_(~ID) %>%
      mutate_(.dots = list(
        lEpP = ~sum(typ_epizodu %in% "praca"),
        lEpB = ~sum(typ_epizodu %in% "bezrobocie"))) %>%
      group_by_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony,
                ~mies_rozp, ~rok_rozp, ~mies_kon, ~rok_kon, ~lEpP, ~lEpB,
                ~forma_zatrudnienia, ~bezrobocie) %>%
      do(data.frame(rok = .$rok_rozp:.$rok_kon)) %>% # to by pewnie dało się zrobić efektywniej...
      left_join(x$dane %>% select_(~ID, ~typ_szkoly, ~m1, ~r5s2)) %>%
      group_by_(~ID, ~rok) %>%
      mutate_(.dots = list(
        lEpPRR = ~ifelse(rok == rok_rozp,
                         sum(typ_epizodu %in% "praca"), 0),
        lEpPRK = ~ifelse(rok == rok_kon,
                         sum(typ_epizodu %in% "praca"), 0),
        lEpBRR = ~ifelse(rok == rok_rozp,
                         sum(typ_epizodu %in% "bezrobocie"), 0),
        lEpBRK = ~ifelse(rok == rok_kon,
                         sum(typ_epizodu %in% "bezrobocie"), 0),
        naukaRR = ~ifelse(rok == rok_rozp,
                          case_when(any(typ_epizodu %in% "studia") ~ "studia",
                                    any(typ_epizodu %in% "SPolic.") ~ "SPolic.",
                                    any(typ_epizodu %in% "LO dla dorosłych") ~ "LOdD",
                                    TRUE ~ "nie"),
                          "nie"),
        naukaRK = ~ifelse(rok == rok_kon,
                          case_when(any(typ_epizodu %in% "studia") ~ "studia",
                                    any(typ_epizodu %in% "SPolic.") ~ "SPolic.",
                                    any(typ_epizodu %in% "LO dla dorosłych") ~ "LOdD",
                                    TRUE ~ "nie"),
                          "nie"))) %>%
      group_by_(~ID, ~typ_szkoly, ~m1, ~r5s2, ~typ_epizodu, ~nr, ~czas_rozp,
                ~czas_kon, ~czy_zakonczony, ~mies_rozp, ~rok_rozp, ~mies_kon,
                ~rok_kon, ~lEpP, ~lEpB, ~forma_zatrudnienia, ~bezrobocie) %>%
      summarise_(.dots = list(
        lEpPRR = ~sum(lEpPRR),
        lEpPRK = ~sum(lEpPRK),
        lEpBRR = ~sum(lEpBRR),
        lEpBRK = ~sum(lEpBRK),
        naukaRR = ~case_when(any(naukaRR %in% "studia") ~ "studia",
                             any(naukaRR %in% "SPolic.") ~ "SPolic.",
                             any(naukaRR %in% "LOdD") ~ "LOdD",
                             TRUE ~ "nie") %>%
          factor(levels = c("nie", "studia", "SPolic.", "LOdD")),
        naukaRK = ~case_when(any(naukaRK %in% "studia") ~ "studia",
                             any(naukaRK %in% "SPolic.") ~ "SPolic.",
                             any(naukaRK %in% "LOdD") ~ "LOdD",
                             TRUE ~ "nie") %>%
          factor(levels = c("nie", "studia", "SPolic.", "LOdD")))) %>%
      ungroup() %>%
      mutate_(.dots = list(
        rok_kon = ~ifelse(czy_zakonczony %in% 2, NA, rok_kon),
        forma = ~factor(case_when(typ_epizodu == "praca" ~ forma_zatrudnienia,
                           typ_epizodu == "bezrobocie" ~ bezrobocie))),
        r5s2 = ~as.numeric(r5s2))))
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
    filter_(~typ_epizodu %in% c("praca", "bezrobocie")) %>%
    mutate_(.dots = list(
      czas_rozp = ~ifelse(is.na(mies_rozp), NA, czas_rozp),
      czas_kon = ~ifelse(is.na(mies_kon), NA, czas_kon),
      rok_rozp_f = ~factor(rok_rozp),
      rok_kon_f = ~factor(ifelse(czy_zakonczony %in% 2, 2100, rok_kon)))) %>%
    split(.$typ_epizodu)
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
  {message("    R-kwadrat")
    lapply(modeleCzas, function(x) {return(summary(x)$r.squared)}) %>% print()
    message("    odsetek poprawnych klasyfikacji")
    lapply(tabeleCzas, function(x) {return(sum(diag(x)) / sum(x))}) %>% print()}
  epizodyImputM = mapply(function(x, m) {
    x = suppressWarnings(
      x %>%
        mutate_(.dots = list(
          czas_rozp_przew = ~round(predict(m, newdata = x), 0),
          czas_rozp = ~ifelse(is.na(czas_rozp), czas_rozp_przew, czas_rozp),
          czas_rozp = ~ifelse(czas_rozp > (18 + r5s2), 18 + r5s2, czas_rozp),
          check = ~sign((2015 + (czas_rozp + 5) %/% 12) - rok_rozp),
          czas_rozp = ~case_when(check == 1 ~ (rok_rozp - 2014) * 12 - 6,
                                 check == -1 ~ (rok_rozp - 2015) * 12 - 6,
                                 TRUE ~ czas_rozp),
          czas_rozp = ~ifelse(czas_rozp > czas_kon & !is.na(czas_kon),
                              czas_kon, czas_rozp))))
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

  {message("    R-kwadrat")
    lapply(modeleDl, function(x) {return(summary(x)$r.squared)}) %>% print()
    message("    odsetek poprawnych klasyfikacji")
    lapply(tabeleDl, function(x) {return(sum(diag(x)) / sum(x))}) %>% print()}
  epizodyImputM = mapply(function(x, m) {
    x = x %>%
      mutate_(.dots = list(
        rok_kon_f =
          ~factor(rok_kon_f, levels = setdiff(levels(rok_kon_f), "2100"))))
    x = suppressWarnings(
      x %>%
        mutate_(.dots = list(
          czas_kon_przew = ~czas_rozp + round(predict(m, newdata = x), 0),
          czas_kon = ~ifelse(is.na(czas_kon), czas_kon_przew, czas_kon),
          czas_kon = ~ifelse(czas_kon > (18 + r5s2), 18 + r5s2, czas_kon),
          check = ~sign((2015 + (czas_kon + 5) %/% 12) - rok_kon),
          czas_kon = ~case_when(check == 1 ~ (rok_kon - 2014) * 12 - 6,
                                check == -1 ~ (rok_kon - 2015) * 12 - 6,
                                TRUE ~ czas_kon),
          czas_kon = ~ifelse(czas_kon < czas_rozp & !is.na(czas_rozp),
                             czas_rozp, czas_kon))))
    return(x)
  }, epizodyImputM, modeleDl, SIMPLIFY = FALSE)
  #lapply(epizodyImputM, function(x) {return(summary(x$czas_kon))})
  # łączenie i zapis
  epizodyImput = bind_rows(epizodyImputM) %>%
    select_(~ID, ~typ_epizodu, ~nr, ~czas_rozp, ~czas_kon, ~czy_zakonczony) %>%
    rename_(czas_rozp_imput = ~czas_rozp, czas_kon_imput = ~czas_kon)
  with(epizodyImput, table(czas_rozp_imput, czas_kon_imput, exclude = NULL)) %>%
    print()
  with(epizodyImput, table(czas_kon_imput, czy_zakonczony, exclude = NULL)) %>%
    print()

  labWspolne = filter(x$epizody, FALSE)
  x$epizody = suppressWarnings(suppressMessages(
    x$epizody %>%
    left_join(epizodyImput) %>%
    mutate_(.dots = list(
      czas_rozp = ~ifelse(!is.na(czas_rozp_imput), czas_rozp_imput, czas_rozp),
      czas_kon = ~ifelse(!is.na(czas_kon_imput), czas_kon_imput, czas_kon))) %>%
    select_(~-czas_rozp_imput, ~-czas_kon_imput)))
  x$epizody = przywroc_etykiety(x$epizody, labWspolne)
  x$epizody$typ_epizodu = enc2native(x$epizody$typ_epizodu)

  return(x)
}
