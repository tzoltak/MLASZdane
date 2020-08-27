#' @title laczy dane ZUS, o wynikach egzaminow, uczniach, studentach i szkolach
#' oraz statystyki powiatow
#' @description Aby poprawnie przyłączyć statystyki powiatów macierz danych
#' uzupełniana jest tak, aby dla każdego absolwenta zawierała informacje o
#' każdym okresie w zadanym przedziale czasu, ale nie później niż do "końca
#' obserwacji w ZUS" (zakładając zamieszkanie absolwenta w uzupełnianych
#' okresach "w Polsce", kodowane jako pna = -1, oraz wszelkie cechy tytułu
#' ubezpieczenia ZUS równe 0, w szczególności także bezrob = 0).
#' @param absolwenci dane opisujące absolwentów
#' @param zus dane o zatrudnieniu w formacie absolwento-miesiąco-płatników
#' @param zdajacyEgzaminy dane o podchodzących do egzaminów
#' @param uczniowie dane o uczniach (kontynuujących naukę)
#' @param studenci dane o studentach (kontynuujących naukę)
#' @param szkoly dane szkół
#' @param pnaPowiaty dane o powiatach
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @return data frame - połączone dane
#' @export
#' @importFrom dplyr anti_join left_join select_ filter_ mutate_ distinct %>%
#' @importFrom methods is
polacz_dane = function(absolwenci, zus, zdajacyEgzaminy, uczniowie, studenci,
                       szkoly, pnaPowiaty, dataMin, dataMax){
  stopifnot(
    is(absolwenci, 'absolwenci_df'),
    is(zus, 'zus_df'),
    is(zdajacyEgzaminy, 'zdajacy_egzaminy_df'),
    is(uczniowie, 'uczniowie_df'),
    is(studenci, 'studenci_df'),
    is(szkoly, 'szkoly_df'),
    is(pnaPowiaty, 'powiaty_df'),
    rownames(anti_join(absolwenci, szkoly) > 0)
  )
  for (i in c("zus", "zdajacyEgaminy", "uczniowie", "studenci"))
  if (rownames(anti_join(get(i), absolwenci) > 0)) {
    warning("Zbiór '", i,
            "' zawiera obserwacje, które nie występują w zbiorze danych 'absolwenci'!")
  }

  # w pakiecie MLAKdane jest od tego funkcja data2okres()
  okresy =
    as.integer(
      sub('-.*$', '', dataMin) * 12 + sub('^[0-9]+-([0-9]+).*$', '\\1', dataMin)
    ):as.integer(
      sub('-.*$', '', dataMax) * 12 + sub('^[0-9]+-([0-9]+).*$', '\\1', dataMax)
    )

  wynik = data.frame(
    id        = rep(absolwenci$id,        each = length(okresy)),
    data_rozp = rep(absolwenci$data_rozp, each = length(okresy)),
    data_zak  = rep(absolwenci$data_zak,  each = length(okresy)),
    szkola_id = rep(absolwenci$szkola_id, each = length(okresy)),
    okres     = rep(okresy,               times = nrow(absolwenci))
  ) %>% left_join(szkoly)

  wynik = wynik %>%
    left_join(
      zus %>%
        select_('-nspraw', '-pkd', '-platnik_kon', '-plec', '-rok_ur',
                '-rolnik', '-zlec', '-koniec')
    ) %>%
    left_join(
      zus %>%
        select_('id', 'koniec') %>%
        distinct()
    ) %>%
    filter_(~ (okres < koniec & data_zak <= koniec) | is.na(koniec))

  wynik = wynik %>%
    left_join(zdajacyEgzaminy) %>%
    left_join(uczniowie) %>%
    left_join(studenci) %>%
    mutate_(
      etat     = ~ifelse(is.na(etat), 0L, etat),
      netat    = ~ifelse(is.na(netat), 0L, netat),
      samoz    = ~ifelse(is.na(samoz), 0L, samoz),
      bezrob   = ~ifelse(is.na(bezrob), 0L, bezrob),
      rentemer = ~ifelse(is.na(rentemer), 0L, rentemer),
      studzus  = ~ifelse(is.na(student), 0L, student), # student wg zus
      prawnik  = ~ifelse(is.na(prawnik), 0L, prawnik),
      mundur   = ~ifelse(is.na(mundur), 0L, mundur),
      # zlec     = ~ifelse(is.na(zlec), 0L, zlec),
      # nspraw   = ~ifelse(is.na(nspraw), 0L, nspraw),
      # rolnik   = ~ifelse(is.na(rolnik), 0, rolnik),
      dziecko  = ~ifelse(is.na(dziecko), 0L, dziecko),
      podst    = ~ifelse(is.na(podst), 0L, podst),
      pna      = ~ifelse(is.na(pna), -1, pna)
    ) %>%
    left_join(wynik, pnaPowiaty)

  stopifnot(
    all(!is.na(wynik$powezar_teryt))
  )

  class(wynik) = c('baza_df', class(wynik))
  return(wynik)
}
