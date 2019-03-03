context("Inne testy")

test_that("przywroc_etykiety()", {
  expect_warning(MLASZdane:::przywroc_etykiety(data.frame(a = 1:10),
                                               data.frame(b = 1:10)),
                 "W ramce z danymi nie ma zmiennych: 'b'")
})

test_that("sprawdz_nazwy()", {
  expect_error(sprawdz_nazwy(c("a", "a", "b", "b"),
                             c("d", "e")),
                 "nazwy zmiennych s. zduplikowane")
  expect_error(sprawdz_nazwy(c("a", "b"),
                             c("d", "e")),
               "brakuje zmiennych")
})

