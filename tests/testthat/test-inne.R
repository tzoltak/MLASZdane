context("Inne testy")
test_that("", {
  expect_warning(MLASZdane:::przywroc_etykiety(data.frame(a = 1:10),
                                               data.frame(b = 1:10)),
                 "W ramce z danymi nie ma zmiennych: 'b'")
})
