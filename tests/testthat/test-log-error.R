library(testthat)
library(RAFALE)

test_that("log_error écrit dans le fichier de log", {
  client <- RAFALE$new(url = "http://api.com")
  f <- tempfile()
  client$log_file <- f
  client$log_error("Erreur test")
  contenu <- readLines(f)
  expect_true(any(grepl("Erreur test", contenu)))
  unlink(f)
})
