library(testthat)
library(RAFALE)

test_that("check_daily_limit se réinitialise bien", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 3)
  client$date <- Sys.Date() - 1  # force réinitialisation
  client$count <- 99L
  expect_no_error(client$check_daily_limit())
  expect_equal(client$count, 1L)
})

test_that("check_daily_limit déclenche une erreur au-delà de la limite", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 1)
  client$count <- 1L
  expect_error(client$check_daily_limit(), "Limite quotidienne dépassée")
})
