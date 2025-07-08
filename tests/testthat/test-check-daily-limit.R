library(testthat)
library(RAFALE)

test_that("check_daily_limit incrémente correctement le compteur", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 3)
  client$date <- Sys.Date()
  client$count <- 1L
  expect_no_error(client$check_daily_limit())
  expect_equal(client$count, 2L)
})

test_that("check_daily_limit fonctionne dans le cas normal", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 2)
  client$date <- Sys.Date()
  client$count <- 0L
  expect_no_error(client$check_daily_limit())
  expect_equal(client$count, 1L)
  expect_no_error(client$check_daily_limit())
  expect_equal(client$count, 2L)
  expect_error(client$check_daily_limit(), "Limite quotidienne dépassée")
  expect_equal(client$count, 3L)
})

test_that("check_daily_limit ne dépasse pas la limite quotidienne", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 2)
  client$date <- Sys.Date()
  client$count <- 2L
  expect_error(client$check_daily_limit(), "Limite quotidienne dépassée")
})

test_that("check_daily_limit réinitialise le compteur après changement de jour", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 2)
  client$date <- Sys.Date() - 1
  client$count <- 2L
  expect_no_error(client$check_daily_limit())
  expect_equal(client$count, 1L)
  expect_equal(client$date, Sys.Date())
})

test_that("check_daily_limit ne réinitialise pas si même jour", {
  client <- RAFALE$new(url = "http://api.com", max_daily = 5)
  client$date <- Sys.Date()
  client$count <- 3L
  expect_no_error(client$check_daily_limit())
  expect_equal(client$count, 4L)
})

