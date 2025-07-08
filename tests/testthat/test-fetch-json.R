library(testthat)
library(RAFALE)

test_that("fetch_json fonctionne pour une réponse simple", {
  client <- RAFALE$new(url = "https://httpbin.org/json")
  out <- client$fetch_json(list())
  expect_true("slideshow" %in% names(out))
})

test_that("fetch_json gère les erreurs réseau", {
  client <- RAFALE$new(url = "http://localhost:9999/doesnotexist")
  expect_error(client$fetch_json(list(), max_tries = 2))
})
