library(testthat)
library(RAFALE)

test_that("get_start_page lit correctement depuis un fichier RDS", {
  f <- tempfile()
  saveRDS(7L, f)
  client <- RAFALE$new(url = "http://api.com")
  client$page_file <- f
  expect_equal(client$get_start_page(), 7L)
  unlink(f)
})

test_that("get_start_page retourne 1 si aucun fichier", {
  client <- RAFALE$new(url = "http://api.com")
  client$page_file <- tempfile()
  expect_equal(client$get_start_page(), 1L)
})

test_that("save_progress et clear_progress fonctionnent", {
  client <- RAFALE$new(url = "http://api.com")
  f1 <- tempfile(); f2 <- tempfile()
  client$page_file <- f1
  client$fallback_file <- f2
  client$save_progress(9L, data.frame(x = 1:3))
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))
  client$clear_progress()
  expect_false(file.exists(f1))
  unlink(f2)
})
