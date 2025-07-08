library(testthat)
library(RAFALE)

test_that("build_query construit les bons paramètres", {
  client <- RAFALE$new(url = "http://fake.api", query = list(a = 1, b = "x"))
  q <- client$build_query(page = 5)
  expect_equal(q$a, 1)
  expect_equal(q$b, "x")
  expect_equal(q$page, 5)
})

test_that("build_query ajoute mailto si fourni", {
  client <- RAFALE$new(url = "http://fake.api", mailto = "test@mail.com")
  q <- client$build_query(page = 2)
  expect_equal(q$mailto, "test@mail.com")
})
