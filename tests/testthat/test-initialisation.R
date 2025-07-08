library(testthat)
library(RAFALE)

test_that("Client s'initialise correctement", {
  client <- RAFALE$new(url = "https://httpbin.org/get")
  expect_s3_class(client, "RAFALE")
  expect_equal(client$url, "https://httpbin.org/get")
  expect_equal(client$rate_per_sec, 10)
  expect_equal(client$per_page, 200)
  expect_equal(client$max_daily, 1e5)
})
