library(testthat)
library(RAFALE)

test_that(
  "Client s'initialise correctement", {
    client <- RAFALE$new(url = "https://httpbin.org/get", max_pages = 5)
    expect_s3_class(client, "RAFALE")
    expect_equal(client$url, "https://httpbin.org/get")
    expect_equal(client$mailto, NULL)
    expect_equal(client$query, list())
    expect_equal(client$per_page, 200)
    expect_equal(client$rate_per_sec, 10)
    expect_equal(client$max_daily, 1e5)
    expect_equal(client$fallback_file, "results.rds")
    expect_equal(client$page_file, "last_page.rds")
    expect_equal(client$log_file, "error_log.txt")
    expect_equal(client$min_delay, 0.1)
    expect_equal(client$count, 0L)
    expect_equal(client$date, Sys.Date())
  }
)
