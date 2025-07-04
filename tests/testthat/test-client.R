library(testthat)
library(RAFALE)

# ---- SECTION : Initialisation ----
test_that("Client s'initialise correctement", {
  client <- RAFALE$new(url = "https://httpbin.org/get")
  expect_s3_class(client, "RAFALE")
  expect_equal(client$url, "https://httpbin.org/get")
  expect_equal(client$rate_per_sec, 10)
  expect_equal(client$per_page, 200)
  expect_equal(client$max_daily, 1e5)
})

# ---- SECTION : Construction des requêtes ----
test_that("build_query construit les bons paramètres", {
  client <- RAFALE$new(url = "http://fake.api", query = list(a = 1, b = "x"))
  q <- client$build_query(page = 5)
  expect_equal(q$a, 1)
  expect_equal(q$b, "x")
  expect_equal(q$page, 5)
  expect_equal(q$per_page, client$per_page)
})

test_that("build_query ajoute mailto si fourni", {
  client <- RAFALE$new(url = "http://fake.api", mailto = "test@mail.com")
  q <- client$build_query(page = 2)
  expect_equal(q$mailto, "test@mail.com")
})

# ---- SECTION : Gestion du quota quotidien ----
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

# ---- SECTION : Persistance et reprise ----
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

# ---- SECTION : Gestion des erreurs ----
test_that("log_error écrit dans le fichier de log", {
  client <- RAFALE$new(url = "http://api.com")
  f <- tempfile()
  client$log_file <- f
  client$log_error("Erreur test")
  contenu <- readLines(f)
  expect_true(any(grepl("Erreur test", contenu)))
  unlink(f)
})

# ---- SECTION : Requêtes API ----
test_that("fetch_json fonctionne pour une réponse simple", {
  client <- RAFALE$new(url = "https://httpbin.org/json")
  out <- client$fetch_json(list())
  expect_true("slideshow" %in% names(out))
})

test_that("fetch_json gère les erreurs réseau", {
  client <- RAFALE$new(url = "http://localhost:9999/doesnotexist")
  expect_error(client$fetch_json(list(), max_tries = 2))
})

