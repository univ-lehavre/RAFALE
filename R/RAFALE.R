#' @title RAFALE
#' @description Classe R6 pour scrapper des APIs paginées avec gestion du débit, des erreurs et de la persistance.
#' @field url URL de l'API à interroger.
#' @field mailto Adresse mail à inclure dans la requête (optionnel).
#' @field query Liste de paramètres de requête additionnels.
#' @field per_page Nombre d'éléments par page.
#' @field max_pages Nombre maximal de pages à récupérer.
#' @field rate_per_sec Nombre maximal de requêtes par seconde.
#' @field max_daily Nombre maximal de requêtes par jour.
#' @field fallback_file Fichier de sauvegarde des résultats.
#' @field page_file Fichier de sauvegarde de la dernière page atteinte.
#' @field log_file Fichier de log des erreurs.
#' @field min_delay Délai minimal entre deux requêtes.
#' @field count Compteur de requêtes du jour.
#' @field date Date du dernier reset du compteur.
#'
#' @section Méthodes publiques :
#'
#' @details
#' \describe{
#'   \item{\code{initialize(url, mailto = NULL, query = list(), per_page = 200, max_pages = Inf, rate_per_sec = 10, max_daily = 1e5, fallback_file = "results.rds")}}{Initialise un nouvel objet RAFALE.}
#'   \item{\code{check_daily_limit()}}{Vérifie et incrémente le compteur de requêtes quotidiennes. Stoppe si la limite est dépassée.}
#'   \item{\code{log_error(msg)}}{Ajoute un message d'erreur au fichier de log.}
#'   \item{\code{fetch_json(query, max_tries = 3)}}{Effectue une requête GET sur l'API, gère les erreurs et retourne le JSON.}
#'   \item{\code{build_query(page)}}{Construit la liste des paramètres de requête pour une page donnée.}
#'   \item{\code{get_start_page()}}{Retourne la page de reprise (1 si aucune sauvegarde).}
#'   \item{\code{save_progress(page, data)}}{Sauvegarde la progression (page courante et données).}
#'   \item{\code{clear_progress()}}{Efface la sauvegarde de progression.}
#'   \item{\code{scrape(show_progress = TRUE)}}{Scrappe toutes les pages de l'API, gère la persistance et retourne un tibble.}
#' }
#'
#' @examples
#' \dontrun{
#' scraper <- RAFALE$new(url = "https://api.exemple.com/data")
#' data <- scraper$scrape()
#' }
#'
#' @name RAFALE
#' @export

library(R6)
library(httr)
library(jsonlite)
library(dplyr)
library(progress)

RAFALE <- R6Class("RAFALE",
  public = list(
    url            = NULL,
    mailto         = NULL,
    query          = list(),
    per_page       = 200,
    max_pages      = Inf,
    rate_per_sec   = 10,
    max_daily      = 1e5,
    fallback_file  = "results.rds",
    page_file      = "last_page.rds",
    log_file       = "error_log.txt",
    min_delay      = 0.1,
    count          = 0L,
    date           = Sys.Date(),

    initialize = function(url, mailto = NULL, query = list(), per_page = 200,
                          max_pages = Inf, rate_per_sec = 10, max_daily = 1e5,
                          fallback_file = "results.rds") {
      self$url           <- url
      self$mailto        <- mailto
      self$query         <- query
      self$per_page      <- per_page
      self$max_pages     <- max_pages
      self$rate_per_sec  <- rate_per_sec
      self$max_daily     <- max_daily
      self$fallback_file <- fallback_file
      self$min_delay     <- 1 / rate_per_sec
    },

    check_daily_limit = function() {
      today <- Sys.Date()
      if (self$date != today) {
        self$date  <- today
        self$count <- 0L
      }
      self$count <- self$count + 1L
      if (self$count > self$max_daily) {
        stop(sprintf("Limite quotidienne dépassée (%d)", self$max_daily))
      }
    },

    log_error = function(msg) {
      cat(sprintf("[%s] %s\n", Sys.time(), msg), file = self$log_file, append = TRUE)
    },

    fetch_json = function(query, max_tries = 3) {
      attempt <- 1
      repeat {
        self$check_daily_limit()
        res <- tryCatch(
          GET(self$url, query = query, timeout(10)),
          error = function(e) e
        )
        if (inherits(res, "error")) {
          self$log_error(paste("Erreur réseau:", res$message))
        } else if (!http_error(res)) {
          Sys.sleep(self$min_delay)
          return(fromJSON(content(res, "text", encoding = "UTF-8"), flatten = FALSE))
        } else {
          code <- status_code(res)
          if (code %in% c(429, 500:599)) {
            self$min_delay <- min(self$min_delay * 1.5, 60)
            self$log_error(sprintf("HTTP %d → délai augmenté à %.1fs", code, self$min_delay))
          } else {
            self$log_error(sprintf("HTTP %d : %s", code, self$url))
          }
        }
        if (attempt >= max_tries) stop(sprintf("Échec après %d tentatives", attempt))
        Sys.sleep(2 ^ (attempt - 1))
        attempt <- attempt + 1
      }
    },

    build_query = function(page) {
      c(
        if (!is.null(self$mailto)) list(mailto = self$mailto),
        self$query,
        list(page = page, per_page = self$per_page)
      )
    },

    get_start_page = function() {
      if (file.exists(self$page_file)) readRDS(self$page_file) else 1L
    },

    save_progress = function(page, data) {
      saveRDS(page, file = self$page_file)
      saveRDS(data, file = self$fallback_file)
    },

    clear_progress = function() {
      if (file.exists(self$page_file)) file.remove(self$page_file)
    },

    scrape = function(show_progress = TRUE) {
      start_pg <- self$get_start_page()
      q1 <- self$build_query(start_pg)
      message(sprintf("Fetching page %d", start_pg))
      res <- self$fetch_json(q1)
      total_pages <- ceiling(res$meta$count / res$meta$per_page)
      n_pages <- min(total_pages, self$max_pages)
      message(sprintf("Récupération de %d pages", n_pages))
      all <- list()
      if (start_pg == 1L) all[[1]] <- res$results

      if (show_progress) {
        pb <- progress_bar$new(
          total = n_pages - start_pg + 1,
          format = "  [:bar] :current/:total (:percent)"
        )
      }

      for (pg in seq(start_pg + (start_pg == 1L), n_pages)) {
        if (show_progress) pb$tick()
        query <- self$build_query(pg)
        tryCatch({
          res <- self$fetch_json(query)
          all[[pg]] <- res$results
          self$save_progress(pg, bind_rows(all))
        }, error = function(e) {
          self$log_error(sprintf("Erreur page %d : %s", pg, e$message))
        })
      }

      self$clear_progress()
      final <- bind_rows(all) |> tibble()
      saveRDS(final, self$fallback_file)
      final
    }
  )
)
