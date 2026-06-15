#' bulletAnalyzr scoring backends
#'
#' bulletAnalyzr can score a bullet comparison three ways:
#' \describe{
#'   \item{\code{"legacy_rtrees"}}{The historical per-land random forest
#'     (\code{bulletxtrctr::rtrees}). This is the engine behind the interactive
#'     land-by-land comparison report and is the current runtime default.}
#'   \item{\code{"api"}}{The deployed bulletAnalyzrResearch v0.6.0 service
#'     (\code{POST /compare}); returns a pair-level match decision at the
#'     production threshold (0.57).}
#'   \item{\code{"local_v060"}}{The same v0.6.0 model run in-process through
#'     bulletAnalyzrResearch (requires that package and its model artifacts).}
#' }
#'
#' The default stays \code{"legacy_rtrees"} until the v0.6.0 pair-level result
#' is surfaced in the report view; \code{"api"} and \code{"local_v060"} are
#' opt-in through \code{options(bulletAnalyzr.scoring_mode = ...)} or the
#' \code{BULLETANALYZR_SCORING_MODE} environment variable.
#'
#' @returns A character vector of the supported scoring modes.
#' @noRd
bulletanalyzr_score_modes <- function() {
  c("legacy_rtrees", "api", "local_v060")
}

#' Resolve the active scoring mode
#'
#' Reads \code{options(bulletAnalyzr.scoring_mode)} first, then the
#' \code{BULLETANALYZR_SCORING_MODE} environment variable, defaulting to
#' \code{"legacy_rtrees"}. An unrecognized value warns and falls back to the
#' default so the app never fails closed on a bad configuration value.
#'
#' @returns One of the values from \code{bulletanalyzr_score_modes()}.
#' @noRd
bulletanalyzr_score_mode <- function() {
  raw <- getOption(
    "bulletAnalyzr.scoring_mode",
    Sys.getenv("BULLETANALYZR_SCORING_MODE", "legacy_rtrees")
  )
  mode <- tolower(trimws(as.character(raw)[[1L]]))
  if (!nzchar(mode)) {
    return("legacy_rtrees")
  }
  if (!mode %in% bulletanalyzr_score_modes()) {
    warning(
      sprintf(
        "Unknown bulletAnalyzr scoring mode '%s'; using 'legacy_rtrees'. Valid modes: %s.",
        mode,
        paste(bulletanalyzr_score_modes(), collapse = ", ")
      ),
      call. = FALSE
    )
    return("legacy_rtrees")
  }
  mode
}

#' Base URL of the bulletAnalyzrResearch comparison service
#'
#' Reads \code{options(bulletAnalyzr.api_url)} first, then the
#' \code{BULLETANALYZR_API_URL} environment variable, defaulting to the hosted
#' production deployment. Any trailing slashes are trimmed.
#'
#' @returns A single character base URL with no trailing slash.
#' @noRd
bulletanalyzr_api_url <- function() {
  raw <- getOption(
    "bulletAnalyzr.api_url",
    Sys.getenv(
      "BULLETANALYZR_API_URL",
      "https://bulletanalyzrresearch-production.up.railway.app"
    )
  )
  sub("/+$", "", as.character(raw)[[1L]])
}

# Internal: perform the request and return the parsed JSON body. Split out so it
# can be mocked in tests without a live network call.
.bulletanalyzr_perform_json <- function(req) {
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Score a bullet pair with the deployed v0.6.0 comparison API
#'
#' Uploads the reference (A) and question (B) samples to the
#' bulletAnalyzrResearch service's synchronous \code{POST /compare} endpoint and
#' returns the parsed response. The comparison runs the maintained v3.2 / v0.6.0
#' scoring pipeline server-side and is multi-minute, so \code{timeout_seconds}
#' defaults to 15 minutes.
#'
#' @param bullet_a_files,bullet_b_files Character vectors of file paths for the
#'   reference (A) and question (B) samples. Each is one or more \code{.x3p}
#'   land scans, or a single per-bullet \code{.zip}; an optional
#'   \code{grooves.csv} may be included.
#' @param api_url Base URL of the service. Defaults to
#'   \code{bulletanalyzr_api_url()}.
#' @param groove_method One of \code{"auto"}, \code{"csv"},
#'   \code{"profile-auto"}, or \code{"surface-region-v1"}.
#' @param model_version Model alias to request (default \code{"latest"}).
#' @param timeout_seconds Request timeout in seconds.
#'
#' @returns The parsed \code{/compare} response: a list with \code{decision},
#'   \code{model}, \code{features}, \code{details}, \code{artifacts}, and the
#'   request metadata.
#' @noRd
api_compare <- function(bullet_a_files,
                        bullet_b_files,
                        api_url = bulletanalyzr_api_url(),
                        groove_method = "auto",
                        model_version = "latest",
                        timeout_seconds = 900) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("API scoring mode requires the 'httr2' package. Install it with install.packages('httr2').", call. = FALSE)
  }
  if (length(bullet_a_files) == 0L || length(bullet_b_files) == 0L) {
    stop("api_compare() requires at least one file for each of bullet A and bullet B.", call. = FALSE)
  }

  a_files <- normalizePath(bullet_a_files, mustWork = TRUE)
  b_files <- normalizePath(bullet_b_files, mustWork = TRUE)

  a_parts <- lapply(a_files, curl::form_file)
  names(a_parts) <- rep("bullet_a", length(a_parts))
  b_parts <- lapply(b_files, curl::form_file)
  names(b_parts) <- rep("bullet_b", length(b_parts))

  parts <- c(
    a_parts,
    b_parts,
    list(groove_method = groove_method, model_version = model_version)
  )

  req <- httr2::request(api_url)
  req <- httr2::req_url_path_append(req, "compare")
  req <- httr2::req_timeout(req, timeout_seconds)
  req <- httr2::req_user_agent(req, "bulletAnalyzr (R Shiny app)")
  req <- do.call(httr2::req_body_multipart, c(list(req), parts))

  tryCatch(
    .bulletanalyzr_perform_json(req),
    error = function(e) {
      stop(
        sprintf(
          "bulletAnalyzr comparison API request to %s failed: %s",
          api_url,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

#' Summarize a \code{/compare} response into the headline decision
#'
#' Pulls the match decision, probability, threshold, and model version out of a
#' parsed \code{api_compare()} result into a flat list suitable for display.
#'
#' @param result A parsed response from \code{api_compare()}.
#'
#' @returns A list with \code{is_match}, \code{match_probability},
#'   \code{threshold}, \code{model_version}, and \code{request_id}.
#' @noRd
api_compare_decision <- function(result) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
  decision <- result$decision %||% list()
  model <- result$model %||% list()
  list(
    is_match = isTRUE(decision$is_match),
    match_probability = decision$match_probability %||% NA_real_,
    threshold = decision$threshold %||% model$threshold %||% NA_real_,
    model_version = model$version %||% NA_character_,
    request_id = result$request_id %||% NA_character_
  )
}
