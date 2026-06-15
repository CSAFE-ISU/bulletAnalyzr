test_that("scoring mode resolves option, then env, then default", {
  old_opt <- options(bulletAnalyzr.scoring_mode = NULL)
  old_env <- Sys.getenv("BULLETANALYZR_SCORING_MODE", unset = NA)
  Sys.unsetenv("BULLETANALYZR_SCORING_MODE")
  on.exit(
    {
      options(old_opt)
      if (!is.na(old_env)) Sys.setenv(BULLETANALYZR_SCORING_MODE = old_env)
    },
    add = TRUE
  )

  expect_equal(bulletanalyzr_score_mode(), "legacy_rtrees")

  Sys.setenv(BULLETANALYZR_SCORING_MODE = "api")
  expect_equal(bulletanalyzr_score_mode(), "api")

  # option takes precedence over the environment variable
  options(bulletAnalyzr.scoring_mode = "local_v060")
  expect_equal(bulletanalyzr_score_mode(), "local_v060")
})

test_that("unknown scoring mode warns and falls back to legacy_rtrees", {
  old_opt <- options(bulletAnalyzr.scoring_mode = "bogus-mode")
  on.exit(options(old_opt), add = TRUE)
  expect_warning(mode <- bulletanalyzr_score_mode(), "Unknown bulletAnalyzr scoring mode")
  expect_equal(mode, "legacy_rtrees")
})

test_that("api url resolves to the hosted default and trims trailing slashes", {
  old_opt <- options(bulletAnalyzr.api_url = NULL)
  old_env <- Sys.getenv("BULLETANALYZR_API_URL", unset = NA)
  Sys.unsetenv("BULLETANALYZR_API_URL")
  on.exit(
    {
      options(old_opt)
      if (!is.na(old_env)) Sys.setenv(BULLETANALYZR_API_URL = old_env)
    },
    add = TRUE
  )

  expect_match(bulletanalyzr_api_url(), "^https://bulletanalyzrresearch-production")

  options(bulletAnalyzr.api_url = "http://localhost:8000///")
  expect_equal(bulletanalyzr_api_url(), "http://localhost:8000")
})

test_that("api_compare_decision flattens the comparison response", {
  result <- list(
    request_id = "comp-xyz",
    decision = list(is_match = TRUE, match_probability = 0.92, threshold = 0.57),
    model = list(version = "v0.6.0", threshold = 0.57)
  )
  d <- api_compare_decision(result)
  expect_true(d$is_match)
  expect_equal(d$match_probability, 0.92)
  expect_equal(d$threshold, 0.57)
  expect_equal(d$model_version, "v0.6.0")
  expect_equal(d$request_id, "comp-xyz")
})

test_that("api_compare_decision tolerates a sparse response", {
  d <- api_compare_decision(list())
  expect_false(d$is_match)
  expect_true(is.na(d$match_probability))
  expect_true(is.na(d$threshold))
})

test_that("api_compare builds a /compare multipart request and returns the parsed body", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("curl")

  a <- tempfile(fileext = ".x3p")
  b <- tempfile(fileext = ".x3p")
  writeLines("reference", a)
  writeLines("question", b)
  on.exit(unlink(c(a, b)), add = TRUE)

  fake <- list(
    decision = list(is_match = FALSE, match_probability = 0.1, threshold = 0.57),
    model = list(version = "v0.6.0"),
    request_id = "comp-1"
  )
  captured <- NULL
  testthat::local_mocked_bindings(
    .bulletanalyzr_perform_json = function(req) {
      captured <<- req
      fake
    },
    .package = "bulletAnalyzr"
  )

  res <- api_compare(a, b, api_url = "http://localhost:8000", groove_method = "auto")
  expect_identical(res, fake)
  expect_match(captured$url, "/compare$")
})
