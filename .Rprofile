if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

if (Sys.getenv("GITHUB_ACTIONS") != "true") {
  library(rhino)
  options("shiny.autoreload" = TRUE)
  library(shiny)
}
