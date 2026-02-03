source("renv/activate.R")
## Project startup
## - Ensures scripts run from the project root in RStudio
## - Activates renv automatically in interactive sessions (optional)

if (interactive()) {
  # Set working directory to project root when opening in RStudio
  try({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      p <- rstudioapi::getActiveProject()
      if (!is.null(p) && nzchar(p)) setwd(dirname(p))
    }
  }, silent = TRUE)
}

# If you initialize renv for this repo, it will create renv/activate.R.
# Only auto-activate renv in interactive sessions so running `Rscript ...`
# won't try to bootstrap/download anything.
if (interactive() && file.exists("renv/activate.R")) {
  try(source("renv/activate.R"), silent = TRUE)
}

