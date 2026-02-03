#!/usr/bin/env Rscript
# Bootstrap a reproducible R environment for this repository using renv.
#
# Run from the repo root:
#   Rscript scripts/r_setup.R
#
# Then restart R (or reopen the .Rproj), and run:
#   renv::snapshot()

required <- c("renv")

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Installing missing package: ", p)
      install.packages(p, repos = "https://cloud.r-project.org")
    }
  }
}

install_if_missing(required)

message("Initializing renv (if not already initialized).")
renv::init(bare = TRUE)

# Optional: install commonly used packages for macro/metrics work.
recommended <- c(
  "tidyverse",   # data + plotting convenience
  "lubridate",   # dates
  "forecast",    # time series tools (if allowed/needed)
  "tseries",     # tests like ADF (if allowed/needed)
  "sandwich",    # robust covariance estimators
  "lmtest"       # testing utilities
)

missing_rec <- recommended[!vapply(recommended, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_rec) > 0) {
  message("Installing recommended packages: ", paste(missing_rec, collapse = ", "))
  install.packages(missing_rec, repos = "https://cloud.r-project.org")
}

message("Setup complete.")
message("Next steps:")
message("  1) Restart R / reopen Quantitative-Macro.Rproj")
message("  2) Run: renv::snapshot()")

