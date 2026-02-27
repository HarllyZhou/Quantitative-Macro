## econ5345 hw3 -- q2
rm(list = ls())

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) >= 1) {
    return(normalizePath(sub("^--file=", "", file_arg[1])))
  }
  ofile <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(ofile)) {
    return(normalizePath(ofile))
  }
  NULL
}

script_path <- get_script_path()
output_dir <- if (!is.null(script_path)) {
  dirname(script_path)
} else if (dir.exists(file.path(getwd(), "assignments", "hw3"))) {
  # Helpful fallback when running interactively from the project root.
  file.path(getwd(), "assignments", "hw3")
} else {
  getwd()
}

save_png <- function(filename, width = 1800, height = 1100, res = 200, expr) {
  png(filename = filename, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

# Define grid of omega
N <- 20000
omega <- seq(-pi, pi, length.out = N)

kuznets_filter <- function(omega) {
  z1 <- exp(-1i * omega) 
  z2 <- exp( 1i * omega) 

  step_2 <- z1^5 - z2^5
  step_1 <- 0.2 * (z1^2 + z1 + 1 + z2 + z2^2)
  step_1 * step_2
}

kuznets_vals <- kuznets_filter(omega)
gain <- Mod(kuznets_vals)

save_png(file.path(output_dir, "hw3_q2.png"), expr = {
  plot(
    omega, gain,
    type = "l",
    xlab = expression(omega),
    ylab = expression(paste("|", K, "(e"^{-i*omega}, ")|")),
    main = expression(paste("Gain of ", K, " (Kuznets filter, year frequency)"))
  )
  abline(v = 0, col = "gray80", lty = 3)
})

argmax_gain <- abs(omega[which.max(gain)])
print(paste("argmax_gain:", argmax_gain))