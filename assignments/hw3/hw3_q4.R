## econ5345 hw3 -- q4
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
output_dir <- if (!is.null(script_path)) dirname(script_path) else getwd()

save_png <- function(filename, width = 1800, height = 1100, res = 200, expr) {
  png(filename = filename, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

latex_table <- function(df, caption = NULL, label = NULL, digits = 9) {
  fmt <- paste0("%.", digits, "f")
  body <- paste0(
    df$lag,
    " & ",
    sprintf(fmt, df$gamma_c),
    " \\\\"
  )

  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\begin{tabular}{rr}",
    "\\hline",
    "$k$ & $\\gamma_c(k)$ \\\\",
    "\\hline",
    body,
    "\\hline",
    "\\end{tabular}"
  )
  if (!is.null(caption)) lines <- c(lines, paste0("\\caption{", caption, "}"))
  if (!is.null(label))   lines <- c(lines, paste0("\\label{", label, "}"))
  lines <- c(lines, "\\end{table}")
  paste(lines, collapse = "\n")
}

# parameters
lambda_hp <- 1600
sigma <- 1

N <- 20001
omega <- seq(-pi, pi, length.out = N)
domega <- omega[2] - omega[1]

# H+(e^{-i omega})
Hplus <- function(omega, lambda) {
  z1 <- exp(-1i * omega)  
  z2 <- exp( 1i * omega)  

  num <- lambda * (1 - z1) * (1 - z2)^2
  den <- 1 + lambda * (1 - z1)^2 * (1 - z2)^2
  num / den
}

Hplus_vals <- Hplus(omega, lambda_hp)
gain <- Mod(Hplus_vals)

# part a
save_png(file.path(output_dir, "hw3_q4a_gain.png"), expr = {
  plot(
    omega, gain,
    type = "l",
    xlab = expression(omega),
    ylab = expression(paste("|", H^"+", "(e"^{-i*omega}, ")|")),
    main = expression(paste("Gain of ", H^"+", " (HP filter, quarterly)"))
  )
  abline(v = 0, col = "gray80", lty = 3)
})

# part d
sc <- (sigma^2 / (2 * pi)) * (gain^2)

lags <- 0:10
idx <- 1:(N - 1)

gamma_c <- vapply(
  lags,
  function(k) {
    val <- sum(sc[idx] * exp(1i * k * omega[idx])) * domega
    Re(val)
  },
  numeric(1)
)

out <- data.frame(lag = lags, gamma_c = gamma_c)
print(out, row.names = FALSE)

cat(
  "\nLaTeX table (copy into your .tex):\n\n",
  latex_table(
    out,
    caption = "Autocovariances of $\\{c_t\\}$.",
    label = "tab:4d"
  ),
  "\n\n",
  sep = ""
)

save_png(file.path(output_dir, "hw3_q4d_autocovariances.png"), expr = {
  plot(
    out$lag, out$gamma_c,
    type = "b", pch = 19,
    xlab = "lag k",
    ylab = expression(gamma[c](k)),
    main = expression(paste("Autocovariances of ", c[t]))
  )
  abline(h = 0, col = "gray80", lty = 3)
})


