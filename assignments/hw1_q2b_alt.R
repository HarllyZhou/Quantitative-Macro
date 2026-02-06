# econ 5345 hw1 - q2b
#
# This is the same as hw1_q2b.R, except the initial point uses a simple uniform start:
#   x0 <- rep(1 / (np * nd), np * nd)

# package installation for constrained optimization
ensure_pkg <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))

  base <- NULL
  if (dir.exists("renv/library")) base <- "."
  if (is.null(base) && dir.exists(file.path("..", "renv", "library"))) base <- ".."
  if (is.null(base)) stop("Cannot find renv/library. Run from repo root or assignments/ folder.")

  r_ver <- paste0("R-", getRversion()$major, ".", getRversion()$minor)
  ver_dir <- file.path(base, "renv", "library", r_ver)
  if (!dir.exists(ver_dir)) {
    candidates <- list.dirs(file.path(base, "renv", "library"), full.names = TRUE, recursive = FALSE)
    candidates <- candidates[basename(candidates) != "cache"]
    ver_dir <- candidates[1]
  }
  arch_dirs <- list.dirs(ver_dir, full.names = TRUE, recursive = FALSE)
  lib <- arch_dirs[1]
  if (is.na(lib) || !dir.exists(lib)) stop("Could not locate a writable renv library directory.")

  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(normalizePath(lib), .libPaths())))

  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib)
  }
  invisible(TRUE)
}

ensure_pkg("nloptr")
library(nloptr)

rm(list = ls())
graphics.off()

# discretization
np <- 20L
nd <- 20L

dMin <- 1 / 9
dMax <- 1 / 2

kappa <- 0.5
gd <- rep(1 / nd, nd) # uniform prior pmf on d-grid

dGrid <- seq(dMin, dMax, length.out = nd)

# full-information optimal price: p*(d) = 1 + d
pMin <- 1 + dMin
pMax <- 1 + dMax
pGrid <- seq(pMin, pMax, length.out = np)

# produce the profit matrix
# ndgrid(pGrid, dGrid): (np x nd)
p2 <- matrix(pGrid, nrow = np, ncol = nd, byrow = FALSE)
d2 <- matrix(dGrid, nrow = np, ncol = nd, byrow = TRUE)

# Pi(p,d) = p^(-(d+1)/d) * (p - 1)
pi_mat <- p2^(-(d2 + 1) / d2) * (p2 - 1)

# functions needed
make_mat <- function(x) {
  matrix(x, nrow = np, ncol = nd, byrow = FALSE)
}

## I(p,d): F is [f_{i,j}]
information <- function(F) {
  fp <- rowSums(F)
  fd <- colSums(F)
  pos <- F > 0
  mi_nats <- sum(F[pos] * log(F[pos])) - sum(fp * log(fp)) - sum(fd * log(fd))
  mi_nats / log(2)
}

## objective function, but negative for minimization
obj <- function(x) {
  F <- make_mat(x)
  obj <- -sum(pi_mat * F)
  grad <- -as.vector(pi_mat) # linear objective
  list(objective = obj, gradient = grad)
}

# Equality constraints: colSums(F) == gd  (marginal over d equals prior)
eq <- function(x) {
  F <- make_mat(x)
  cons <- colSums(F) - gd

  # Jacobian: nd constraints x (np*nd) variables
  J <- matrix(0, nrow = nd, ncol = np * nd)
  for (j in 1:nd) {
    idx <- ((j - 1L) * np + 1L):(j * np) # column-major indices for column j
    J[j, idx] <- 1
  }
  list(constraints = cons, jacobian = J)
}

# Inequality constraint: I(p,d) - kappa <= 0
ineq <- function(x) {
  F <- make_mat(x)
  fp <- rowSums(F)
  fd <- colSums(F)

  # Gradient of MI (bits) w.r.t. each f_ij:
  # d/d f_ij [I_bits] = (log(f_ij) - log(fp_i) - log(fd_j) - 1) / log(2)
  grad_mat <- (log(F) - log(fp) - matrix(log(fd), nrow = np, ncol = nd, byrow = TRUE) - 1) / log(2)
  grad <- as.vector(grad_mat)

  list(constraints = information(F) - kappa, jacobian = matrix(grad, nrow = 1))
}

# optimization (mirror fmincon)
# initial point
x0 <- rep(1 / (np * nd), np * nd)

# Keep strictly positive to avoid log(0) in information / gradients
lb <- rep(1e-12, np * nd)
ub <- rep(1, np * nd)

opts <- list(
  algorithm = "NLOPT_LD_SLSQP",
  # Matlab sample uses extremely tight tolerances; relax for practicality
  maxeval = 30000L,
  maxtime = 45,     # seconds
  xtol_rel = 1e-8,
  ftol_rel = 1e-8,
  print_level = 0
)

res <- nloptr(
  x0 = x0,
  eval_f = obj,
  lb = lb,
  ub = ub,
  eval_g_eq = eq,
  eval_g_ineq = ineq,
  opts = opts
)

F_pmf <- make_mat(res$solution)
F_pdf <- F_pmf / ((dMax - dMin) * (pMax - pMin))  # pmf -> pdf

cat("Optimization status:", res$status, "\n")
cat("Message:", res$message, "\n")
cat("Expected profit:", -res$objective, "\n")
cat("Mutual information (bits):", format(information(F_pmf), digits = 6), "\n")

# plotting and saving (3D surface only)
out_png <- "hw1_q2b_alt_surface.png"
png(out_png, width = 900, height = 700, res = 130)
z <- F_pdf
zlim <- range(z, finite = TRUE)
if (!is.finite(zlim[1]) || !is.finite(zlim[2]) || zlim[1] == zlim[2]) {
  zlim <- c(0, max(1e-8, zlim[2] + 1e-8))
}
persp(
  x = pGrid, y = dGrid, z = z,
  theta = 35, phi = 25, expand = 0.6, col = "lightblue",
  xlab = "p", ylab = "d", zlab = "f(p, d)",
  main = "Optimal joint density f(p, d)",
  zlim = zlim,
  ticktype = "detailed"
)
dev.off()
cat("Saved 3D plot to:", out_png, "\n")

