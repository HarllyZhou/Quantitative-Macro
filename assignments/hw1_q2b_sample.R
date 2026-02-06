# econ 5345 hw1 - q2b

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
np <- 15L
nmu <- 15L

muMin <- 0.8
muMax <- 1.2

theta <- 3

pMin <- theta / (theta - 1) * muMin
pMax <- theta / (theta - 1) * muMax

muGrid <- seq(muMin, muMax, length.out = nmu)
pGrid <- seq(pMin, pMax, length.out = np)

kappa <- 1
gmu <- rep(1 / nmu, nmu)


# produce the profit matrix
p2 <- matrix(pGrid, nrow = np, ncol = nmu, byrow = FALSE)
mu2 <- matrix(muGrid, nrow = np, ncol = nmu, byrow = TRUE)

pi_mat <- p2^(-theta) * (p2 - mu2)

# functions needed
make_mat <- function(x) {
  matrix(x, nrow = np, ncol = nmu, byrow = FALSE)
}

## I(p,d)
## F is [f_{i,j}]
information <- function(F) {
  fp <- rowSums(F)
  fmu <- colSums(F)
  pos <- F > 0
  mi_nats <- sum(F[pos] * log(F[pos])) - sum(fp * log(fp)) - sum(fmu * log(fmu))
  mi_nats / log(2)
}

## objective function, but negative for minimization
obj_func <- function(x) {
  F <- make_mat(x)
  obj <- -sum(pi_mat * F)
  grad <- -as.vector(pi_mat) # linear objective
  list(objective = obj, gradient = grad)
}

# Equality constraints: colSums(F) == gmu  (marginal over mu equals prior)
eval_g_eq <- function(x) {
  F <- make_mat(x)
  cons <- colSums(F) - gmu

  # Jacobian: nmu constraints x (np*nmu) variables
  J <- matrix(0, nrow = nmu, ncol = np * nmu)
  for (j in 1:nmu) {
    idx <- ((j - 1L) * np + 1L):(j * np) # column-major indices for column j
    J[j, idx] <- 1
  }
  list(constraints = cons, jacobian = J)
}

# Inequality constraint: MI(F) - kappa <= 0
eval_g_ineq <- function(x) {
  F <- make_mat(x)
  fp <- rowSums(F)
  fmu <- colSums(F)

  # Gradient of MI (bits) w.r.t. each f_ij:
  # d/d f_ij [MI_bits] = (log(f_ij) - log(fp_i) - log(fmu_j) - 1) / log(2)
  # (derived from the stable MI decomposition)
  grad_mat <- (log(F) - log(fp) - matrix(log(fmu), nrow = np, ncol = nmu, byrow = TRUE) - 1) / log(2)
  grad <- as.vector(grad_mat)

  list(constraints = information(F) - kappa, jacobian = matrix(grad, nrow = 1))
}

# -----------------------------
# optimization (mirror fmincon)
# -----------------------------
x0 <- rep(1 / (np * nmu), np * nmu)
# Keep strictly positive to avoid log(0) in MI / gradients
lb <- rep(1e-12, np * nmu)
ub <- rep(1, np * nmu)

opts <- list(
  algorithm = "NLOPT_LD_SLSQP",
  # The Matlab sample uses extremely tight tolerances; that can be very slow in R.
  # These defaults are usually enough to reproduce the qualitative figure.
  maxeval = 20000L,
  maxtime = 30,      # seconds
  xtol_rel = 1e-8,
  ftol_rel = 1e-8,
  print_level = 0
)

res <- nloptr(
  x0 = x0,
  obj_func = obj_func,
  lb = lb,
  ub = ub,
  eval_g_eq = eval_g_eq,
  eval_g_ineq = eval_g_ineq,
  opts = opts
)

F_pmf <- make_mat(res$solution)

# transform pmf on grid to a pdf on [pMin,pMax] x [muMin,muMax] (same as Matlab)
F_pdf <- F_pmf / ((muMax - muMin) * (pMax - pMin))

cat("Optimization status:", res$status, "\n")
cat("Message:", res$message, "\n")
cat("Expected profit (objective):", -res$objective, "\n")
cat("Mutual information (bits):", information(F_pmf), "\n")

# -----------------------------
# 3D plot (Matlab surf equivalent)
# -----------------------------
out_png <- "hw1_q2b_sample_surface.png"
png(out_png, width = 900, height = 700, res = 130)
persp(
  x = pGrid, y = muGrid, z = F_pdf,
  theta = 35, phi = 25, expand = 0.6, col = "lightblue",
  xlab = "p", ylab = expression(mu), zlab = "f(p, mu)",
  main = "Optimal joint density f(p, mu) (R translation)"
)
dev.off()
cat("Saved 3D plot to:", out_png, "\n")

