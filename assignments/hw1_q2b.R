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
p2 <- matrix(pGrid, nrow = np, ncol = nd, byrow = FALSE)
d2 <- matrix(dGrid, nrow = np, ncol = nd, byrow = TRUE)
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

## construct a feasible initial point with information close to kappa
## create this under the help of AI
init_feasible <- function(sigma = 0.08) {
  F_indep <- matrix(rep(gd / np, each = np), nrow = np, ncol = nd, byrow = FALSE)

  pstar <- 1 + dGrid
  W <- exp(-(p2 - matrix(pstar, nrow = np, ncol = nd, byrow = TRUE))^2 / (2 * sigma^2))
  W <- sweep(W, 2, colSums(W), "/")
  F_corr <- sweep(W, 2, gd, "*")

  a_lo <- 0
  a_hi <- 1
  for (iter in 1:60) {
    a_mid <- 0.5 * (a_lo + a_hi)
    F_mid <- (1 - a_mid) * F_indep + a_mid * F_corr
    mi <- information(F_mid)
    if (mi > kappa) a_hi <- a_mid else a_lo <- a_mid
  }

  F0 <- (1 - a_lo) * F_indep + a_lo * F_corr
  F0 <- pmax(F0, 1e-12)                 
  F0 <- sweep(F0, 2, colSums(F0), "/")  
  F0 <- sweep(F0, 2, gd, "*")           
  F0
}

## objective function, but negative for minimization
obj <- function(x) {
  F <- make_mat(x)
  obj <- -sum(pi_mat * F)
  grad <- -as.vector(pi_mat)
  list(objective = obj, gradient = grad)
}

# equality constraint
eq <- function(x) {
  F <- make_mat(x)
  cons <- colSums(F) - gd

  J <- matrix(0, nrow = nd, ncol = np * nd)
  for (j in 1:nd) {
    idx <- ((j - 1L) * np + 1L):(j * np)
    J[j, idx] <- 1
  }
  list(constraints = cons, jacobian = J)
}

# inequality constraint
ineq <- function(x) {
  F <- make_mat(x)
  fp <- rowSums(F)
  fd <- colSums(F)

  grad_mat <- (log(F) - log(fp) - matrix(log(fd), nrow = np, ncol = nd, byrow = TRUE) - 1) / log(2)
  grad <- as.vector(grad_mat)

  list(constraints = information(F) - kappa, jacobian = matrix(grad, nrow = 1))
}

# optimization 
## initial point
x0 <- as.vector(init_feasible())
lb <- rep(1e-12, np * nd)
ub <- rep(1, np * nd)

opts <- list(
  algorithm = "NLOPT_LD_SLSQP",
  maxeval = 30000L,
  maxtime = 45,
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
F_pdf <- F_pmf / ((dMax - dMin) * (pMax - pMin)) 

cat("Optimization status:", res$status, "\n")
cat("Message:", res$message, "\n")
cat("Expected profit:", -res$objective, "\n")
cat("Mutual information (bits):", format(information(F_pmf), digits = 6), "\n")

# plotting and saving
out_png <- "hw1_q2b_surface.png"
png(out_png, width = 900, height = 700, res = 130)
persp(
  x = pGrid, y = dGrid, z = F_pdf,
  theta = 35, phi = 25, expand = 0.6, col = "lightblue",
  xlab = "p", ylab = "d", zlab = "f(p, d)",
  main = "Optimal joint density f(p, d)",
  ticktype = "detailed"
)
dev.off()
cat("Saved 3D plot to:", out_png, "\n")

