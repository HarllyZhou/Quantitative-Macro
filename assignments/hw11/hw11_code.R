rm(list = ls())
graphics.off()

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    active_path <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    if (nzchar(active_path)) {
      return(dirname(normalizePath(active_path)))
    }
  }

  getwd()
}

write_txt_table <- function(df, file_path, row_names = FALSE) {
  utils::write.table(
    df,
    file = file_path,
    sep = "\t",
    row.names = row_names,
    col.names = TRUE,
    quote = FALSE
  )
}

script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "hw11_output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================================
# Model primitives
# ============================================================
beta <- 0.9
cost_new <- 75
n_age <- 10L
ages <- 1:n_age
sim_horizon <- 16L
tolerance <- 1e-10
max_iter <- 10000L

profit_fun <- function(a) {
  50 - 2.5 * a - 2.5 * a^2
}

p0 <- profit_fun(0)

# ============================================================
# Value function iteration
# ============================================================
solve_asset_replacement <- function(beta, cost_new, n_age, tol, max_iter) {
  ages <- 1:n_age
  V_old <- rep(0, n_age)
  names(V_old) <- paste0("a", ages)

  for (iter in seq_len(max_iter)) {
    replace_value <- p0 - cost_new + beta * V_old[1]
    V_nplus1 <- replace_value

    keep_value <- numeric(n_age)
    for (j in seq_along(ages)) {
      age_now <- ages[j]
      continuation <- if (age_now < n_age) V_old[j + 1L] else V_nplus1
      keep_value[j] <- profit_fun(age_now) + beta * continuation
    }

    V_new <- pmax(keep_value, rep(replace_value, n_age))
    sup_norm <- max(abs(V_new - V_old))

    if (sup_norm < tol) {
      policy <- ifelse(keep_value >= replace_value, "keep", "replace")
      return(list(
        value = V_new,
        keep_value = keep_value,
        replace_value = rep(replace_value, n_age),
        policy = policy,
        iterations = iter,
        sup_norm = sup_norm
      ))
    }

    V_old <- V_new
  }

  stop("Value iteration did not converge.")
}

solution <- solve_asset_replacement(
  beta = beta,
  cost_new = cost_new,
  n_age = n_age,
  tol = tolerance,
  max_iter = max_iter
)

# ============================================================
# Simulate optimal age path
# ============================================================
simulate_age_path <- function(initial_age, horizon, policy) {
  age_path <- numeric(horizon)
  action_path <- character(horizon)

  age_now <- initial_age
  for (t in seq_len(horizon)) {
    age_path[t] <- age_now
    action_now <- policy[age_now]
    action_path[t] <- action_now

    if (action_now == "keep") {
      age_now <- age_now + 1L
    } else {
      age_now <- 1L
    }
  }

  data.frame(
    t = 1:horizon,
    age = age_path,
    action = action_path,
    stringsAsFactors = FALSE
  )
}

sim_path <- simulate_age_path(
  initial_age = 1L,
  horizon = sim_horizon,
  policy = solution$policy
)

# ============================================================
# Save tables
# ============================================================
value_table <- data.frame(
  age = ages,
  profit_keep_now = profit_fun(ages),
  value_keep = solution$keep_value,
  value_replace = solution$replace_value,
  value_function = solution$value,
  optimal_policy = solution$policy,
  stringsAsFactors = FALSE
)

write_txt_table(
  value_table,
  file.path(output_dir, "hw11_value_and_policy.txt")
)

write_txt_table(
  sim_path,
  file.path(output_dir, "hw11_optimal_age_path.txt")
)

# ============================================================
# Plots
# ============================================================
png(
  filename = file.path(output_dir, "hw11_value_function.png"),
  width = 900,
  height = 600,
  res = 140
)
plot(
  ages, solution$value,
  type = "b",
  pch = 19,
  lwd = 2,
  col = "steelblue",
  xlab = "Asset age",
  ylab = "Value",
  main = "Value Function"
)
grid()
dev.off()

png(
  filename = file.path(output_dir, "hw11_optimal_age_path.png"),
  width = 900,
  height = 600,
  res = 140
)
plot(
  sim_path$t, sim_path$age,
  type = "s",
  lwd = 2,
  col = "firebrick",
  xlab = "t",
  ylab = "Asset age",
  main = "Optimal Age Path"
)
points(sim_path$t, sim_path$age, pch = 19, col = "firebrick")
grid()
dev.off()

# ============================================================
# Summary file
# ============================================================
replace_ages <- ages[solution$policy == "replace"]

summary_text <- capture.output({
  cat("ECON 5345 HW11 - Asset Replacement Problem\n")
  cat("==========================================\n\n")
  cat("Parameters:\n")
  cat("beta =", beta, "\n")
  cat("c =", cost_new, "\n")
  cat("n =", n_age, "\n")
  cat("p(a) = 50 - 2.5 a - 2.5 a^2\n\n")

  cat("Convergence:\n")
  cat("iterations =", solution$iterations, "\n")
  cat("sup norm =", format(solution$sup_norm, scientific = TRUE), "\n\n")

  cat("Value function V(a):\n")
  print(data.frame(age = ages, V = round(solution$value, 6)), row.names = FALSE)

  cat("\nOptimal policy:\n")
  print(data.frame(age = ages, action = solution$policy), row.names = FALSE)

  cat("\nReplacement ages:\n")
  if (length(replace_ages) == 0) {
    cat("Never replace within ages 1,...,n.\n")
  } else {
    cat(paste(replace_ages, collapse = ", "), "\n")
  }

  cat("\nSimulated age path for t = 1,...,16 starting from a_1 = 1:\n")
  print(sim_path, row.names = FALSE)
})

writeLines(summary_text, con = file.path(output_dir, "hw11_summary.txt"))

cat("Saved output files to:\n")
cat(output_dir, "\n")
