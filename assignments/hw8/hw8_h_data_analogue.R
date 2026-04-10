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

write_matrix_txt <- function(mat, file_path) {
  utils::write.table(
    as.data.frame(mat),
    file = file_path,
    sep = "\t",
    row.names = TRUE,
    col.names = NA,
    quote = FALSE
  )
}

hp_filter_series <- function(x, lambda = 1600) {
  n <- length(x)

  if (n < 4) {
    stop("HP filter requires at least 4 observations.")
  }

  D <- matrix(0, nrow = n - 2L, ncol = n)
  for (i in seq_len(n - 2L)) {
    D[i, i] <- 1
    D[i, i + 1L] <- -2
    D[i, i + 2L] <- 1
  }

  trend <- solve(diag(n) + lambda * crossprod(D), x)
  cycle <- x - trend

  list(trend = trend, cycle = cycle)
}

script_dir <- get_script_dir()
input_file <- file.path(script_dir, "hw8_h.csv")
output_dir <- file.path(script_dir, "hw8_h_output")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Data mapping to model variables:
# Y_t -> GDPC1  (real GDP)
# C_t -> PCEC96 (real consumption)
# L_t -> AWHAETP (average weekly hours, data analogue for labor input)
# I_t -> GPDI   (gross private domestic investment)
raw <- read.csv(input_file, stringsAsFactors = FALSE)
required_cols <- c("observation_date", "GDPC1", "PCEC96", "GPDI", "AWHAETP")
missing_cols <- setdiff(required_cols, names(raw))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

data <- raw[, required_cols]
data$observation_date <- as.Date(data$observation_date)
data <- data[order(data$observation_date), ]
data <- data[complete.cases(data), ]
rownames(data) <- NULL

colnames(data) <- c("date", "Y", "C", "I", "L")

log_data <- data.frame(
  date = data$date,
  logY = log(data$Y),
  logC = log(data$C),
  logL = log(data$L),
  logI = log(data$I)
)

hp_y <- hp_filter_series(log_data$logY, lambda = 1600)
hp_c <- hp_filter_series(log_data$logC, lambda = 1600)
hp_l <- hp_filter_series(log_data$logL, lambda = 1600)
hp_i <- hp_filter_series(log_data$logI, lambda = 1600)

hp_cycle <- data.frame(
  date = log_data$date,
  cY = hp_y$cycle,
  cC = hp_c$cycle,
  cL = hp_l$cycle,
  cI = hp_i$cycle
)

hp_variance_table <- data.frame(
  variable = c("cY", "cC", "cL", "cI"),
  variance = c(
    var(hp_cycle$cY),
    var(hp_cycle$cC),
    var(hp_cycle$cL),
    var(hp_cycle$cI)
  )
)

hp_vcov_matrix <- cov(hp_cycle[, c("cY", "cC", "cL", "cI")])

hp_covariance_table <- {
  out <- list()
  idx <- 1L
  for (i in seq_len(nrow(hp_vcov_matrix))) {
    for (j in i:ncol(hp_vcov_matrix)) {
      out[[idx]] <- data.frame(
        variable_1 = rownames(hp_vcov_matrix)[i],
        variable_2 = colnames(hp_vcov_matrix)[j],
        covariance = hp_vcov_matrix[i, j]
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, out)
}

# Quarterly log growth rates.
growth <- data.frame(
  date = data$date[-1],
  dY = diff(log(data$Y)),
  dC = diff(log(data$C)),
  dL = diff(log(data$L)),
  dI = diff(log(data$I))
)

variance_table <- data.frame(
  variable = c("dY", "dC", "dL", "dI"),
  variance = c(
    var(growth$dY),
    var(growth$dC),
    var(growth$dL),
    var(growth$dI)
  )
)

vcov_matrix <- cov(growth[, c("dY", "dC", "dL", "dI")])

covariance_table <- {
  out <- list()
  idx <- 1L
  for (i in seq_len(nrow(vcov_matrix))) {
    for (j in i:ncol(vcov_matrix)) {
      out[[idx]] <- data.frame(
        variable_1 = rownames(vcov_matrix)[i],
        variable_2 = colnames(vcov_matrix)[j],
        covariance = vcov_matrix[i, j]
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, out)
}

write_txt_table(
  data,
  file.path(output_dir, "hw8_h_clean_data.txt")
)

write_txt_table(
  growth,
  file.path(output_dir, "hw8_h_log_growth_rates.txt")
)

write_txt_table(
  hp_cycle,
  file.path(output_dir, "hw8_h_hp_cycles.txt")
)

write_txt_table(
  variance_table,
  file.path(output_dir, "hw8_h_variances.txt")
)

write_txt_table(
  covariance_table,
  file.path(output_dir, "hw8_h_covariances.txt")
)

write_matrix_txt(
  vcov_matrix,
  file.path(output_dir, "hw8_h_variance_covariance_matrix.txt")
)

write_txt_table(
  hp_variance_table,
  file.path(output_dir, "hw8_h_hp_variances.txt")
)

write_txt_table(
  hp_covariance_table,
  file.path(output_dir, "hw8_h_hp_covariances.txt")
)

write_matrix_txt(
  hp_vcov_matrix,
  file.path(output_dir, "hw8_h_hp_variance_covariance_matrix.txt")
)

summary_text <- capture.output({
  cat("HW8 Part (h) Data Analogues\n")
  cat("===========================\n\n")
  cat("Input file:\n")
  cat(input_file, "\n\n")
  cat("Sample size after dropping missing observations:", nrow(data), "\n")
  cat("Growth-rate observations:", nrow(growth), "\n\n")
  cat("Variables:\n")
  cat("Y = GDPC1\n")
  cat("C = PCEC96\n")
  cat("L = AWHAETP\n")
  cat("I = GPDI\n\n")
  cat("Growth rates are computed as quarterly log differences.\n\n")
  cat("Variance table:\n")
  print(variance_table, row.names = FALSE)
  cat("\nVariance-covariance matrix:\n")
  print(round(vcov_matrix, 8))
  cat("\n\nHP filter:\n")
  cat("Variables are first transformed into logs and then HP filtered with lambda = 1600.\n\n")
  cat("HP-filtered variance table:\n")
  print(hp_variance_table, row.names = FALSE)
  cat("\nHP-filtered variance-covariance matrix:\n")
  print(round(hp_vcov_matrix, 8))
})

writeLines(summary_text, con = file.path(output_dir, "hw8_h_summary.txt"))

cat("Saved output files to:\n")
cat(output_dir, "\n")
