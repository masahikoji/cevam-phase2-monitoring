# cevam_trend_binary_boundaries.R
#
# Standalone R program to compute CEVAM binary monitoring boundaries
# for the TREND trial setting:
#   p0 = 0.41, p1 = 0.56, maximum sample size N = 65
#   retrospective reported look: n = 44, x = 30
#
# It computes:
#   1) Fixed-threshold CEVAM boundaries:
#        efficacy threshold = 1 / alpha
#        futility threshold = 1 / beta
#   2) Planned-look calibrated CEVAM boundaries:
#        thresholds are calibrated over the specified planned looks
#        using exact binomial probabilities.
#
# Default looks are c(44, 65), corresponding to a retrospective look
# at the reported efficacy-evaluable result and the maximum sample size.
# For comparison with the TREND protocol-specified interim look, change
# looks to c(48, 65).

# -----------------------------
# User settings
# -----------------------------

p0 <- 0.41
p1 <- 0.56
alpha <- 0.05
beta  <- 0.20

N_max <- 65
looks <- c(44, 65)

reported_n <- 44
reported_x <- 30

# -----------------------------
# Core likelihood-ratio functions
# -----------------------------

log_lr_eff <- function(x, n, p0, p1) {
  x * log(p1 / p0) + (n - x) * log((1 - p1) / (1 - p0))
}

log_lr_fut <- function(x, n, p0, p1) {
  x * log(p0 / p1) + (n - x) * log((1 - p0) / (1 - p1))
}

eff_boundary_at_n <- function(n, p0, p1, A_eff) {
  xs <- 0:n
  hit <- xs[log_lr_eff(xs, n, p0, p1) >= log(A_eff)]
  if (length(hit) == 0L) return(NA_integer_)
  min(hit)
}

fut_boundary_at_n <- function(n, p0, p1, A_fut) {
  xs <- 0:n
  hit <- xs[log_lr_fut(xs, n, p0, p1) >= log(A_fut)]
  if (length(hit) == 0L) return(NA_integer_)
  max(hit)
}

make_boundary_table <- function(looks, p0, p1, A_eff, A_fut, label) {
  out <- data.frame(
    method = label,
    look_n = looks,
    futility_boundary = vapply(
      looks,
      fut_boundary_at_n,
      integer(1),
      p0 = p0,
      p1 = p1,
      A_fut = A_fut
    ),
    efficacy_boundary = vapply(
      looks,
      eff_boundary_at_n,
      integer(1),
      p0 = p0,
      p1 = p1,
      A_eff = A_eff
    ),
    stringsAsFactors = FALSE
  )
  out$decision_rule <- paste0(
    "Stop for futility if x <= ", out$futility_boundary,
    "; stop for efficacy if x >= ", out$efficacy_boundary,
    "; otherwise continue."
  )
  out
}

# -----------------------------
# Exact operating characteristics
# -----------------------------

prob_ever_eff <- function(looks, p, p0, p1, A_eff) {
  looks <- sort(unique(as.integer(looks)))
  dist <- c("0" = 1)
  prev_n <- 0L
  hit_prob <- 0

  for (n in looks) {
    inc <- n - prev_n
    new_dist <- numeric(0)

    for (x_prev_name in names(dist)) {
      x_prev <- as.integer(x_prev_name)
      pr_prev <- dist[[x_prev_name]]

      y <- 0:inc
      pr_y <- dbinom(y, size = inc, prob = p)
      x_new <- x_prev + y

      for (j in seq_along(x_new)) {
        key <- as.character(x_new[j])
        new_dist[key] <- ifelse(is.na(new_dist[key]), 0, new_dist[key]) +
          pr_prev * pr_y[j]
      }
    }

    e_bd <- eff_boundary_at_n(n, p0, p1, A_eff)
    cont <- numeric(0)

    for (key in names(new_dist)) {
      x <- as.integer(key)
      pr <- new_dist[[key]]
      if (!is.na(e_bd) && x >= e_bd) {
        hit_prob <- hit_prob + pr
      } else {
        cont[key] <- pr
      }
    }

    dist <- cont
    prev_n <- n
  }

  hit_prob
}

prob_ever_fut <- function(looks, p, p0, p1, A_fut) {
  looks <- sort(unique(as.integer(looks)))
  dist <- c("0" = 1)
  prev_n <- 0L
  hit_prob <- 0

  for (n in looks) {
    inc <- n - prev_n
    new_dist <- numeric(0)

    for (x_prev_name in names(dist)) {
      x_prev <- as.integer(x_prev_name)
      pr_prev <- dist[[x_prev_name]]

      y <- 0:inc
      pr_y <- dbinom(y, size = inc, prob = p)
      x_new <- x_prev + y

      for (j in seq_along(x_new)) {
        key <- as.character(x_new[j])
        new_dist[key] <- ifelse(is.na(new_dist[key]), 0, new_dist[key]) +
          pr_prev * pr_y[j]
      }
    }

    f_bd <- fut_boundary_at_n(n, p0, p1, A_fut)
    cont <- numeric(0)

    for (key in names(new_dist)) {
      x <- as.integer(key)
      pr <- new_dist[[key]]
      if (!is.na(f_bd) && x <= f_bd) {
        hit_prob <- hit_prob + pr
      } else {
        cont[key] <- pr
      }
    }

    dist <- cont
    prev_n <- n
  }

  hit_prob
}

# -----------------------------
# Planned-look threshold calibration
# -----------------------------
# We use exact binomial probabilities over the chosen looks.
# Thresholds are constrained to be at least 1 so that stopping still
# corresponds to evidence in favor of the relevant alternative.

candidate_eff_thresholds <- function(looks, p0, p1) {
  vals <- 1
  for (n in looks) {
    x <- 0:n
    vals <- c(vals, exp(log_lr_eff(x, n, p0, p1)))
  }
  sort(unique(vals[is.finite(vals) & vals >= 1]))
}

candidate_fut_thresholds <- function(looks, p0, p1) {
  vals <- 1
  for (n in looks) {
    x <- 0:n
    vals <- c(vals, exp(log_lr_fut(x, n, p0, p1)))
  }
  sort(unique(vals[is.finite(vals) & vals >= 1]))
}

calibrate_eff_threshold <- function(looks, p0, p1, alpha) {
  cand <- candidate_eff_thresholds(looks, p0, p1)
  for (A in cand) {
    pe <- prob_ever_eff(looks, p = p0, p0 = p0, p1 = p1, A_eff = A)
    if (pe <= alpha) {
      return(list(A_eff = A, type1_error = pe))
    }
  }
  stop("No efficacy threshold found. Expand candidates or check inputs.")
}

calibrate_fut_threshold <- function(looks, p0, p1, beta) {
  cand <- candidate_fut_thresholds(looks, p0, p1)
  for (A in cand) {
    pf <- prob_ever_fut(looks, p = p1, p0 = p0, p1 = p1, A_fut = A)
    if (pf <= beta) {
      return(list(A_fut = A, type2_error = pf))
    }
  }
  stop("No futility threshold found. Expand candidates or check inputs.")
}

# -----------------------------
# Decision function
# -----------------------------

classify_decision <- function(n, x, p0, p1, A_eff, A_fut) {
  f_bd <- fut_boundary_at_n(n, p0, p1, A_fut)
  e_bd <- eff_boundary_at_n(n, p0, p1, A_eff)

  if (!is.na(e_bd) && x >= e_bd) {
    return("Stop for efficacy")
  }
  if (!is.na(f_bd) && x <= f_bd) {
    return("Stop for futility")
  }
  "Continue"
}

# -----------------------------
# Run analysis
# -----------------------------

A_eff_fixed <- 1 / alpha
A_fut_fixed <- 1 / beta

fixed_table <- make_boundary_table(
  looks = looks,
  p0 = p0,
  p1 = p1,
  A_eff = A_eff_fixed,
  A_fut = A_fut_fixed,
  label = "CEVAM fixed threshold"
)

cal_eff <- calibrate_eff_threshold(looks, p0, p1, alpha)
cal_fut <- calibrate_fut_threshold(looks, p0, p1, beta)

calibrated_table <- make_boundary_table(
  looks = looks,
  p0 = p0,
  p1 = p1,
  A_eff = cal_eff$A_eff,
  A_fut = cal_fut$A_fut,
  label = "CEVAM planned-look calibrated"
)

boundary_table <- rbind(fixed_table, calibrated_table)

reported_decision <- data.frame(
  method = c("CEVAM fixed threshold", "CEVAM planned-look calibrated"),
  n = reported_n,
  x = reported_x,
  futility_boundary = c(
    fut_boundary_at_n(reported_n, p0, p1, A_fut_fixed),
    fut_boundary_at_n(reported_n, p0, p1, cal_fut$A_fut)
  ),
  efficacy_boundary = c(
    eff_boundary_at_n(reported_n, p0, p1, A_eff_fixed),
    eff_boundary_at_n(reported_n, p0, p1, cal_eff$A_eff)
  ),
  decision = c(
    classify_decision(reported_n, reported_x, p0, p1, A_eff_fixed, A_fut_fixed),
    classify_decision(reported_n, reported_x, p0, p1, cal_eff$A_eff, cal_fut$A_fut)
  ),
  stringsAsFactors = FALSE
)

oc_summary <- data.frame(
  method = c("CEVAM fixed threshold", "CEVAM planned-look calibrated"),
  A_eff = c(A_eff_fixed, cal_eff$A_eff),
  A_fut = c(A_fut_fixed, cal_fut$A_fut),
  type1_error_under_p0 = c(
    prob_ever_eff(looks, p = p0, p0 = p0, p1 = p1, A_eff = A_eff_fixed),
    cal_eff$type1_error
  ),
  type2_error_under_p1 = c(
    prob_ever_fut(looks, p = p1, p0 = p0, p1 = p1, A_fut = A_fut_fixed),
    cal_fut$type2_error
  ),
  stringsAsFactors = FALSE
)

cat("\nTREND setting\n")
cat("p0 =", p0, ", p1 =", p1, ", alpha =", alpha, ", beta =", beta, "\n")
cat("Maximum sample size N =", N_max, "\n")
cat("Looks used for this re-analysis:", paste(looks, collapse = ", "), "\n\n")

cat("Boundary table\n")
print(boundary_table, row.names = FALSE)

cat("\nReported aggregate result classification\n")
print(reported_decision, row.names = FALSE)

cat("\nOperating-characteristic summary over the chosen looks\n")
print(oc_summary, row.names = FALSE)

# Optional: write CSV outputs
write.csv(boundary_table, "cevam_trend_boundary_table.csv", row.names = FALSE)
write.csv(reported_decision, "cevam_trend_reported_decision.csv", row.names = FALSE)
write.csv(oc_summary, "cevam_trend_oc_summary.csv", row.names = FALSE)
