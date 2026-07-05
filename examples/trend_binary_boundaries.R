# trend_binary_boundaries.R
#
# Standalone R program to compute the CEVAM boundaries used for the
# TREND trial re-analysis in the manuscript.
#
# This script follows the manuscript definitions:
#   CEVAM-NT: beta is fixed at beta0, and alpha_eff is calibrated.
#   CEVAM-T : alpha_eff and beta are jointly calibrated.
#
# The calibration uses exact binomial operating characteristics over the
# specified planned looks and the same selection rule as the simulation code:
#   1) keep candidates with planned-look type I error <= alpha_star + tolerance;
#   2) choose the candidate closest to alpha_star;
#   3) break ties by larger power, smaller EN under p0, and larger PET under p0.
#
# Default TREND settings:
#   p0 = 0.41, p1 = 0.56, maximum sample size N = 65
#   retrospective reported look: n = 44, x = 30
#   looks used for re-analysis: 44 and 65
#
# To compare with the protocol-specified interim look, change looks to c(48, 65).

# -----------------------------
# User settings
# -----------------------------

p0 <- 0.41
p1 <- 0.56
alpha_star <- 0.05
beta0 <- 0.20

type1_tolerance <- 0.005

N_max <- 65L
looks <- c(44L, 65L)

reported_n <- 44L
reported_x <- 30L

alpha_eff_grid <- seq(0.01, 1.00, by = 0.01)
beta_grid <- seq(0.01, 1.00, by = 0.01)

output_prefix <- "cevam_trend"

# -----------------------------
# Core likelihood-ratio functions
# -----------------------------

log_lr_eff <- function(x, n, p0, p1) {
  x * log(p1 / p0) + (n - x) * log((1 - p1) / (1 - p0))
}

log_lr_fut <- function(x, n, p0, p1) {
  x * log(p0 / p1) + (n - x) * log((1 - p0) / (1 - p1))
}

eff_boundary_at_n <- function(n, p0, p1, alpha_eff) {
  xs <- 0:n
  hit <- xs[log_lr_eff(xs, n, p0, p1) >= log(1 / alpha_eff)]
  if (length(hit) == 0L) return(n + 1L)
  min(hit)
}

fut_boundary_at_n <- function(n, p0, p1, beta) {
  xs <- 0:n
  hit <- xs[log_lr_fut(xs, n, p0, p1) >= log(1 / beta)]
  if (length(hit) == 0L) return(-1L)
  max(hit)
}

build_cevam_binary_design <- function(method, looks, p0, p1, alpha_star,
                                      alpha_eff, beta) {
  futility_le <- vapply(
    looks,
    fut_boundary_at_n,
    integer(1),
    p0 = p0,
    p1 = p1,
    beta = beta
  )
  efficacy_ge <- vapply(
    looks,
    eff_boundary_at_n,
    integer(1),
    p0 = p0,
    p1 = p1,
    alpha_eff = alpha_eff
  )
  names(futility_le) <- names(efficacy_ge) <- as.character(looks)

  list(
    method = method,
    alpha_star = alpha_star,
    alpha_eff = alpha_eff,
    beta = beta,
    futility_le = futility_le,
    efficacy_ge = efficacy_ge,
    final_reject_if_not_futile = FALSE
  )
}

make_boundary_table <- function(design, looks) {
  data.frame(
    method = design$method,
    look_n = as.integer(looks),
    futility_boundary = as.integer(design$futility_le[as.character(looks)]),
    efficacy_boundary = as.integer(design$efficacy_ge[as.character(looks)]),
    alpha_star = design$alpha_star,
    alpha_eff = design$alpha_eff,
    beta = design$beta,
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Exact operating characteristics
# -----------------------------

classify_decision <- function(n, x, design, N) {
  key <- as.character(n)
  e_bd <- design$efficacy_ge[key]
  f_bd <- design$futility_le[key]

  if (!is.na(e_bd) && x >= e_bd) return("efficacy")
  if (!is.na(f_bd) && x <= f_bd) return("futility")
  if (n == N) {
    if (isTRUE(design$final_reject_if_not_futile)) return("efficacy")
    return("no_reject")
  }
  "continue"
}

exact_oc_binary <- function(truth_p, design, looks, N) {
  looks <- sort(unique(as.integer(looks)))
  if (tail(looks, 1L) != N) {
    stop("The final look must equal N.", call. = FALSE)
  }

  prev_n <- 0L
  state <- 1
  pr_reject <- 0
  pr_etf <- 0
  pr_ete <- 0
  en <- 0

  for (n in looks) {
    m <- n - prev_n
    pmf <- dbinom(0:m, size = m, prob = truth_p)
    dist <- rep(0, n + 1L)

    for (x0 in 0:prev_n) {
      pr0 <- state[x0 + 1L]
      if (pr0 == 0) next
      idx <- x0 + (0:m) + 1L
      dist[idx] <- dist[idx] + pr0 * pmf
    }

    next_state <- if (n < N) rep(0, n + 1L) else numeric(0)
    for (x in 0:n) {
      px <- dist[x + 1L]
      if (px == 0) next

      dec <- classify_decision(n, x, design, N)
      if (dec == "efficacy") {
        pr_reject <- pr_reject + px
        en <- en + n * px
        if (n < N) pr_ete <- pr_ete + px
      } else if (dec == "futility") {
        en <- en + n * px
        if (n < N) pr_etf <- pr_etf + px
      } else if (dec == "no_reject") {
        en <- en + n * px
      } else {
        next_state[x + 1L] <- next_state[x + 1L] + px
      }
    }

    state <- next_state
    prev_n <- n
  }

  list(
    reject = pr_reject,
    ETF = pr_etf,
    ETE = pr_ete,
    PET = pr_etf + pr_ete,
    EN = en
  )
}

make_candidate <- function(method, p0, p1, looks, N, alpha_star,
                           alpha_eff, beta) {
  design <- build_cevam_binary_design(
    method = method,
    looks = looks,
    p0 = p0,
    p1 = p1,
    alpha_star = alpha_star,
    alpha_eff = alpha_eff,
    beta = beta
  )
  list(
    design = design,
    oc_null = exact_oc_binary(p0, design, looks, N),
    oc_alt = exact_oc_binary(p1, design, looks, N)
  )
}

choose_cevam_candidate <- function(candidates, alpha_star,
                                   type1_tolerance = 0.005,
                                   label = "CEVAM") {
  if (length(candidates) == 0L) {
    stop("No candidate provided for ", label, ".", call. = FALSE)
  }

  keep <- vapply(
    candidates,
    function(z) z$oc_null$reject <= alpha_star + type1_tolerance + 1e-12,
    logical(1)
  )
  candidates <- candidates[keep]

  if (length(candidates) == 0L) {
    stop(
      "No candidate controlled type I error within tolerance for ", label, ".",
      call. = FALSE
    )
  }

  type1 <- vapply(candidates, function(z) z$oc_null$reject, numeric(1))
  dist <- abs(type1 - alpha_star)
  candidates <- candidates[abs(dist - min(dist)) <= 1e-12]
  if (length(candidates) == 1L) return(candidates[[1]])

  power <- vapply(candidates, function(z) z$oc_alt$reject, numeric(1))
  candidates <- candidates[abs(power - max(power)) <= 1e-12]
  if (length(candidates) == 1L) return(candidates[[1]])

  en0 <- vapply(candidates, function(z) z$oc_null$EN, numeric(1))
  candidates <- candidates[abs(en0 - min(en0)) <= 1e-12]
  if (length(candidates) == 1L) return(candidates[[1]])

  pet0 <- vapply(candidates, function(z) z$oc_null$PET, numeric(1))
  candidates[[which.max(pet0)]]
}

calibrate_cevam_nt <- function(p0, p1, looks, N, alpha_star, beta0,
                               alpha_eff_grid, type1_tolerance) {
  candidates <- lapply(
    alpha_eff_grid,
    function(ae) make_candidate(
      method = "CEVAM-NT",
      p0 = p0,
      p1 = p1,
      looks = looks,
      N = N,
      alpha_star = alpha_star,
      alpha_eff = ae,
      beta = beta0
    )
  )
  choose_cevam_candidate(
    candidates,
    alpha_star = alpha_star,
    type1_tolerance = type1_tolerance,
    label = "CEVAM-NT"
  )
}

calibrate_cevam_t <- function(p0, p1, looks, N, alpha_star,
                              alpha_eff_grid, beta_grid, type1_tolerance) {
  grid <- expand.grid(
    alpha_eff = alpha_eff_grid,
    beta = beta_grid,
    KEEP.OUT.ATTRS = FALSE
  )

  candidates <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    candidates[[i]] <- make_candidate(
      method = "CEVAM-T",
      p0 = p0,
      p1 = p1,
      looks = looks,
      N = N,
      alpha_star = alpha_star,
      alpha_eff = grid$alpha_eff[i],
      beta = grid$beta[i]
    )
  }

  choose_cevam_candidate(
    candidates,
    alpha_star = alpha_star,
    type1_tolerance = type1_tolerance,
    label = "CEVAM-T"
  )
}

# -----------------------------
# Run TREND re-analysis
# -----------------------------

nt <- calibrate_cevam_nt(
  p0 = p0,
  p1 = p1,
  looks = looks,
  N = N_max,
  alpha_star = alpha_star,
  beta0 = beta0,
  alpha_eff_grid = alpha_eff_grid,
  type1_tolerance = type1_tolerance
)

tuned <- calibrate_cevam_t(
  p0 = p0,
  p1 = p1,
  looks = looks,
  N = N_max,
  alpha_star = alpha_star,
  alpha_eff_grid = alpha_eff_grid,
  beta_grid = beta_grid,
  type1_tolerance = type1_tolerance
)

boundary_table <- rbind(
  make_boundary_table(nt$design, looks),
  make_boundary_table(tuned$design, looks)
)

reported_decision <- data.frame(
  method = c(nt$design$method, tuned$design$method),
  n = reported_n,
  x = reported_x,
  futility_boundary = c(
    nt$design$futility_le[as.character(reported_n)],
    tuned$design$futility_le[as.character(reported_n)]
  ),
  efficacy_boundary = c(
    nt$design$efficacy_ge[as.character(reported_n)],
    tuned$design$efficacy_ge[as.character(reported_n)]
  ),
  decision = c(
    classify_decision(reported_n, reported_x, nt$design, N_max),
    classify_decision(reported_n, reported_x, tuned$design, N_max)
  ),
  stringsAsFactors = FALSE
)

reported_decision$decision <- ifelse(
  reported_decision$decision == "efficacy",
  "Stop for efficacy",
  ifelse(
    reported_decision$decision == "futility",
    "Stop for futility",
    ifelse(reported_decision$decision == "no_reject", "No rejection", "Continue")
  )
)

oc_summary <- data.frame(
  method = c(nt$design$method, tuned$design$method),
  alpha_star = alpha_star,
  alpha_eff = c(nt$design$alpha_eff, tuned$design$alpha_eff),
  beta = c(nt$design$beta, tuned$design$beta),
  type1_at_p0 = c(nt$oc_null$reject, tuned$oc_null$reject),
  power_at_p1 = c(nt$oc_alt$reject, tuned$oc_alt$reject),
  ETF_at_p0 = c(nt$oc_null$ETF, tuned$oc_null$ETF),
  ETE_at_p0 = c(nt$oc_null$ETE, tuned$oc_null$ETE),
  PET_at_p0 = c(nt$oc_null$PET, tuned$oc_null$PET),
  EN_at_p0 = c(nt$oc_null$EN, tuned$oc_null$EN),
  ETF_at_p1 = c(nt$oc_alt$ETF, tuned$oc_alt$ETF),
  ETE_at_p1 = c(nt$oc_alt$ETE, tuned$oc_alt$ETE),
  PET_at_p1 = c(nt$oc_alt$PET, tuned$oc_alt$PET),
  EN_at_p1 = c(nt$oc_alt$EN, tuned$oc_alt$EN),
  stringsAsFactors = FALSE
)

cat("\nTREND setting\n")
cat("p0 =", p0, ", p1 =", p1,
    ", alpha_star =", alpha_star,
    ", beta0 =", beta0, "\n")
cat("Type I tolerance =", type1_tolerance, "\n")
cat("Maximum sample size N =", N_max, "\n")
cat("Looks used for this re-analysis:", paste(looks, collapse = ", "), "\n\n")

cat("Boundary table\n")
print(boundary_table, row.names = FALSE)

cat("\nReported aggregate result classification\n")
print(reported_decision, row.names = FALSE)

cat("\nOperating-characteristic summary over the chosen looks\n")
print(oc_summary, row.names = FALSE)

# Optional: write CSV outputs
write.csv(boundary_table, paste0(output_prefix, "_binary_boundaries.csv"), row.names = FALSE)
write.csv(reported_decision, paste0(output_prefix, "_reported_decision.csv"), row.names = FALSE)
write.csv(oc_summary, paste0(output_prefix, "_oc_summary.csv"), row.names = FALSE)
