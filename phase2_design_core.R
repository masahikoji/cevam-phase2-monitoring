# phase2_design_core.R
#
# Core utilities for:
#   1) generating pretabulated boundary tables for BOP2, BOP2-FE, and CEVAM,
#   2) exporting those boundary tables to Excel, and
#   3) reading the Excel workbook back for simulation.
#
# Design scope
# -----------
# BOP2-F      : original BOP2 futility-only design.
# BOP2-FE-1   : futility + efficacy with power-function CF(n), CE(n).
# BOP2-FE-2   : futility + efficacy with power-function CF(n) and
#               O'Brien-Fleming-type CE(n).
# CEVAM-NT   : planned-look calibrated e-value design; beta fixed by the user,
#               alpha_eff selected to use the nominal type I error.
# CEVAM-T    : planned-look calibrated e-value design; alpha_eff and beta
#               jointly selected to use the nominal type I error, then maximize
#               power and reduce EN under H0.
#
# The paper settings follow:
#   - type I error = 0.10
#   - N = 40
#   - looks = 10, 15, 20, 25, 30, 35, 40
#   - Example 1 pair-specific null/alternative as in Zhou et al. (2017)
#   - Examples 2-4: scenario 1 as design H0 and scenario 7 as design H1
#
# Required package for Excel IO:
#   openxlsx

# ============================================================
# Package helpers
# ============================================================
ensure_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required. Install it with install.packages('openxlsx').",
      call. = FALSE
    )
  }
}

# ============================================================
# Paper settings
# ============================================================

paper_settings <- function(alpha = 0.10,
                           beta = alpha,
                           N = 40L,
                           looks = c(10L, 15L, 20L, 25L, 30L, 35L, 40L)) {
  stopifnot(tail(looks, 1L) == N)

  # Example 1: Zhou Table II scenarios 1-4
  ex1_pairs <- list(
    list(pair_id = 1L, p0 = 0.20, p1 = 0.40, truths = c(0.20, 0.30, 0.40, 0.50)),
    list(pair_id = 2L, p0 = 0.30, p1 = 0.50, truths = c(0.30, 0.40, 0.50, 0.60)),
    list(pair_id = 3L, p0 = 0.40, p1 = 0.60, truths = c(0.40, 0.50, 0.60, 0.70)),
    list(pair_id = 4L, p0 = 0.50, p1 = 0.70, truths = c(0.50, 0.60, 0.70, 0.80))
  )

  # Example 2: Zhou Table III
  ex2_thetas <- rbind(
    c(0.15, 0.15, 0.30, 0.40),
    c(0.20, 0.10, 0.30, 0.40),
    c(0.20, 0.15, 0.30, 0.35),
    c(0.20, 0.20, 0.30, 0.30),
    c(0.20, 0.25, 0.30, 0.25),
    c(0.25, 0.20, 0.30, 0.25),
    c(0.25, 0.25, 0.25, 0.25),
    c(0.30, 0.25, 0.25, 0.20)
  )

  # Example 3: Zhou Table IV / Xu Table 5
  ex3_thetas <- rbind(
    c(0.05, 0.05, 0.15, 0.75),
    c(0.05, 0.10, 0.15, 0.70),
    c(0.10, 0.10, 0.15, 0.65),
    c(0.10, 0.15, 0.15, 0.60),
    c(0.10, 0.15, 0.20, 0.55),
    c(0.15, 0.15, 0.10, 0.60),
    c(0.15, 0.15, 0.20, 0.50),
    c(0.15, 0.15, 0.25, 0.45)
  )

  # Example 4: Zhou Table V / Xu Table 6
  ex4_thetas <- rbind(
    c(0.15, 0.30, 0.15, 0.40),
    c(0.20, 0.30, 0.15, 0.35),
    c(0.10, 0.30, 0.15, 0.45),
    c(0.15, 0.35, 0.10, 0.40),
    c(0.15, 0.35, 0.05, 0.45),
    c(0.15, 0.40, 0.05, 0.40),
    c(0.18, 0.42, 0.02, 0.38),
    c(0.15, 0.50, 0.05, 0.30)
  )

  ex2 <- list(
    example = "example2",
    title = "Nested efficacy endpoints",
    comp1_name = "CR",
    comp2_name = "CRPR",
    mode_fut = "and",
    mode_eff = "or",
    # Rows are component success indicators over categories 1:4
    B = matrix(c(1, 0, 0, 0,
                 1, 1, 0, 0), nrow = 2L, byrow = TRUE),
    K = 4L,
    phi0 = c(0.15, 0.30),
    phi1 = c(0.25, 0.50),
    theta_null = ex2_thetas[1L, ],
    theta_alt = ex2_thetas[7L, ],
    theta_scenarios = ex2_thetas,
    scenario_ids = 1:8,
    prior_s = c(0.25, 0.50),
    prior_f = c(0.75, 0.50)
  )

  ex3 <- list(
    example = "example3",
    title = "Co-primary efficacy endpoints",
    comp1_name = "ORR",
    comp2_name = "EFS6",
    mode_fut = "and",
    mode_eff = "or",
    B = matrix(c(1, 1, 0, 0,
                 1, 0, 1, 0), nrow = 2L, byrow = TRUE),
    K = 4L,
    phi0 = c(0.10, 0.20),
    phi1 = c(0.30, 0.35),
    theta_null = ex3_thetas[1L, ],
    theta_alt = ex3_thetas[7L, ],
    theta_scenarios = ex3_thetas,
    scenario_ids = 1:8,
    prior_s = c(0.50, 0.50),
    prior_f = c(0.50, 0.50)
  )

  ex4 <- list(
    example = "example4",
    title = "Joint efficacy-toxicity",
    comp1_name = "Responses",
    comp2_name = "Toxicities",
    mode_fut = "or",
    mode_eff = "and",
    # For state recursion we track responses and toxicities.
    B = matrix(c(1, 1, 0, 0,
                 1, 0, 1, 0), nrow = 2L, byrow = TRUE),
    K = 4L,
    # comp1 = response, comp2 = toxicity probabilities
    phi0 = c(0.45, 0.30),
    phi1 = c(0.60, 0.20),
    theta_null = ex4_thetas[1L, ],
    theta_alt = ex4_thetas[7L, ],
    theta_scenarios = ex4_thetas,
    scenario_ids = 1:8,
    prior_s = c(0.50, 0.50),
    prior_f = c(0.50, 0.50)
  )

  list(
    alpha = alpha,
    beta = beta,
    N = as.integer(N),
    looks = as.integer(looks),
    ex1_pairs = ex1_pairs,
    ex2 = ex2,
    ex3 = ex3,
    ex4 = ex4
  )
}

# ============================================================
# Search grids
# ============================================================

default_search_grids <- function(search_mode = c("adaptive", "paper", "coarse")) {
  search_mode <- match.arg(search_mode)

  # search_mode = "paper" keeps the original broad exhaustive grid.
  # The default "adaptive" mode evaluates a coarse grid first, then
  # refines locally around the current best candidate.
  list(
    bop2_lambda = seq(0.50, 0.98, by = 0.02),
    bop2_gamma  = seq(0.20, 3.00, by = 0.10),
    fe_d        = seq(0.70, 0.98, by = 0.02),
    fe_gamma    = seq(0.10, 1.00, by = 0.05),
    fe_eta      = seq(0.10, 3.00, by = 0.10),
    ebop_beta   = seq(0.05, 0.50, by = 0.01),
    # Planned-look calibrated efficacy level for CEVAM.
    # Values > alpha lower the e-value threshold and are selected by exact
    # calibration so that the simulated/planned-look type I error is close to
    # but not above alpha. This sacrifices arbitrary anytime-validity but avoids
    # severe alpha under-use in the finite-look phase II setting.
    ebop_alpha_eff = seq(0.10, 0.50, by = 0.01),
    ebop_beta_component = seq(0.05, 0.50, by = 0.05),
    # Because count boundaries are discrete, exact type I error may jump from
    # below alpha to slightly above alpha.  A small tolerance keeps the design
    # close to the nominal level; set this to 0 for strict non-exceedance.
    ebop_type1_tolerance = 0.005,
    power_loss_tol = 0.03,
    search_mode = search_mode,
    coarse_every = if (search_mode == "coarse") 3L else 2L,
    refine_radius = 2L
  )
}

paper_search_grids <- function() {
  g <- default_search_grids(search_mode = "paper")
  g$ebop_beta <- seq(0.01, 0.80, by = 0.01)
  g$ebop_alpha_eff <- seq(0.01, 0.99, by = 0.01)
  g$ebop_beta_component <- seq(0.05, 0.50, by = 0.05)
  g
}

fast_search_grids <- function() {
  default_search_grids(search_mode = "adaptive")
}
# ============================================================
# Posterior probability helpers
# ============================================================

post_prob_le <- function(phi0, a_s, a_f, x, n) {
  pbeta(phi0, a_s + x, a_f + n - x)
}

post_prob_gt <- function(phi0, a_s, a_f, x, n) {
  1 - pbeta(phi0, a_s + x, a_f + n - x)
}

# For toxicity example when the clinical condition is p_tox < phi0
post_prob_lt <- function(phi0, a_s, a_f, x, n) {
  pbeta(phi0, a_s + x, a_f + n - x)
}

# ============================================================
# E-values for dual CEVAM
# ============================================================

e_plus <- function(x, n, p0, p1) {
  (p1 / p0)^x * ((1 - p1) / (1 - p0))^(n - x)
}

e_minus <- function(x, n, p0, p1) {
  (p0 / p1)^x * ((1 - p0) / (1 - p1))^(n - x)
}

binary_eff_boundary_e <- function(looks, p0, p1, alpha) {
  out <- integer(length(looks))
  names(out) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    rr <- 0:n
    ee <- (p1 / p0)^rr * ((1 - p1) / (1 - p0))^(n - rr)
    idx <- which(ee >= 1 / alpha)
    out[i] <- if (length(idx) == 0L) n + 1L else rr[idx[1L]]
  }
  out
}

binary_fut_boundary_e <- function(looks, p0, p1, beta) {
  out <- integer(length(looks))
  names(out) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    rr <- 0:n
    ee <- (p0 / p1)^rr * ((1 - p0) / (1 - p1))^(n - rr)
    idx <- which(ee >= 1 / beta)
    out[i] <- if (length(idx) == 0L) -1L else max(rr[idx])
  }
  out
}

# ============================================================
# Cutoff functions
# ============================================================

cutoff_bop2 <- function(n, N, lambda, gamma) {
  1 - lambda * (n / N)^gamma
}

cutoff_fe_fut <- function(n, N, d, gamma_f) {
  d * (n / N)^gamma_f
}

cutoff_fe_eff_power <- function(n, N, d, eta) {
  1 - (1 - d) * (n / N)^eta
}

# O'Brien-Fleming-type efficacy cutoff. The paper states that CE(N)=d and the
# boundary is monotonically decreasing in n/N. The implementation below is a
# standard OBF-style monotone curve satisfying CE(N)=d exactly.
cutoff_fe_eff_obf <- function(n, N, d) {
  z <- qnorm((1 + d) / 2)
  2 * pnorm(z * sqrt(N / n)) - 1
}

# ============================================================
# Boundary builders for binary designs
# ============================================================

build_binary_boundary_bop2 <- function(p0, looks, N, lambda, gamma,
                                       prior_s = 0.5, prior_f = 0.5) {
  fut <- integer(length(looks))
  names(fut) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    c_n <- cutoff_bop2(n, N, lambda, gamma)
    rr <- 0:n
    post <- vapply(rr, function(x) post_prob_le(p0, prior_s, prior_f, x, n), numeric(1L))
    idx <- which(post > c_n)
    fut[i] <- if (length(idx) == 0L) -1L else max(rr[idx])
  }
  list(
    method = "BOP2-F",
    lambda = lambda,
    gamma = gamma,
    futility_le = fut,
    efficacy_ge = setNames(rep(NA_integer_, length(looks)), as.character(looks)),
    final_reject_if_not_futile = TRUE
  )
}

build_binary_boundary_bop2fe1 <- function(p0, looks, N, d, gamma_f, eta,
                                          prior_s = 0.5, prior_f = 0.5) {
  fut <- eff <- integer(length(looks))
  names(fut) <- names(eff) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    cf <- cutoff_fe_fut(n, N, d, gamma_f)
    ce <- cutoff_fe_eff_power(n, N, d, eta)
    rr <- 0:n
    post <- vapply(rr, function(x) post_prob_gt(p0, prior_s, prior_f, x, n), numeric(1L))
    idx_f <- which(post < cf)
    idx_e <- which(post >= ce)
    fut[i] <- if (length(idx_f) == 0L) -1L else max(rr[idx_f])
    eff[i] <- if (length(idx_e) == 0L) n + 1L else rr[idx_e[1L]]
  }
  list(
    method = "BOP2-FE-1",
    d = d,
    gamma_f = gamma_f,
    eta = eta,
    futility_le = fut,
    efficacy_ge = eff,
    final_reject_if_not_futile = TRUE
  )
}

build_binary_boundary_bop2fe2 <- function(p0, looks, N, d, gamma_f,
                                          prior_s = 0.5, prior_f = 0.5) {
  fut <- eff <- integer(length(looks))
  names(fut) <- names(eff) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    cf <- cutoff_fe_fut(n, N, d, gamma_f)
    ce <- cutoff_fe_eff_obf(n, N, d)
    rr <- 0:n
    post <- vapply(rr, function(x) post_prob_gt(p0, prior_s, prior_f, x, n), numeric(1L))
    idx_f <- which(post < cf)
    idx_e <- which(post >= ce)
    fut[i] <- if (length(idx_f) == 0L) -1L else max(rr[idx_f])
    eff[i] <- if (length(idx_e) == 0L) n + 1L else rr[idx_e[1L]]
  }
  list(
    method = "BOP2-FE-2",
    d = d,
    gamma_f = gamma_f,
    futility_le = fut,
    efficacy_ge = eff,
    final_reject_if_not_futile = TRUE
  )
}

build_binary_boundary_ebop2 <- function(p0, p1, looks, alpha, beta,
                                        tuned = FALSE, alpha_eff = alpha) {
  list(
    method = if (tuned) "CEVAM-T" else "CEVAM-NT",
    alpha = alpha,
    alpha_eff = alpha_eff,
    beta = beta,
    futility_le = binary_fut_boundary_e(looks, p0, p1, beta),
    efficacy_ge = binary_eff_boundary_e(looks, p0, p1, alpha_eff),
    final_reject_if_not_futile = FALSE
  )
}

# ============================================================
# Boundary builders for projected designs (Examples 2-4)
# ============================================================

build_projected_boundary_bop2 <- function(exspec, looks, N, lambda, gamma) {
  fut1 <- fut2 <- integer(length(looks))
  eff1 <- eff2 <- rep(NA_integer_, length(looks))
  names(fut1) <- names(fut2) <- names(eff1) <- names(eff2) <- as.character(looks)

  for (i in seq_along(looks)) {
    n <- looks[i]
    c_n <- cutoff_bop2(n, N, lambda, gamma)

    rr1 <- 0:n
    rr2 <- 0:n

    if (exspec$example != "example4") {
      post1 <- vapply(rr1, function(x) post_prob_le(exspec$phi0[1L], exspec$prior_s[1L], exspec$prior_f[1L], x, n), numeric(1L))
      post2 <- vapply(rr2, function(x) post_prob_le(exspec$phi0[2L], exspec$prior_s[2L], exspec$prior_f[2L], x, n), numeric(1L))
      idx1 <- which(post1 > c_n)
      idx2 <- which(post2 > c_n)
      fut1[i] <- if (length(idx1) == 0L) -1L else max(rr1[idx1])
      fut2[i] <- if (length(idx2) == 0L) -1L else max(rr2[idx2])
    } else {
      # Responses <= 0.45 OR toxicities > 0.30
      post_resp <- vapply(rr1, function(x) post_prob_le(exspec$phi0[1L], exspec$prior_s[1L], exspec$prior_f[1L], x, n), numeric(1L))
      post_tox  <- vapply(rr2, function(x) post_prob_gt(exspec$phi0[2L], exspec$prior_s[2L], exspec$prior_f[2L], x, n), numeric(1L))
      idx1 <- which(post_resp > c_n)
      idx2 <- which(post_tox > c_n)
      fut1[i] <- if (length(idx1) == 0L) -1L else max(rr1[idx1])
      fut2[i] <- if (length(idx2) == 0L) n + 1L else rr2[idx2[1L]]
    }
  }

  list(
    method = "BOP2-F",
    lambda = lambda,
    gamma = gamma,
    mode_fut = exspec$mode_fut,
    mode_eff = "none",
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = if (exspec$example == "example4") "le" else "le",
    fut2_dir = if (exspec$example == "example4") "ge" else "le",
    eff1_dir = NA_character_,
    eff2_dir = NA_character_,
    fut1 = fut1,
    fut2 = fut2,
    eff1 = eff1,
    eff2 = eff2,
    final_reject_if_not_futile = TRUE
  )
}

build_projected_boundary_bop2fe1 <- function(exspec, looks, N, d, gamma_f, eta) {
  fut1 <- fut2 <- eff1 <- eff2 <- integer(length(looks))
  names(fut1) <- names(fut2) <- names(eff1) <- names(eff2) <- as.character(looks)

  for (i in seq_along(looks)) {
    n <- looks[i]
    cf <- cutoff_fe_fut(n, N, d, gamma_f)
    ce <- cutoff_fe_eff_power(n, N, d, eta)
    rr <- 0:n

    if (exspec$example != "example4") {
      post1 <- vapply(rr, function(x) post_prob_gt(exspec$phi0[1L], exspec$prior_s[1L], exspec$prior_f[1L], x, n), numeric(1L))
      post2 <- vapply(rr, function(x) post_prob_gt(exspec$phi0[2L], exspec$prior_s[2L], exspec$prior_f[2L], x, n), numeric(1L))
      idx1f <- which(post1 < cf)
      idx2f <- which(post2 < cf)
      idx1e <- which(post1 >= ce)
      idx2e <- which(post2 >= ce)
      fut1[i] <- if (length(idx1f) == 0L) -1L else max(rr[idx1f])
      fut2[i] <- if (length(idx2f) == 0L) -1L else max(rr[idx2f])
      eff1[i] <- if (length(idx1e) == 0L) n + 1L else rr[idx1e[1L]]
      eff2[i] <- if (length(idx2e) == 0L) n + 1L else rr[idx2e[1L]]
    } else {
      post_resp <- vapply(rr, function(x) post_prob_gt(exspec$phi0[1L], exspec$prior_s[1L], exspec$prior_f[1L], x, n), numeric(1L))
      post_tox_lt <- vapply(rr, function(x) post_prob_lt(exspec$phi0[2L], exspec$prior_s[2L], exspec$prior_f[2L], x, n), numeric(1L))
      idx1f <- which(post_resp < cf)
      idx2f <- which(post_tox_lt < cf)
      idx1e <- which(post_resp >= ce)
      idx2e <- which(post_tox_lt >= ce)
      fut1[i] <- if (length(idx1f) == 0L) -1L else max(rr[idx1f])
      fut2[i] <- if (length(idx2f) == 0L) n + 1L else rr[idx2f[1L]]
      eff1[i] <- if (length(idx1e) == 0L) n + 1L else rr[idx1e[1L]]
      eff2[i] <- if (length(idx2e) == 0L) -1L else max(rr[idx2e])
    }
  }

  list(
    method = "BOP2-FE-1",
    d = d,
    gamma_f = gamma_f,
    eta = eta,
    mode_fut = exspec$mode_fut,
    mode_eff = exspec$mode_eff,
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = "le",
    fut2_dir = if (exspec$example == "example4") "ge" else "le",
    eff1_dir = "ge",
    eff2_dir = if (exspec$example == "example4") "le" else "ge",
    fut1 = fut1,
    fut2 = fut2,
    eff1 = eff1,
    eff2 = eff2,
    final_reject_if_not_futile = TRUE
  )
}

build_projected_boundary_bop2fe2 <- function(exspec, looks, N, d, gamma_f) {
  fut1 <- fut2 <- eff1 <- eff2 <- integer(length(looks))
  names(fut1) <- names(fut2) <- names(eff1) <- names(eff2) <- as.character(looks)

  for (i in seq_along(looks)) {
    n <- looks[i]
    cf <- cutoff_fe_fut(n, N, d, gamma_f)
    ce <- cutoff_fe_eff_obf(n, N, d)
    rr <- 0:n

    if (exspec$example != "example4") {
      post1 <- vapply(rr, function(x) post_prob_gt(exspec$phi0[1L], exspec$prior_s[1L], exspec$prior_f[1L], x, n), numeric(1L))
      post2 <- vapply(rr, function(x) post_prob_gt(exspec$phi0[2L], exspec$prior_s[2L], exspec$prior_f[2L], x, n), numeric(1L))
      idx1f <- which(post1 < cf)
      idx2f <- which(post2 < cf)
      idx1e <- which(post1 >= ce)
      idx2e <- which(post2 >= ce)
      fut1[i] <- if (length(idx1f) == 0L) -1L else max(rr[idx1f])
      fut2[i] <- if (length(idx2f) == 0L) -1L else max(rr[idx2f])
      eff1[i] <- if (length(idx1e) == 0L) n + 1L else rr[idx1e[1L]]
      eff2[i] <- if (length(idx2e) == 0L) n + 1L else rr[idx2e[1L]]
    } else {
      post_resp <- vapply(rr, function(x) post_prob_gt(exspec$phi0[1L], exspec$prior_s[1L], exspec$prior_f[1L], x, n), numeric(1L))
      post_tox_lt <- vapply(rr, function(x) post_prob_lt(exspec$phi0[2L], exspec$prior_s[2L], exspec$prior_f[2L], x, n), numeric(1L))
      idx1f <- which(post_resp < cf)
      idx2f <- which(post_tox_lt < cf)
      idx1e <- which(post_resp >= ce)
      idx2e <- which(post_tox_lt >= ce)
      fut1[i] <- if (length(idx1f) == 0L) -1L else max(rr[idx1f])
      fut2[i] <- if (length(idx2f) == 0L) n + 1L else rr[idx2f[1L]]
      eff1[i] <- if (length(idx1e) == 0L) n + 1L else rr[idx1e[1L]]
      eff2[i] <- if (length(idx2e) == 0L) -1L else max(rr[idx2e])
    }
  }

  list(
    method = "BOP2-FE-2",
    d = d,
    gamma_f = gamma_f,
    mode_fut = exspec$mode_fut,
    mode_eff = exspec$mode_eff,
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = "le",
    fut2_dir = if (exspec$example == "example4") "ge" else "le",
    eff1_dir = "ge",
    eff2_dir = if (exspec$example == "example4") "le" else "ge",
    fut1 = fut1,
    fut2 = fut2,
    eff1 = eff1,
    eff2 = eff2,
    final_reject_if_not_futile = TRUE
  )
}

build_projected_boundary_ebop2 <- function(exspec, looks, alpha, beta, tuned = FALSE, alpha_eff = alpha) {
  if (exspec$example != "example4") {
    fut1 <- binary_fut_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], beta)
    fut2 <- binary_fut_boundary_e(looks, exspec$phi0[2L], exspec$phi1[2L], beta)
    eff1 <- binary_eff_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], alpha_eff)
    eff2 <- binary_eff_boundary_e(looks, exspec$phi0[2L], exspec$phi1[2L], alpha_eff)
    list(
      method = if (tuned) "CEVAM-T" else "CEVAM-NT",
      alpha = alpha,
      alpha_eff = alpha_eff,
      beta = beta,
      mode_fut = exspec$mode_fut,
      mode_eff = exspec$mode_eff,
      comp1_name = exspec$comp1_name,
      comp2_name = exspec$comp2_name,
      fut1_dir = "le",
      fut2_dir = "le",
      eff1_dir = "ge",
      eff2_dir = "ge",
      fut1 = fut1,
      fut2 = fut2,
      eff1 = eff1,
      eff2 = eff2,
      final_reject_if_not_futile = FALSE
    )
  } else {
    # Example 4: use response and safe for dual e-process, then convert safe
    # boundaries to toxicity boundaries for storage and simulation.
    safe0 <- 1 - exspec$phi0[2L]
    safe1 <- 1 - exspec$phi1[2L]
    fut_resp <- binary_fut_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], beta)
    eff_resp <- binary_eff_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], alpha_eff)
    fut_safe <- binary_fut_boundary_e(looks, safe0, safe1, beta)
    eff_safe <- binary_eff_boundary_e(looks, safe0, safe1, alpha_eff)

    tox_fut_ge <- setNames(looks - fut_safe, as.character(looks))
    tox_eff_le <- setNames(looks - eff_safe, as.character(looks))

    list(
      method = if (tuned) "CEVAM-T" else "CEVAM-NT",
      alpha = alpha,
      alpha_eff = alpha_eff,
      beta = beta,
      mode_fut = exspec$mode_fut,
      mode_eff = exspec$mode_eff,
      comp1_name = exspec$comp1_name,
      comp2_name = exspec$comp2_name,
      fut1_dir = "le",
      fut2_dir = "ge",
      eff1_dir = "ge",
      eff2_dir = "le",
      fut1 = fut_resp,
      fut2 = tox_fut_ge,
      eff1 = eff_resp,
      eff2 = tox_eff_le,
      final_reject_if_not_futile = FALSE
    )
  }
}

# ============================================================
# Published BOP2-F boundaries from Zhou et al. (2017)
# ============================================================

is_paper_looks <- function(looks, N) {
  identical(as.integer(looks), as.integer(c(10L, 15L, 20L, 25L, 30L, 35L, 40L))) && as.integer(N) == 40L
}

published_bop2f_binary <- function(pair_id, looks, N) {
  if (!is_paper_looks(looks, N)) return(NULL)
  tab <- switch(as.character(pair_id),
                "1" = c(1L, 2L, 4L, 5L, 7L, 9L, 10L),
                "2" = c(2L, 3L, 5L, 7L, 10L, 12L, 15L),
                "3" = c(3L, 5L, 7L, 10L, 13L, 16L, 19L),
                "4" = c(3L, 6L, 9L, 12L, 16L, 19L, 24L),
                NULL)
  if (is.null(tab)) return(NULL)
  list(
    method = "BOP2-F",
    source = "published_BOP2_Table_II_reproduction",
    futility_le = setNames(as.integer(tab), as.character(looks)),
    efficacy_ge = setNames(rep(NA_integer_, length(looks)), as.character(looks)),
    final_reject_if_not_futile = TRUE
  )
}

published_bop2f_projected <- function(exspec, looks, N) {
  if (!is_paper_looks(looks, N)) return(NULL)
  if (exspec$example == "example2") {
    fut1 <- c(0L, 1L, 3L, 4L, 5L, 7L, 9L)
    fut2 <- c(2L, 3L, 5L, 8L, 10L, 13L, 16L)
    fut1_dir <- "le"; fut2_dir <- "le"
  } else if (exspec$example == "example3") {
    fut1 <- c(0L, 1L, 2L, 3L, 4L, 5L, 7L)
    fut2 <- c(1L, 2L, 4L, 5L, 7L, 9L, 12L)
    fut1_dir <- "le"; fut2_dir <- "le"
  } else if (exspec$example == "example4") {
    fut1 <- c(2L, 5L, 7L, 10L, 13L, 16L, 19L)
    fut2 <- c(5L, 6L, 8L, 9L, 10L, 11L, 12L)
    fut1_dir <- "le"; fut2_dir <- "ge"
  } else {
    return(NULL)
  }
  list(
    method = "BOP2-F",
    source = "published_BOP2_Table_I",
    mode_fut = exspec$mode_fut,
    mode_eff = "none",
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = fut1_dir,
    fut2_dir = fut2_dir,
    eff1_dir = NA_character_,
    eff2_dir = NA_character_,
    fut1 = setNames(as.integer(fut1), as.character(looks)),
    fut2 = setNames(as.integer(fut2), as.character(looks)),
    eff1 = setNames(rep(NA_integer_, length(looks)), as.character(looks)),
    eff2 = setNames(rep(NA_integer_, length(looks)), as.character(looks)),
    final_reject_if_not_futile = TRUE
  )
}

get_bop2f_binary_design <- function(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every) {
  des <- published_bop2f_binary(pair$pair_id, looks, N)
  if (!is.null(des)) return(des)
  calibrate_bop2_binary(pair$p0, pair$p1, looks, N, alpha, grids,
                        n_cores = n_cores, show_progress = show_progress,
                        progress_every = progress_every)
}

get_bop2f_projected_design <- function(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every) {
  des <- published_bop2f_projected(exspec, looks, N)
  if (!is.null(des)) return(des)
  calibrate_bop2_projected(exspec, looks, N, alpha, grids,
                           n_cores = n_cores, show_progress = show_progress,
                           progress_every = progress_every)
}


# ============================================================
# Published BOP2-FE boundaries from Xu et al. (2025)
# ============================================================
# For the paper setting (N = 40; looks = 10, 15, ..., 40), the
# BOP2-FE comparator must use the published BOP2-FE boundaries rather
# than boundaries regenerated by a different local grid search.  This is
# necessary because small changes in the grid, tie-breaking, or posterior
# cutoff-to-count conversion can change the tabulated boundaries.  These
# tables are taken from Tables 1 and 2 of Xu et al. (2025).

published_bop2fe_binary <- function(pair_id, method, looks, N) {
  if (!is_paper_looks(looks, N)) return(NULL)
  # Tables 1 and 2 in Xu et al. (2025) report the binary setting with
  # H0 = 0.20 and H1 = 0.40, corresponding to pair_id = 1 here.
  if (as.integer(pair_id) != 1L) return(NULL)
  method <- as.character(method)
  if (method == "BOP2-FE-1") {
    fut <- c(1L, 2L, 3L, 5L, 7L, 8L, 11L)
    eff <- c(6L, 8L, 9L, 9L, 10L, 11L, 12L)
    source <- "published_BOP2FE_Table_1"
  } else if (method == "BOP2-FE-2") {
    fut <- c(1L, 2L, 4L, 5L, 7L, 8L, 11L)
    eff <- c(7L, 7L, 8L, 9L, 10L, 11L, 12L)
    source <- "published_BOP2FE_Table_2"
  } else {
    return(NULL)
  }
  list(
    method = method,
    source = source,
    futility_le = setNames(as.integer(fut), as.character(looks)),
    efficacy_ge = setNames(as.integer(eff), as.character(looks)),
    final_reject_if_not_futile = TRUE
  )
}

published_bop2fe_projected <- function(exspec, method, looks, N) {
  if (!is_paper_looks(looks, N)) return(NULL)
  method <- as.character(method)
  if (!(method %in% c("BOP2-FE-1", "BOP2-FE-2"))) return(NULL)

  if (method == "BOP2-FE-1") {
    source <- "published_BOP2FE_Table_1"
    if (exspec$example == "example2") {
      fut1 <- c(0L, 1L, 3L, 4L, 5L, 7L, 10L)
      fut2 <- c(2L, 3L, 6L, 8L, 10L, 13L, 17L)
      eff1 <- c(6L, 7L, 7L, 8L, 9L, 10L, 11L)
      eff2 <- c(7L, 9L, 11L, 13L, 14L, 16L, 18L)
    } else if (exspec$example == "example3") {
      fut1 <- c(0L, 1L, 2L, 3L, 4L, 5L, 7L)
      fut2 <- c(1L, 2L, 4L, 6L, 7L, 9L, 12L)
      eff1 <- c(5L, 6L, 6L, 7L, 7L, 7L, 8L)
      eff2 <- c(7L, 8L, 9L, 10L, 11L, 12L, 13L)
    } else if (exspec$example == "example4") {
      fut1 <- c(3L, 5L, 8L, 10L, 13L, 16L, 19L)
      fut2 <- c(5L, 6L, 8L, 9L, 10L, 11L, 12L)
      eff1 <- c(8L, 11L, 13L, 15L, 16L, 18L, 20L)
      eff2 <- c(0L, 1L, 3L, 5L, 6L, 8L, 11L)
    } else {
      return(NULL)
    }
  } else {
    source <- "published_BOP2FE_Table_2"
    if (exspec$example == "example2") {
      fut1 <- c(1L, 2L, 3L, 5L, 6L, 8L, 9L)
      fut2 <- c(2L, 4L, 6L, 8L, 11L, 13L, 16L)
      eff1 <- c(7L, 8L, 8L, 9L, 9L, 10L, 10L)
      eff2 <- c(9L, 11L, 12L, 13L, 15L, 16L, 17L)
    } else if (exspec$example == "example3") {
      fut1 <- c(0L, 1L, 2L, 3L, 4L, 5L, 7L)
      fut2 <- c(1L, 2L, 4L, 6L, 7L, 9L, 12L)
      eff1 <- c(6L, 6L, 6L, 7L, 7L, 7L, 8L)
      eff2 <- c(8L, 8L, 9L, 10L, 11L, 12L, 13L)
    } else if (exspec$example == "example4") {
      fut1 <- c(3L, 5L, 8L, 10L, 13L, 16L, 19L)
      fut2 <- c(5L, 6L, 7L, 8L, 9L, 10L, 11L)
      eff1 <- c(8L, 10L, 12L, 14L, 16L, 18L, 20L)
      eff2 <- c(1L, 2L, 4L, 5L, 7L, 9L, 10L)
    } else {
      return(NULL)
    }
  }

  list(
    method = method,
    source = source,
    mode_fut = exspec$mode_fut,
    mode_eff = exspec$mode_eff,
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = "le",
    fut2_dir = if (exspec$example == "example4") "ge" else "le",
    eff1_dir = "ge",
    eff2_dir = if (exspec$example == "example4") "le" else "ge",
    fut1 = setNames(as.integer(fut1), as.character(looks)),
    fut2 = setNames(as.integer(fut2), as.character(looks)),
    eff1 = setNames(as.integer(eff1), as.character(looks)),
    eff2 = setNames(as.integer(eff2), as.character(looks)),
    final_reject_if_not_futile = TRUE
  )
}

get_bop2fe1_binary_design <- function(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every) {
  des <- published_bop2fe_binary(pair$pair_id, "BOP2-FE-1", looks, N)
  if (!is.null(des)) return(des)
  calibrate_bop2fe1_binary(pair$p0, pair$p1, looks, N, alpha, grids,
                           n_cores = n_cores, show_progress = show_progress,
                           progress_every = progress_every)
}

get_bop2fe2_binary_design <- function(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every) {
  des <- published_bop2fe_binary(pair$pair_id, "BOP2-FE-2", looks, N)
  if (!is.null(des)) return(des)
  calibrate_bop2fe2_binary(pair$p0, pair$p1, looks, N, alpha, grids,
                           n_cores = n_cores, show_progress = show_progress,
                           progress_every = progress_every)
}

get_bop2fe1_projected_design <- function(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every) {
  des <- published_bop2fe_projected(exspec, "BOP2-FE-1", looks, N)
  if (!is.null(des)) return(des)
  calibrate_bop2fe1_projected(exspec, looks, N, alpha, grids,
                              n_cores = n_cores, show_progress = show_progress,
                              progress_every = progress_every)
}

get_bop2fe2_projected_design <- function(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every) {
  des <- published_bop2fe_projected(exspec, "BOP2-FE-2", looks, N)
  if (!is.null(des)) return(des)
  calibrate_bop2fe2_projected(exspec, looks, N, alpha, grids,
                              n_cores = n_cores, show_progress = show_progress,
                              progress_every = progress_every)
}


# ============================================================
# Decision helpers
# ============================================================

hit_dir <- function(x, dir, b) {
  if (is.na(b) || is.na(dir) || dir == "none") return(FALSE)
  if (dir == "le") return(x <= b)
  if (dir == "ge") return(x >= b)
  stop("Unknown direction: ", dir)
}

binary_eval <- function(x, n, look_idx, design, looks, N) {
  key <- as.character(n)
  eff_hit <- if (all(is.na(design$efficacy_ge))) FALSE else x >= design$efficacy_ge[key]
  fut_hit <- x <= design$futility_le[key]

  if (eff_hit) {
    return(list(status = "efficacy"))
  }
  if (fut_hit) {
    return(list(status = "futility"))
  }
  if (n == N) {
    if (isTRUE(design$final_reject_if_not_futile)) {
      return(list(status = "efficacy"))
    }
    return(list(status = "no_reject"))
  }
  list(status = "continue")
}

projected_eval <- function(u, v, n, design, N) {
  key <- as.character(n)

  eff1 <- hit_dir(u, design$eff1_dir, design$eff1[key])
  eff2 <- hit_dir(v, design$eff2_dir, design$eff2[key])
  fut1 <- hit_dir(u, design$fut1_dir, design$fut1[key])
  fut2 <- hit_dir(v, design$fut2_dir, design$fut2[key])

  eff_hit <- switch(design$mode_eff,
                    "or" = eff1 || eff2,
                    "and" = eff1 && eff2,
                    "none" = FALSE,
                    FALSE)

  fut_hit <- switch(design$mode_fut,
                    "and" = fut1 && fut2,
                    "or" = fut1 || fut2,
                    FALSE)

  if (eff_hit) return(list(status = "efficacy"))
  if (fut_hit) return(list(status = "futility"))
  if (n == N) {
    if (isTRUE(design$final_reject_if_not_futile)) return(list(status = "efficacy"))
    return(list(status = "no_reject"))
  }
  list(status = "continue")
}

# ============================================================
# Exact operating characteristics: binary
# ============================================================

exact_oc_binary <- function(truth_p, design, looks, N) {
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
      dec <- binary_eval(x, n, NA, design, looks, N)$status
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

  list(reject = pr_reject,
       ETF = pr_etf,
       ETE = pr_ete,
       PET = pr_etf + pr_ete,
       EN = en)
}

# ============================================================
# Exact operating characteristics: projected endpoints
# ============================================================

cohort_delta_pmf <- function(m, probs, B) {
  stopifnot(length(probs) == ncol(B), abs(sum(probs) - 1) < 1e-8)
  K <- length(probs)
  out <- new.env(parent = emptyenv())

  add_prob <- function(d1, d2, p) {
    key <- paste(d1, d2, sep = ",")
    if (exists(key, envir = out, inherits = FALSE)) {
      assign(key, get(key, envir = out, inherits = FALSE) + p, envir = out)
    } else {
      assign(key, p, envir = out)
    }
  }

  if (K == 4L) {
    for (x1 in 0:m) {
      for (x2 in 0:(m - x1)) {
        for (x3 in 0:(m - x1 - x2)) {
          x4 <- m - x1 - x2 - x3
          x <- c(x1, x2, x3, x4)
          p <- dmultinom(x, prob = probs)
          d <- as.vector(B %*% x)
          add_prob(d[1L], d[2L], p)
        }
      }
    }
  } else if (K == 3L) {
    for (x1 in 0:m) {
      for (x2 in 0:(m - x1)) {
        x3 <- m - x1 - x2
        x <- c(x1, x2, x3)
        p <- dmultinom(x, prob = probs)
        d <- as.vector(B %*% x)
        add_prob(d[1L], d[2L], p)
      }
    }
  } else {
    stop("K must be 3 or 4.")
  }

  keys <- ls(out, all.names = TRUE)
  dd <- do.call(rbind, strsplit(keys, ",", fixed = TRUE))
  data.frame(
    du = as.integer(dd[, 1L]),
    dv = as.integer(dd[, 2L]),
    prob = vapply(keys, function(k) get(k, envir = out, inherits = FALSE), numeric(1L)),
    stringsAsFactors = FALSE
  )
}

# Lightweight cache for cohort-level multinomial increments used by exact_oc_projected().
.phase2_delta_pmf_cache <- new.env(parent = emptyenv())

cohort_delta_pmf_cached <- function(m, probs, B) {
  key <- paste(
    m,
    paste(formatC(probs, digits = 15, format = "fg"), collapse = ","),
    paste(as.integer(B), collapse = ","),
    paste(dim(B), collapse = "x"),
    sep = "|"
  )
  if (exists(key, envir = .phase2_delta_pmf_cache, inherits = FALSE)) {
    return(get(key, envir = .phase2_delta_pmf_cache, inherits = FALSE))
  }
  out <- cohort_delta_pmf(m, probs, B)
  assign(key, out, envir = .phase2_delta_pmf_cache)
  out
}

make_projected_oc_cache <- function(exspec, looks, N) {
  cohort_sizes <- c(looks[1L], diff(looks))
  list(
    null = lapply(cohort_sizes, function(m) cohort_delta_pmf_cached(m, exspec$theta_null, exspec$B)),
    alt  = lapply(cohort_sizes, function(m) cohort_delta_pmf_cached(m, exspec$theta_alt,  exspec$B)),
    looks = looks,
    N = N
  )
}

exact_oc_projected_from_delta <- function(delta_pmfs, design, looks, N) {
  state <- c("0,0" = 1)
  pr_reject <- 0
  pr_etf <- 0
  pr_ete <- 0
  en <- 0

  for (k in seq_along(looks)) {
    n <- looks[k]
    delta_df <- delta_pmfs[[k]]
    next_dist <- new.env(parent = emptyenv())

    for (key in names(state)) {
      pr0 <- state[[key]]
      if (pr0 == 0) next
      uv <- as.integer(strsplit(key, ",", fixed = TRUE)[[1L]])
      u0 <- uv[1L]
      v0 <- uv[2L]
      for (j in seq_len(nrow(delta_df))) {
        u <- u0 + delta_df$du[j]
        v <- v0 + delta_df$dv[j]
        p <- pr0 * delta_df$prob[j]
        dec <- projected_eval(u, v, n, design, N)$status
        if (dec == "efficacy") {
          pr_reject <- pr_reject + p
          en <- en + n * p
          if (n < N) pr_ete <- pr_ete + p
        } else if (dec == "futility") {
          en <- en + n * p
          if (n < N) pr_etf <- pr_etf + p
        } else if (dec == "no_reject") {
          en <- en + n * p
        } else {
          key2 <- paste(u, v, sep = ",")
          if (exists(key2, envir = next_dist, inherits = FALSE)) {
            assign(key2, get(key2, envir = next_dist, inherits = FALSE) + p, envir = next_dist)
          } else {
            assign(key2, p, envir = next_dist)
          }
        }
      }
    }

    state <- as.list.environment(next_dist, all.names = TRUE)
  }

  list(reject = pr_reject,
       ETF = pr_etf,
       ETE = pr_ete,
       PET = pr_etf + pr_ete,
       EN = en)
}

exact_oc_projected <- function(theta_true, exspec, design, looks, N) {
  cohort_sizes <- c(looks[1L], diff(looks))
  delta_pmfs <- lapply(cohort_sizes, function(m) cohort_delta_pmf_cached(m, theta_true, exspec$B))
  exact_oc_projected_from_delta(delta_pmfs, design, looks, N)
}
# ============================================================
# Candidate selection and grid-evaluation helpers
# ============================================================

choose_best_candidate <- function(cands, method_type = c("bop2", "fe", "ebop2_tuned"), alpha) {
  method_type <- match.arg(method_type)
  if (length(cands) == 0L) stop("No feasible candidate found.")

  keep <- vapply(cands, function(z) !is.null(z$oc_null) && z$oc_null$reject <= alpha + 1e-12, logical(1L))
  cands <- cands[keep]
  if (length(cands) == 0L) stop("No candidate controlled type I error at the target level.")

  keep_alt <- vapply(cands, function(z) !is.null(z$oc_alt), logical(1L))
  cands <- cands[keep_alt]
  if (length(cands) == 0L) stop("No feasible candidate had alternative-hypothesis OC computed.")

  if (method_type == "ebop2_tuned") {
    # CEVAM-T is a futility-efficiency tuning rule.
    # The candidate set has already been filtered by min_alt_reject in
    # evaluate_candidate_grid(), so the primary objective here must be
    # the expected sample size under H0, not maximum power.  The previous
    # implementation maximized power first and therefore often chose a
    # weak futility rule, making CEVAM-T look unnecessarily conservative
    # and inefficient.
    en0 <- vapply(cands, function(z) z$oc_null$EN, numeric(1L))
    mn <- min(en0)
    cands <- cands[abs(en0 - mn) <= 1e-12]
    if (length(cands) == 1L) return(cands[[1L]])

    pow <- vapply(cands, function(z) z$oc_alt$reject, numeric(1L))
    mx <- max(pow)
    cands <- cands[abs(pow - mx) <= 1e-12]
    if (length(cands) == 1L) return(cands[[1L]])

    etf <- vapply(cands, function(z) z$oc_null$ETF, numeric(1L))
    return(cands[[which.max(etf)]])
  }

  pow <- vapply(cands, function(z) z$oc_alt$reject, numeric(1L))
  max_pow <- max(pow)
  cands <- cands[abs(pow - max_pow) <= 1e-12]

  if (length(cands) == 1L) return(cands[[1L]])

  if (method_type == "fe") {
    etf <- vapply(cands, function(z) z$oc_null$ETF, numeric(1L))
    mx <- max(etf)
    cands <- cands[abs(etf - mx) <= 1e-12]
    if (length(cands) == 1L) return(cands[[1L]])
  }

  en0 <- vapply(cands, function(z) z$oc_null$EN, numeric(1L))
  mn <- min(en0)
  cands <- cands[abs(en0 - mn) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  etf <- vapply(cands, function(z) z$oc_null$ETF, numeric(1L))
  cands[[which.max(etf)]]
}

phase2_default_n_cores <- function(max_cores = 4L) {
  dc <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  if (is.na(dc) || dc < 2L) return(1L)
  max(1L, min(as.integer(max_cores), as.integer(dc) - 1L))
}

resolve_n_cores <- function(n_cores = NULL) {
  if (is.null(n_cores) || is.na(n_cores)) return(1L)
  n_cores <- as.integer(n_cores)
  if (n_cores < 1L) return(1L)
  dc <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (!is.na(dc)) n_cores <- min(n_cores, dc)
  max(1L, n_cores)
}

format_elapsed <- function(x) {
  if (!is.finite(x)) return("NA")
  x <- max(0, as.numeric(x))
  if (x < 60) return(sprintf("%.0fs", x))
  if (x < 3600) return(sprintf("%.1fmin", x / 60))
  sprintf("%.1fh", x / 3600)
}

progress_message <- function(label, done, total, start_time, best = NULL, stage = NULL) {
  if (total <= 0L) return(invisible(NULL))
  pct <- 100 * done / total
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  eta <- if (done > 0L) elapsed * (total - done) / done else NA_real_
  prefix <- if (!is.null(stage)) paste0("[", stage, "] ") else ""
  best_txt <- ""
  if (!is.null(best)) {
    best_txt <- sprintf(" | current best: typeI=%.4f, power=%.4f, EN0=%.2f",
                        best$oc_null$reject, best$oc_alt$reject, best$oc_null$EN)
  }
  message(sprintf("%s%s: %d/%d (%.1f%%), elapsed %s, ETA %s%s",
                  prefix, label, done, total, pct, format_elapsed(elapsed),
                  format_elapsed(eta), best_txt))
}

make_param_grid <- function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

coarsen_values <- function(x, every = 2L) {
  x <- sort(unique(x))
  every <- max(1L, as.integer(every))
  if (every <= 1L || length(x) <= 2L) return(x)
  idx <- unique(c(seq(1L, length(x), by = every), length(x)))
  x[idx]
}

local_values_from_grid <- function(grid_values, center, radius = 2L) {
  grid_values <- sort(unique(grid_values))
  if (length(grid_values) == 0L || is.null(center) || is.na(center)) return(grid_values)
  radius <- max(1L, as.integer(radius))
  j <- which.min(abs(grid_values - center))
  lo <- max(1L, j - radius)
  hi <- min(length(grid_values), j + radius)
  grid_values[lo:hi]
}

evaluate_candidate_grid <- function(param_grid,
                                    build_design,
                                    oc_null_fun,
                                    oc_alt_fun,
                                    alpha,
                                    method_type,
                                    label = "calibration",
                                    n_cores = 1L,
                                    show_progress = TRUE,
                                    progress_every = 100L,
                                    chunk_size = NULL,
                                    prefilter_type1 = TRUE,
                                    min_alt_reject = NULL,
                                    stage = NULL) {
  total <- nrow(param_grid)
  if (total == 0L) stop("Empty parameter grid.")
  n_cores <- resolve_n_cores(n_cores)
  progress_every <- max(1L, as.integer(progress_every))
  if (is.null(chunk_size)) {
    chunk_size <- if (n_cores > 1L) max(progress_every, n_cores * 10L) else progress_every
  }
  chunk_size <- max(1L, as.integer(chunk_size))

  eval_one <- function(i) {
    pars <- as.list(param_grid[i, , drop = FALSE])
    des <- build_design(pars)
    oc0 <- oc_null_fun(des)
    if (isTRUE(prefilter_type1) && oc0$reject > alpha + 1e-12) {
      return(list(design = des, oc_null = oc0, oc_alt = NULL, feasible = FALSE))
    }
    oc1 <- oc_alt_fun(des)
    list(design = des, oc_null = oc0, oc_alt = oc1, feasible = TRUE)
  }

  chunks <- split(seq_len(total), ceiling(seq_len(total) / chunk_size))
  start_time <- Sys.time()
  cands <- vector("list", total)
  pos <- 0L

  cl <- NULL
  if (n_cores > 1L && .Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    # PSOCK workers do not inherit the sourced functions automatically.
    # Export the current global functions/objects used by the closures.
    parallel::clusterExport(cl, varlist = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  }

  for (ids in chunks) {
    out <- if (n_cores > 1L) {
      if (.Platform$OS.type == "windows") {
        parallel::parLapplyLB(cl, ids, eval_one)
      } else {
        parallel::mclapply(ids, eval_one, mc.cores = n_cores)
      }
    } else {
      lapply(ids, eval_one)
    }
    cands[(pos + 1L):(pos + length(out))] <- out
    pos <- pos + length(out)

    if (isTRUE(show_progress)) {
      feasible <- cands[seq_len(pos)]
      feasible <- feasible[vapply(feasible, function(z) !is.null(z$oc_alt), logical(1L))]
      best_now <- NULL
      if (length(feasible) > 0L) {
        best_now <- tryCatch(choose_best_candidate(feasible, method_type = method_type, alpha = alpha),
                             error = function(e) NULL)
      }
      progress_message(label, pos, total, start_time, best = best_now, stage = stage)
    }
  }

  feasible <- cands[vapply(cands, function(z) !is.null(z$oc_alt), logical(1L))]
  if (!is.null(min_alt_reject)) {
    feasible <- feasible[vapply(feasible, function(z) z$oc_alt$reject + 1e-12 >= min_alt_reject, logical(1L))]
  }
  if (length(feasible) == 0L) {
    stop("No candidate controlled type I error/power constraints for ", label, call. = FALSE)
  }
  choose_best_candidate(feasible, method_type = method_type, alpha = alpha)
}

calibrate_with_optional_adaptive_search <- function(full_grid,
                                                    make_coarse_grid,
                                                    make_local_grid,
                                                    build_design,
                                                    oc_null_fun,
                                                    oc_alt_fun,
                                                    alpha,
                                                    method_type,
                                                    label,
                                                    grids,
                                                    n_cores,
                                                    show_progress,
                                                    progress_every,
                                                    chunk_size = NULL) {
  mode <- grids$search_mode
  if (is.null(mode)) mode <- "paper"
  mode <- match.arg(mode, c("adaptive", "paper", "coarse"))

  if (mode == "paper") {
    return(evaluate_candidate_grid(
      full_grid, build_design, oc_null_fun, oc_alt_fun, alpha, method_type,
      label = label, n_cores = n_cores, show_progress = show_progress,
      progress_every = progress_every, chunk_size = chunk_size, stage = "full"
    )$design)
  }

  coarse_grid <- make_coarse_grid()
  coarse <- evaluate_candidate_grid(
    coarse_grid, build_design, oc_null_fun, oc_alt_fun, alpha, method_type,
    label = label, n_cores = n_cores, show_progress = show_progress,
    progress_every = progress_every, chunk_size = chunk_size, stage = "coarse"
  )

  if (mode == "coarse") return(coarse$design)

  local_grid <- make_local_grid(coarse$design)
  local <- tryCatch(
    evaluate_candidate_grid(
      local_grid, build_design, oc_null_fun, oc_alt_fun, alpha, method_type,
      label = label, n_cores = n_cores, show_progress = show_progress,
      progress_every = progress_every, chunk_size = chunk_size, stage = "refine"
    ),
    error = function(e) {
      warning("Local refinement failed for ", label, "; using coarse best. Error: ", conditionMessage(e), call. = FALSE)
      coarse
    }
  )
  local$design
}

# ============================================================
# Calibration: BOP2 and BOP2-FE
# ============================================================

calibrate_bop2_binary <- function(p0, p1, looks, N, alpha, grids,
                                  prior_s = 0.5, prior_f = 0.5,
                                  n_cores = 1L, show_progress = TRUE,
                                  progress_every = 100L) {
  full_grid <- make_param_grid(lambda = grids$bop2_lambda, gamma = grids$bop2_gamma)
  make_coarse <- function() make_param_grid(
    lambda = coarsen_values(grids$bop2_lambda, grids$coarse_every),
    gamma = coarsen_values(grids$bop2_gamma, grids$coarse_every)
  )
  build <- function(pars) build_binary_boundary_bop2(p0, looks, N, pars$lambda, pars$gamma, prior_s, prior_f)
  oc0 <- function(des) exact_oc_binary(p0, des, looks, N)
  oc1 <- function(des) exact_oc_binary(p1, des, looks, N)
  make_local <- function(des) make_param_grid(
    lambda = local_values_from_grid(grids$bop2_lambda, des$lambda, grids$refine_radius),
    gamma = local_values_from_grid(grids$bop2_gamma, des$gamma, grids$refine_radius)
  )
  calibrate_with_optional_adaptive_search(full_grid, make_coarse, make_local, build, oc0, oc1, alpha, "bop2",
                                          sprintf("binary p0=%.2f p1=%.2f / BOP2-F", p0, p1),
                                          grids, n_cores, show_progress, progress_every)
}

calibrate_bop2_projected <- function(exspec, looks, N, alpha, grids,
                                     n_cores = 1L, show_progress = TRUE,
                                     progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  full_grid <- make_param_grid(lambda = grids$bop2_lambda, gamma = grids$bop2_gamma)
  make_coarse <- function() make_param_grid(
    lambda = coarsen_values(grids$bop2_lambda, grids$coarse_every),
    gamma = coarsen_values(grids$bop2_gamma, grids$coarse_every)
  )
  build <- function(pars) build_projected_boundary_bop2(exspec, looks, N, pars$lambda, pars$gamma)
  oc0 <- function(des) exact_oc_projected_from_delta(cache$null, des, looks, N)
  oc1 <- function(des) exact_oc_projected_from_delta(cache$alt, des, looks, N)
  make_local <- function(des) make_param_grid(
    lambda = local_values_from_grid(grids$bop2_lambda, des$lambda, grids$refine_radius),
    gamma = local_values_from_grid(grids$bop2_gamma, des$gamma, grids$refine_radius)
  )
  calibrate_with_optional_adaptive_search(full_grid, make_coarse, make_local, build, oc0, oc1, alpha, "bop2",
                                          sprintf("%s / BOP2-F", exspec$example),
                                          grids, n_cores, show_progress, progress_every)
}

calibrate_bop2fe1_binary <- function(p0, p1, looks, N, alpha, grids,
                                     prior_s = 0.5, prior_f = 0.5,
                                     n_cores = 1L, show_progress = TRUE,
                                     progress_every = 100L) {
  full_grid <- make_param_grid(d = grids$fe_d, gamma_f = grids$fe_gamma, eta = grids$fe_eta)
  make_coarse <- function() make_param_grid(
    d = coarsen_values(grids$fe_d, grids$coarse_every),
    gamma_f = coarsen_values(grids$fe_gamma, grids$coarse_every),
    eta = coarsen_values(grids$fe_eta, grids$coarse_every)
  )
  build <- function(pars) build_binary_boundary_bop2fe1(p0, looks, N, pars$d, pars$gamma_f, pars$eta, prior_s, prior_f)
  oc0 <- function(des) exact_oc_binary(p0, des, looks, N)
  oc1 <- function(des) exact_oc_binary(p1, des, looks, N)
  make_local <- function(des) make_param_grid(
    d = local_values_from_grid(grids$fe_d, des$d, grids$refine_radius),
    gamma_f = local_values_from_grid(grids$fe_gamma, des$gamma_f, grids$refine_radius),
    eta = local_values_from_grid(grids$fe_eta, des$eta, grids$refine_radius)
  )
  calibrate_with_optional_adaptive_search(full_grid, make_coarse, make_local, build, oc0, oc1, alpha, "fe",
                                          sprintf("binary p0=%.2f p1=%.2f / BOP2-FE-1", p0, p1),
                                          grids, n_cores, show_progress, progress_every)
}

calibrate_bop2fe1_projected <- function(exspec, looks, N, alpha, grids,
                                        n_cores = 1L, show_progress = TRUE,
                                        progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  full_grid <- make_param_grid(d = grids$fe_d, gamma_f = grids$fe_gamma, eta = grids$fe_eta)
  make_coarse <- function() make_param_grid(
    d = coarsen_values(grids$fe_d, grids$coarse_every),
    gamma_f = coarsen_values(grids$fe_gamma, grids$coarse_every),
    eta = coarsen_values(grids$fe_eta, grids$coarse_every)
  )
  build <- function(pars) build_projected_boundary_bop2fe1(exspec, looks, N, pars$d, pars$gamma_f, pars$eta)
  oc0 <- function(des) exact_oc_projected_from_delta(cache$null, des, looks, N)
  oc1 <- function(des) exact_oc_projected_from_delta(cache$alt, des, looks, N)
  make_local <- function(des) make_param_grid(
    d = local_values_from_grid(grids$fe_d, des$d, grids$refine_radius),
    gamma_f = local_values_from_grid(grids$fe_gamma, des$gamma_f, grids$refine_radius),
    eta = local_values_from_grid(grids$fe_eta, des$eta, grids$refine_radius)
  )
  calibrate_with_optional_adaptive_search(full_grid, make_coarse, make_local, build, oc0, oc1, alpha, "fe",
                                          sprintf("%s / BOP2-FE-1", exspec$example),
                                          grids, n_cores, show_progress, progress_every)
}

calibrate_bop2fe2_binary <- function(p0, p1, looks, N, alpha, grids,
                                     prior_s = 0.5, prior_f = 0.5,
                                     n_cores = 1L, show_progress = TRUE,
                                     progress_every = 100L) {
  full_grid <- make_param_grid(d = grids$fe_d, gamma_f = grids$fe_gamma)
  make_coarse <- function() make_param_grid(
    d = coarsen_values(grids$fe_d, grids$coarse_every),
    gamma_f = coarsen_values(grids$fe_gamma, grids$coarse_every)
  )
  build <- function(pars) build_binary_boundary_bop2fe2(p0, looks, N, pars$d, pars$gamma_f, prior_s, prior_f)
  oc0 <- function(des) exact_oc_binary(p0, des, looks, N)
  oc1 <- function(des) exact_oc_binary(p1, des, looks, N)
  make_local <- function(des) make_param_grid(
    d = local_values_from_grid(grids$fe_d, des$d, grids$refine_radius),
    gamma_f = local_values_from_grid(grids$fe_gamma, des$gamma_f, grids$refine_radius)
  )
  calibrate_with_optional_adaptive_search(full_grid, make_coarse, make_local, build, oc0, oc1, alpha, "fe",
                                          sprintf("binary p0=%.2f p1=%.2f / BOP2-FE-2", p0, p1),
                                          grids, n_cores, show_progress, progress_every)
}

calibrate_bop2fe2_projected <- function(exspec, looks, N, alpha, grids,
                                        n_cores = 1L, show_progress = TRUE,
                                        progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  full_grid <- make_param_grid(d = grids$fe_d, gamma_f = grids$fe_gamma)
  make_coarse <- function() make_param_grid(
    d = coarsen_values(grids$fe_d, grids$coarse_every),
    gamma_f = coarsen_values(grids$fe_gamma, grids$coarse_every)
  )
  build <- function(pars) build_projected_boundary_bop2fe2(exspec, looks, N, pars$d, pars$gamma_f)
  oc0 <- function(des) exact_oc_projected_from_delta(cache$null, des, looks, N)
  oc1 <- function(des) exact_oc_projected_from_delta(cache$alt, des, looks, N)
  make_local <- function(des) make_param_grid(
    d = local_values_from_grid(grids$fe_d, des$d, grids$refine_radius),
    gamma_f = local_values_from_grid(grids$fe_gamma, des$gamma_f, grids$refine_radius)
  )
  calibrate_with_optional_adaptive_search(full_grid, make_coarse, make_local, build, oc0, oc1, alpha, "fe",
                                          sprintf("%s / BOP2-FE-2", exspec$example),
                                          grids, n_cores, show_progress, progress_every)
}

# ============================================================
# Planned-look calibration for CEVAM
# ============================================================

ebop_alpha_eff_values <- function(grids) {
  vals <- grids$ebop_alpha_eff
  if (is.null(vals) || length(vals) == 0L) vals <- seq(0.01, 0.99, by = 0.01)
  sort(unique(as.numeric(vals)))
}

ebop_beta_values <- function(grids) {
  vals <- grids$ebop_beta
  if (is.null(vals) || length(vals) == 0L) vals <- seq(0.01, 0.80, by = 0.01)
  sort(unique(as.numeric(vals)))
}

ebop_type1_tolerance <- function(grids) {
  tol <- grids$ebop_type1_tolerance
  if (is.null(tol) || length(tol) == 0L || is.na(tol)) tol <- 0.005
  max(0, as.numeric(tol[1L]))
}

choose_ebop_calibrated_candidate <- function(cands, alpha, label = "CEVAM", type1_tolerance = 0.005) {
  if (length(cands) == 0L) stop("No CEVAM candidate found for ", label, call. = FALSE)
  type1_tolerance <- max(0, as.numeric(type1_tolerance))
  keep <- vapply(cands, function(z) !is.null(z$oc_null) && !is.null(z$oc_alt) &&
                   z$oc_null$reject <= alpha + type1_tolerance + 1e-12, logical(1L))
  cands <- cands[keep]
  if (length(cands) == 0L) stop("No CEVAM candidate controlled type I error within tolerance for ", label, call. = FALSE)

  # First choose the candidate whose planned-look type I error is closest to
  # the nominal alpha. This avoids the severe alpha under-use caused by the
  # universal threshold 1/alpha.
  type1 <- vapply(cands, function(z) z$oc_null$reject, numeric(1L))
  dist <- abs(type1 - alpha)
  mn_dist <- min(dist)
  cands <- cands[abs(dist - mn_dist) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  # Among equally type-I-efficient candidates, prefer higher power.
  pow <- vapply(cands, function(z) z$oc_alt$reject, numeric(1L))
  mx_pow <- max(pow)
  cands <- cands[abs(pow - mx_pow) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  # Then prefer smaller expected sample size under H0.
  en0 <- vapply(cands, function(z) z$oc_null$EN, numeric(1L))
  mn_en <- min(en0)
  cands <- cands[abs(en0 - mn_en) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  # Finally prefer more early termination under H0.
  et0 <- vapply(cands, function(z) z$oc_null$PET, numeric(1L))
  cands[[which.max(et0)]]
}

calibrate_ebop2_binary_nt <- function(p0, p1, looks, N, alpha, beta_default, grids,
                                      n_cores = 1L, show_progress = TRUE,
                                      progress_every = 100L) {
  alpha_vals <- ebop_alpha_eff_values(grids)
  cands <- vector("list", length(alpha_vals))
  start_time <- Sys.time()
  for (i in seq_along(alpha_vals)) {
    des <- build_binary_boundary_ebop2(p0, p1, looks, alpha, beta_default,
                                       tuned = FALSE, alpha_eff = alpha_vals[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_binary(p0, des, looks, N),
      oc_alt = exact_oc_binary(p1, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == length(alpha_vals) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-NT",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("binary p0=%.2f p1=%.2f / CEVAM-NT alpha_eff", p0, p1),
                       i, length(alpha_vals), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("binary p0=%.2f p1=%.2f / CEVAM-NT", p0, p1),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_ebop2_projected_nt <- function(exspec, looks, N, alpha, beta_default, grids,
                                         n_cores = 1L, show_progress = TRUE,
                                         progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  cands <- vector("list", length(alpha_vals))
  start_time <- Sys.time()
  for (i in seq_along(alpha_vals)) {
    des <- build_projected_boundary_ebop2(exspec, looks, alpha, beta_default,
                                          tuned = FALSE, alpha_eff = alpha_vals[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == length(alpha_vals) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-NT",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-NT alpha_eff", exspec$example),
                       i, length(alpha_vals), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-NT", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

# ============================================================
# CEVAM tuned beta search
# ============================================================

calibrate_ebop2_binary_tuned <- function(p0, p1, looks, N, alpha, beta_default, grids,
                                         n_cores = 1L, show_progress = TRUE,
                                         progress_every = 100L) {
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_vals <- ebop_beta_values(grids)
  param_grid <- make_param_grid(alpha_eff = alpha_vals, beta = beta_vals)
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  for (i in seq_len(nrow(param_grid))) {
    des <- build_binary_boundary_ebop2(p0, p1, looks, alpha, param_grid$beta[i],
                                       tuned = TRUE, alpha_eff = param_grid$alpha_eff[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_binary(p0, des, looks, N),
      oc_alt = exact_oc_binary(p1, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-T",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("binary p0=%.2f p1=%.2f / CEVAM-T alpha_eff,beta", p0, p1),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("binary p0=%.2f p1=%.2f / CEVAM-T", p0, p1),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_ebop2_projected_tuned <- function(exspec, looks, N, alpha, beta_default, grids,
                                            n_cores = 1L, show_progress = TRUE,
                                            progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_vals <- ebop_beta_values(grids)
  param_grid <- make_param_grid(alpha_eff = alpha_vals, beta = beta_vals)
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  for (i in seq_len(nrow(param_grid))) {
    des <- build_projected_boundary_ebop2(exspec, looks, alpha, param_grid$beta[i],
                                          tuned = TRUE, alpha_eff = param_grid$alpha_eff[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-T",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-T alpha_eff,beta", exspec$example),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-T", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

# ============================================================
# Build all boundaries under paper settings
# ============================================================

build_all_boundaries <- function(settings = paper_settings(),
                                 grids = default_search_grids(),
                                 n_cores = 1L,
                                 show_progress = TRUE,
                                 progress_every = 100L) {
  looks <- settings$looks
  N <- settings$N
  alpha <- settings$alpha
  beta <- settings$beta
  n_cores <- resolve_n_cores(n_cores)

  if (isTRUE(show_progress)) {
    message("Starting boundary calibration")
    message("  search_mode = ", ifelse(is.null(grids$search_mode), "paper", grids$search_mode))
    message("  n_cores     = ", n_cores)
  }

  ex1 <- lapply(settings$ex1_pairs, function(pair) {
    list(
      pair_id = pair$pair_id,
      p0 = pair$p0,
      p1 = pair$p1,
      bop2f   = get_bop2f_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                           n_cores = n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
      ebop2t  = calibrate_ebop2_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                             n_cores = n_cores, show_progress = show_progress,
                                             progress_every = progress_every)
    )
  })
  names(ex1) <- paste0("pair", vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)))

  ex2 <- list(
    bop2f   = get_bop2f_projected_design(settings$ex2, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    bop2fe1 = get_bop2fe1_projected_design(settings$ex2, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    bop2fe2 = get_bop2fe2_projected_design(settings$ex2, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    ebop2nt = calibrate_ebop2_projected_nt(settings$ex2, looks, N, alpha, beta, grids,
                                           n_cores = n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
    ebop2t  = calibrate_ebop2_projected_tuned(settings$ex2, looks, N, alpha, beta, grids,
                                              n_cores = n_cores, show_progress = show_progress,
                                              progress_every = progress_every)
  )

  ex3 <- list(
    bop2f   = get_bop2f_projected_design(settings$ex3, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    bop2fe1 = get_bop2fe1_projected_design(settings$ex3, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    bop2fe2 = get_bop2fe2_projected_design(settings$ex3, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    ebop2nt = calibrate_ebop2_projected_nt(settings$ex3, looks, N, alpha, beta, grids,
                                           n_cores = n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
    ebop2t  = calibrate_ebop2_projected_tuned(settings$ex3, looks, N, alpha, beta, grids,
                                              n_cores = n_cores, show_progress = show_progress,
                                              progress_every = progress_every)
  )

  ex4 <- list(
    bop2f   = get_bop2f_projected_design(settings$ex4, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    bop2fe1 = get_bop2fe1_projected_design(settings$ex4, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    bop2fe2 = get_bop2fe2_projected_design(settings$ex4, looks, N, alpha, grids, n_cores, show_progress, progress_every),
    ebop2nt = calibrate_ebop2_projected_nt(settings$ex4, looks, N, alpha, beta, grids,
                                           n_cores = n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
    ebop2t  = calibrate_ebop2_projected_tuned(settings$ex4, looks, N, alpha, beta, grids,
                                              n_cores = n_cores, show_progress = show_progress,
                                              progress_every = progress_every)
  )

  list(settings = settings, grids = grids, example1 = ex1, example2 = ex2, example3 = ex3, example4 = ex4)
}

# ============================================================
# Workbook serialization
# ============================================================

flatten_tuning_row <- function(example, pair_id, method, design) {
  params <- list()
  nm <- names(design)
  keys <- setdiff(nm, c("method", "futility_le", "efficacy_ge", "final_reject_if_not_futile",
                        "mode_fut", "mode_eff", "comp1_name", "comp2_name",
                        "fut1_dir", "fut2_dir", "eff1_dir", "eff2_dir",
                        "fut1", "fut2", "eff1", "eff2"))
  for (k in keys) params[[k]] <- design[[k]]
  out <- data.frame(example = example, pair_id = ifelse(is.null(pair_id), NA, pair_id),
                    method = method, stringsAsFactors = FALSE)
  if (length(params) == 0L) return(out)
  for (k in names(params)) out[[k]] <- params[[k]]
  out
}

sheet_example1_df <- function(boundaries, looks) {
  rows <- list()
  i <- 0L
  for (nm in names(boundaries$example1)) {
    entry <- boundaries$example1[[nm]]
    des_list <- list(entry$bop2f, entry$bop2fe1, entry$bop2fe2, entry$ebop2nt, entry$ebop2t)
    for (des in des_list) {
      for (n in looks) {
        i <- i + 1L
        rows[[i]] <- data.frame(
          example = "example1",
          pair_id = entry$pair_id,
          method = des$method,
          look = n,
          comp1_name = "ORR",
          futility_direction = "le",
          futility_bound = des$futility_le[as.character(n)],
          efficacy_direction = ifelse(all(is.na(des$efficacy_ge)), NA, "ge"),
          efficacy_bound = des$efficacy_ge[as.character(n)],
          final_reject_if_not_futile = des$final_reject_if_not_futile,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

sheet_projected_df <- function(example_name, ex_list, looks) {
  rows <- list(); i <- 0L
  des_list <- list(ex_list$bop2f, ex_list$bop2fe1, ex_list$bop2fe2, ex_list$ebop2nt, ex_list$ebop2t)
  for (des in des_list) {
    for (n in looks) {
      i <- i + 1L
      rows[[i]] <- data.frame(
        example = example_name,
        method = des$method,
        look = n,
        mode_fut = des$mode_fut,
        mode_eff = des$mode_eff,
        comp1_name = des$comp1_name,
        comp2_name = des$comp2_name,
        fut1_dir = des$fut1_dir,
        fut1_bound = des$fut1[as.character(n)],
        fut2_dir = des$fut2_dir,
        fut2_bound = des$fut2[as.character(n)],
        eff1_dir = des$eff1_dir,
        eff1_bound = des$eff1[as.character(n)],
        eff2_dir = des$eff2_dir,
        eff2_bound = des$eff2[as.character(n)],
        final_reject_if_not_futile = des$final_reject_if_not_futile,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

write_boundary_workbook <- function(boundaries,
                                    path = "phase2_boundaries.xlsx") {
  ensure_openxlsx()
  wb <- openxlsx::createWorkbook()

  settings_df <- data.frame(
    key = c("alpha", "beta", "N", "looks"),
    value = c(boundaries$settings$alpha,
              boundaries$settings$beta,
              boundaries$settings$N,
              paste(boundaries$settings$looks, collapse = ",")),
    stringsAsFactors = FALSE
  )
  openxlsx::addWorksheet(wb, "settings")
  openxlsx::writeData(wb, "settings", settings_df)

  scen_rows <- list(); i <- 0L
  for (pair in boundaries$settings$ex1_pairs) {
    for (truth in pair$truths) {
      i <- i + 1L
      scen_rows[[i]] <- data.frame(example = "example1", pair_id = pair$pair_id,
                                   scenario_id = NA_integer_, truth_value = truth,
                                   stringsAsFactors = FALSE)
    }
  }
  for (exnm in c("ex2", "ex3", "ex4")) {
    ex <- boundaries$settings[[exnm]]
    for (s in seq_len(nrow(ex$theta_scenarios))) {
      i <- i + 1L
      scen_rows[[i]] <- data.frame(example = ex$example, pair_id = NA_integer_,
                                   scenario_id = s,
                                   theta1 = ex$theta_scenarios[s, 1],
                                   theta2 = ex$theta_scenarios[s, 2],
                                   theta3 = ex$theta_scenarios[s, 3],
                                   theta4 = ex$theta_scenarios[s, 4],
                                   stringsAsFactors = FALSE)
    }
  }
  scen_colset <- unique(unlist(lapply(scen_rows, names)))
  scenarios_df <- do.call(rbind, lapply(scen_rows, function(df) {
    miss <- setdiff(scen_colset, names(df))
    for (m in miss) df[[m]] <- NA
    df[, scen_colset, drop = FALSE]
  }))
  rownames(scenarios_df) <- NULL
  openxlsx::addWorksheet(wb, "scenarios")
  openxlsx::writeData(wb, "scenarios", scenarios_df)

  tune_rows <- list(); j <- 0L
  for (nm in names(boundaries$example1)) {
    entry <- boundaries$example1[[nm]]
    for (des in list(entry$bop2f, entry$bop2fe1, entry$bop2fe2, entry$ebop2nt, entry$ebop2t)) {
      j <- j + 1L; tune_rows[[j]] <- flatten_tuning_row("example1", entry$pair_id, des$method, des)
    }
  }
  for (exnm in c("example2", "example3", "example4")) {
    ex <- boundaries[[exnm]]
    for (des in list(ex$bop2f, ex$bop2fe1, ex$bop2fe2, ex$ebop2nt, ex$ebop2t)) {
      j <- j + 1L; tune_rows[[j]] <- flatten_tuning_row(exnm, NA, des$method, des)
    }
  }
  colset <- unique(unlist(lapply(tune_rows, names)))
  tuning_df <- do.call(rbind, lapply(tune_rows, function(df) {
    miss <- setdiff(colset, names(df))
    for (m in miss) df[[m]] <- NA
    df[, colset, drop = FALSE]
  }))
  rownames(tuning_df) <- NULL
  openxlsx::addWorksheet(wb, "tuning_parameters")
  openxlsx::writeData(wb, "tuning_parameters", tuning_df)

  ex1_df <- sheet_example1_df(boundaries, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example1_boundaries")
  openxlsx::writeData(wb, "example1_boundaries", ex1_df)

  ex2_df <- sheet_projected_df("example2", boundaries$example2, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example2_boundaries")
  openxlsx::writeData(wb, "example2_boundaries", ex2_df)

  ex3_df <- sheet_projected_df("example3", boundaries$example3, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example3_boundaries")
  openxlsx::writeData(wb, "example3_boundaries", ex3_df)

  ex4_df <- sheet_projected_df("example4", boundaries$example4, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example4_boundaries")
  openxlsx::writeData(wb, "example4_boundaries", ex4_df)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

read_boundary_workbook <- function(path) {
  ensure_openxlsx()
  settings_df <- openxlsx::read.xlsx(path, sheet = "settings")
  getv <- function(k) settings_df$value[match(k, settings_df$key)]
  looks <- as.integer(strsplit(getv("looks"), ",", fixed = TRUE)[[1L]])
  settings <- paper_settings(
    alpha = as.numeric(getv("alpha")),
    beta = as.numeric(getv("beta")),
    N = as.integer(getv("N")),
    looks = looks
  )

  list(
    settings = settings,
    scenarios = openxlsx::read.xlsx(path, sheet = "scenarios"),
    tuning = openxlsx::read.xlsx(path, sheet = "tuning_parameters"),
    ex1 = openxlsx::read.xlsx(path, sheet = "example1_boundaries"),
    ex2 = openxlsx::read.xlsx(path, sheet = "example2_boundaries"),
    ex3 = openxlsx::read.xlsx(path, sheet = "example3_boundaries"),
    ex4 = openxlsx::read.xlsx(path, sheet = "example4_boundaries")
  )
}


# Backward-compatible method labels: old workbooks may contain E-BOP2 names,
# while manuscript tables should show CEVAM names.
phase2_method_internal_alias <- function(method) {
  method <- as.character(method)
  if (method == "CEVAM-NT") return("E-BOP2-NT")
  if (method == "CEVAM-T") return("E-BOP2-T")
  method
}

phase2_method_display <- function(method) {
  method <- as.character(method)
  method <- gsub("E-BOP2", "CEVAM", method, fixed = TRUE)
  method
}

# ============================================================
# Reconstruct design objects from workbook rows
# ============================================================

read_binary_design <- function(df, pair_id, method) {
  sub <- df[df$pair_id == pair_id & df$method == method, , drop = FALSE]
  sub <- sub[order(sub$look), , drop = FALSE]
  list(
    method = method,
    futility_le = setNames(as.integer(sub$futility_bound), as.character(sub$look)),
    efficacy_ge = setNames(as.integer(sub$efficacy_bound), as.character(sub$look)),
    final_reject_if_not_futile = as.logical(sub$final_reject_if_not_futile[1L])
  )
}

read_projected_design <- function(df, method) {
  sub <- df[df$method == method, , drop = FALSE]
  sub <- sub[order(sub$look), , drop = FALSE]
  list(
    method = method,
    mode_fut = as.character(sub$mode_fut[1L]),
    mode_eff = as.character(sub$mode_eff[1L]),
    comp1_name = as.character(sub$comp1_name[1L]),
    comp2_name = as.character(sub$comp2_name[1L]),
    fut1_dir = as.character(sub$fut1_dir[1L]),
    fut2_dir = as.character(sub$fut2_dir[1L]),
    eff1_dir = as.character(sub$eff1_dir[1L]),
    eff2_dir = as.character(sub$eff2_dir[1L]),
    fut1 = setNames(as.integer(sub$fut1_bound), as.character(sub$look)),
    fut2 = setNames(as.integer(sub$fut2_bound), as.character(sub$look)),
    eff1 = setNames(as.integer(sub$eff1_bound), as.character(sub$look)),
    eff2 = setNames(as.integer(sub$eff2_bound), as.character(sub$look)),
    final_reject_if_not_futile = as.logical(sub$final_reject_if_not_futile[1L])
  )
}

# ============================================================
# Monte Carlo simulation using precomputed boundaries
# ============================================================

mc_binary <- function(truth_p, design, looks, N, nsim = 10000L, seed = 12345L) {
  set.seed(seed)
  reject <- ETF <- ETE <- 0
  Nbar <- 0
  cohorts <- c(looks[1L], diff(looks))

  for (s in seq_len(nsim)) {
    x <- 0L
    n_now <- 0L
    stopped <- FALSE
    for (k in seq_along(looks)) {
      m <- cohorts[k]
      x <- x + rbinom(1L, size = m, prob = truth_p)
      n_now <- looks[k]
      dec <- binary_eval(x, n_now, k, design, looks, N)$status
      if (dec == "efficacy") {
        reject <- reject + 1L
        if (n_now < N) ETE <- ETE + 1L
        Nbar <- Nbar + n_now
        stopped <- TRUE
        break
      }
      if (dec == "futility") {
        if (n_now < N) ETF <- ETF + 1L
        Nbar <- Nbar + n_now
        stopped <- TRUE
        break
      }
      if (dec == "no_reject") {
        Nbar <- Nbar + n_now
        stopped <- TRUE
        break
      }
    }
    if (!stopped) Nbar <- Nbar + N
  }

  list(reject = reject / nsim,
       ETF = ETF / nsim,
       ETE = ETE / nsim,
       PET = (ETF + ETE) / nsim,
       EN = Nbar / nsim)
}

mc_projected <- function(theta_true, exspec, design, looks, N, nsim = 10000L, seed = 12345L) {
  set.seed(seed)
  reject <- ETF <- ETE <- 0
  Nbar <- 0
  cohorts <- c(looks[1L], diff(looks))

  for (s in seq_len(nsim)) {
    counts <- integer(exspec$K)
    stopped <- FALSE
    for (k in seq_along(looks)) {
      counts <- counts + as.vector(rmultinom(1L, size = cohorts[k], prob = theta_true))
      uv <- as.vector(exspec$B %*% counts)
      dec <- projected_eval(uv[1L], uv[2L], looks[k], design, N)$status
      if (dec == "efficacy") {
        reject <- reject + 1L
        if (looks[k] < N) ETE <- ETE + 1L
        Nbar <- Nbar + looks[k]
        stopped <- TRUE
        break
      }
      if (dec == "futility") {
        if (looks[k] < N) ETF <- ETF + 1L
        Nbar <- Nbar + looks[k]
        stopped <- TRUE
        break
      }
      if (dec == "no_reject") {
        Nbar <- Nbar + looks[k]
        stopped <- TRUE
        break
      }
    }
    if (!stopped) Nbar <- Nbar + N
  }

  list(reject = reject / nsim,
       ETF = ETF / nsim,
       ETE = ETE / nsim,
       PET = (ETF + ETE) / nsim,
       EN = Nbar / nsim)
}

# ============================================================
# Result workbook writer
# ============================================================

write_simulation_results_excel <- function(results, path = "phase2_simulation_results.xlsx") {
  ensure_openxlsx()
  wb <- openxlsx::createWorkbook()
  for (nm in names(results)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, results[[nm]])
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}


# ============================================================
# Joint projected likelihood-ratio CEVAM extension
# ============================================================
# This extension adds two additional methods for complex endpoints:
#   CEVAM-JNT    : joint projected LR efficacy with beta fixed by the user
#   CEVAM-JNT-T  : joint projected LR efficacy with alpha_eff and beta tuned
#
# For Scenarios 2--4, the current componentwise CEVAM rules use separate
# Bernoulli projections.  The joint extension instead uses the joint
# distribution of the two projected counts (U,V) induced by the multinomial
# endpoint.  At look n, the efficacy evidence is
#   L_n^+ = Pr_{theta_alt}(U_n=u,V_n=v) / Pr_{theta_null}(U_n=u,V_n=v),
# and futility evidence is the reverse LR.  In this implementation these are
# planned-look calibrated under the design null, like the other numerical
# CEVAM versions in this project.

.phase2_joint_method_names <- function() c("CEVAM-JNT", "CEVAM-JNT-T")

.projected_pmf_matrix <- function(n, probs, B) {
  n <- as.integer(n)
  K <- length(probs)
  max_u <- n
  max_v <- n
  mat <- matrix(0, nrow = max_u + 1L, ncol = max_v + 1L)
  if (K != 4L) stop("The joint projected LR implementation currently expects K = 4.", call. = FALSE)
  for (x1 in 0:n) {
    for (x2 in 0:(n - x1)) {
      for (x3 in 0:(n - x1 - x2)) {
        x4 <- n - x1 - x2 - x3
        x <- c(x1, x2, x3, x4)
        p <- dmultinom(x, prob = probs)
        d <- as.vector(B %*% x)
        mat[d[1L] + 1L, d[2L] + 1L] <- mat[d[1L] + 1L, d[2L] + 1L] + p
      }
    }
  }
  mat
}

.phase2_projected_pmf_cache <- new.env(parent = emptyenv())

.projected_pmf_matrix_cached <- function(n, probs, B) {
  key <- paste(
    as.integer(n),
    paste(formatC(probs, digits = 15, format = "fg"), collapse = ","),
    paste(as.integer(B), collapse = ","),
    paste(dim(B), collapse = "x"),
    sep = "|"
  )
  if (exists(key, envir = .phase2_projected_pmf_cache, inherits = FALSE)) {
    return(get(key, envir = .phase2_projected_pmf_cache, inherits = FALSE))
  }
  out <- .projected_pmf_matrix(n, probs, B)
  assign(key, out, envir = .phase2_projected_pmf_cache)
  out
}

.joint_projected_lr_values <- function(u, v, n, design) {
  u <- as.integer(u); v <- as.integer(v); n <- as.integer(n)
  pmf0 <- .projected_pmf_matrix_cached(n, design$theta0, design$B)
  pmf1 <- .projected_pmf_matrix_cached(n, design$theta1, design$B)
  p0 <- pmf0[u + 1L, v + 1L]
  p1 <- pmf1[u + 1L, v + 1L]
  eff <- if (p0 <= 0 && p1 > 0) Inf else if (p0 <= 0 && p1 <= 0) 0 else p1 / p0
  fut <- if (p1 <= 0 && p0 > 0) Inf else if (p0 <= 0 && p1 <= 0) 0 else p0 / p1
  c(eff = eff, fut = fut)
}


.joint_projected_count_boundaries <- function(exspec, looks, theta0, theta1, B,
                                              alpha_eff, beta) {
  # Convert the joint projected LR stopping regions into protocol-readable
  # count boundaries with the same layout as BOP2-FE/CEVAM.
  #
  # The conversion is conservative with respect to the joint LR region: the
  # displayed count rule is chosen so that the region implied by the displayed
  # rule is contained in the underlying joint LR region at that look.  Operating
  # characteristics are then evaluated exactly using these displayed count rules.
  make_support_hit <- function(n, kind, threshold) {
    pmf0 <- .projected_pmf_matrix_cached(n, theta0, B)
    pmf1 <- .projected_pmf_matrix_cached(n, theta1, B)
    support <- (pmf0 + pmf1) > 1e-15
    hit <- matrix(FALSE, nrow = n + 1L, ncol = n + 1L)
    for (u in 0:n) {
      for (v in 0:n) {
        if (!support[u + 1L, v + 1L]) next
        p0 <- pmf0[u + 1L, v + 1L]
        p1 <- pmf1[u + 1L, v + 1L]
        lr <- if (kind == "eff") {
          if (p0 <= 0 && p1 > 0) Inf else if (p0 <= 0 && p1 <= 0) 0 else p1 / p0
        } else {
          if (p1 <= 0 && p0 > 0) Inf else if (p0 <= 0 && p1 <= 0) 0 else p0 / p1
        }
        hit[u + 1L, v + 1L] <- lr >= threshold
      }
    }
    list(support = support, hit = hit, pmf0 = pmf0, pmf1 = pmf1)
  }

  uv_grid <- function(n) {
    list(
      U = row(matrix(0, nrow = n + 1L, ncol = n + 1L)) - 1L,
      V = col(matrix(0, nrow = n + 1L, ncol = n + 1L)) - 1L
    )
  }

  all_hit <- function(support, hit, mask) {
    idx <- support & mask
    if (!any(idx)) return(FALSE)
    all(hit[idx])
  }

  prob_region <- function(pmf, support, mask) {
    idx <- support & mask
    if (!any(idx)) return(0)
    sum(pmf[idx])
  }

  sufficient_ge_u <- function(obj, n) {
    g <- uv_grid(n)
    for (b in 0:n) {
      if (all_hit(obj$support, obj$hit, g$U >= b)) return(b)
    }
    n + 1L
  }

  sufficient_ge_v <- function(obj, n) {
    g <- uv_grid(n)
    for (b in 0:n) {
      if (all_hit(obj$support, obj$hit, g$V >= b)) return(b)
    }
    n + 1L
  }

  sufficient_le_u <- function(obj, n) {
    g <- uv_grid(n)
    best <- -1L
    for (b in 0:n) {
      if (all_hit(obj$support, obj$hit, g$U <= b)) best <- b
    }
    best
  }

  sufficient_ge_v_high <- function(obj, n) {
    g <- uv_grid(n)
    for (b in 0:n) {
      if (all_hit(obj$support, obj$hit, g$V >= b)) return(b)
    }
    n + 1L
  }

  best_lower_left_rectangle <- function(obj, n) {
    # Futility for nested/co-primary: stop if U <= f1 AND V <= f2.
    g <- uv_grid(n)
    best <- list(f1 = -1L, f2 = -1L, prob = -1, size = -1L)
    for (f1 in 0:n) {
      for (f2 in 0:n) {
        mask <- (g$U <= f1) & (g$V <= f2)
        if (!all_hit(obj$support, obj$hit, mask)) next
        pr <- prob_region(obj$pmf0, obj$support, mask)
        sz <- sum(obj$support & mask)
        if (pr > best$prob + 1e-15 ||
            (abs(pr - best$prob) <= 1e-15 && sz > best$size)) {
          best <- list(f1 = f1, f2 = f2, prob = pr, size = sz)
        }
      }
    }
    c(f1 = as.integer(best$f1), f2 = as.integer(best$f2))
  }

  best_upper_left_rectangle <- function(obj, n) {
    # Efficacy for efficacy-toxicity: stop if response U >= e1 AND toxicity V <= e2.
    g <- uv_grid(n)
    best <- list(e1 = n + 1L, e2 = -1L, prob = -1, size = -1L)
    for (e1 in 0:n) {
      for (e2 in 0:n) {
        mask <- (g$U >= e1) & (g$V <= e2)
        if (!all_hit(obj$support, obj$hit, mask)) next
        pr <- prob_region(obj$pmf1, obj$support, mask)
        sz <- sum(obj$support & mask)
        if (pr > best$prob + 1e-15 ||
            (abs(pr - best$prob) <= 1e-15 && sz > best$size)) {
          best <- list(e1 = e1, e2 = e2, prob = pr, size = sz)
        }
      }
    }
    c(e1 = as.integer(best$e1), e2 = as.integer(best$e2))
  }

  looks_chr <- as.character(looks)
  fut1 <- fut2 <- eff1 <- eff2 <- integer(length(looks))
  names(fut1) <- names(fut2) <- names(eff1) <- names(eff2) <- looks_chr

  for (ii in seq_along(looks)) {
    n <- as.integer(looks[ii])
    eff_obj <- make_support_hit(n, "eff", 1 / alpha_eff)
    fut_obj <- make_support_hit(n, "fut", 1 / beta)

    if (identical(exspec$example, "example4")) {
      # Clinical rule: efficacy = response high AND toxicity low;
      # futility = response low OR toxicity high.
      er <- best_upper_left_rectangle(eff_obj, n)
      eff1[ii] <- er["e1"]
      eff2[ii] <- er["e2"]
      fut1[ii] <- sufficient_le_u(fut_obj, n)
      fut2[ii] <- sufficient_ge_v_high(fut_obj, n)
    } else {
      # Clinical rule for nested/co-primary: efficacy = component1 high OR
      # component2 high; futility = both components low.
      eff1[ii] <- sufficient_ge_u(eff_obj, n)
      eff2[ii] <- sufficient_ge_v(eff_obj, n)
      fr <- best_lower_left_rectangle(fut_obj, n)
      fut1[ii] <- fr["f1"]
      fut2[ii] <- fr["f2"]
    }
  }

  list(fut1 = fut1, fut2 = fut2, eff1 = eff1, eff2 = eff2)
}

build_projected_boundary_ebop2_joint <- function(exspec, looks, alpha, beta,
                                                 tuned = FALSE, alpha_eff = alpha) {
  looks_chr <- as.character(looks)
  method <- if (tuned) "CEVAM-JNT-T" else "CEVAM-JNT"
  theta0 <- as.numeric(exspec$theta_null)
  theta1 <- as.numeric(exspec$theta_alt)
  B <- exspec$B

  bnd <- .joint_projected_count_boundaries(
    exspec = exspec,
    looks = looks,
    theta0 = theta0,
    theta1 = theta1,
    B = B,
    alpha_eff = alpha_eff,
    beta = beta
  )

  list(
    method = method,
    design_type = "joint_count_projected",
    alpha = alpha,
    alpha_eff = alpha_eff,
    beta = beta,
    theta0 = theta0,
    theta1 = theta1,
    B = B,
    mode_fut = exspec$mode_fut,
    mode_eff = exspec$mode_eff,
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = "le",
    fut2_dir = if (identical(exspec$example, "example4")) "ge" else "le",
    eff1_dir = "ge",
    eff2_dir = if (identical(exspec$example, "example4")) "le" else "ge",
    fut1 = setNames(as.integer(bnd$fut1), looks_chr),
    fut2 = setNames(as.integer(bnd$fut2), looks_chr),
    eff1 = setNames(as.integer(bnd$eff1), looks_chr),
    eff2 = setNames(as.integer(bnd$eff2), looks_chr),
    fut_joint_threshold = setNames(rep(1 / beta, length(looks)), looks_chr),
    eff_joint_threshold = setNames(rep(1 / alpha_eff, length(looks)), looks_chr),
    final_reject_if_not_futile = FALSE
  )
}

projected_eval <- function(u, v, n, design, N) {
  key <- as.character(n)

  if (!is.null(design$design_type) && identical(as.character(design$design_type), "joint_lr_projected")) {
    lrs <- .joint_projected_lr_values(u, v, n, design)
    eff_thr <- 1 / as.numeric(design$alpha_eff)
    fut_thr <- 1 / as.numeric(design$beta)
    if ((is.finite(lrs["eff"]) && lrs["eff"] >= eff_thr) || is.infinite(lrs["eff"])) {
      return(list(status = "efficacy"))
    }
    if ((is.finite(lrs["fut"]) && lrs["fut"] >= fut_thr) || is.infinite(lrs["fut"])) {
      return(list(status = "futility"))
    }
    if (n == N) return(list(status = "no_reject"))
    return(list(status = "continue"))
  }

  eff1 <- hit_dir(u, design$eff1_dir, design$eff1[key])
  eff2 <- hit_dir(v, design$eff2_dir, design$eff2[key])
  fut1 <- hit_dir(u, design$fut1_dir, design$fut1[key])
  fut2 <- hit_dir(v, design$fut2_dir, design$fut2[key])

  eff_hit <- switch(design$mode_eff,
                    "or" = eff1 || eff2,
                    "and" = eff1 && eff2,
                    "none" = FALSE,
                    FALSE)

  fut_hit <- switch(design$mode_fut,
                    "and" = fut1 && fut2,
                    "or" = fut1 || fut2,
                    FALSE)

  if (eff_hit) return(list(status = "efficacy"))
  if (fut_hit) return(list(status = "futility"))
  if (n == N) {
    if (isTRUE(design$final_reject_if_not_futile)) return(list(status = "efficacy"))
    return(list(status = "no_reject"))
  }
  list(status = "continue")
}

calibrate_ebop2_joint_projected_nt <- function(exspec, looks, N, alpha, beta_default, grids,
                                               n_cores = 1L, show_progress = TRUE,
                                               progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  cands <- vector("list", length(alpha_vals))
  start_time <- Sys.time()
  for (i in seq_along(alpha_vals)) {
    des <- build_projected_boundary_ebop2_joint(exspec, looks, alpha, beta_default,
                                                tuned = FALSE, alpha_eff = alpha_vals[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == length(alpha_vals) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-JNT",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-JNT alpha_eff", exspec$example),
                       i, length(alpha_vals), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-JNT", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_ebop2_joint_projected_tuned <- function(exspec, looks, N, alpha, beta_default, grids,
                                                  n_cores = 1L, show_progress = TRUE,
                                                  progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_vals <- ebop_beta_values(grids)
  param_grid <- make_param_grid(alpha_eff = alpha_vals, beta = beta_vals)
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  for (i in seq_len(nrow(param_grid))) {
    des <- build_projected_boundary_ebop2_joint(exspec, looks, alpha, param_grid$beta[i],
                                                tuned = TRUE, alpha_eff = param_grid$alpha_eff[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-JNT-T",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-JNT-T alpha_eff,beta", exspec$example),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-JNT-T", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

.designs_example1 <- function(entry) {
  list(entry$bop2f, entry$bop2fe1, entry$bop2fe2, entry$ebop2nt, entry$ebop2t)
}

.designs_projected <- function(ex_list) {
  # Main CEVAM analysis uses componentwise e-value boundaries with the
  # same OR/AND clinical decision logic as BOP2-FE.  Pure joint LR designs
  # are retained in the file only as development code and are not exported
  # to the main boundary/simulation workbooks.
  list(ex_list$bop2f, ex_list$bop2fe1, ex_list$bop2fe2, ex_list$ebop2nt, ex_list$ebop2t)
}

build_all_boundaries <- function(settings = paper_settings(),
                                 grids = default_search_grids(),
                                 n_cores = 1L,
                                 show_progress = TRUE,
                                 progress_every = 100L) {
  looks <- settings$looks
  N <- settings$N
  alpha <- settings$alpha
  beta <- settings$beta
  n_cores <- resolve_n_cores(n_cores)

  if (isTRUE(show_progress)) {
    message("Starting boundary calibration")
    message("  search_mode = ", ifelse(is.null(grids$search_mode), "paper", grids$search_mode))
    message("  n_cores     = ", n_cores)
    message("  CEVAM methods use componentwise e-value boundaries with OR/AND decision rules")
  }

  ex1 <- lapply(settings$ex1_pairs, function(pair) {
    list(
      pair_id = pair$pair_id,
      p0 = pair$p0,
      p1 = pair$p1,
      bop2f   = get_bop2f_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                           n_cores = n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
      ebop2t  = calibrate_ebop2_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                             n_cores = n_cores, show_progress = show_progress,
                                             progress_every = progress_every)
    )
  })
  names(ex1) <- paste0("pair", vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)))

  make_ex <- function(exspec) {
    list(
      bop2f   = get_bop2f_projected_design(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_projected_design(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_projected_design(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_projected_nt(exspec, looks, N, alpha, beta, grids,
                                             n_cores = n_cores, show_progress = show_progress,
                                             progress_every = progress_every),
      ebop2t  = calibrate_ebop2_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                n_cores = n_cores, show_progress = show_progress,
                                                progress_every = progress_every)
    )
  }

  ex2 <- make_ex(settings$ex2)
  ex3 <- make_ex(settings$ex3)
  ex4 <- make_ex(settings$ex4)

  list(settings = settings, grids = grids, example1 = ex1, example2 = ex2, example3 = ex3, example4 = ex4)
}

flatten_tuning_row <- function(example, pair_id, method, design) {
  params <- list()
  nm <- names(design)
  keys <- setdiff(nm, c("method", "futility_le", "efficacy_ge", "final_reject_if_not_futile",
                        "mode_fut", "mode_eff", "comp1_name", "comp2_name",
                        "fut1_dir", "fut2_dir", "eff1_dir", "eff2_dir",
                        "fut1", "fut2", "eff1", "eff2", "B", "theta0", "theta1",
                        "fut_joint_threshold", "eff_joint_threshold"))
  for (k in keys) {
    val <- design[[k]]
    if (length(val) > 1L) val <- paste(val, collapse = ",")
    params[[k]] <- val
  }
  out <- data.frame(example = example, pair_id = ifelse(is.null(pair_id), NA, pair_id),
                    method = method, stringsAsFactors = FALSE)
  if (length(params) == 0L) return(out)
  for (k in names(params)) out[[k]] <- params[[k]]
  out
}

.add_joint_design_columns <- function(row, des, n) {
  row$design_type <- ifelse(is.null(des$design_type), "component_count", as.character(des$design_type))
  row$alpha_eff <- ifelse(is.null(des$alpha_eff), NA_real_, as.numeric(des$alpha_eff))
  row$beta <- ifelse(is.null(des$beta), NA_real_, as.numeric(des$beta))
  row$eff_joint_threshold <- if (!is.null(des$eff_joint_threshold)) as.numeric(des$eff_joint_threshold[as.character(n)]) else NA_real_
  row$fut_joint_threshold <- if (!is.null(des$fut_joint_threshold)) as.numeric(des$fut_joint_threshold[as.character(n)]) else NA_real_
  for (j in 1:4) {
    row[[paste0("theta0_", j)]] <- if (!is.null(des$theta0)) as.numeric(des$theta0[j]) else NA_real_
    row[[paste0("theta1_", j)]] <- if (!is.null(des$theta1)) as.numeric(des$theta1[j]) else NA_real_
  }
  if (!is.null(des$B)) {
    for (r in 1:nrow(des$B)) {
      for (c in 1:ncol(des$B)) {
        row[[paste0("B", r, c)]] <- as.integer(des$B[r, c])
      }
    }
  } else {
    for (r in 1:2) for (c in 1:4) row[[paste0("B", r, c)]] <- NA_integer_
  }
  row
}

sheet_projected_df <- function(example_name, ex_list, looks) {
  rows <- list(); i <- 0L
  for (des in .designs_projected(ex_list)) {
    for (n in looks) {
      i <- i + 1L
      row <- data.frame(
        example = example_name,
        method = des$method,
        look = n,
        mode_fut = des$mode_fut,
        mode_eff = des$mode_eff,
        comp1_name = des$comp1_name,
        comp2_name = des$comp2_name,
        fut1_dir = des$fut1_dir,
        fut1_bound = des$fut1[as.character(n)],
        fut2_dir = des$fut2_dir,
        fut2_bound = des$fut2[as.character(n)],
        eff1_dir = des$eff1_dir,
        eff1_bound = des$eff1[as.character(n)],
        eff2_dir = des$eff2_dir,
        eff2_bound = des$eff2[as.character(n)],
        final_reject_if_not_futile = des$final_reject_if_not_futile,
        stringsAsFactors = FALSE
      )
      rows[[i]] <- .add_joint_design_columns(row, des, n)
    }
  }
  colset <- unique(unlist(lapply(rows, names)))
  out <- do.call(rbind, lapply(rows, function(df) {
    miss <- setdiff(colset, names(df))
    for (m in miss) df[[m]] <- NA
    df[, colset, drop = FALSE]
  }))
  rownames(out) <- NULL
  out
}

write_boundary_workbook <- function(boundaries, path = "phase2_boundaries.xlsx") {
  ensure_openxlsx()
  wb <- openxlsx::createWorkbook()

  settings_df <- data.frame(
    key = c("alpha", "beta", "N", "looks"),
    value = c(boundaries$settings$alpha,
              boundaries$settings$beta,
              boundaries$settings$N,
              paste(boundaries$settings$looks, collapse = ",")),
    stringsAsFactors = FALSE
  )
  openxlsx::addWorksheet(wb, "settings")
  openxlsx::writeData(wb, "settings", settings_df)

  scen_rows <- list(); i <- 0L
  for (pair in boundaries$settings$ex1_pairs) {
    for (truth in pair$truths) {
      i <- i + 1L
      scen_rows[[i]] <- data.frame(example = "example1", pair_id = pair$pair_id,
                                   scenario_id = NA_integer_, truth_value = truth,
                                   stringsAsFactors = FALSE)
    }
  }
  for (exnm in c("ex2", "ex3", "ex4")) {
    ex <- boundaries$settings[[exnm]]
    for (s in seq_len(nrow(ex$theta_scenarios))) {
      i <- i + 1L
      scen_rows[[i]] <- data.frame(example = ex$example, pair_id = NA_integer_,
                                   scenario_id = s,
                                   theta1 = ex$theta_scenarios[s, 1],
                                   theta2 = ex$theta_scenarios[s, 2],
                                   theta3 = ex$theta_scenarios[s, 3],
                                   theta4 = ex$theta_scenarios[s, 4],
                                   stringsAsFactors = FALSE)
    }
  }
  scen_colset <- unique(unlist(lapply(scen_rows, names)))
  scenarios_df <- do.call(rbind, lapply(scen_rows, function(df) {
    miss <- setdiff(scen_colset, names(df))
    for (m in miss) df[[m]] <- NA
    df[, scen_colset, drop = FALSE]
  }))
  rownames(scenarios_df) <- NULL
  openxlsx::addWorksheet(wb, "scenarios")
  openxlsx::writeData(wb, "scenarios", scenarios_df)

  tune_rows <- list(); j <- 0L
  for (nm in names(boundaries$example1)) {
    entry <- boundaries$example1[[nm]]
    for (des in .designs_example1(entry)) {
      j <- j + 1L; tune_rows[[j]] <- flatten_tuning_row("example1", entry$pair_id, des$method, des)
    }
  }
  for (exnm in c("example2", "example3", "example4")) {
    ex <- boundaries[[exnm]]
    for (des in .designs_projected(ex)) {
      j <- j + 1L; tune_rows[[j]] <- flatten_tuning_row(exnm, NA, des$method, des)
    }
  }
  colset <- unique(unlist(lapply(tune_rows, names)))
  tuning_df <- do.call(rbind, lapply(tune_rows, function(df) {
    miss <- setdiff(colset, names(df))
    for (m in miss) df[[m]] <- NA
    df[, colset, drop = FALSE]
  }))
  rownames(tuning_df) <- NULL
  openxlsx::addWorksheet(wb, "tuning_parameters")
  openxlsx::writeData(wb, "tuning_parameters", tuning_df)

  ex1_df <- sheet_example1_df(boundaries, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example1_boundaries")
  openxlsx::writeData(wb, "example1_boundaries", ex1_df)

  ex2_df <- sheet_projected_df("example2", boundaries$example2, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example2_boundaries")
  openxlsx::writeData(wb, "example2_boundaries", ex2_df)

  ex3_df <- sheet_projected_df("example3", boundaries$example3, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example3_boundaries")
  openxlsx::writeData(wb, "example3_boundaries", ex3_df)

  ex4_df <- sheet_projected_df("example4", boundaries$example4, boundaries$settings$looks)
  openxlsx::addWorksheet(wb, "example4_boundaries")
  openxlsx::writeData(wb, "example4_boundaries", ex4_df)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

read_projected_design <- function(df, method) {
  sub <- df[df$method == method, , drop = FALSE]
  sub <- sub[order(sub$look), , drop = FALSE]
  design_type <- if ("design_type" %in% names(sub)) as.character(sub$design_type[1L]) else "component_count"
  if (!is.na(design_type) && identical(design_type, "joint_lr_projected")) {
    theta0 <- as.numeric(unlist(sub[1L, paste0("theta0_", 1:4)], use.names = FALSE))
    theta1 <- as.numeric(unlist(sub[1L, paste0("theta1_", 1:4)], use.names = FALSE))
    B <- matrix(NA_integer_, nrow = 2L, ncol = 4L)
    for (r in 1:2) for (c in 1:4) B[r, c] <- as.integer(sub[[paste0("B", r, c)]][1L])
    looks <- as.character(sub$look)
    return(list(
      method = method,
      design_type = "joint_lr_projected",
      alpha_eff = as.numeric(sub$alpha_eff[1L]),
      beta = as.numeric(sub$beta[1L]),
      theta0 = theta0,
      theta1 = theta1,
      B = B,
      mode_fut = "joint_lr",
      mode_eff = "joint_lr",
      comp1_name = as.character(sub$comp1_name[1L]),
      comp2_name = as.character(sub$comp2_name[1L]),
      fut1_dir = NA_character_, fut2_dir = NA_character_, eff1_dir = NA_character_, eff2_dir = NA_character_,
      fut1 = setNames(rep(NA_integer_, nrow(sub)), looks),
      fut2 = setNames(rep(NA_integer_, nrow(sub)), looks),
      eff1 = setNames(rep(NA_integer_, nrow(sub)), looks),
      eff2 = setNames(rep(NA_integer_, nrow(sub)), looks),
      fut_joint_threshold = setNames(as.numeric(sub$fut_joint_threshold), looks),
      eff_joint_threshold = setNames(as.numeric(sub$eff_joint_threshold), looks),
      final_reject_if_not_futile = FALSE
    ))
  }
  list(
    method = method,
    mode_fut = as.character(sub$mode_fut[1L]),
    mode_eff = as.character(sub$mode_eff[1L]),
    comp1_name = as.character(sub$comp1_name[1L]),
    comp2_name = as.character(sub$comp2_name[1L]),
    fut1_dir = as.character(sub$fut1_dir[1L]),
    fut2_dir = as.character(sub$fut2_dir[1L]),
    eff1_dir = as.character(sub$eff1_dir[1L]),
    eff2_dir = as.character(sub$eff2_dir[1L]),
    fut1 = setNames(as.integer(sub$fut1_bound), as.character(sub$look)),
    fut2 = setNames(as.integer(sub$fut2_bound), as.character(sub$look)),
    eff1 = setNames(as.integer(sub$eff1_bound), as.character(sub$look)),
    eff2 = setNames(as.integer(sub$eff2_bound), as.character(sub$look)),
    final_reject_if_not_futile = as.logical(sub$final_reject_if_not_futile[1L])
  )
}


# ============================================================
# Final method-label compatibility overrides
# ============================================================
# These definitions intentionally appear at the end of the file so they override
# earlier versions if this file has been patched incrementally.
phase2_method_internal_alias <- function(method) {
  method <- as.character(method)
  if (method == "CEVAM-NT") return("E-BOP2-NT")
  if (method == "CEVAM-T") return("E-BOP2-T")
  method
}

phase2_method_display <- function(method) {
  method <- as.character(method)
  method <- gsub("E-BOP2", "CEVAM", method, fixed = TRUE)
  method
}

read_binary_design <- function(df, pair_id, method) {
  method_req <- as.character(method)
  sub <- df[df$pair_id == pair_id & df$method == method_req, , drop = FALSE]
  if (nrow(sub) == 0L) {
    old <- phase2_method_internal_alias(method_req)
    sub <- df[df$pair_id == pair_id & df$method == old, , drop = FALSE]
  }
  if (nrow(sub) == 0L) stop("Cannot find binary design for method: ", method_req, call. = FALSE)
  sub <- sub[order(sub$look), , drop = FALSE]
  list(
    method = method_req,
    futility_le = setNames(as.integer(sub$futility_bound), as.character(sub$look)),
    efficacy_ge = setNames(as.integer(sub$efficacy_bound), as.character(sub$look)),
    final_reject_if_not_futile = as.logical(sub$final_reject_if_not_futile[1L])
  )
}

read_projected_design <- function(df, method) {
  method_req <- as.character(method)
  sub <- df[df$method == method_req, , drop = FALSE]
  if (nrow(sub) == 0L) {
    old <- phase2_method_internal_alias(method_req)
    sub <- df[df$method == old, , drop = FALSE]
  }
  if (nrow(sub) == 0L) stop("Cannot find projected design for method: ", method_req, call. = FALSE)
  sub <- sub[order(sub$look), , drop = FALSE]
  design_type <- if ("design_type" %in% names(sub)) as.character(sub$design_type[1L]) else "component_count"
  if (!is.na(design_type) && identical(design_type, "joint_lr_projected")) {
    theta0 <- as.numeric(unlist(sub[1L, paste0("theta0_", 1:4)], use.names = FALSE))
    theta1 <- as.numeric(unlist(sub[1L, paste0("theta1_", 1:4)], use.names = FALSE))
    B <- matrix(NA_integer_, nrow = 2L, ncol = 4L)
    for (r in 1:2) for (c in 1:4) B[r, c] <- as.integer(sub[[paste0("B", r, c)]][1L])
    looks <- as.character(sub$look)
    return(list(
      method = method_req,
      design_type = "joint_lr_projected",
      alpha_eff = as.numeric(sub$alpha_eff[1L]),
      beta = as.numeric(sub$beta[1L]),
      theta0 = theta0,
      theta1 = theta1,
      B = B,
      mode_fut = "joint_lr",
      mode_eff = "joint_lr",
      comp1_name = as.character(sub$comp1_name[1L]),
      comp2_name = as.character(sub$comp2_name[1L]),
      fut1_dir = NA_character_, fut2_dir = NA_character_, eff1_dir = NA_character_, eff2_dir = NA_character_,
      fut1 = setNames(rep(NA_integer_, nrow(sub)), looks),
      fut2 = setNames(rep(NA_integer_, nrow(sub)), looks),
      eff1 = setNames(rep(NA_integer_, nrow(sub)), looks),
      eff2 = setNames(rep(NA_integer_, nrow(sub)), looks),
      fut_joint_threshold = setNames(as.numeric(sub$fut_joint_threshold), looks),
      eff_joint_threshold = setNames(as.numeric(sub$eff_joint_threshold), looks),
      final_reject_if_not_futile = FALSE
    ))
  }
  list(
    method = method_req,
    mode_fut = as.character(sub$mode_fut[1L]),
    mode_eff = as.character(sub$mode_eff[1L]),
    comp1_name = as.character(sub$comp1_name[1L]),
    comp2_name = as.character(sub$comp2_name[1L]),
    fut1_dir = as.character(sub$fut1_dir[1L]),
    fut2_dir = as.character(sub$fut2_dir[1L]),
    eff1_dir = as.character(sub$eff1_dir[1L]),
    eff2_dir = as.character(sub$eff2_dir[1L]),
    fut1 = setNames(as.integer(sub$fut1_bound), as.character(sub$look)),
    fut2 = setNames(as.integer(sub$fut2_bound), as.character(sub$look)),
    eff1 = setNames(as.integer(sub$eff1_bound), as.character(sub$look)),
    eff2 = setNames(as.integer(sub$eff2_bound), as.character(sub$look)),
    final_reject_if_not_futile = as.logical(sub$final_reject_if_not_futile[1L])
  )
}


# ============================================================
# Mixture e-process extensions for CEVAM
# ============================================================
# These functions intentionally appear at the end of the file so that they
# extend the previously defined CEVAM functions without disrupting the main
# BOP2/BOP2-FE comparator code.  The mixture version is optional and can be
# included by calling build_all_boundaries(..., include_mixture = TRUE).

phase2_mix_normalize_weights <- function(w, m) {
  if (is.null(w) || length(w) == 0L) w <- rep(1 / m, m)
  w <- as.numeric(w)
  if (length(w) != m) stop("Mixture weights must have the same length as the mixture grid.", call. = FALSE)
  if (any(!is.finite(w)) || any(w < 0) || sum(w) <= 0) stop("Invalid mixture weights.", call. = FALSE)
  w / sum(w)
}

phase2_default_mix_q_eff <- function(p0, p1, fractions = c(0.33, 0.67, 1.00)) {
  delta <- p1 - p0
  q <- p0 + fractions * delta
  q <- sort(unique(pmin(pmax(q, p0 + 1e-8), 1 - 1e-8)))
  q[q > p0 & q < 1]
}

phase2_default_mix_q_fut <- function(p0, p1, fractions = c(0.00, 0.33, 0.67)) {
  delta <- p1 - p0
  q <- p0 + fractions * delta
  q <- sort(unique(pmin(pmax(q, 1e-8), p1 - 1e-8)))
  q[q > 0 & q < p1]
}

binary_eff_boundary_e_mix <- function(looks, p0, q_eff, alpha, w_eff = NULL) {
  q_eff <- as.numeric(q_eff)
  q_eff <- q_eff[q_eff > p0 & q_eff < 1]
  if (length(q_eff) == 0L) stop("The efficacy mixture grid must contain values in (p0, 1).", call. = FALSE)
  w_eff <- phase2_mix_normalize_weights(w_eff, length(q_eff))
  out <- integer(length(looks))
  names(out) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    rr <- 0:n
    ee <- Reduce(`+`, lapply(seq_along(q_eff), function(m) {
      q <- q_eff[m]
      w_eff[m] * (q / p0)^rr * ((1 - q) / (1 - p0))^(n - rr)
    }))
    idx <- which(ee >= 1 / alpha)
    out[i] <- if (length(idx) == 0L) n + 1L else rr[idx[1L]]
  }
  out
}

binary_fut_boundary_e_mix <- function(looks, p1, q_fut, beta, w_fut = NULL) {
  q_fut <- as.numeric(q_fut)
  q_fut <- q_fut[q_fut > 0 & q_fut < p1]
  if (length(q_fut) == 0L) stop("The futility mixture grid must contain values in (0, p1).", call. = FALSE)
  w_fut <- phase2_mix_normalize_weights(w_fut, length(q_fut))
  out <- integer(length(looks))
  names(out) <- as.character(looks)
  for (i in seq_along(looks)) {
    n <- looks[i]
    rr <- 0:n
    ee <- Reduce(`+`, lapply(seq_along(q_fut), function(m) {
      q <- q_fut[m]
      w_fut[m] * (q / p1)^rr * ((1 - q) / (1 - p1))^(n - rr)
    }))
    idx <- which(ee >= 1 / beta)
    out[i] <- if (length(idx) == 0L) -1L else max(rr[idx])
  }
  out
}

build_binary_boundary_CEVAM_mix <- function(p0, p1, looks, alpha, beta,
                                            tuned = FALSE, alpha_eff = alpha,
                                            mix_q_eff = NULL, mix_w_eff = NULL,
                                            mix_q_fut = NULL, mix_w_fut = NULL) {
  if (is.null(mix_q_eff)) mix_q_eff <- phase2_default_mix_q_eff(p0, p1)
  if (is.null(mix_q_fut)) mix_q_fut <- phase2_default_mix_q_fut(p0, p1)
  mix_w_eff <- phase2_mix_normalize_weights(mix_w_eff, length(mix_q_eff))
  mix_w_fut <- phase2_mix_normalize_weights(mix_w_fut, length(mix_q_fut))
  list(
    method = if (tuned) "CEVAM-Mix-T" else "CEVAM-Mix-NT",
    design_type = "component_count_mixture",
    alpha = alpha,
    alpha_eff = alpha_eff,
    beta = beta,
    mix_q_eff = mix_q_eff,
    mix_w_eff = mix_w_eff,
    mix_q_fut = mix_q_fut,
    mix_w_fut = mix_w_fut,
    futility_le = binary_fut_boundary_e_mix(looks, p1, mix_q_fut, beta, mix_w_fut),
    efficacy_ge = binary_eff_boundary_e_mix(looks, p0, mix_q_eff, alpha_eff, mix_w_eff),
    final_reject_if_not_futile = FALSE
  )
}

build_projected_boundary_CEVAM_mix <- function(exspec, looks, alpha, beta,
                                               tuned = FALSE, alpha_eff = alpha,
                                               mix_w_eff = NULL, mix_w_fut = NULL) {
  if (exspec$example != "example4") {
    q1_eff <- phase2_default_mix_q_eff(exspec$phi0[1L], exspec$phi1[1L])
    q2_eff <- phase2_default_mix_q_eff(exspec$phi0[2L], exspec$phi1[2L])
    q1_fut <- phase2_default_mix_q_fut(exspec$phi0[1L], exspec$phi1[1L])
    q2_fut <- phase2_default_mix_q_fut(exspec$phi0[2L], exspec$phi1[2L])
    fut1 <- binary_fut_boundary_e_mix(looks, exspec$phi1[1L], q1_fut, beta, mix_w_fut)
    fut2 <- binary_fut_boundary_e_mix(looks, exspec$phi1[2L], q2_fut, beta, mix_w_fut)
    eff1 <- binary_eff_boundary_e_mix(looks, exspec$phi0[1L], q1_eff, alpha_eff, mix_w_eff)
    eff2 <- binary_eff_boundary_e_mix(looks, exspec$phi0[2L], q2_eff, alpha_eff, mix_w_eff)
    list(
      method = if (tuned) "CEVAM-Mix-T" else "CEVAM-Mix-NT",
      design_type = "component_count_mixture",
      alpha = alpha,
      alpha_eff = alpha_eff,
      beta = beta,
      mix_q_eff_1 = q1_eff, mix_q_eff_2 = q2_eff,
      mix_q_fut_1 = q1_fut, mix_q_fut_2 = q2_fut,
      mode_fut = exspec$mode_fut,
      mode_eff = exspec$mode_eff,
      comp1_name = exspec$comp1_name,
      comp2_name = exspec$comp2_name,
      fut1_dir = "le", fut2_dir = "le",
      eff1_dir = "ge", eff2_dir = "ge",
      fut1 = fut1, fut2 = fut2, eff1 = eff1, eff2 = eff2,
      final_reject_if_not_futile = FALSE
    )
  } else {
    safe0 <- 1 - exspec$phi0[2L]
    safe1 <- 1 - exspec$phi1[2L]
    q_resp_eff <- phase2_default_mix_q_eff(exspec$phi0[1L], exspec$phi1[1L])
    q_resp_fut <- phase2_default_mix_q_fut(exspec$phi0[1L], exspec$phi1[1L])
    q_safe_eff <- phase2_default_mix_q_eff(safe0, safe1)
    q_safe_fut <- phase2_default_mix_q_fut(safe0, safe1)
    fut_resp <- binary_fut_boundary_e_mix(looks, exspec$phi1[1L], q_resp_fut, beta, mix_w_fut)
    eff_resp <- binary_eff_boundary_e_mix(looks, exspec$phi0[1L], q_resp_eff, alpha_eff, mix_w_eff)
    fut_safe <- binary_fut_boundary_e_mix(looks, safe1, q_safe_fut, beta, mix_w_fut)
    eff_safe <- binary_eff_boundary_e_mix(looks, safe0, q_safe_eff, alpha_eff, mix_w_eff)
    tox_fut_ge <- setNames(looks - fut_safe, as.character(looks))
    tox_eff_le <- setNames(looks - eff_safe, as.character(looks))
    list(
      method = if (tuned) "CEVAM-Mix-T" else "CEVAM-Mix-NT",
      design_type = "component_count_mixture",
      alpha = alpha,
      alpha_eff = alpha_eff,
      beta = beta,
      mix_q_eff_1 = q_resp_eff, mix_q_eff_2 = q_safe_eff,
      mix_q_fut_1 = q_resp_fut, mix_q_fut_2 = q_safe_fut,
      mode_fut = exspec$mode_fut,
      mode_eff = exspec$mode_eff,
      comp1_name = exspec$comp1_name,
      comp2_name = exspec$comp2_name,
      fut1_dir = "le", fut2_dir = "ge",
      eff1_dir = "ge", eff2_dir = "le",
      fut1 = fut_resp, fut2 = tox_fut_ge, eff1 = eff_resp, eff2 = tox_eff_le,
      final_reject_if_not_futile = FALSE
    )
  }
}

calibrate_CEVAM_mix_binary_nt <- function(p0, p1, looks, N, alpha, beta_default, grids,
                                          n_cores = 1L, show_progress = TRUE,
                                          progress_every = 100L) {
  alpha_vals <- ebop_alpha_eff_values(grids)
  cands <- vector("list", length(alpha_vals))
  start_time <- Sys.time()
  for (i in seq_along(alpha_vals)) {
    des <- build_binary_boundary_CEVAM_mix(p0, p1, looks, alpha, beta_default,
                                           tuned = FALSE, alpha_eff = alpha_vals[i])
    cands[[i]] <- list(design = des,
                       oc_null = exact_oc_binary(p0, des, looks, N),
                       oc_alt = exact_oc_binary(p1, des, looks, N))
    if (isTRUE(show_progress) && (i == length(alpha_vals) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-Mix-NT",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("binary p0=%.2f p1=%.2f / CEVAM-Mix-NT alpha_eff", p0, p1),
                       i, length(alpha_vals), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("binary p0=%.2f p1=%.2f / CEVAM-Mix-NT", p0, p1),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_CEVAM_mix_binary_tuned <- function(p0, p1, looks, N, alpha, beta_default, grids,
                                             n_cores = 1L, show_progress = TRUE,
                                             progress_every = 100L) {
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_vals <- ebop_beta_values(grids)
  param_grid <- make_param_grid(alpha_eff = alpha_vals, beta = beta_vals)
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  for (i in seq_len(nrow(param_grid))) {
    des <- build_binary_boundary_CEVAM_mix(p0, p1, looks, alpha, param_grid$beta[i],
                                           tuned = TRUE, alpha_eff = param_grid$alpha_eff[i])
    cands[[i]] <- list(design = des,
                       oc_null = exact_oc_binary(p0, des, looks, N),
                       oc_alt = exact_oc_binary(p1, des, looks, N))
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-Mix-T",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("binary p0=%.2f p1=%.2f / CEVAM-Mix-T alpha_eff,beta", p0, p1),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("binary p0=%.2f p1=%.2f / CEVAM-Mix-T", p0, p1),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_CEVAM_mix_projected_nt <- function(exspec, looks, N, alpha, beta_default, grids,
                                             n_cores = 1L, show_progress = TRUE,
                                             progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  cands <- vector("list", length(alpha_vals))
  start_time <- Sys.time()
  for (i in seq_along(alpha_vals)) {
    des <- build_projected_boundary_CEVAM_mix(exspec, looks, alpha, beta_default,
                                              tuned = FALSE, alpha_eff = alpha_vals[i])
    cands[[i]] <- list(design = des,
                       oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
                       oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N))
    if (isTRUE(show_progress) && (i == length(alpha_vals) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-Mix-NT",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-Mix-NT alpha_eff", exspec$example),
                       i, length(alpha_vals), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-Mix-NT", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_CEVAM_mix_projected_tuned <- function(exspec, looks, N, alpha, beta_default, grids,
                                                n_cores = 1L, show_progress = TRUE,
                                                progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_vals <- ebop_beta_values(grids)
  param_grid <- make_param_grid(alpha_eff = alpha_vals, beta = beta_vals)
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  for (i in seq_len(nrow(param_grid))) {
    des <- build_projected_boundary_CEVAM_mix(exspec, looks, alpha, param_grid$beta[i],
                                              tuned = TRUE, alpha_eff = param_grid$alpha_eff[i])
    cands[[i]] <- list(design = des,
                       oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
                       oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N))
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-Mix-T",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-Mix-T alpha_eff,beta", exspec$example),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-Mix-T", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

.designs_example1 <- function(entry) {
  Filter(Negate(is.null), list(entry$bop2f, entry$bop2fe1, entry$bop2fe2,
                               entry$ebop2nt, entry$ebop2t,
                               entry$CEVAM_mix_nt, entry$CEVAM_mix_t))
}

.designs_projected <- function(ex_list) {
  Filter(Negate(is.null), list(ex_list$bop2f, ex_list$bop2fe1, ex_list$bop2fe2,
                               ex_list$ebop2nt, ex_list$ebop2t,
                               ex_list$CEVAM_mix_nt, ex_list$CEVAM_mix_t))
}

sheet_example1_df <- function(boundaries, looks) {
  rows <- list(); i <- 0L
  for (nm in names(boundaries$example1)) {
    entry <- boundaries$example1[[nm]]
    for (des in .designs_example1(entry)) {
      for (n in looks) {
        i <- i + 1L
        rows[[i]] <- data.frame(
          example = "example1",
          pair_id = entry$pair_id,
          method = des$method,
          look = n,
          comp1_name = "ORR",
          futility_direction = "le",
          futility_bound = des$futility_le[as.character(n)],
          efficacy_direction = ifelse(all(is.na(des$efficacy_ge)), NA, "ge"),
          efficacy_bound = des$efficacy_ge[as.character(n)],
          final_reject_if_not_futile = des$final_reject_if_not_futile,
          design_type = ifelse(is.null(des$design_type), "component_count", as.character(des$design_type)),
          alpha_eff = ifelse(is.null(des$alpha_eff), NA_real_, as.numeric(des$alpha_eff)),
          beta = ifelse(is.null(des$beta), NA_real_, as.numeric(des$beta)),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

build_all_boundaries <- function(settings = paper_settings(),
                                 grids = default_search_grids(),
                                 n_cores = 1L,
                                 show_progress = TRUE,
                                 progress_every = 100L,
                                 include_mixture = FALSE) {
  looks <- settings$looks
  N <- settings$N
  alpha <- settings$alpha
  beta <- settings$beta
  n_cores <- resolve_n_cores(n_cores)

  if (isTRUE(show_progress)) {
    message("Starting boundary calibration")
    message("  search_mode      = ", ifelse(is.null(grids$search_mode), "paper", grids$search_mode))
    message("  n_cores          = ", n_cores)
    message("  include_mixture  = ", include_mixture)
    message("  CEVAM methods use componentwise e-value boundaries with OR/AND decision rules")
  }

  ex1 <- lapply(settings$ex1_pairs, function(pair) {
    out <- list(
      pair_id = pair$pair_id,
      p0 = pair$p0,
      p1 = pair$p1,
      bop2f   = get_bop2f_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_binary_design(pair, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                           n_cores = n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
      ebop2t  = calibrate_ebop2_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                             n_cores = n_cores, show_progress = show_progress,
                                             progress_every = progress_every)
    )
    if (isTRUE(include_mixture)) {
      out$CEVAM_mix_nt <- calibrate_CEVAM_mix_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                                        n_cores = n_cores, show_progress = show_progress,
                                                        progress_every = progress_every)
      out$CEVAM_mix_t <- calibrate_CEVAM_mix_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                                          n_cores = n_cores, show_progress = show_progress,
                                                          progress_every = progress_every)
    }
    out
  })
  names(ex1) <- paste0("pair", vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)))

  make_ex <- function(exspec) {
    out <- list(
      bop2f   = get_bop2f_projected_design(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_projected_design(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_projected_design(exspec, looks, N, alpha, grids, n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_projected_nt(exspec, looks, N, alpha, beta, grids,
                                             n_cores = n_cores, show_progress = show_progress,
                                             progress_every = progress_every),
      ebop2t  = calibrate_ebop2_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                n_cores = n_cores, show_progress = show_progress,
                                                progress_every = progress_every)
    )
    if (isTRUE(include_mixture)) {
      out$CEVAM_mix_nt <- calibrate_CEVAM_mix_projected_nt(exspec, looks, N, alpha, beta, grids,
                                                           n_cores = n_cores, show_progress = show_progress,
                                                           progress_every = progress_every)
      out$CEVAM_mix_t <- calibrate_CEVAM_mix_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                             n_cores = n_cores, show_progress = show_progress,
                                                             progress_every = progress_every)
    }
    out
  }

  ex2 <- make_ex(settings$ex2)
  ex3 <- make_ex(settings$ex3)
  ex4 <- make_ex(settings$ex4)

  list(settings = settings, grids = grids, example1 = ex1, example2 = ex2, example3 = ex3, example4 = ex4)
}

phase2_method_internal_alias <- function(method) {
  method <- as.character(method)
  if (method == "CEVAM-NT") return("E-BOP2-NT")
  if (method == "CEVAM-T") return("E-BOP2-T")
  if (method == "CEVAM-Mix-NT") return("E-BOP2-Mix-NT")
  if (method == "CEVAM-Mix-T") return("E-BOP2-Mix-T")
  method
}

phase2_method_display <- function(method) {
  method <- as.character(method)
  method <- gsub("E-BOP2", "CEVAM", method, fixed = TRUE)
  method
}

# ============================================================
# Example-level parallel boundary calibration override
# ============================================================
# This override preserves the same exact boundary/OC calculations, but can run
# Example 1 pair blocks and Examples 2--4 concurrently.  When outer
# example-level parallelism is used, inner grid-search parallelism is set to
# one core by default to avoid oversubscribing the machine.

.phase2_can_fork_parallel <- function() {
  .Platform$OS.type != "windows" && requireNamespace("parallel", quietly = TRUE)
}

.phase2_example_parallel_lapply <- function(jobs, FUN, n_example_cores = 1L,
                                            parallel_examples = FALSE,
                                            show_progress = TRUE) {
  n_jobs <- length(jobs)
  if (n_jobs == 0L) return(list())
  n_example_cores <- max(1L, min(as.integer(n_example_cores), n_jobs))

  if (isTRUE(parallel_examples) && n_example_cores > 1L && .phase2_can_fork_parallel()) {
    if (isTRUE(show_progress)) {
      message("Running example-level parallel calibration with ", n_example_cores,
              " workers for ", n_jobs, " example jobs.")
    }
    return(parallel::mclapply(jobs, FUN, mc.cores = n_example_cores,
                              mc.preschedule = FALSE))
  }

  if (isTRUE(parallel_examples) && .Platform$OS.type == "windows" && isTRUE(show_progress)) {
    message("Example-level parallelism uses fork-based parallelism and is disabled on Windows; running examples sequentially.")
  }
  lapply(jobs, FUN)
}

build_all_boundaries <- function(settings = paper_settings(),
                                 grids = default_search_grids(),
                                 n_cores = 1L,
                                 show_progress = TRUE,
                                 progress_every = 100L,
                                 include_mixture = FALSE,
                                 parallel_examples = FALSE,
                                 n_example_cores = NULL,
                                 inner_n_cores = NULL) {
  looks <- settings$looks
  N <- settings$N
  alpha <- settings$alpha
  beta <- settings$beta
  n_cores <- resolve_n_cores(n_cores)

  # If the examples are run in parallel, do not also parallelize the inner grid
  # search by default.  This prevents using n_example_cores * n_cores processes.
  if (is.null(inner_n_cores)) {
    inner_n_cores <- if (isTRUE(parallel_examples)) 1L else n_cores
  }
  inner_n_cores <- resolve_n_cores(inner_n_cores)

  n_example_jobs <- length(settings$ex1_pairs) + 3L
  if (is.null(n_example_cores)) {
    n_example_cores <- if (isTRUE(parallel_examples)) min(n_cores, n_example_jobs) else 1L
  }
  n_example_cores <- max(1L, min(resolve_n_cores(n_example_cores), n_example_jobs))

  if (isTRUE(show_progress)) {
    message("Starting boundary calibration")
    message("  search_mode       = ", ifelse(is.null(grids$search_mode), "paper", grids$search_mode))
    message("  total n_cores     = ", n_cores)
    message("  parallel_examples = ", parallel_examples)
    message("  n_example_cores   = ", n_example_cores)
    message("  inner_n_cores     = ", inner_n_cores)
    message("  include_mixture   = ", include_mixture)
    message("  CEVAM methods use componentwise e-value boundaries with OR/AND decision rules")
  }

  build_ex1_pair <- function(pair) {
    if (isTRUE(show_progress)) message("[example job] starting example1 pair ", pair$pair_id)
    out <- list(
      pair_id = pair$pair_id,
      p0 = pair$p0,
      p1 = pair$p1,
      bop2f   = get_bop2f_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                           n_cores = inner_n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
      ebop2t  = calibrate_ebop2_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                             n_cores = inner_n_cores, show_progress = show_progress,
                                             progress_every = progress_every)
    )
    if (isTRUE(include_mixture)) {
      out$CEVAM_mix_nt <- calibrate_CEVAM_mix_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                                        n_cores = inner_n_cores, show_progress = show_progress,
                                                        progress_every = progress_every)
      out$CEVAM_mix_t <- calibrate_CEVAM_mix_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                                          n_cores = inner_n_cores, show_progress = show_progress,
                                                          progress_every = progress_every)
    }
    if (isTRUE(show_progress)) message("[example job] finished example1 pair ", pair$pair_id)
    out
  }

  build_projected_ex <- function(exspec) {
    if (isTRUE(show_progress)) message("[example job] starting ", exspec$example)
    out <- list(
      bop2f   = get_bop2f_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_projected_nt(exspec, looks, N, alpha, beta, grids,
                                             n_cores = inner_n_cores, show_progress = show_progress,
                                             progress_every = progress_every),
      ebop2t  = calibrate_ebop2_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                n_cores = inner_n_cores, show_progress = show_progress,
                                                progress_every = progress_every)
    )
    if (isTRUE(include_mixture)) {
      out$CEVAM_mix_nt <- calibrate_CEVAM_mix_projected_nt(exspec, looks, N, alpha, beta, grids,
                                                           n_cores = inner_n_cores, show_progress = show_progress,
                                                           progress_every = progress_every)
      out$CEVAM_mix_t <- calibrate_CEVAM_mix_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                             n_cores = inner_n_cores, show_progress = show_progress,
                                                             progress_every = progress_every)
    }
    if (isTRUE(show_progress)) message("[example job] finished ", exspec$example)
    out
  }

  jobs <- list()
  for (pair in settings$ex1_pairs) {
    jobs[[length(jobs) + 1L]] <- list(type = "example1", name = paste0("pair", pair$pair_id), pair = pair)
  }
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example2", exspec = settings$ex2)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example3", exspec = settings$ex3)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example4", exspec = settings$ex4)

  run_job <- function(job) {
    if (identical(job$type, "example1")) {
      return(list(type = job$type, name = job$name, value = build_ex1_pair(job$pair)))
    }
    list(type = job$type, name = job$name, value = build_projected_ex(job$exspec))
  }

  job_results <- .phase2_example_parallel_lapply(
    jobs, run_job,
    n_example_cores = n_example_cores,
    parallel_examples = parallel_examples,
    show_progress = show_progress
  )

  ex1 <- list()
  ex2 <- ex3 <- ex4 <- NULL
  for (res in job_results) {
    if (identical(res$type, "example1")) {
      ex1[[res$name]] <- res$value
    } else if (identical(res$name, "example2")) {
      ex2 <- res$value
    } else if (identical(res$name, "example3")) {
      ex3 <- res$value
    } else if (identical(res$name, "example4")) {
      ex4 <- res$value
    }
  }

  # Preserve pair ordering from settings.
  ex1_names <- paste0("pair", vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)))
  ex1 <- ex1[ex1_names]

  list(settings = settings, grids = grids, example1 = ex1, example2 = ex2, example3 = ex3, example4 = ex4)
}

# ============================================================
# Biometrics revision extensions: component-specific futility
# budgets and pure joint likelihood-ratio CEVAM
# ============================================================
# These definitions are appended at the end of the file so that they override
# earlier incremental definitions while preserving the file name used by the
# user's workflow.

phase2_component_beta_values <- function(grids) {
  vals <- grids$ebop_beta_component
  if (is.null(vals) || length(vals) == 0L) vals <- grids$ebop_beta_cs
  if (is.null(vals) || length(vals) == 0L) vals <- ebop_beta_values(grids)
  vals <- sort(unique(as.numeric(vals)))
  vals[is.finite(vals) & vals > 0 & vals < 1]
}

phase2_make_beta_pair_grid <- function(grids) {
  vals1 <- grids$ebop_beta_component1
  vals2 <- grids$ebop_beta_component2
  if (is.null(vals1) || length(vals1) == 0L) vals1 <- phase2_component_beta_values(grids)
  if (is.null(vals2) || length(vals2) == 0L) vals2 <- phase2_component_beta_values(grids)
  vals1 <- sort(unique(as.numeric(vals1)))
  vals2 <- sort(unique(as.numeric(vals2)))
  vals1 <- vals1[is.finite(vals1) & vals1 > 0 & vals1 < 1]
  vals2 <- vals2[is.finite(vals2) & vals2 > 0 & vals2 < 1]
  if (length(vals1) == 0L || length(vals2) == 0L) stop("Empty component-specific beta grid.", call. = FALSE)
  make_param_grid(beta1 = vals1, beta2 = vals2)
}

choose_CEVAM_en_candidate <- function(cands, alpha, label = "CEVAM",
                                      type1_tolerance = 0.005,
                                      power_loss_tol = 0.03) {
  if (length(cands) == 0L) stop("No candidate found for ", label, call. = FALSE)
  type1_tolerance <- max(0, as.numeric(type1_tolerance))
  power_loss_tol <- max(0, as.numeric(power_loss_tol))

  keep <- vapply(cands, function(z) {
    !is.null(z) && !is.null(z$oc_null) && !is.null(z$oc_alt) &&
      is.finite(z$oc_null$reject) && z$oc_null$reject <= alpha + type1_tolerance + 1e-12
  }, logical(1L))
  cands <- cands[keep]
  if (length(cands) == 0L) stop("No candidate controlled type I error within tolerance for ", label, call. = FALSE)

  pow <- vapply(cands, function(z) z$oc_alt$reject, numeric(1L))
  max_pow <- max(pow)
  keep_pow <- pow + 1e-12 >= max_pow - power_loss_tol
  cands <- cands[keep_pow]
  if (length(cands) == 0L) stop("No candidate satisfied the power-retention rule for ", label, call. = FALSE)

  en0 <- vapply(cands, function(z) z$oc_null$EN, numeric(1L))
  mn_en <- min(en0)
  cands <- cands[abs(en0 - mn_en) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  pow <- vapply(cands, function(z) z$oc_alt$reject, numeric(1L))
  mx_pow <- max(pow)
  cands <- cands[abs(pow - mx_pow) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  type1 <- vapply(cands, function(z) z$oc_null$reject, numeric(1L))
  dist <- abs(type1 - alpha)
  cands <- cands[abs(dist - min(dist)) <= 1e-12]
  if (length(cands) == 1L) return(cands[[1L]])

  pet0 <- vapply(cands, function(z) z$oc_null$PET, numeric(1L))
  cands[[which.max(pet0)]]
}

build_projected_boundary_ebop2_component_specific <- function(exspec, looks, alpha,
                                                              beta_vec,
                                                              tuned = TRUE,
                                                              alpha_eff = alpha) {
  beta_vec <- as.numeric(beta_vec)
  if (length(beta_vec) == 1L) beta_vec <- rep(beta_vec, 2L)
  if (length(beta_vec) != 2L || any(!is.finite(beta_vec)) || any(beta_vec <= 0) || any(beta_vec >= 1)) {
    stop("beta_vec must contain two values in (0, 1).", call. = FALSE)
  }
  looks_chr <- as.character(looks)

  if (exspec$example != "example4") {
    fut1 <- binary_fut_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], beta_vec[1L])
    fut2 <- binary_fut_boundary_e(looks, exspec$phi0[2L], exspec$phi1[2L], beta_vec[2L])
    eff1 <- binary_eff_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], alpha_eff)
    eff2 <- binary_eff_boundary_e(looks, exspec$phi0[2L], exspec$phi1[2L], alpha_eff)
    list(
      method = "CEVAM-CS-T",
      design_type = "component_count_component_specific",
      alpha = alpha,
      alpha_eff = alpha_eff,
      beta = NA_real_,
      beta1 = beta_vec[1L],
      beta2 = beta_vec[2L],
      mode_fut = exspec$mode_fut,
      mode_eff = exspec$mode_eff,
      comp1_name = exspec$comp1_name,
      comp2_name = exspec$comp2_name,
      fut1_dir = "le", fut2_dir = "le",
      eff1_dir = "ge", eff2_dir = "ge",
      fut1 = setNames(as.integer(fut1), looks_chr),
      fut2 = setNames(as.integer(fut2), looks_chr),
      eff1 = setNames(as.integer(eff1), looks_chr),
      eff2 = setNames(as.integer(eff2), looks_chr),
      final_reject_if_not_futile = FALSE
    )
  } else {
    # Component 1 is response; component 2 is safety = 1 - toxicity.
    safe0 <- 1 - exspec$phi0[2L]
    safe1 <- 1 - exspec$phi1[2L]
    fut_resp <- binary_fut_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], beta_vec[1L])
    eff_resp <- binary_eff_boundary_e(looks, exspec$phi0[1L], exspec$phi1[1L], alpha_eff)
    fut_safe <- binary_fut_boundary_e(looks, safe0, safe1, beta_vec[2L])
    eff_safe <- binary_eff_boundary_e(looks, safe0, safe1, alpha_eff)
    tox_fut_ge <- setNames(looks - fut_safe, looks_chr)
    tox_eff_le <- setNames(looks - eff_safe, looks_chr)
    list(
      method = "CEVAM-CS-T",
      design_type = "component_count_component_specific",
      alpha = alpha,
      alpha_eff = alpha_eff,
      beta = NA_real_,
      beta1 = beta_vec[1L],
      beta2 = beta_vec[2L],
      mode_fut = exspec$mode_fut,
      mode_eff = exspec$mode_eff,
      comp1_name = exspec$comp1_name,
      comp2_name = exspec$comp2_name,
      fut1_dir = "le", fut2_dir = "ge",
      eff1_dir = "ge", eff2_dir = "le",
      fut1 = setNames(as.integer(fut_resp), looks_chr),
      fut2 = setNames(as.integer(tox_fut_ge), looks_chr),
      eff1 = setNames(as.integer(eff_resp), looks_chr),
      eff2 = setNames(as.integer(tox_eff_le), looks_chr),
      final_reject_if_not_futile = FALSE
    )
  }
}

calibrate_ebop2_projected_component_specific_tuned <- function(exspec, looks, N,
                                                               alpha, beta_default, grids,
                                                               n_cores = 1L,
                                                               show_progress = TRUE,
                                                               progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_grid <- phase2_make_beta_pair_grid(grids)
  param_grid <- cbind(
    alpha_eff = rep(alpha_vals, each = nrow(beta_grid)),
    beta_grid[rep(seq_len(nrow(beta_grid)), times = length(alpha_vals)), , drop = FALSE]
  )
  rownames(param_grid) <- NULL
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  progress_every <- max(1L, as.integer(progress_every))
  for (i in seq_len(nrow(param_grid))) {
    des <- build_projected_boundary_ebop2_component_specific(
      exspec = exspec,
      looks = looks,
      alpha = alpha,
      beta_vec = c(param_grid$beta1[i], param_grid$beta2[i]),
      tuned = TRUE,
      alpha_eff = param_grid$alpha_eff[i]
    )
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(
        choose_CEVAM_en_candidate(cands[seq_len(i)], alpha,
                                  label = "CEVAM-CS-T",
                                  type1_tolerance = ebop_type1_tolerance(grids),
                                  power_loss_tol = grids$power_loss_tol),
        error = function(e) NULL
      )
      progress_message(sprintf("%s / CEVAM-CS-T alpha_eff,beta1,beta2", exspec$example),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_CEVAM_en_candidate(cands, alpha,
                            label = sprintf("%s / CEVAM-CS-T", exspec$example),
                            type1_tolerance = ebop_type1_tolerance(grids),
                            power_loss_tol = grids$power_loss_tol)$design
}

# Pure joint likelihood-ratio CEVAM.  Unlike the count-summary JNT prototype,
# this design evaluates the bivariate LR region directly during simulation/OC.
build_projected_boundary_CEVAM_joint_lr <- function(exspec, looks, alpha, beta,
                                                    tuned = FALSE,
                                                    alpha_eff = alpha) {
  looks_chr <- as.character(looks)
  method <- if (tuned) "CEVAM-JLR-T" else "CEVAM-JLR"
  list(
    method = method,
    design_type = "joint_lr_projected",
    alpha = alpha,
    alpha_eff = alpha_eff,
    beta = beta,
    theta0 = as.numeric(exspec$theta_null),
    theta1 = as.numeric(exspec$theta_alt),
    B = exspec$B,
    mode_fut = "joint_lr",
    mode_eff = "joint_lr",
    comp1_name = exspec$comp1_name,
    comp2_name = exspec$comp2_name,
    fut1_dir = NA_character_, fut2_dir = NA_character_,
    eff1_dir = NA_character_, eff2_dir = NA_character_,
    fut1 = setNames(rep(NA_integer_, length(looks)), looks_chr),
    fut2 = setNames(rep(NA_integer_, length(looks)), looks_chr),
    eff1 = setNames(rep(NA_integer_, length(looks)), looks_chr),
    eff2 = setNames(rep(NA_integer_, length(looks)), looks_chr),
    fut_joint_threshold = setNames(rep(1 / beta, length(looks)), looks_chr),
    eff_joint_threshold = setNames(rep(1 / alpha_eff, length(looks)), looks_chr),
    final_reject_if_not_futile = FALSE
  )
}

projected_eval <- function(u, v, n, design, N) {
  key <- as.character(n)
  if (!is.null(design$design_type) && identical(as.character(design$design_type), "joint_lr_projected")) {
    lrs <- .joint_projected_lr_values(u, v, n, design)
    eff_thr <- if (!is.null(design$eff_joint_threshold) && key %in% names(design$eff_joint_threshold)) {
      as.numeric(design$eff_joint_threshold[key])
    } else {
      1 / as.numeric(design$alpha_eff)
    }
    fut_thr <- if (!is.null(design$fut_joint_threshold) && key %in% names(design$fut_joint_threshold)) {
      as.numeric(design$fut_joint_threshold[key])
    } else {
      1 / as.numeric(design$beta)
    }
    if ((is.finite(lrs["eff"]) && lrs["eff"] >= eff_thr) || is.infinite(lrs["eff"])) {
      return(list(status = "efficacy"))
    }
    if ((is.finite(lrs["fut"]) && lrs["fut"] >= fut_thr) || is.infinite(lrs["fut"])) {
      return(list(status = "futility"))
    }
    if (n == N) return(list(status = "no_reject"))
    return(list(status = "continue"))
  }

  eff1 <- hit_dir(u, design$eff1_dir, design$eff1[key])
  eff2 <- hit_dir(v, design$eff2_dir, design$eff2[key])
  fut1 <- hit_dir(u, design$fut1_dir, design$fut1[key])
  fut2 <- hit_dir(v, design$fut2_dir, design$fut2[key])

  eff_hit <- switch(design$mode_eff,
                    "or" = eff1 || eff2,
                    "and" = eff1 && eff2,
                    "none" = FALSE,
                    FALSE)
  fut_hit <- switch(design$mode_fut,
                    "and" = fut1 && fut2,
                    "or" = fut1 || fut2,
                    FALSE)
  if (eff_hit) return(list(status = "efficacy"))
  if (fut_hit) return(list(status = "futility"))
  if (n == N) {
    if (isTRUE(design$final_reject_if_not_futile)) return(list(status = "efficacy"))
    return(list(status = "no_reject"))
  }
  list(status = "continue")
}

calibrate_CEVAM_joint_lr_projected_nt <- function(exspec, looks, N, alpha, beta_default, grids,
                                                  n_cores = 1L, show_progress = TRUE,
                                                  progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  cands <- vector("list", length(alpha_vals))
  start_time <- Sys.time()
  for (i in seq_along(alpha_vals)) {
    des <- build_projected_boundary_CEVAM_joint_lr(exspec, looks, alpha, beta_default,
                                                   tuned = FALSE, alpha_eff = alpha_vals[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == length(alpha_vals) || i %% progress_every == 0L)) {
      best_now <- tryCatch(choose_ebop_calibrated_candidate(cands[seq_len(i)], alpha,
                                                            label = "CEVAM-JLR",
                                                            type1_tolerance = ebop_type1_tolerance(grids)),
                           error = function(e) NULL)
      progress_message(sprintf("%s / CEVAM-JLR alpha_eff", exspec$example),
                       i, length(alpha_vals), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_ebop_calibrated_candidate(cands, alpha,
                                   label = sprintf("%s / CEVAM-JLR", exspec$example),
                                   type1_tolerance = ebop_type1_tolerance(grids))$design
}

calibrate_CEVAM_joint_lr_projected_tuned <- function(exspec, looks, N, alpha, beta_default, grids,
                                                     n_cores = 1L, show_progress = TRUE,
                                                     progress_every = 100L) {
  cache <- make_projected_oc_cache(exspec, looks, N)
  alpha_vals <- ebop_alpha_eff_values(grids)
  beta_vals <- ebop_beta_values(grids)
  param_grid <- make_param_grid(alpha_eff = alpha_vals, beta = beta_vals)
  cands <- vector("list", nrow(param_grid))
  start_time <- Sys.time()
  progress_every <- max(1L, as.integer(progress_every))
  for (i in seq_len(nrow(param_grid))) {
    des <- build_projected_boundary_CEVAM_joint_lr(exspec, looks, alpha, param_grid$beta[i],
                                                   tuned = TRUE, alpha_eff = param_grid$alpha_eff[i])
    cands[[i]] <- list(
      design = des,
      oc_null = exact_oc_projected_from_delta(cache$null, des, looks, N),
      oc_alt = exact_oc_projected_from_delta(cache$alt, des, looks, N)
    )
    if (isTRUE(show_progress) && (i == nrow(param_grid) || i %% progress_every == 0L)) {
      best_now <- tryCatch(
        choose_CEVAM_en_candidate(cands[seq_len(i)], alpha,
                                  label = "CEVAM-JLR-T",
                                  type1_tolerance = ebop_type1_tolerance(grids),
                                  power_loss_tol = grids$power_loss_tol),
        error = function(e) NULL
      )
      progress_message(sprintf("%s / CEVAM-JLR-T alpha_eff,beta", exspec$example),
                       i, nrow(param_grid), start_time, best = best_now, stage = "calibrate")
    }
  }
  choose_CEVAM_en_candidate(cands, alpha,
                            label = sprintf("%s / CEVAM-JLR-T", exspec$example),
                            type1_tolerance = ebop_type1_tolerance(grids),
                            power_loss_tol = grids$power_loss_tol)$design
}

.phase2_joint_method_names <- function() c("CEVAM-JLR", "CEVAM-JLR-T")

phase2_method_internal_alias <- function(method) {
  method <- as.character(method)
  if (method == "CEVAM-NT") return("E-BOP2-NT")
  if (method == "CEVAM-T") return("E-BOP2-T")
  if (method == "CEVAM-Mix-NT") return("E-BOP2-Mix-NT")
  if (method == "CEVAM-Mix-T") return("E-BOP2-Mix-T")
  method
}

phase2_method_display <- function(method) {
  method <- as.character(method)
  method <- gsub("E-BOP2", "CEVAM", method, fixed = TRUE)
  method
}

.designs_projected <- function(ex_list) {
  Filter(Negate(is.null), list(ex_list$bop2f, ex_list$bop2fe1, ex_list$bop2fe2,
                               ex_list$ebop2nt, ex_list$ebop2t,
                               ex_list$CEVAM_cs_t,
                               ex_list$CEVAM_jlr_nt, ex_list$CEVAM_jlr_t,
                               ex_list$CEVAM_mix_nt, ex_list$CEVAM_mix_t))
}

read_projected_design <- function(df, method) {
  method_req <- as.character(method)
  sub <- df[df$method == method_req, , drop = FALSE]
  if (nrow(sub) == 0L) {
    old <- phase2_method_internal_alias(method_req)
    sub <- df[df$method == old, , drop = FALSE]
  }
  if (nrow(sub) == 0L) stop("Cannot find projected design for method: ", method_req, call. = FALSE)
  sub <- sub[order(sub$look), , drop = FALSE]
  design_type <- if ("design_type" %in% names(sub)) as.character(sub$design_type[1L]) else "component_count"
  if (is.na(design_type) || !nzchar(design_type)) design_type <- "component_count"
  if (identical(design_type, "joint_lr_projected")) {
    theta0 <- as.numeric(unlist(sub[1L, paste0("theta0_", 1:4)], use.names = FALSE))
    theta1 <- as.numeric(unlist(sub[1L, paste0("theta1_", 1:4)], use.names = FALSE))
    B <- matrix(NA_integer_, nrow = 2L, ncol = 4L)
    for (r in 1:2) for (cc in 1:4) B[r, cc] <- as.integer(sub[[paste0("B", r, cc)]][1L])
    looks <- as.character(sub$look)
    return(list(
      method = method_req,
      design_type = "joint_lr_projected",
      alpha_eff = as.numeric(sub$alpha_eff[1L]),
      beta = as.numeric(sub$beta[1L]),
      theta0 = theta0,
      theta1 = theta1,
      B = B,
      mode_fut = "joint_lr",
      mode_eff = "joint_lr",
      comp1_name = as.character(sub$comp1_name[1L]),
      comp2_name = as.character(sub$comp2_name[1L]),
      fut1_dir = NA_character_, fut2_dir = NA_character_,
      eff1_dir = NA_character_, eff2_dir = NA_character_,
      fut1 = setNames(rep(NA_integer_, nrow(sub)), looks),
      fut2 = setNames(rep(NA_integer_, nrow(sub)), looks),
      eff1 = setNames(rep(NA_integer_, nrow(sub)), looks),
      eff2 = setNames(rep(NA_integer_, nrow(sub)), looks),
      fut_joint_threshold = setNames(as.numeric(sub$fut_joint_threshold), looks),
      eff_joint_threshold = setNames(as.numeric(sub$eff_joint_threshold), looks),
      final_reject_if_not_futile = FALSE
    ))
  }
  list(
    method = method_req,
    design_type = design_type,
    mode_fut = as.character(sub$mode_fut[1L]),
    mode_eff = as.character(sub$mode_eff[1L]),
    comp1_name = as.character(sub$comp1_name[1L]),
    comp2_name = as.character(sub$comp2_name[1L]),
    fut1_dir = as.character(sub$fut1_dir[1L]),
    fut2_dir = as.character(sub$fut2_dir[1L]),
    eff1_dir = as.character(sub$eff1_dir[1L]),
    eff2_dir = as.character(sub$eff2_dir[1L]),
    fut1 = setNames(as.integer(sub$fut1_bound), as.character(sub$look)),
    fut2 = setNames(as.integer(sub$fut2_bound), as.character(sub$look)),
    eff1 = setNames(as.integer(sub$eff1_bound), as.character(sub$look)),
    eff2 = setNames(as.integer(sub$eff2_bound), as.character(sub$look)),
    final_reject_if_not_futile = as.logical(sub$final_reject_if_not_futile[1L])
  )
}

build_all_boundaries <- function(settings = paper_settings(),
                                 grids = default_search_grids(),
                                 n_cores = 1L,
                                 show_progress = TRUE,
                                 progress_every = 100L,
                                 include_mixture = FALSE,
                                 include_component_specific = TRUE,
                                 include_joint_lr = TRUE,
                                 parallel_examples = FALSE,
                                 n_example_cores = NULL,
                                 inner_n_cores = NULL) {
  looks <- settings$looks
  N <- settings$N
  alpha <- settings$alpha
  beta <- settings$beta
  n_cores <- resolve_n_cores(n_cores)

  if (is.null(inner_n_cores)) {
    inner_n_cores <- if (isTRUE(parallel_examples)) 1L else n_cores
  }
  inner_n_cores <- resolve_n_cores(inner_n_cores)

  n_example_jobs <- length(settings$ex1_pairs) + 3L
  if (is.null(n_example_cores)) {
    n_example_cores <- if (isTRUE(parallel_examples)) min(n_cores, n_example_jobs) else 1L
  }
  n_example_cores <- max(1L, min(resolve_n_cores(n_example_cores), n_example_jobs))

  if (isTRUE(show_progress)) {
    message("Starting boundary calibration")
    message("  search_mode                = ", ifelse(is.null(grids$search_mode), "paper", grids$search_mode))
    message("  total n_cores              = ", n_cores)
    message("  parallel_examples          = ", parallel_examples)
    message("  n_example_cores            = ", n_example_cores)
    message("  inner_n_cores              = ", inner_n_cores)
    message("  include_mixture            = ", include_mixture)
    message("  include_component_specific = ", include_component_specific)
    message("  include_joint_lr           = ", include_joint_lr)
  }

  build_ex1_pair <- function(pair) {
    if (isTRUE(show_progress)) message("[example job] starting example1 pair ", pair$pair_id)
    out <- list(
      pair_id = pair$pair_id,
      p0 = pair$p0,
      p1 = pair$p1,
      bop2f   = get_bop2f_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                           n_cores = inner_n_cores, show_progress = show_progress,
                                           progress_every = progress_every),
      ebop2t  = calibrate_ebop2_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                             n_cores = inner_n_cores, show_progress = show_progress,
                                             progress_every = progress_every)
    )
    if (isTRUE(include_mixture)) {
      out$CEVAM_mix_nt <- calibrate_CEVAM_mix_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                                        n_cores = inner_n_cores, show_progress = show_progress,
                                                        progress_every = progress_every)
      out$CEVAM_mix_t <- calibrate_CEVAM_mix_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                                          n_cores = inner_n_cores, show_progress = show_progress,
                                                          progress_every = progress_every)
    }
    if (isTRUE(show_progress)) message("[example job] finished example1 pair ", pair$pair_id)
    out
  }

  build_projected_ex <- function(exspec) {
    if (isTRUE(show_progress)) message("[example job] starting ", exspec$example)
    out <- list(
      bop2f   = get_bop2f_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe1 = get_bop2fe1_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      bop2fe2 = get_bop2fe2_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every),
      ebop2nt = calibrate_ebop2_projected_nt(exspec, looks, N, alpha, beta, grids,
                                             n_cores = inner_n_cores, show_progress = show_progress,
                                             progress_every = progress_every),
      ebop2t  = calibrate_ebop2_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                n_cores = inner_n_cores, show_progress = show_progress,
                                                progress_every = progress_every)
    )
    if (isTRUE(include_component_specific)) {
      out$CEVAM_cs_t <- calibrate_ebop2_projected_component_specific_tuned(
        exspec, looks, N, alpha, beta, grids,
        n_cores = inner_n_cores, show_progress = show_progress,
        progress_every = progress_every
      )
    }
    if (isTRUE(include_joint_lr)) {
      out$CEVAM_jlr_nt <- calibrate_CEVAM_joint_lr_projected_nt(
        exspec, looks, N, alpha, beta, grids,
        n_cores = inner_n_cores, show_progress = show_progress,
        progress_every = progress_every
      )
      out$CEVAM_jlr_t <- calibrate_CEVAM_joint_lr_projected_tuned(
        exspec, looks, N, alpha, beta, grids,
        n_cores = inner_n_cores, show_progress = show_progress,
        progress_every = progress_every
      )
    }
    if (isTRUE(include_mixture)) {
      out$CEVAM_mix_nt <- calibrate_CEVAM_mix_projected_nt(exspec, looks, N, alpha, beta, grids,
                                                           n_cores = inner_n_cores, show_progress = show_progress,
                                                           progress_every = progress_every)
      out$CEVAM_mix_t <- calibrate_CEVAM_mix_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                                             n_cores = inner_n_cores, show_progress = show_progress,
                                                             progress_every = progress_every)
    }
    if (isTRUE(show_progress)) message("[example job] finished ", exspec$example)
    out
  }

  jobs <- list()
  for (pair in settings$ex1_pairs) {
    jobs[[length(jobs) + 1L]] <- list(type = "example1", name = paste0("pair", pair$pair_id), pair = pair)
  }
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example2", exspec = settings$ex2)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example3", exspec = settings$ex3)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example4", exspec = settings$ex4)

  run_job <- function(job) {
    if (identical(job$type, "example1")) {
      return(list(type = job$type, name = job$name, value = build_ex1_pair(job$pair)))
    }
    list(type = job$type, name = job$name, value = build_projected_ex(job$exspec))
  }

  job_results <- .phase2_example_parallel_lapply(
    jobs, run_job,
    n_example_cores = n_example_cores,
    parallel_examples = parallel_examples,
    show_progress = show_progress
  )

  ex1 <- list()
  ex2 <- ex3 <- ex4 <- NULL
  for (res in job_results) {
    if (identical(res$type, "example1")) {
      ex1[[res$name]] <- res$value
    } else if (identical(res$name, "example2")) {
      ex2 <- res$value
    } else if (identical(res$name, "example3")) {
      ex3 <- res$value
    } else if (identical(res$name, "example4")) {
      ex4 <- res$value
    }
  }
  ex1_names <- paste0("pair", vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)))
  ex1 <- ex1[ex1_names]
  list(settings = settings, grids = grids, example1 = ex1, example2 = ex2, example3 = ex3, example4 = ex4)
}



# ============================================================
# GitHub-friendly method selection override
# ============================================================
# The original paper code can build all comparators.  For public use, it is
# often useful to build only selected methods, especially for complex endpoint
# cases where joint likelihood-ratio variants can be slow.  The following
# helper functions override build_all_boundaries() and support method filters.

.phase2_normalize_methods <- function(methods, available) {
  if (is.null(methods) || any(tolower(methods) == "all")) {
    return(available)
  }
  if (length(methods) == 0L || any(tolower(methods) == "none")) {
    return(character(0L))
  }
  methods <- unique(as.character(methods))
  bad <- setdiff(methods, available)
  if (length(bad) > 0L) {
    stop("Unknown method(s): ", paste(bad, collapse = ", "),
         "\nAvailable methods: ", paste(available, collapse = ", "), call. = FALSE)
  }
  methods
}

.phase2_add_design_if_requested <- function(out, key, method, methods, expr) {
  if (method %in% methods) out[[key]] <- force(expr)
  out
}

build_all_boundaries <- function(settings = paper_settings(),
                                 grids = default_search_grids(),
                                 n_cores = 1L,
                                 show_progress = TRUE,
                                 progress_every = 100L,
                                 methods_binary = c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T"),
                                 methods_projected = c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T"),
                                 include_mixture = FALSE,
                                 include_component_specific = FALSE,
                                 include_joint_lr = FALSE,
                                 parallel_examples = FALSE,
                                 n_example_cores = NULL,
                                 inner_n_cores = NULL) {
  looks <- settings$looks
  N <- settings$N
  alpha <- settings$alpha
  beta <- settings$beta
  n_cores <- resolve_n_cores(n_cores)

  available_binary <- c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T",
                        "CEVAM-Mix-NT", "CEVAM-Mix-T")
  available_projected <- c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T",
                           "CEVAM-CS-T", "CEVAM-JLR", "CEVAM-JLR-T",
                           "CEVAM-Mix-NT", "CEVAM-Mix-T")
  methods_binary <- .phase2_normalize_methods(methods_binary, available_binary)
  methods_projected <- .phase2_normalize_methods(methods_projected, available_projected)

  # Backward-compatible switches.  These add optional methods if requested.
  if (isTRUE(include_mixture)) {
    methods_binary <- union(methods_binary, c("CEVAM-Mix-NT", "CEVAM-Mix-T"))
    methods_projected <- union(methods_projected, c("CEVAM-Mix-NT", "CEVAM-Mix-T"))
  }
  if (isTRUE(include_component_specific)) {
    methods_projected <- union(methods_projected, "CEVAM-CS-T")
  }
  if (isTRUE(include_joint_lr)) {
    methods_projected <- union(methods_projected, c("CEVAM-JLR", "CEVAM-JLR-T"))
  }

  if (is.null(inner_n_cores)) inner_n_cores <- if (isTRUE(parallel_examples)) 1L else n_cores
  inner_n_cores <- resolve_n_cores(inner_n_cores)

  n_example_jobs <- length(settings$ex1_pairs) + 3L
  if (is.null(n_example_cores)) n_example_cores <- if (isTRUE(parallel_examples)) min(n_cores, n_example_jobs) else 1L
  n_example_cores <- max(1L, min(resolve_n_cores(n_example_cores), n_example_jobs))

  if (isTRUE(show_progress)) {
    message("Starting boundary calibration")
    message("  search_mode        = ", ifelse(is.null(grids$search_mode), "paper", grids$search_mode))
    message("  methods_binary     = ", paste(methods_binary, collapse = ", "))
    message("  methods_projected  = ", paste(methods_projected, collapse = ", "))
    message("  total n_cores      = ", n_cores)
    message("  parallel_examples  = ", parallel_examples)
    message("  n_example_cores    = ", n_example_cores)
    message("  inner_n_cores      = ", inner_n_cores)
  }

  build_ex1_pair <- function(pair) {
    if (isTRUE(show_progress)) message("[example job] starting example1 pair ", pair$pair_id)
    out <- list(pair_id = pair$pair_id, p0 = pair$p0, p1 = pair$p1)
    out <- .phase2_add_design_if_requested(out, "bop2f", "BOP2-F", methods_binary,
      get_bop2f_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every))
    out <- .phase2_add_design_if_requested(out, "bop2fe1", "BOP2-FE-1", methods_binary,
      get_bop2fe1_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every))
    out <- .phase2_add_design_if_requested(out, "bop2fe2", "BOP2-FE-2", methods_binary,
      get_bop2fe2_binary_design(pair, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every))
    out <- .phase2_add_design_if_requested(out, "ebop2nt", "CEVAM-NT", methods_binary,
      calibrate_ebop2_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                n_cores = inner_n_cores, show_progress = show_progress,
                                progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "ebop2t", "CEVAM-T", methods_binary,
      calibrate_ebop2_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                   n_cores = inner_n_cores, show_progress = show_progress,
                                   progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_mix_nt", "CEVAM-Mix-NT", methods_binary,
      calibrate_CEVAM_mix_binary_nt(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                    n_cores = inner_n_cores, show_progress = show_progress,
                                    progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_mix_t", "CEVAM-Mix-T", methods_binary,
      calibrate_CEVAM_mix_binary_tuned(pair$p0, pair$p1, looks, N, alpha, beta, grids,
                                       n_cores = inner_n_cores, show_progress = show_progress,
                                       progress_every = progress_every))
    if (isTRUE(show_progress)) message("[example job] finished example1 pair ", pair$pair_id)
    out
  }

  build_projected_ex <- function(exspec) {
    if (isTRUE(show_progress)) message("[example job] starting ", exspec$example)
    out <- list()
    out <- .phase2_add_design_if_requested(out, "bop2f", "BOP2-F", methods_projected,
      get_bop2f_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every))
    out <- .phase2_add_design_if_requested(out, "bop2fe1", "BOP2-FE-1", methods_projected,
      get_bop2fe1_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every))
    out <- .phase2_add_design_if_requested(out, "bop2fe2", "BOP2-FE-2", methods_projected,
      get_bop2fe2_projected_design(exspec, looks, N, alpha, grids, inner_n_cores, show_progress, progress_every))
    out <- .phase2_add_design_if_requested(out, "ebop2nt", "CEVAM-NT", methods_projected,
      calibrate_ebop2_projected_nt(exspec, looks, N, alpha, beta, grids,
                                   n_cores = inner_n_cores, show_progress = show_progress,
                                   progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "ebop2t", "CEVAM-T", methods_projected,
      calibrate_ebop2_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                      n_cores = inner_n_cores, show_progress = show_progress,
                                      progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_cs_t", "CEVAM-CS-T", methods_projected,
      calibrate_ebop2_projected_component_specific_tuned(exspec, looks, N, alpha, beta, grids,
                                                         n_cores = inner_n_cores, show_progress = show_progress,
                                                         progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_jlr_nt", "CEVAM-JLR", methods_projected,
      calibrate_CEVAM_joint_lr_projected_nt(exspec, looks, N, alpha, beta, grids,
                                            n_cores = inner_n_cores, show_progress = show_progress,
                                            progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_jlr_t", "CEVAM-JLR-T", methods_projected,
      calibrate_CEVAM_joint_lr_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                               n_cores = inner_n_cores, show_progress = show_progress,
                                               progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_mix_nt", "CEVAM-Mix-NT", methods_projected,
      calibrate_CEVAM_mix_projected_nt(exspec, looks, N, alpha, beta, grids,
                                       n_cores = inner_n_cores, show_progress = show_progress,
                                       progress_every = progress_every))
    out <- .phase2_add_design_if_requested(out, "CEVAM_mix_t", "CEVAM-Mix-T", methods_projected,
      calibrate_CEVAM_mix_projected_tuned(exspec, looks, N, alpha, beta, grids,
                                          n_cores = inner_n_cores, show_progress = show_progress,
                                          progress_every = progress_every))
    if (isTRUE(show_progress)) message("[example job] finished ", exspec$example)
    out
  }

  jobs <- list()
  for (pair in settings$ex1_pairs) jobs[[length(jobs) + 1L]] <- list(type = "example1", name = paste0("pair", pair$pair_id), pair = pair)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example2", exspec = settings$ex2)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example3", exspec = settings$ex3)
  jobs[[length(jobs) + 1L]] <- list(type = "projected", name = "example4", exspec = settings$ex4)

  run_job <- function(job) {
    if (identical(job$type, "example1")) return(list(type = job$type, name = job$name, value = build_ex1_pair(job$pair)))
    list(type = job$type, name = job$name, value = build_projected_ex(job$exspec))
  }

  job_results <- .phase2_example_parallel_lapply(jobs, run_job,
    n_example_cores = n_example_cores,
    parallel_examples = parallel_examples,
    show_progress = show_progress)

  ex1 <- list(); ex2 <- ex3 <- ex4 <- NULL
  for (res in job_results) {
    if (identical(res$type, "example1")) ex1[[res$name]] <- res$value
    else if (identical(res$name, "example2")) ex2 <- res$value
    else if (identical(res$name, "example3")) ex3 <- res$value
    else if (identical(res$name, "example4")) ex4 <- res$value
  }
  ex1_names <- paste0("pair", vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)))
  ex1 <- ex1[ex1_names]
  list(settings = settings, grids = grids, example1 = ex1, example2 = ex2, example3 = ex3, example4 = ex4)
}
