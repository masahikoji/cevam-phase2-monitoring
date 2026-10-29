# cevam_current_core.R
# Current analysis engine for the CEVAM Statistics in Medicine manuscript.
#
# Key principles implemented here:
#   * Main finite-horizon CEVAM uses constant interim log-likelihood-ratio
#     boundaries plus an independently calibrated terminal count cutoff.
#   * Design selection is exact (binomial recursion), not Monte Carlo.
#   * Main criterion: among designs with type I error <= alpha and power >= target,
#     minimize E_{p0}(T). No ad-hoc objective weight is used in the main analysis.
#   * BOP2-FE1 and BOP2-FE2 use Beta(1/2,1/2) throughout and are recalibrated
#     under the same type I error, power, and E_{p0}(T) criterion.
#   * Weighted EN criteria are available only for sensitivity analyses.
#
# No package is required for the core exact calculations.

`%||%` <- function(x, y) if (is.null(x)) y else x

cevam_check_looks <- function(looks) {
  looks <- as.integer(looks)
  if (length(looks) < 2L || any(diff(looks) <= 0L)) stop("looks must be strictly increasing")
  looks
}

# ---------- exact binary operating characteristics ----------
cevam_exact_oc <- function(p, design) {
  looks <- cevam_check_looks(design$looks)
  N <- tail(looks, 1L)
  lower <- as.integer(design$lower)
  upper <- as.integer(design$upper)
  terminal <- as.integer(design$terminal)
  if (length(lower) != length(looks) - 1L || length(upper) != length(looks) - 1L) {
    stop("interim lower/upper lengths must equal length(looks)-1")
  }

  alive <- 1
  prev_n <- 0L
  reject <- 0
  early_futility <- 0
  early_success <- 0
  terminal_nonreject <- 0
  en <- 0
  success_by_look <- numeric(length(looks))
  failure_by_look <- numeric(length(looks))
  reach <- numeric(length(looks))

  for (k in seq_along(looks)) {
    n <- looks[k]
    m <- n - prev_n
    reach[k] <- sum(alive)
    en <- en + m * sum(alive)

    inc <- convolve(alive, dbinom(0:m, m, p), type = "open")
    # R's convolve(type='open') reverses one argument; numerical check below
    # makes the intended ordinary convolution explicit if needed.
    inc2 <- numeric(length(alive) + m)
    for (r in seq_along(alive)) {
      if (alive[r] == 0) next
      idx <- (r - 1L) + (0:m) + 1L
      inc2[idx] <- inc2[idx] + alive[r] * dbinom(0:m, m, p)
    }
    inc <- inc2

    rr <- 0:n
    if (n == N) {
      s <- rr >= terminal
      success_by_look[k] <- sum(inc[s])
      failure_by_look[k] <- sum(inc[!s])
      reject <- reject + success_by_look[k]
      terminal_nonreject <- failure_by_look[k]
      alive <- numeric(0)
    } else {
      s <- rr >= upper[k]
      f <- rr <= lower[k]
      if (any(s & f)) stop("overlapping efficacy/futility regions")
      success_by_look[k] <- sum(inc[s])
      failure_by_look[k] <- sum(inc[f])
      reject <- reject + success_by_look[k]
      early_success <- early_success + success_by_look[k]
      early_futility <- early_futility + failure_by_look[k]
      inc[s | f] <- 0
      alive <- inc
    }
    prev_n <- n
  }

  mass <- reject + early_futility + terminal_nonreject
  if (abs(mass - 1) > 1e-9) stop(sprintf("probability mass check failed: %.12f", mass))
  list(
    rejection = reject,
    EN = en,
    early_futility = early_futility,
    early_success = early_success,
    terminal_nonrejection = terminal_nonreject,
    success_by_look = success_by_look,
    failure_by_look = failure_by_look,
    reach = reach
  )
}

# Return early rejection mass, early futility mass, EN, and incoming terminal distribution.
.cevam_incoming_terminal <- function(p, looks, lower, upper) {
  looks <- cevam_check_looks(looks)
  N <- tail(looks, 1L)
  alive <- 1
  prev_n <- 0L
  rej <- fut <- en <- 0
  for (k in seq_along(looks)) {
    n <- looks[k]
    m <- n - prev_n
    en <- en + m * sum(alive)
    inc <- numeric(length(alive) + m)
    pmf <- dbinom(0:m, m, p)
    for (r in seq_along(alive)) {
      if (alive[r] == 0) next
      idx <- (r - 1L) + (0:m) + 1L
      inc[idx] <- inc[idx] + alive[r] * pmf
    }
    if (n == N) return(list(rej = rej, fut = fut, EN = en, terminal_mass = inc))
    rr <- 0:n
    s <- rr >= upper[k]
    f <- rr <= lower[k]
    if (any(s & f)) return(NULL)
    rej <- rej + sum(inc[s])
    fut <- fut + sum(inc[f])
    inc[s | f] <- 0
    alive <- inc
    prev_n <- n
  }
  stop("internal recursion error")
}

.cevam_eval_free_terminal <- function(p0, p1, looks, lower, upper, alpha) {
  z0 <- .cevam_incoming_terminal(p0, looks, lower, upper)
  z1 <- .cevam_incoming_terminal(p1, looks, lower, upper)
  if (is.null(z0) || is.null(z1) || z0$rej > alpha + 1e-14) return(NULL)
  N <- tail(looks, 1L)
  tail0 <- tail1 <- 0
  c <- N + 1L
  for (r in N:0) {
    add0 <- z0$terminal_mass[r + 1L]
    if (z0$rej + tail0 + add0 > alpha + 1e-14) break
    tail0 <- tail0 + add0
    tail1 <- tail1 + z1$terminal_mass[r + 1L]
    c <- r
  }
  list(
    size = z0$rej + tail0,
    power = z1$rej + tail1,
    EN0 = z0$EN,
    EN1 = z1$EN,
    early_fut0 = z0$fut,
    early_fut1 = z1$fut,
    early_eff0 = z0$rej,
    early_eff1 = z1$rej,
    terminal = c
  )
}

.cevam_eval_fixed_terminal <- function(p0, p1, looks, lower, upper, terminal) {
  d <- list(looks = looks, lower = lower, upper = upper, terminal = terminal)
  z0 <- cevam_exact_oc(p0, d)
  z1 <- cevam_exact_oc(p1, d)
  list(size = z0$rejection, power = z1$rejection, EN0 = z0$EN, EN1 = z1$EN,
       early_fut0 = z0$early_futility, early_fut1 = z1$early_futility,
       early_eff0 = z0$early_success, early_eff1 = z1$early_success,
       terminal = terminal)
}

# ---------- CEVAM candidate family ----------
.cevam_lr_constants <- function(p0, p1) {
  a <- log(p1 * (1 - p0) / (p0 * (1 - p1)))
  b0 <- log((1 - p1) / (1 - p0))
  if (!(a > 0 && b0 < 0)) stop("require 0 < p0 < p1 < 1")
  c(a = a, b0 = b0)
}

.cevam_unique_rows <- function(mat) {
  if (is.null(dim(mat)) || nrow(mat) <= 1L) return(mat)
  key <- apply(mat, 1L, paste, collapse = ",")
  mat[!duplicated(key), , drop = FALSE]
}

cevam_lr_boundary_tables <- function(p0, p1, looks) {
  looks <- cevam_check_looks(looks)
  ns <- looks[-length(looks)]
  ab <- .cevam_lr_constants(p0, p1); a <- ab[["a"]]; b0 <- ab[["b0"]]
  vals <- sort(unique(unlist(lapply(ns, function(n) a * (0:n) + b0 * n))))
  pos <- vals[vals > 1e-10]
  neg <- vals[vals < -1e-10]

  # One representative threshold from every interval producing a distinct table,
  # including no-crossing extremes and zero.
  hgrid <- c(0)
  if (length(pos)) {
    pext <- c(0, pos, max(1, max(pos) + 1))
    hgrid <- unique(c(0, (pext[-length(pext)] + pext[-1L]) / 2, tail(pext, 1L)))
  } else hgrid <- c(0, 1)

  lgrid <- c(0)
  if (length(neg)) {
    nextv <- c(min(-1, min(neg) - 1), neg, 0)
    lgrid <- unique(c(head(nextv, 1L), (nextv[-length(nextv)] + nextv[-1L]) / 2, 0))
  } else lgrid <- c(-1, 0)

  upper_list <- lapply(hgrid, function(h) {
    u <- ceiling((h - b0 * ns) / a - 1e-11)
    pmin(pmax(as.integer(u), 0L), ns + 1L)
  })
  lower_list <- lapply(lgrid, function(l) {
    x <- floor((l - b0 * ns) / a + 1e-11)
    pmin(pmax(as.integer(x), -1L), ns)
  })

  U <- .cevam_unique_rows(do.call(rbind, upper_list))
  L <- .cevam_unique_rows(do.call(rbind, lower_list))
  out <- vector("list", nrow(U) * nrow(L)); k <- 0L
  for (iu in seq_len(nrow(U))) for (il in seq_len(nrow(L))) {
    if (all(L[il, ] < U[iu, ])) {
      k <- k + 1L
      out[[k]] <- list(lower = as.integer(L[il, ]), upper = as.integer(U[iu, ]))
    }
  }
  out[seq_len(k)]
}

# ---------- BOP2-FE candidates ----------
.cevam_post_gt <- function(p0, r, n, prior = c(.5, .5)) {
  1 - pbeta(p0, prior[1] + r, prior[2] + n - r)
}

.cevam_first_ge <- function(v, cutoff) {
  ix <- which(v >= cutoff)
  if (length(ix)) ix[1L] - 1L else length(v)
}

bop2fe_candidates <- function(p0, looks, method = c("BOP2-FE1", "BOP2-FE2"),
                              lambda_step = .01, shape_step = .1,
                              prior = c(.5, .5)) {
  method <- match.arg(method)
  looks <- cevam_check_looks(looks); N <- tail(looks, 1L); t <- looks / N
  posts <- lapply(looks, function(n) .cevam_post_gt(p0, 0:n, n, prior))
  lambdas <- seq(lambda_step, 1 - lambda_step, by = lambda_step)
  shapes <- seq(shape_step, 3, by = shape_step)
  records <- list(); seen <- new.env(parent = emptyenv()); idx <- 0L

  for (la in lambdas) {
    fut_set <- list(); fseen <- new.env(parent = emptyenv())
    for (ga in shapes) {
      th <- la * t^ga
      allb <- vapply(seq_along(looks), function(k) .cevam_first_ge(posts[[k]], th[k]), integer(1L))
      l <- allb[-length(allb)] - 1L
      key <- paste(l, collapse = ",")
      if (!exists(key, fseen, inherits = FALSE)) {
        assign(key, TRUE, fseen); fut_set[[length(fut_set)+1L]] <- list(lower=l, gamma=ga)
      }
    }

    eff_set <- list(); eseen <- new.env(parent = emptyenv())
    if (method == "BOP2-FE2") {
      cut <- 2 * pnorm(qnorm((1 + la)/2) / sqrt(t)) - 1
      allb <- vapply(seq_along(looks), function(k) .cevam_first_ge(posts[[k]], cut[k]), integer(1L))
      eff_set[[1L]] <- list(upper=allb[-length(allb)], terminal=tail(allb,1L), eta=NA_real_)
    } else {
      for (eta in shapes) {
        cut <- 1 - (1 - la) * t^eta
        allb <- vapply(seq_along(looks), function(k) .cevam_first_ge(posts[[k]], cut[k]), integer(1L))
        u <- allb[-length(allb)]; c <- tail(allb,1L)
        key <- paste(c(u,c), collapse=",")
        if (!exists(key, eseen, inherits=FALSE)) {
          assign(key, TRUE, eseen); eff_set[[length(eff_set)+1L]] <- list(upper=u, terminal=c, eta=eta)
        }
      }
    }

    for (ff in fut_set) for (ee in eff_set) {
      if (any(ff$lower >= ee$upper)) next
      key <- paste(c(ff$lower, ee$upper, ee$terminal), collapse=",")
      if (exists(key, seen, inherits=FALSE)) next
      assign(key, TRUE, seen); idx <- idx + 1L
      records[[idx]] <- list(lower=as.integer(ff$lower), upper=as.integer(ee$upper),
                             terminal=as.integer(ee$terminal),
                             pars=list(lambda=la, gamma=ff$gamma, eta=ee$eta, prior=prior))
    }
  }
  records
}

# ---------- common design selection ----------
.cevam_lex_less <- function(x,y,tol=1e-12) {
  if (is.null(y)) return(TRUE)
  for (j in seq_along(x)) {
    if (abs(x[j]-y[j]) <= tol) next
    return(x[j] < y[j])
  }
  FALSE
}

cevam_choose_design <- function(p0, p1, looks, alpha, target_power,
                                method = c("CEVAM", "BOP2-FE1", "BOP2-FE2"),
                                criterion = c("EN0", "weighted"), weight = 1,
                                lambda_step = .01, shape_step = .1,
                                prior = c(.5,.5), power_upper = NULL) {
  method <- match.arg(method); criterion <- match.arg(criterion); looks <- cevam_check_looks(looks)
  cand <- if (method == "CEVAM") cevam_lr_boundary_tables(p0,p1,looks) else
    bop2fe_candidates(p0, looks, method, lambda_step, shape_step, prior)
  best <- NULL; best_key <- NULL; nvalid <- 0L
  for (i in seq_along(cand)) {
    z <- cand[[i]]
    oc <- if (method == "CEVAM") .cevam_eval_free_terminal(p0,p1,looks,z$lower,z$upper,alpha - 1e-12) else
      .cevam_eval_fixed_terminal(p0,p1,looks,z$lower,z$upper,z$terminal)
    if (is.null(oc)) next
    valid <- oc$size <= alpha - 1e-12 && oc$power >= target_power + 1e-12
    if (!is.null(power_upper)) valid <- valid && oc$power <= power_upper + 1e-12
    if (!valid) next
    nvalid <- nvalid + 1L
    objective <- if (criterion == "EN0") oc$EN0 else weight*oc$EN0 + (1-weight)*oc$EN1
    key <- c(objective, oc$size, -oc$power, i)
    if (.cevam_lex_less(key,best_key)) {
      best_key <- key
      best <- list(method=method, looks=looks, lower=as.integer(z$lower), upper=as.integer(z$upper),
                   terminal=as.integer(oc$terminal), parameters=z$pars %||% list(),
                   size=oc$size, power=oc$power, EN0=oc$EN0, EN1=oc$EN1,
                   objective=objective, candidate_index=i)
    }
  }
  if (is.null(best)) return(NULL)
  best$n_candidates <- length(cand); best$n_feasible <- nvalid
  class(best) <- c("cevam_design","list"); best
}

# ---------- fixed-sample reference and feasibility ceiling ----------
cevam_fixed_reference_N <- function(p0,p1,alpha,target_plus_margin=.83,Nmax=300L) {
  for (N in seq(20L,Nmax,by=5L)) {
    feasible <- FALSE
    for (c in 0:(N+1L)) {
      size <- if (c<=N) pbinom(c-1,N,p0,lower.tail=FALSE) else 0
      power <- if (c<=N) pbinom(c-1,N,p1,lower.tail=FALSE) else 0
      if (size <= alpha + 1e-14 && power >= target_plus_margin - 1e-14) { feasible <- TRUE; break }
    }
    if (feasible) return(N)
  }
  stop("No fixed-sample reference N found")
}

cevam_np_ceiling <- function(p0,p1,N,alpha) {
  mass0 <- dbinom(0:N,N,p0); mass1 <- dbinom(0:N,N,p1)
  a <- power <- 0; c <- N+1L; rho <- 0
  for (r in N:0) {
    if (a + mass0[r+1L] > alpha) {
      rho <- (alpha-a)/mass0[r+1L]
      power <- power + rho*mass1[r+1L]; c <- r; break
    }
    a <- a + mass0[r+1L]; power <- power + mass1[r+1L]
  }
  list(power=power, randomize_at=c, randomize_probability=rho)
}

# ---------- Monte Carlo validation ----------
cevam_mc_binary <- function(p, design, nsim=200000L, seed=71926L) {
  set.seed(seed); looks <- design$looks; N <- tail(looks,1L); cohorts <- c(looks[1L],diff(looks))
  rej <- ef <- es <- total_n <- 0
  for (b in seq_len(nsim)) {
    x <- 0L; stopped <- FALSE
    for (k in seq_along(looks)) {
      x <- x + rbinom(1L, cohorts[k], p); n <- looks[k]
      if (n == N) {
        if (x >= design$terminal) rej <- rej + 1L
        total_n <- total_n + n; stopped <- TRUE; break
      }
      if (x >= design$upper[k]) { rej<-rej+1L; es<-es+1L; total_n<-total_n+n; stopped<-TRUE; break }
      if (x <= design$lower[k]) { ef<-ef+1L; total_n<-total_n+n; stopped<-TRUE; break }
    }
    if (!stopped) total_n <- total_n + N
  }
  pr <- rej/nsim
  list(rejection=pr, MCSE_rejection=sqrt(pr*(1-pr)/nsim), EN=total_n/nsim,
       early_futility=ef/nsim, early_success=es/nsim, nsim=nsim, seed=seed)
}

cevam_design_to_row <- function(d, scenario="") {
  data.frame(scenario=scenario, method=d$method, size=d$size, power=d$power,
             EN0=d$EN0, EN1=d$EN1, objective=d$objective,
             terminal=d$terminal,
             lower=paste(d$lower,collapse=","), upper=paste(d$upper,collapse=","),
             stringsAsFactors=FALSE)
}
