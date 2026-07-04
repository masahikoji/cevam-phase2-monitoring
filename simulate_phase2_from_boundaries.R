# simulate_phase2_from_boundaries.R
#
# Read a precomputed boundary workbook and evaluate operating characteristics.
# This script does not recalibrate boundaries and does not auto-run when sourced.
# Command-line example:
#   Rscript simulate_phase2_from_boundaries.R --xlsx=results/phase2_boundaries.xlsx --out=results/phase2_simulation_results.xlsx --nsim=10000

.phase2_script_dir <- function() {
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    ofile <- frames[[i]]$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
  }
  args <- commandArgs(FALSE)
  idx <- grep("^--file=", args)
  if (length(idx) > 0L) return(dirname(normalizePath(sub("^--file=", "", args[idx[1L]]), winslash = "/", mustWork = FALSE)))
  getwd()
}
.phase2_dir <- .phase2_script_dir()
source(file.path(.phase2_dir, "phase2_design_core.R"))

.phase2_parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  for (a in args) {
    if (!grepl("^--", a)) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1L]]
    key <- kv[1L]
    val <- if (length(kv) >= 2L) paste(kv[-1L], collapse = "=") else "TRUE"
    out[[key]] <- val
  }
  out
}
.phase2_as_bool <- function(x, default = FALSE) {
  if (is.null(x)) return(default)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}
.phase2_running_as_main <- function(file_name) {
  args <- commandArgs(FALSE)
  idx <- grep("^--file=", args)
  if (length(idx) == 0L) return(FALSE)
  identical(basename(normalizePath(sub("^--file=", "", args[idx[1L]]), winslash = "/", mustWork = FALSE)), file_name)
}

.phase2_as_int <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.integer(x)
}
.phase2_as_pair_ids <- function(x, default = 1L) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.integer(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
}

# ============================================================
# Progress helpers
# ============================================================

.phase2_sim_format_elapsed <- function(x) {
  if (!is.finite(x)) return("NA")
  x <- max(0, as.numeric(x))
  if (x < 60) return(sprintf("%.0fs", x))
  if (x < 3600) return(sprintf("%.1fmin", x / 60))
  sprintf("%.1fh", x / 3600)
}

.phase2_sim_progress_message <- function(label, done, total, start_time, extra = NULL) {
  if (total <= 0L) return(invisible(NULL))
  pct <- 100 * done / total
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  eta <- if (done > 0L) elapsed * (total - done) / done else NA_real_
  extra_txt <- if (is.null(extra) || !nzchar(extra)) "" else paste0(" | ", extra)
  message(sprintf(
    "%s: %d/%d (%.1f%%), elapsed %s, ETA %s%s",
    label, done, total, pct,
    .phase2_sim_format_elapsed(elapsed),
    .phase2_sim_format_elapsed(eta),
    extra_txt
  ))
  flush.console()
  invisible(NULL)
}

.phase2_sim_report_at <- function(total, progress_pct_step = 10L) {
  total <- as.integer(total)
  progress_pct_step <- max(1L, min(100L, as.integer(progress_pct_step)))
  pct <- seq(progress_pct_step, 100L, by = progress_pct_step)
  unique(pmax(1L, pmin(total, ceiling(total * pct / 100))))
}

.phase2_sim_metric_text <- function(reject, ETF, ETE, Nbar, denom) {
  sprintf(
    "reject_prob=%.4f, early_stop_prob=%.4f, avg_n=%.2f",
    reject / denom,
    (ETF + ETE) / denom,
    Nbar / denom
  )
}

# ============================================================
# Monte Carlo simulation with within-simulation progress
# ============================================================

mc_binary_progress <- function(truth_p,
                               design,
                               looks,
                               N,
                               nsim = 10000L,
                               seed = 12345L,
                               progress_label = "binary MC",
                               show_progress = TRUE,
                               progress_pct_step = 10L) {
  set.seed(seed)
  reject <- ETF <- ETE <- 0
  Nbar <- 0
  cohorts <- c(looks[1L], diff(looks))
  nsim <- as.integer(nsim)
  report_at <- .phase2_sim_report_at(nsim, progress_pct_step)
  start_time <- Sys.time()

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

    if (isTRUE(show_progress) && s %in% report_at) {
      .phase2_sim_progress_message(
        progress_label,
        s,
        nsim,
        start_time,
        extra = .phase2_sim_metric_text(reject, ETF, ETE, Nbar, s)
      )
    }
  }

  list(reject = reject / nsim,
       ETF = ETF / nsim,
       ETE = ETE / nsim,
       PET = (ETF + ETE) / nsim,
       EN = Nbar / nsim)
}

mc_projected_progress <- function(theta_true,
                                  exspec,
                                  design,
                                  looks,
                                  N,
                                  nsim = 10000L,
                                  seed = 12345L,
                                  progress_label = "projected MC",
                                  show_progress = TRUE,
                                  progress_pct_step = 10L) {
  set.seed(seed)
  reject <- ETF <- ETE <- 0
  Nbar <- 0
  cohorts <- c(looks[1L], diff(looks))
  nsim <- as.integer(nsim)
  report_at <- .phase2_sim_report_at(nsim, progress_pct_step)
  start_time <- Sys.time()

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

    if (isTRUE(show_progress) && s %in% report_at) {
      .phase2_sim_progress_message(
        progress_label,
        s,
        nsim,
        start_time,
        extra = .phase2_sim_metric_text(reject, ETF, ETE, Nbar, s)
      )
    }
  }

  list(reject = reject / nsim,
       ETF = ETF / nsim,
       ETE = ETE / nsim,
       PET = (ETF + ETE) / nsim,
       EN = Nbar / nsim)
}

run_phase2_simulations_from_workbook <- function(
  xlsx_path = "phase2_boundaries.xlsx",
  nsim = 10000L,
  seed = 12345L,
  use_exact = FALSE,
  results_xlsx = NULL,
  min_look = 10L,
  example1_pair_ids = 1L,
  include_mixture = FALSE,
  include_component_specific = TRUE,
  include_joint_lr = TRUE,
  show_progress = TRUE,
  progress_pct_step = 10L,
  progress_every = NULL,
  show_trial_progress = TRUE,
  show_overall_progress = FALSE,
  parallel_examples = TRUE,
  n_example_cores = phase2_default_n_cores()
) {
  wb <- read_boundary_workbook(xlsx_path)
  settings <- wb$settings
  N <- settings$N
  min_look <- as.integer(min_look)
  if (is.na(min_look) || min_look < 1L) min_look <- min(settings$looks)
  looks <- settings$looks[settings$looks >= min_look]
  if (length(looks) == 0L) {
    stop("No planned looks remain after applying min_look = ", min_look, ".", call. = FALSE)
  }
  settings$looks <- looks

  example1_pair_ids <- as.integer(example1_pair_ids)
  if (length(example1_pair_ids) > 0L && !any(is.na(example1_pair_ids))) {
    settings$ex1_pairs <- settings$ex1_pairs[vapply(settings$ex1_pairs, function(z) z$pair_id %in% example1_pair_ids, logical(1L))]
    if (length(settings$ex1_pairs) == 0L) {
      stop("No Example 1 pairs remain after applying example1_pair_ids.", call. = FALSE)
    }
  }

  methods_binary <- c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T")
  methods_projected <- c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T")
  if (isTRUE(include_component_specific)) {
    methods_projected <- c(methods_projected, "CEVAM-CS-T")
  }
  if (isTRUE(include_joint_lr)) {
    methods_projected <- c(methods_projected, "CEVAM-JLR", "CEVAM-JLR-T")
  }
  if (isTRUE(include_mixture)) {
    methods_binary <- c(methods_binary, "CEVAM-Mix-NT", "CEVAM-Mix-T")
    methods_projected <- c(methods_projected, "CEVAM-Mix-NT", "CEVAM-Mix-T")
  }
  methods_binary <- intersect(methods_binary, unique(wb$ex1$method))
  methods_projected <- Reduce(intersect, list(methods_projected, unique(wb$ex2$method), unique(wb$ex3$method), unique(wb$ex4$method)))
  nsim <- as.integer(nsim)
  progress_pct_step <- max(1L, min(100L, as.integer(progress_pct_step)))
  n_example_cores <- resolve_n_cores(n_example_cores)
  if (!is.null(progress_every)) {
    warning("progress_every is kept only for backward compatibility; progress is now reported every progress_pct_step percent.", call. = FALSE)
  }

  total_jobs <- length(settings$ex1_pairs) * length(settings$ex1_pairs[[1L]]$truths) * length(methods_binary) +
    length(settings$ex2$scenario_ids) * length(methods_projected) +
    length(settings$ex3$scenario_ids) * length(methods_projected) +
    length(settings$ex4$scenario_ids) * length(methods_projected)

  overall_start <- Sys.time()
  job_done <- 0L

  use_example_parallel <- isTRUE(parallel_examples) && n_example_cores > 1L
  if (use_example_parallel && .Platform$OS.type == "windows") {
    warning("parallel_examples uses fork-based parallel::mclapply and is disabled on Windows in this script; running sequentially.", call. = FALSE)
    use_example_parallel <- FALSE
  }

  if (isTRUE(show_progress)) {
    message("Starting phase II simulation")
    message("  xlsx_path           = ", xlsx_path)
    message("  use_exact           = ", use_exact)
    message("  nsim                = ", nsim)
    message("  planned looks       = ", paste(looks, collapse = ","))
    message("  min_look            = ", min_look)
    message("  example1_pair_ids   = ", paste(vapply(settings$ex1_pairs, function(z) z$pair_id, integer(1L)), collapse = ","))
    message("  include_mixture            = ", include_mixture)
    message("  include_component_specific = ", include_component_specific)
    message("  include_joint_lr           = ", include_joint_lr)
    message("  total scenario jobs = ", total_jobs)
    message("  progress_pct_step   = ", progress_pct_step, "%")
    message("  trial-level progress= ", show_trial_progress)
    message("  overall job progress= ", show_overall_progress)
    message("  parallel_examples   = ", use_example_parallel)
    message("  n_example_cores     = ", ifelse(use_example_parallel, n_example_cores, 1L))
    flush.console()
  }

  run_one_binary <- function(pair, truth, method, i_seed) {
    label <- sprintf("example1 pair=%d truth=%.2f / %s", pair$pair_id, truth, method)
    des <- read_binary_design(wb$ex1, pair$pair_id, method)
    oc <- if (isTRUE(use_exact)) {
      exact_oc_binary(truth, des, looks, N)
    } else {
      mc_binary_progress(
        truth,
        des,
        looks,
        N,
        nsim = nsim,
        seed = seed + i_seed,
        progress_label = label,
        show_progress = isTRUE(show_progress) && isTRUE(show_trial_progress),
        progress_pct_step = progress_pct_step
      )
    }
    oc
  }

  run_one_projected <- function(exspec, wb_sheet, scenario_id, method, j_seed) {
    theta <- exspec$theta_scenarios[scenario_id, ]
    label <- sprintf("%s scenario=%d / %s", exspec$example, scenario_id, method)
    des <- read_projected_design(wb_sheet, method)
    oc <- if (isTRUE(use_exact)) {
      exact_oc_projected(theta, exspec, des, looks, N)
    } else {
      mc_projected_progress(
        theta,
        exspec,
        des,
        looks,
        N,
        nsim = nsim,
        seed = seed + j_seed,
        progress_label = label,
        show_progress = isTRUE(show_progress) && isTRUE(show_trial_progress),
        progress_pct_step = progress_pct_step
      )
    }
    oc
  }

  report_overall <- function() {
    if (isTRUE(show_progress) && isTRUE(show_overall_progress)) {
      .phase2_sim_progress_message("[overall simulation]", job_done, total_jobs, overall_start)
    }
  }

  run_example1_block <- function() {
    ex1_rows <- list(); i <- 0L
    for (pair in settings$ex1_pairs) {
      for (truth in pair$truths) {
        for (method in methods_binary) {
          oc <- run_one_binary(pair, truth, method, i_seed = i)
          i <- i + 1L
          ex1_rows[[i]] <- data.frame(
            pair_id = pair$pair_id,
            truth = truth,
            method = method,
            reject = oc$reject,
            ETF = oc$ETF,
            ETE = oc$ETE,
            PET = oc$PET,
            EN = oc$EN,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    do.call(rbind, ex1_rows)
  }

  run_projected_block <- function(exspec, wb_sheet) {
    out <- list(); j <- 0L
    for (s in exspec$scenario_ids) {
      theta <- exspec$theta_scenarios[s, ]
      for (method in methods_projected) {
        oc <- run_one_projected(exspec, wb_sheet, scenario_id = s, method = method, j_seed = j)
        j <- j + 1L
        row <- data.frame(
          scenario_id = s,
          method = method,
          reject = oc$reject,
          ETF = oc$ETF,
          ETE = oc$ETE,
          PET = oc$PET,
          EN = oc$EN,
          stringsAsFactors = FALSE
        )
        if (length(theta) == 4L) {
          row$theta1 <- theta[1L]; row$theta2 <- theta[2L]
          row$theta3 <- theta[3L]; row$theta4 <- theta[4L]
        }
        out[[j]] <- row
      }
    }
    do.call(rbind, out)
  }

  example_jobs <- list(
    example1 = function() run_example1_block(),
    example2 = function() run_projected_block(settings$ex2, wb$ex2),
    example3 = function() run_projected_block(settings$ex3, wb$ex3),
    example4 = function() run_projected_block(settings$ex4, wb$ex4)
  )

  if (use_example_parallel) {
    if (isTRUE(show_progress)) {
      message("Running examples in parallel: ", paste(names(example_jobs), collapse = ", "))
      message("Note: progress messages from parallel examples may appear interleaved.")
      flush.console()
    }
    results <- parallel::mclapply(
      example_jobs,
      function(f) f(),
      mc.cores = min(n_example_cores, length(example_jobs)),
      mc.preschedule = FALSE
    )
  } else {
    results <- list()
    for (nm in names(example_jobs)) {
      if (isTRUE(show_progress)) {
        message("Running ", nm)
        flush.console()
      }
      results[[nm]] <- example_jobs[[nm]]()
      job_done <- job_done + switch(nm,
                                    example1 = length(settings$ex1_pairs) * length(settings$ex1_pairs[[1L]]$truths) * length(methods_binary),
                                    example2 = length(settings$ex2$scenario_ids) * length(methods_projected),
                                    example3 = length(settings$ex3$scenario_ids) * length(methods_projected),
                                    example4 = length(settings$ex4$scenario_ids) * length(methods_projected))
      report_overall()
    }
  }

  results <- results[c("example1", "example2", "example3", "example4")]

  if (!is.null(results_xlsx)) {
    if (isTRUE(show_progress)) message("Writing simulation results to: ", results_xlsx)
    write_simulation_results_excel(results, path = results_xlsx)
  }

  if (isTRUE(show_progress)) {
    elapsed <- as.numeric(difftime(Sys.time(), overall_start, units = "secs"))
    message("Finished phase II simulation in ", .phase2_sim_format_elapsed(elapsed))
    flush.console()
  }

  invisible(results)
}



if (.phase2_running_as_main("simulate_phase2_from_boundaries.R")) {
  opt <- .phase2_parse_cli()
  res <- run_phase2_simulations_from_workbook(
    xlsx_path = if (!is.null(opt$xlsx)) opt$xlsx else file.path("results", "phase2_boundaries.xlsx"),
    nsim = .phase2_as_int(opt$nsim, 10000L),
    seed = .phase2_as_int(opt$seed, 12345L),
    use_exact = .phase2_as_bool(opt$use_exact, FALSE),
    results_xlsx = if (!is.null(opt$out)) opt$out else file.path("results", "phase2_simulation_results.xlsx"),
    min_look = .phase2_as_int(opt$min_look, 10L),
    example1_pair_ids = .phase2_as_pair_ids(opt$example1_pair_ids, 1L),
    include_mixture = .phase2_as_bool(opt$include_mixture, FALSE),
    include_component_specific = .phase2_as_bool(opt$include_component_specific, FALSE),
    include_joint_lr = .phase2_as_bool(opt$include_joint_lr, FALSE),
    show_progress = .phase2_as_bool(opt$show_progress, TRUE),
    progress_pct_step = .phase2_as_int(opt$progress_pct_step, 10L),
    show_trial_progress = .phase2_as_bool(opt$show_trial_progress, TRUE),
    show_overall_progress = .phase2_as_bool(opt$show_overall_progress, FALSE),
    parallel_examples = .phase2_as_bool(opt$parallel_examples, TRUE),
    n_example_cores = .phase2_as_int(opt$n_example_cores, phase2_default_n_cores())
  )
  invisible(res)
}
