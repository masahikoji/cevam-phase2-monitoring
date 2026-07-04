# simulate_phase2_optional_monitoring.R
#
# Operational-flexibility simulation for CEVAM versus planned-look rules.
# This script does not auto-run when sourced.
# Command-line example:
#   Rscript simulate_phase2_optional_monitoring.R --xlsx=results/phase2_boundaries.xlsx --out=results/phase2_optional_monitoring_results.xlsx --nsim=10000

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

.phase2_output_path <- function(path) {
  if (is.null(path)) return(NULL)
  path <- path.expand(path)
  if (grepl("^/", path) || grepl("^[A-Za-z]:[/\\\\]", path)) return(path)
  out_dir <- path.expand(getOption("phase2.output_dir", getwd()))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(out_dir, path)
}

.phase2_require_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install it with install.packages('openxlsx').", call. = FALSE)
  }
}


.phase2_optional_display <- function(x) {
  if (exists("phase2_method_display", mode = "function")) {
    phase2_method_display(as.character(x))
  } else {
    gsub("E-BOP2", "CEVAM", as.character(x), fixed = TRUE)
  }
}

.phase2_extend_named_vector <- function(x, actual_looks, direction = c("le", "ge", "none")) {
  direction <- match.arg(direction)
  old_looks <- suppressWarnings(as.integer(names(x)))
  actual_looks <- as.integer(actual_looks)
  out <- rep(NA_integer_, length(actual_looks))
  names(out) <- as.character(actual_looks)

  for (i in seq_along(actual_looks)) {
    n <- actual_looks[i]
    candidates <- which(old_looks <= n)
    if (length(candidates) == 0L) {
      out[i] <- if (direction == "ge") n + 1L else -1L
    } else {
      out[i] <- as.integer(x[candidates[length(candidates)]])
    }
  }
  out
}

phase2_carry_forward_binary_design <- function(design, actual_looks) {
  out <- design
  out$futility_le <- .phase2_extend_named_vector(design$futility_le, actual_looks, "le")
  out$efficacy_ge <- .phase2_extend_named_vector(design$efficacy_ge, actual_looks, "ge")
  out$method <- paste0(design$method, "-naiveCF")
  out
}

phase2_carry_forward_projected_design <- function(design, actual_looks) {
  out <- design
  out$fut1 <- .phase2_extend_named_vector(
    design$fut1, actual_looks,
    ifelse(identical(design$fut1_dir, "ge"), "ge", "le")
  )
  out$fut2 <- .phase2_extend_named_vector(
    design$fut2, actual_looks,
    ifelse(identical(design$fut2_dir, "ge"), "ge", "le")
  )
  out$eff1 <- .phase2_extend_named_vector(
    design$eff1, actual_looks,
    ifelse(identical(design$eff1_dir, "le"), "le", "ge")
  )
  out$eff2 <- .phase2_extend_named_vector(
    design$eff2, actual_looks,
    ifelse(identical(design$eff2_dir, "le"), "le", "ge")
  )
  out$method <- paste0(design$method, "-naiveCF")
  out
}

phase2_CEVAM_anytime_binary_design <- function(p0, p1, actual_looks, alpha = 0.10, beta = alpha) {
  des <- build_binary_boundary_ebop2(
    p0 = p0,
    p1 = p1,
    looks = as.integer(actual_looks),
    alpha = alpha,
    beta = beta,
    tuned = FALSE,
    alpha_eff = alpha
  )
  des$method <- "CEVAM-anytime"
  des$design_type <- "component_count_anytime"
  des
}

phase2_CEVAM_anytime_projected_design <- function(exspec, actual_looks, alpha = 0.10, beta = alpha) {
  des <- build_projected_boundary_ebop2(
    exspec = exspec,
    looks = as.integer(actual_looks),
    alpha = alpha,
    beta = beta,
    tuned = FALSE,
    alpha_eff = alpha
  )
  des$method <- "CEVAM-anytime"
  des$design_type <- "component_count_anytime"
  des
}

mc_binary_optional <- function(truth_p, design, looks, N, nsim = 10000L, seed = 12345L) {
  mc_binary(
    truth_p = truth_p,
    design = design,
    looks = as.integer(looks),
    N = as.integer(N),
    nsim = nsim,
    seed = seed
  )
}

mc_projected_optional <- function(theta_true, exspec, design, looks, N, nsim = 10000L, seed = 12345L) {
  mc_projected(
    theta_true = theta_true,
    exspec = exspec,
    design = design,
    looks = as.integer(looks),
    N = as.integer(N),
    nsim = nsim,
    seed = seed
  )
}

.phase2_add_oc_row <- function(example, scenario_id, truth_label, monitoring_regime,
                               actual_looks, method, rule_type, oc, nsim, seed) {
  data.frame(
    example = example,
    scenario_id = scenario_id,
    truth_label = truth_label,
    monitoring_regime = monitoring_regime,
    actual_looks = paste(as.integer(actual_looks), collapse = ","),
    method = .phase2_optional_display(method),
    rule_type = rule_type,
    reject = oc$reject,
    ETF = oc$ETF,
    ETE = oc$ETE,
    PET = oc$PET,
    EN = oc$EN,
    nsim = as.integer(nsim),
    seed = as.integer(seed),
    stringsAsFactors = FALSE
  )
}

phase2_optional_truth_label <- function(example, theta_or_p) {
  if (identical(example, "example1")) return(sprintf("p=%.2f", as.numeric(theta_or_p)))
  th <- as.numeric(theta_or_p)
  if (identical(example, "example2")) return(sprintf("CR=%.2f, CRPR=%.2f", th[1L], th[1L] + th[2L]))
  if (identical(example, "example3")) return(sprintf("ORR=%.2f, EFS6=%.2f", th[1L] + th[2L], th[1L] + th[3L]))
  if (identical(example, "example4")) return(sprintf("Eff=%.2f, Tox=%.2f", th[1L] + th[2L], th[1L] + th[3L]))
  paste(sprintf("%.2f", th), collapse = ",")
}

.phase2_find_pair <- function(settings, example1_pair_id) {
  ids <- vapply(settings$ex1_pairs, function(z) as.integer(z$pair_id), integer(1L))
  j <- which(ids == as.integer(example1_pair_id))
  if (length(j) == 0L) stop("Cannot find example1 pair_id = ", example1_pair_id, call. = FALSE)
  settings$ex1_pairs[[j[1L]]]
}

run_phase2_optional_monitoring_simulation <- function(
  xlsx_path = "phase2_boundaries.xlsx",
  nsim = 10000L,
  seed = 20260401L,
  results_xlsx = "phase2_optional_monitoring_results.xlsx",
  alpha = 0.10,
  beta = alpha,
  example1_pair_id = 1L,
  run_all_scenarios = FALSE,
  planned_looks = NULL,
  extra_looks = c(12L, 18L, 23L, 28L, 33L, 38L),
  extension_looks = c(45L, 50L, 55L, 60L),
  planned_methods_binary = c("BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T"),
  planned_methods_projected = c("BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T", "CEVAM-CS-T", "CEVAM-JLR", "CEVAM-JLR-T"),
  include_joint_lr = TRUE,
  show_progress = TRUE
) {
  .phase2_require_openxlsx()

  xlsx_path <- path.expand(xlsx_path)
  if (!file.exists(xlsx_path)) {
    stop(
      "Boundary workbook not found: ", xlsx_path, "\n",
      "Run generate_phase2_boundaries_excel*.R first, or pass the correct xlsx_path.",
      call. = FALSE
    )
  }

  results_xlsx <- .phase2_output_path(results_xlsx)
  wb <- read_boundary_workbook(xlsx_path)
  settings <- wb$settings

  if (is.null(planned_looks)) planned_looks <- settings$looks
  planned_looks <- as.integer(planned_looks)
  unplanned_looks <- sort(unique(as.integer(c(planned_looks, extra_looks))))
  extended_looks <- sort(unique(as.integer(c(unplanned_looks, extension_looks))))

  if (!isTRUE(include_joint_lr)) {
    planned_methods_projected <- setdiff(planned_methods_projected, c("CEVAM-JLR", "CEVAM-JLR-T"))
  }

  regimes <- list(
    planned = planned_looks,
    unplanned = unplanned_looks,
    extension = extended_looks
  )

  message("Optional-monitoring simulation settings:")
  message("  boundary workbook: ", normalizePath(xlsx_path, winslash = "/", mustWork = FALSE))
  message("  nsim: ", nsim)
  message("  run_all_scenarios: ", run_all_scenarios)
  message("  planned looks: ", paste(planned_looks, collapse = ","))
  message("  unplanned looks: ", paste(unplanned_looks, collapse = ","))
  message("  extension looks: ", paste(extended_looks, collapse = ","))

  out <- list(example1 = list(), example2 = list(), example3 = list(), example4 = list())
  row_counter <- 0L
  add <- function(ex, row) {
    out[[ex]][[length(out[[ex]]) + 1L]] <<- row
    row_counter <<- row_counter + 1L
    if (isTRUE(show_progress)) {
      message(
        "[optional] ", row_counter, " | ", ex, " | ", row$monitoring_regime,
        " | ", row$method, " | ", row$truth_label,
        " | PRN=", sprintf("%.1f%%", 100 * row$reject),
        ", EN=", sprintf("%.1f", row$EN)
      )
    }
  }

  # Example 1, selected binary pair.
  pair <- .phase2_find_pair(settings, example1_pair_id)
  truths <- if (isTRUE(run_all_scenarios)) pair$truths else pair$p0
  for (truth in truths) {
    for (regime_name in names(regimes)) {
      actual_looks <- regimes[[regime_name]]
      N_actual <- max(actual_looks)

      des_any <- phase2_CEVAM_anytime_binary_design(pair$p0, pair$p1, actual_looks, alpha, beta)
      oc <- mc_binary_optional(truth, des_any, actual_looks, N_actual, nsim, seed + row_counter + 1L)
      add("example1", .phase2_add_oc_row(
        "example1", NA_integer_, phase2_optional_truth_label("example1", truth),
        regime_name, actual_looks, des_any$method, "fixed e-process threshold", oc, nsim, seed
      ))

      for (method in intersect(planned_methods_binary, unique(wb$ex1$method))) {
        des0 <- read_binary_design(wb$ex1, pair_id = pair$pair_id, method = method)
        des <- if (identical(regime_name, "planned")) des0 else phase2_carry_forward_binary_design(des0, actual_looks)
        oc <- mc_binary_optional(truth, des, actual_looks, N_actual, nsim, seed + row_counter + 1L)
        add("example1", .phase2_add_oc_row(
          "example1", NA_integer_, phase2_optional_truth_label("example1", truth),
          regime_name, actual_looks, des$method,
          ifelse(identical(regime_name, "planned"), "planned table", "naive carry-forward table"),
          oc, nsim, seed
        ))
      }
    }
  }

  run_projected <- function(exspec, wb_sheet, exname) {
    scenario_ids <- if (isTRUE(run_all_scenarios)) exspec$scenario_ids else 1L
    for (sid in scenario_ids) {
      theta <- exspec$theta_scenarios[sid, ]
      for (regime_name in names(regimes)) {
        actual_looks <- regimes[[regime_name]]
        N_actual <- max(actual_looks)

        des_any <- phase2_CEVAM_anytime_projected_design(exspec, actual_looks, alpha, beta)
        oc <- mc_projected_optional(theta, exspec, des_any, actual_looks, N_actual, nsim, seed + row_counter + 1L)
        add(exname, .phase2_add_oc_row(
          exname, sid, phase2_optional_truth_label(exname, theta),
          regime_name, actual_looks, des_any$method, "fixed e-process threshold", oc, nsim, seed
        ))

        for (method in intersect(planned_methods_projected, unique(wb_sheet$method))) {
          des0 <- read_projected_design(wb_sheet, method = method)
          if (!is.null(des0$design_type) && identical(des0$design_type, "joint_lr_projected") && !identical(regime_name, "planned")) {
            des <- des0
            des$method <- paste0(des0$method, "-naiveExtra")
          } else {
            des <- if (identical(regime_name, "planned")) des0 else phase2_carry_forward_projected_design(des0, actual_looks)
          }
          oc <- mc_projected_optional(theta, exspec, des, actual_looks, N_actual, nsim, seed + row_counter + 1L)
          add(exname, .phase2_add_oc_row(
            exname, sid, phase2_optional_truth_label(exname, theta),
            regime_name, actual_looks, des$method,
            ifelse(identical(regime_name, "planned"), "planned table", "naive extra-look use"),
            oc, nsim, seed
          ))
        }
      }
    }
  }

  run_projected(settings$ex2, wb$ex2, "example2")
  run_projected(settings$ex3, wb$ex3, "example3")
  run_projected(settings$ex4, wb$ex4, "example4")

  results <- lapply(out, function(x) {
    if (length(x) == 0L) return(data.frame())
    ans <- do.call(rbind, x)
    rownames(ans) <- NULL
    ans
  })

  if (!is.null(results_xlsx)) {
    write_simulation_results_excel(results, path = results_xlsx)
    if (!file.exists(results_xlsx)) {
      stop("Failed to write optional-monitoring workbook: ", results_xlsx, call. = FALSE)
    }
    message("Wrote optional-monitoring workbook: ", normalizePath(results_xlsx, winslash = "/", mustWork = FALSE))
    message("Workbook size: ", format(file.info(results_xlsx)$size, big.mark = ","), " bytes")
  }

  invisible(results)
}



if (.phase2_running_as_main("simulate_phase2_optional_monitoring.R")) {
  opt <- .phase2_parse_cli()
  results <- run_phase2_optional_monitoring_simulation(
    xlsx_path = if (!is.null(opt$xlsx)) opt$xlsx else file.path("results", "phase2_boundaries.xlsx"),
    nsim = .phase2_as_int(opt$nsim, 10000L),
    seed = .phase2_as_int(opt$seed, 20260401L),
    results_xlsx = if (!is.null(opt$out)) opt$out else file.path("results", "phase2_optional_monitoring_results.xlsx"),
    alpha = as.numeric(ifelse(is.null(opt$alpha), 0.10, opt$alpha)),
    beta = as.numeric(ifelse(is.null(opt$beta), 0.10, opt$beta)),
    example1_pair_id = .phase2_as_int(opt$example1_pair_id, 1L),
    run_all_scenarios = .phase2_as_bool(opt$run_all_scenarios, FALSE),
    include_joint_lr = .phase2_as_bool(opt$include_joint_lr, FALSE),
    show_progress = .phase2_as_bool(opt$show_progress, TRUE)
  )
  invisible(results)
}
