# make_current_alpha_spending_plots_corrected.R
#
# Exact cumulative false-positive probability under the design null for the
# current anchor CEVAM / BOP2-FE1 / BOP2-FE2 designs.
#
# IMPORTANT:
#   This is a descriptive cumulative rejection-probability curve,
#   A(k) = P_{p0}(efficacy rejection at or before planned look k),
#   under the ACTUAL binding-futility trial rule.
#   It is not a prespecified Lan-DeMets alpha-spending function.
#
# The code reads the current anchor boundary table and exact operating
# characteristics from results_current and verifies automatically that the
# final cumulative probability equals the stored exact type I error.

options(stringsAsFactors = FALSE)

.default_results_dir <- path.expand(
  "results_current"
)

find_existing_file <- function(dir, candidates) {
  paths <- file.path(dir, candidates)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) {
    stop(
      "None of the expected files was found in: ", dir, "\nTried: ",
      paste(candidates, collapse = ", "), call. = FALSE
    )
  }
  hit[[1L]]
}

# Propagate the probability mass that is still active after each look.
# At interim looks:
#   r <= lower -> futility absorption
#   r >= upper -> efficacy absorption
#   otherwise  -> continue
# At final look:
#   r >= terminal upper -> efficacy
#   otherwise           -> non-success
exact_cumulative_fp <- function(p0, looks, lower, upper, terminal_upper) {
  K <- length(looks)
  stopifnot(K >= 1L, length(lower) == K - 1L, length(upper) == K - 1L)

  active <- c(`0` = 1.0)
  prev_n <- 0L
  cum_eff <- 0.0
  out <- vector("list", K)

  for (k in seq_len(K)) {
    n_now <- as.integer(looks[k])
    m <- n_now - prev_n
    if (m < 0L) stop("Looks must be increasing.", call. = FALSE)

    incoming <- numeric(n_now + 1L)
    names(incoming) <- as.character(0:n_now)

    for (s_chr in names(active)) {
      s <- as.integer(s_chr)
      probs <- dbinom(0:m, size = m, prob = p0)
      rr <- s + 0:m
      incoming[as.character(rr)] <- incoming[as.character(rr)] + active[[s_chr]] * probs
    }

    reach_prob <- sum(incoming)

    if (k < K) {
      lo <- as.integer(lower[k])
      up <- as.integer(upper[k])
      rvals <- 0:n_now

      eff_now <- sum(incoming[rvals >= up + 1L]) # placeholder overwritten below
      # Correct inclusive rule: efficacy when r >= upper; futility when r <= lower.
      eff_now <- sum(incoming[as.character(rvals[rvals >= up])])
      fut_now <- sum(incoming[as.character(rvals[rvals <= lo])])

      keep <- rvals[(rvals > lo) & (rvals < up)]
      if (length(keep) == 0L) {
        active <- numeric(0)
      } else {
        active <- incoming[as.character(keep)]
      }
    } else {
      up <- as.integer(terminal_upper)
      rvals <- 0:n_now
      eff_now <- sum(incoming[as.character(rvals[rvals >= up])])
      fut_now <- 0.0
      active <- numeric(0)
    }

    cum_eff <- cum_eff + eff_now
    out[[k]] <- data.frame(
      look_index = k,
      look = n_now,
      cumulative_alpha = cum_eff,
      incremental_alpha = eff_now,
      reach_probability = reach_prob,
      futility_absorbed = fut_now,
      stringsAsFactors = FALSE
    )
    prev_n <- n_now
  }

  do.call(rbind, out)
}

make_current_alpha_spending_plots <- function(
    results_dir = .default_results_dir,
    out_prefix = "current_alpha_spending_exact_corrected",
    tolerance = 5e-10) {

  results_dir <- path.expand(results_dir)
  if (!dir.exists(results_dir)) {
    stop("Results directory not found: ", results_dir, call. = FALSE)
  }

  boundary_file <- find_existing_file(
    results_dir,
    c("main_anchor_boundaries.csv", "main_anchor_boundaries(1).csv")
  )
  exact_file <- find_existing_file(
    results_dir,
    c("main_anchor_exact.csv", "main_anchor_exact(1).csv")
  )

  bnd <- read.csv(boundary_file, check.names = FALSE)
  ex <- read.csv(exact_file, check.names = FALSE)

  required_bnd <- c("method", "look", "lower", "upper", "final")
  required_ex <- c("method", "size", "p0", "N", "looks", "alpha")
  if (!all(required_bnd %in% names(bnd))) {
    stop("Boundary CSV is missing columns: ",
         paste(setdiff(required_bnd, names(bnd)), collapse = ", "), call. = FALSE)
  }
  if (!all(required_ex %in% names(ex))) {
    stop("Exact CSV is missing columns: ",
         paste(setdiff(required_ex, names(ex)), collapse = ", "), call. = FALSE)
  }

  methods <- c("CEVAM", "BOP2-FE1", "BOP2-FE2")
  rows <- list()

  for (method in methods) {
    bb <- bnd[bnd$method == method, , drop = FALSE]
    ee <- ex[ex$method == method, , drop = FALSE]
    if (nrow(bb) == 0L || nrow(ee) != 1L) {
      stop("Could not identify unique anchor design for method: ", method, call. = FALSE)
    }

    bb <- bb[order(bb$look), , drop = FALSE]
    looks <- as.integer(bb$look)
    final_idx <- which(as.logical(bb$final))
    if (length(final_idx) != 1L || final_idx != nrow(bb)) {
      stop("Final boundary row is not uniquely identified for ", method, call. = FALSE)
    }

    lower <- as.integer(bb$lower[-final_idx])
    upper <- as.integer(bb$upper[-final_idx])
    terminal_upper <- as.integer(bb$upper[final_idx])
    p0 <- as.numeric(ee$p0)

    dd <- exact_cumulative_fp(
      p0 = p0,
      looks = looks,
      lower = lower,
      upper = upper,
      terminal_upper = terminal_upper
    )
    dd$method <- method
    dd$design_null_p <- p0
    dd$nominal_alpha <- as.numeric(ee$alpha)
    dd$stored_exact_size <- as.numeric(ee$size)

    final_calc <- tail(dd$cumulative_alpha, 1L)
    final_stored <- as.numeric(ee$size)
    diff <- abs(final_calc - final_stored)
    if (diff > tolerance) {
      stop(sprintf(
        "%s validation failed: final cumulative FP %.12f != stored exact size %.12f (|diff|=%.3g).",
        method, final_calc, final_stored, diff
      ), call. = FALSE)
    }

    message(sprintf(
      "%s: final cumulative FP = %.9f; stored exact size = %.9f [OK]",
      method, final_calc, final_stored
    ))
    rows[[method]] <- dd
  }

  out <- do.call(rbind, rows)
  out <- out[, c(
    "method", "look_index", "look", "cumulative_alpha", "incremental_alpha",
    "reach_probability", "futility_absorbed", "design_null_p", "nominal_alpha",
    "stored_exact_size"
  )]
  rownames(out) <- NULL

  csv_path <- file.path(results_dir, paste0(out_prefix, ".csv"))
  pdf_path <- file.path(results_dir, paste0(out_prefix, ".pdf"))
  png_path <- file.path(results_dir, paste0(out_prefix, ".png"))
  tex_path <- file.path(results_dir, paste0(out_prefix, "_table.tex"))

  write.csv(out, csv_path, row.names = FALSE)

  plot_fun <- function() {
    looks <- sort(unique(out$look))
    methods <- unique(out$method)
    mat <- sapply(methods, function(m) {
      d <- out[out$method == m, , drop = FALSE]
      d$cumulative_alpha[match(looks, d$look)]
    })

    ymax <- max(c(out$nominal_alpha, mat), na.rm = TRUE)
    ylim <- c(0, max(0.105, ymax * 1.08))
    matplot(
      looks, mat, type = "o", pch = seq_along(methods), lty = seq_along(methods),
      xlab = "Number of patients treated",
      ylab = "Cumulative false-positive probability",
      ylim = ylim,
      main = "Cumulative false-positive probability under the design null"
    )
    abline(h = unique(out$nominal_alpha)[1L], lty = 2)
    legend(
      "topleft", legend = ifelse(methods == "CEVAM", "CLRM", methods),
      lty = seq_along(methods), pch = seq_along(methods), bty = "n"
    )
  }

  pdf(pdf_path, width = 7, height = 5)
  plot_fun()
  dev.off()

  png(png_path, width = 1400, height = 1000, res = 180)
  plot_fun()
  dev.off()

  # Compact TeX table for manuscript/supplement checking.
  con <- file(tex_path, open = "wt")
  on.exit(close(con), add = TRUE)
  looks <- sort(unique(out$look))
  writeLines("% Auto-generated by make_current_alpha_spending_plots_corrected.R", con)
  writeLines("\\begin{table}[htbp]", con)
  writeLines("\\centering\\small", con)
  writeLines("\\caption{Descriptive cumulative false-positive probability under the anchor design null.}", con)
  writeLines("\\label{tab:cumulative-fp}", con)
  writeLines(paste0("\\begin{tabular}{l", paste(rep("r", length(looks)), collapse = ""), "}"), con)
  writeLines("\\toprule", con)
  writeLines(paste(c("Method", looks), collapse = " & ") |> paste0(" \\\\"), con)
  writeLines("\\midrule", con)
  for (m in unique(out$method)) {
    d <- out[out$method == m, , drop = FALSE]
    vals <- sprintf("%.4f", d$cumulative_alpha[match(looks, d$look)])
    writeLines(paste(c(if (m == "CEVAM") "CLRM" else m, vals), collapse = " & ") |> paste0(" \\\\"), con)
  }
  writeLines("\\bottomrule", con)
  writeLines("\\end{tabular}", con)
  writeLines("\\end{table}", con)

  message("Wrote: ", csv_path)
  message("Wrote: ", pdf_path)
  message("Wrote: ", png_path)
  message("Wrote: ", tex_path)

  invisible(out)
}

# Quick start
alpha_spending_results <- make_current_alpha_spending_plots()
print(alpha_spending_results)
