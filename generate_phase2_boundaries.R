# generate_phase2_boundaries.R
#
# GitHub-friendly boundary generator for the CEVAM phase II paper.
# This script does not auto-run when sourced.  It can be used either as
#   source("generate_phase2_boundaries.R")
# or from the command line:
#   Rscript generate_phase2_boundaries.R --out=results/phase2_boundaries.xlsx
#
# Slow complex-endpoint methods can be selected explicitly, e.g.
#   Rscript generate_phase2_boundaries.R \
#     --methods_projected=CEVAM-NT,CEVAM-T \
#     --include_component_specific=false \
#     --include_joint_lr=false

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
.phase2_as_methods <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
}
.phase2_as_pair_ids <- function(x, default = 1L) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.integer(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
}

filter_example1_pairs <- function(settings, example1_pair_ids = 1L) {
  example1_pair_ids <- as.integer(example1_pair_ids)
  if (length(example1_pair_ids) > 0L && !any(is.na(example1_pair_ids))) {
    settings$ex1_pairs <- settings$ex1_pairs[vapply(settings$ex1_pairs, function(z) z$pair_id %in% example1_pair_ids, logical(1L))]
    if (length(settings$ex1_pairs) == 0L) stop("No Example 1 pairs remain after applying example1_pair_ids.", call. = FALSE)
  }
  settings
}

generate_phase2_boundary_workbook <- function(
  xlsx_path = file.path("results", "phase2_boundaries.xlsx"),
  settings = paper_settings(alpha = 0.10, beta = 0.10),
  grids = default_search_grids(search_mode = "adaptive"),
  n_cores = phase2_default_n_cores(max_cores = 8L),
  show_progress = TRUE,
  progress_every = 100L,
  example1_pair_ids = 1L,
  methods_binary = c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T"),
  methods_projected = c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T"),
  include_mixture = FALSE,
  include_component_specific = FALSE,
  include_joint_lr = FALSE,
  parallel_examples = FALSE,
  n_example_cores = NULL,
  inner_n_cores = NULL
) {
  settings <- filter_example1_pairs(settings, example1_pair_ids)
  out_dir <- dirname(xlsx_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  boundaries <- build_all_boundaries(
    settings = settings,
    grids = grids,
    n_cores = n_cores,
    show_progress = show_progress,
    progress_every = progress_every,
    methods_binary = methods_binary,
    methods_projected = methods_projected,
    include_mixture = include_mixture,
    include_component_specific = include_component_specific,
    include_joint_lr = include_joint_lr,
    parallel_examples = parallel_examples,
    n_example_cores = n_example_cores,
    inner_n_cores = inner_n_cores
  )
  write_boundary_workbook(boundaries, path = xlsx_path)
  invisible(list(xlsx_path = normalizePath(xlsx_path, winslash = "/", mustWork = FALSE), boundaries = boundaries))
}

if (.phase2_running_as_main("generate_phase2_boundaries.R")) {
  opt <- .phase2_parse_cli()
  search_mode <- if (!is.null(opt$search_mode)) opt$search_mode else "adaptive"
  grids <- if (identical(search_mode, "paper")) paper_search_grids() else default_search_grids(search_mode = search_mode)
  res <- generate_phase2_boundary_workbook(
    xlsx_path = if (!is.null(opt$out)) opt$out else file.path("results", "phase2_boundaries.xlsx"),
    settings = paper_settings(alpha = as.numeric(ifelse(is.null(opt$alpha), 0.10, opt$alpha)),
                              beta  = as.numeric(ifelse(is.null(opt$beta),  0.10, opt$beta))),
    grids = grids,
    n_cores = .phase2_as_int(opt$n_cores, phase2_default_n_cores(max_cores = 8L)),
    show_progress = .phase2_as_bool(opt$show_progress, TRUE),
    progress_every = .phase2_as_int(opt$progress_every, 100L),
    example1_pair_ids = .phase2_as_pair_ids(opt$example1_pair_ids, 1L),
    methods_binary = .phase2_as_methods(opt$methods_binary, c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T")),
    methods_projected = .phase2_as_methods(opt$methods_projected, c("BOP2-F", "BOP2-FE-1", "BOP2-FE-2", "CEVAM-NT", "CEVAM-T")),
    include_mixture = .phase2_as_bool(opt$include_mixture, FALSE),
    include_component_specific = .phase2_as_bool(opt$include_component_specific, FALSE),
    include_joint_lr = .phase2_as_bool(opt$include_joint_lr, FALSE),
    parallel_examples = .phase2_as_bool(opt$parallel_examples, FALSE),
    n_example_cores = if (!is.null(opt$n_example_cores)) as.integer(opt$n_example_cores) else NULL,
    inner_n_cores = if (!is.null(opt$inner_n_cores)) as.integer(opt$inner_n_cores) else NULL
  )
  message("Wrote boundary workbook: ", res$xlsx_path)
}
