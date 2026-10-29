# CLRM reproducibility materials (Statistics in Medicine manuscript 1559798)

These files support the revised manuscript, "Likelihood-ratio monitoring for phase II oncology trials with finite-horizon calibration and trial-level guarantees" (Masahiro Kojima).

CLRM is the manuscript name of the current method. Some R function names, internal identifiers, and output labels retain the former name **CEVAM**. They are preserved to avoid changing the computational implementation.

## Contents

- `cevam_current_core.R`: exact binary calibration and operating-characteristic calculations, and comparator calculations.
- `run_main_analysis.R`: anchor and 34 additional binary configurations; exact calculation and optional Monte Carlo validation.
- `run_trend_analysis.R`: published TREND aggregate-results illustration.
- `run_supplement_sensitivity.R`: selected sensitivity analyses.
- `run_supplement_complex.R`: multicomponent analyses.
- `run_supplement_joint_mc.R`: joint-information Monte Carlo sensitivity analysis.
- `run_supplement_adaptation.R`: conditional-error and conditional-power extensions.
- `run_review_priority.R`: additional reviewer-requested fixed-threshold and prior-sensitivity calculations.
- `make_current_alpha_spending_plots_corrected_CLRM.R`: cumulative rejection-probability figure. The copied script points to the local `results_current/` directory.
- `results_current/`: stored results and numerical outputs from the underlying analysis.
- `review_results/review_priors_all.csv`: 35 scenarios x two BOP2-FE designs x two priors (140 rows); contains exact operating characteristics and selected stopping-count boundaries.
- `review_results/review_fixed_e_all.csv`: fixed-threshold likelihood-ratio e-process results in 35 scenarios.

## Run from the package root

Open a Terminal in this directory (or set `setwd()` to it in R). Example commands:

```bash
Rscript run_main_analysis.R
Rscript run_trend_analysis.R
Rscript run_supplement_sensitivity.R
Rscript run_supplement_complex.R
Rscript run_supplement_joint_mc.R
Rscript run_supplement_adaptation.R
Rscript run_review_priority.R --mode=all --cores=4
Rscript make_current_alpha_spending_plots_corrected_CLRM.R
```

Running all analyses can be computationally demanding, and the programs may generate additional output folders.

**Important:** `run_review_priority.R` overwrites its consolidated `review_results/review_priors_all.csv` and `review_results/review_fixed_e_all.csv` when executed. Use a copy of this package for computational checks if you wish to preserve the archived outputs. In particular, a run with `--subset=anchor` replaces the corresponding consolidated results with the anchor subset; do not run it directly in the release folder.

This package was assembled from existing R sources and computed CSV outputs. Full end-to-end execution and exact agreement of every output should be verified before labeling a GitHub/Zenodo release as fully reproducible. No individual patient-level data are included.
