# CEVAM phase II monitoring code

This repository contains R code for boundary construction and operating-characteristic simulations for **calibrated e-value monitoring (CEVAM)** in benchmark-based early-phase oncology trials.

The code reproduces the boundary tables and simulation studies used for the manuscript:

> Likelihood-Ratio E-Value Monitoring for Benchmark-Based Decisions in Early-Phase Oncology Trials

## Files

| File | Purpose |
|---|---|
| `phase2_design_core.R` | Core functions for BOP2-F, BOP2-FE, CEVAM boundary construction, exact operating-characteristic calculations, workbook I/O, and simulation helpers. |
| `generate_phase2_boundaries.R` | Generates pretabulated boundary workbooks. This is the main entry point for boundary construction. |
| `simulate_phase2_from_boundaries.R` | Reads a boundary workbook and evaluates operating characteristics. It does not recalibrate boundaries. |
| `simulate_phase2_optional_monitoring.R` | Operational-flexibility simulation for additional reviews and optional continuation. |
| `examples/trend_binary_boundaries.R` | Standalone binary CEVAM boundary calculation for the TREND trial application. |

## Requirements

The scripts require R and the `openxlsx` package.

```r
install.packages("openxlsx")
```

No package installation is otherwise required; scripts can be run directly with `source()` or `Rscript`.

## Quick start

Generate the main boundary workbook using the default adaptive grid search:

```bash
Rscript generate_phase2_boundaries.R --out=results/phase2_boundaries.xlsx
```

Run the main simulation from the generated workbook:

```bash
Rscript simulate_phase2_from_boundaries.R \
  --xlsx=results/phase2_boundaries.xlsx \
  --out=results/phase2_simulation_results.xlsx \
  --nsim=10000
```

Run exact operating-characteristic evaluation instead of Monte Carlo where supported:

```bash
Rscript simulate_phase2_from_boundaries.R \
  --xlsx=results/phase2_boundaries.xlsx \
  --out=results/phase2_simulation_results_exact.xlsx \
  --use_exact=true
```

## Choosing methods for boundary construction

Some complex-endpoint methods, especially joint likelihood-ratio variants, can take time to calibrate.  Boundary generation therefore allows method selection.

Fast boundary generation for the main paper methods only:

```bash
Rscript generate_phase2_boundaries.R \
  --out=results/phase2_boundaries_main.xlsx \
  --methods_binary=BOP2-F,BOP2-FE-1,BOP2-FE-2,CEVAM-NT,CEVAM-T \
  --methods_projected=BOP2-F,BOP2-FE-1,BOP2-FE-2,CEVAM-NT,CEVAM-T \
  --include_component_specific=false \
  --include_joint_lr=false
```

Include the component-specific CEVAM sensitivity design:

```bash
Rscript generate_phase2_boundaries.R \
  --out=results/phase2_boundaries_component_specific.xlsx \
  --methods_projected=BOP2-F,BOP2-FE-1,BOP2-FE-2,CEVAM-NT,CEVAM-T,CEVAM-CS-T
```

Include the joint likelihood-ratio sensitivity designs only when needed:

```bash
Rscript generate_phase2_boundaries.R \
  --out=results/phase2_boundaries_with_jlr.xlsx \
  --methods_projected=BOP2-F,BOP2-FE-1,BOP2-FE-2,CEVAM-NT,CEVAM-T,CEVAM-JLR,CEVAM-JLR-T
```

Available binary methods:

```text
BOP2-F, BOP2-FE-1, BOP2-FE-2, CEVAM-NT, CEVAM-T, CEVAM-Mix-NT, CEVAM-Mix-T
```

Available projected/complex-endpoint methods:

```text
BOP2-F, BOP2-FE-1, BOP2-FE-2, CEVAM-NT, CEVAM-T,
CEVAM-CS-T, CEVAM-JLR, CEVAM-JLR-T, CEVAM-Mix-NT, CEVAM-Mix-T
```

## Reproducibility settings

The default boundary script uses `default_search_grids(search_mode = "adaptive")`, which is faster than the exhaustive paper grid.  To use the exhaustive grid:

```bash
Rscript generate_phase2_boundaries.R \
  --out=results/phase2_boundaries_paper_grid.xlsx \
  --search_mode=paper
```

Parallel execution can be controlled with:

```bash
Rscript generate_phase2_boundaries.R \
  --out=results/phase2_boundaries.xlsx \
  --n_cores=8 \
  --parallel_examples=true
```

When `parallel_examples=true`, the scripts avoid using many inner grid-search workers by default to reduce oversubscription.

## TREND trial binary application

The TREND trial application can be reproduced using:

```bash
Rscript examples/trend_binary_boundaries.R
```

The default settings are:

```text
p0 = 0.41, p1 = 0.56, N = 65, looks = 44 and 65,
reported result: n = 44, x = 30 pCRs.
```

## Notes

- `simulate_phase2_from_boundaries.R` does not recalibrate boundaries. It only reads the precomputed workbook.
- `simulate_phase2_optional_monitoring.R` uses a clearly labeled carry-forward sensitivity rule for planned-look posterior-cutoff designs under extra reviews. The CEVAM anytime-valid rule recomputes fixed-threshold e-process boundaries at each actual review time.
- Generated Excel and CSV outputs are ignored by `.gitignore` under `results/`.
