# BI-SCUDO Analysis and Benchmarking

This repository contains supporting analysis and benchmarking scripts for evaluating rank-based methods on high-dimensional omics and single-cell RNA-seq data.

> [!IMPORTANT]
> This is an independent academic analysis repository, not an official BI-SCUDO software distribution. The BI-SCUDO MATLAB implementation is intentionally **not included** because permission to publish or redistribute it has not been granted. This repository does not provide, reconstruct, or grant a licence to that source code.

## Scope

The scripts in this repository support three parts of the analysis:

- regression benchmarking under varying sample sizes, feature counts, and noise levels;
- cell-type classification on a healthy peripheral blood mononuclear cell dataset;
- comparison of selected biomarkers with external marker-identification methods.

BI-SCUDO outputs were compared with baseline approaches such as LASSO and Random Forest. Predictive performance and feature recovery were evaluated using metrics including validation \(R^2\), classification accuracy, and Jaccard similarity.

## Repository contents

| File | Purpose |
| --- | --- |
| `pbmc3k_analysis.Rmd` | Single-cell quality control, normalization, dimensionality reduction, clustering, annotation, and marker analysis |
| `RF_classifier.py` | Random Forest cell-type classification, validation metrics, confusion matrices, and Kneedle-based feature selection |
| `LASSO.R` | LASSO regression benchmarking and feature selection |
| `Random_Forest.R` | Random Forest regression benchmarking and feature-importance extraction |
| `compare_benchmark .R` | Aggregation and comparison of regression benchmark results |
| `jaccard_score.R` | Jaccard-based comparison of recovered feature sets |
| `compare_with_acc.R` | Biomarker comparison with Cell Marker Accordion and related visualizations |

## Code and data availability

This repository includes only the supporting analysis scripts. It does **not** include:

- the BI-SCUDO MATLAB source code;
- BI-SCUDO executables or a reimplementation;
- restricted BI-SCUDO input or output files;
- the complete datasets or locally generated intermediate results.

The repository therefore cannot reproduce the complete BI-SCUDO workflow by itself. Full reproduction requires authorized access to the original implementation and the corresponding datasets and outputs.

Some scripts preserve absolute file paths from the original analysis environment. Replace these paths with locations appropriate for your system before running the analyses.

## Dependencies

### R

The R analyses use packages including:

- `caret`
- `cellmarkeraccordion`
- `doParallel`
- `dplyr`
- `ggbeeswarm`
- `ggplot2`
- `ggrepel`
- `glmnet`
- `patchwork`
- `purrr`
- `randomForest`
- `Seurat`
- `stringr`
- `tidyverse`

### Python

The Python analysis uses:

- `kneed`
- `matplotlib`
- `mlxtend`
- `numpy`
- `pandas`
- `scikit-learn`

## General workflow

1. Prepare the single-cell or simulated benchmark datasets.
2. Update the dataset and output paths in the relevant scripts.
3. Run the LASSO and Random Forest benchmarks.
4. Run the authorized BI-SCUDO workflow separately.
5. Place only the permitted result files in the expected analysis directories.
6. Use the comparison scripts to evaluate predictive performance and feature overlap.

## Limitations

- The restricted BI-SCUDO implementation is not distributed here.
- The scripts require local path configuration and are not currently packaged as a single automated pipeline.
- Complete reproduction depends on external data and authorized BI-SCUDO outputs.
- Results from a single dataset or run should not be interpreted as broad validation.

## Related work

The original SCUDO method was introduced in:

> Lauria M, Moyseos P, Priami C. *SCUDO: a tool for signature-based clustering of expression profiles.* Nucleic Acids Research. 2015;43(W1):W188-W192. [https://doi.org/10.1093/nar/gkv449](https://doi.org/10.1093/nar/gkv449)

A related open-source R implementation is available through [Bioconductor](https://bioconductor.org/packages/rScudo/) and [GitHub](https://github.com/Matteo-Ciciani/rScudo). It is separate from the unpublished BI-SCUDO MATLAB implementation used with these analyses.

## Disclaimer

This repository documents supporting analysis work and is not affiliated with or endorsed as an official BI-SCUDO release. All rights in the unpublished BI-SCUDO implementation remain with its respective authors or rightsholders.
