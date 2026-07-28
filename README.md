# Liver transplant adherence analysis

This repository contains the R code used to reconstruct questionnaire scores and conduct the inferential, sensitivity, and exploratory prediction analyses for the liver transplant adherence study.

## Repository contents

```text
.
├── liver_transplant_adherence_analysis_repository.R
├── README.md
├── renv.lock
├── data/
│   └── README_data_access.md
└── results/                 # generated locally; not committed
```

The participant-level dataset is not included in the public repository.

## Required input

The script expects the analytic SPSS file at:

```text
data/Nazari_Liver_Transplant_Clean.sav
```

A different location can be supplied without editing the script:

```r
Sys.setenv(ANALYSIS_DATA_FILE = "path/to/analytic_file.sav")
Sys.setenv(ANALYSIS_OUTPUT_DIR = "path/to/results")
source("liver_transplant_adherence_analysis_repository.R")
```

## Reproducibility

Run the analysis from the repository root in the R environment recorded by `renv.lock`. The script records the random seed, input-file SHA-256 hash, package versions, session information, resampling configuration, and generated output inventory.

Before the final repository release:

1. verify the questionnaire scoring keys and the ATQ cutoff;
2. run the script on the locked final analytic dataset;
3. reconcile every regenerated estimate, table, and figure with the manuscript;
4. inspect model and resampling error logs;
5. create or update `renv.lock` from the successful final run;
6. confirm that no participant-level data or fitted model objects are tracked by Git.

## Data confidentiality

The repository must not include the SPSS dataset, participant identifiers, source-row numbers, duplicate-cluster details, row-level predictions, or fitted workflow objects. Public repository runs use:

```r
EXPORT_ROW_LEVEL_OUTPUTS <- FALSE
SAVE_FITTED_MODELS <- FALSE
```

## Suggested manuscript statement

> The complete R analysis script used for questionnaire score reconstruction, inferential analyses, sensitivity analyses, and exploratory grouped nested cross-validation is available at [VERIFIED REPOSITORY URL]. Participant-level data are not publicly available because of confidentiality restrictions but may be available from the corresponding author, subject to institutional approval and an appropriate data-use agreement.

## Scope of the prediction analyses

The machine-learning analyses are exploratory and use grouped nested repeated cross-validation. Potential duplicate clusters remain in the same resampling partition. The repository does not provide a clinically deployable model and does not constitute external validation.
