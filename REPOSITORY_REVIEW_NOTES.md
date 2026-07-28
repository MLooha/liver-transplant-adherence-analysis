# Repository review notes

## Files prepared

- `liver_transplant_adherence_analysis_repository.R`: cleaned repository version of the analysis script.
- `README_repository.md`: repository structure, run instructions, and reproducibility guidance.
- `.gitignore_repository`: recommended exclusions for a public repository.

## Critical checks before release

### 1. Confirm item scoring keys

The supplied script did not contain explicit reverse-keying rules for ATQ or NEO-FFI items. The cleaned script therefore makes the assumption explicit through:

```r
ITEMS_ALREADY_KEYED <- TRUE
ATQ_REVERSE_ITEMS <- character()
NEO_REVERSE_ITEMS <- character()
```

Before release, verify that all item variables in the analytic SPSS file are already coded in the intended scoring direction. If they are raw questionnaire responses, enter the exact reverse-keyed item names and set `ITEMS_ALREADY_KEYED <- FALSE`. Do not publish score reconstruction as reproducible until this has been verified against the instrument manual and the final codebook.

### 2. Verify the ATQ cutoff of 75

The classification outcome is defined as `ATQ_Total_Pct >= 75`. Confirm that this cutoff is supported by the instrument scoring guidance or was prespecified in the protocol. Otherwise, describe the classification analysis as an exploratory dichotomization and avoid calling the category “established.”

### 3. Match the manuscript and code exactly

The current repository configuration uses:

- 5 outer folds and 10 outer repeats;
- 5 inner folds and 1 inner repeat;
- 5,000 cluster-bootstrap repetitions;
- a fixed classification probability threshold of 0.50;
- SHAP analysis enabled;
- no item-level CFA;
- no decision-curve analysis.

These values and analyses must match the Statistical Analysis section, Results, tables, figures, and supplementary material. Change the code or manuscript before release if the final reported analysis differs.

### 4. Do not upload participant-level material

Do not commit any of the following to a public repository:

- the SPSS dataset or any extracted participant-level dataset;
- workbooks containing record-level predictions, observed outcomes, duplicate-cluster membership, source-row numbers, or participant identifiers;
- fitted workflow objects (`.rds`), because fitted recipes or models may retain training information;
- local logs containing absolute paths or user names;
- temporary, cached, or serialized R objects.

The cleaned script sets `EXPORT_ROW_LEVEL_OUTPUTS <- FALSE` and `SAVE_FITTED_MODELS <- FALSE` by default.

### 5. Interpret algorithm selection cautiously

The script compares candidate algorithms using outer cross-validation and then identifies the model with the best mean outer performance. Performance for all candidate models should be reported. The performance estimate of the selected winner may be optimistic because the same outer results are used to select and describe the winning algorithm. Avoid presenting the chosen algorithm as externally validated or clinically deployable.

### 6. Cross-validation variability is not a confidence interval

The original script labelled the 2.5th and 97.5th percentiles across repeated cross-validation runs as 95% confidence limits. Repeated folds are dependent, and ten repeat-level estimates do not form a conventional confidence interval. The cleaned script relabels these values as empirical percentiles across repeats and reports the mean and standard deviation.

### 7. Failed folds must not improve apparent performance

In the original script, a model that failed in one or more outer folds could still be summarized using the remaining predictions. This can bias performance upward. The cleaned script checks prediction completeness for every model and repeat and calculates repeat-level metrics only when every record has exactly one assessment prediction in that repeat.

### 8. Use the same inner resamples across algorithms

The original script generated different inner resampling partitions for different candidate algorithms. The cleaned script generates the inner grouped folds once per outer split and reuses them across candidates, improving the fairness of algorithm comparisons.

### 9. Decision-curve analysis was removed

The original script produced a decision curve for predicting “very good adherence,” but no specific clinical action, treatment decision, harm, or benefit was defined. Without a prespecified decision context, net benefit is difficult to interpret and can be misleading. The repository version omits this analysis. Remove the decision-curve results from the manuscript unless a defensible clinical decision framework is specified.

### 10. Beta-regression boundary handling

Standard beta regression requires an outcome strictly between zero and one. The cleaned script detects exact boundary values after dividing ATQ by 100 and applies a documented finite-sample transformation only when needed. Report this transformation if the beta-regression sensitivity analysis is retained.

### 11. Adjusted-model sensitivity analysis was corrected

The original “All retained records” and “Complete cases” models were effectively identical because `lm()` performs listwise deletion. The cleaned script identifies the complete-case adjusted model as primary and compares it with two meaningful duplicate-handling analyses:

- exclusion of all duplicate-flagged records;
- retention of one deterministic record per flagged cluster.

### 12. Primary FDR display was corrected

The original primary forest plot colored results using FDR adjustment across all 40 personality-by-adherence tests, although the primary analysis separately adjusted the five NEO-versus-total-adherence tests. The cleaned plot uses the five-test primary FDR values.

## Other changes made

- Removed personal Windows paths and replaced them with repository-relative paths or environment variables.
- Removed automatic package installation.
- Removed unused packages and dormant CFA code.
- Removed duplicated main/supplementary figure copies.
- Replaced participant identifiers in machine-learning outputs with internal sequential record numbers.
- Added grouped stratification where feasible while keeping potential duplicate clusters intact.
- Added a joint HC3 Wald test for the five-trait personality block.
- Changed the delta-R-squared bootstrap to resample duplicate groups as units.
- Made score reconstruction the actual source of downstream ATQ and NEO domain scores; imported score columns are now used only for validation.
- Added package-version, session-information, run-configuration, prediction-completeness, and data-hash records.
- Changed the default font family from `Arial` to portable `sans`.

## Validation status

The revised script passed static delimiter, section-reference, path, package-reference, and privacy-pattern checks. It could not be executed end-to-end in this environment because the participant-level SPSS dataset and a local R installation with the study package versions were not available. Run the final script on the locked analytic dataset, inspect the error workbooks, compare all regenerated results with the manuscript, and create an `renv.lock` file from that successful final environment before assigning a repository release or DOI.
