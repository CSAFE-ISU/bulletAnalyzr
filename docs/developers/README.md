# Developer Scripts

Utility and pipeline scripts for bullet scan analysis are stored in BulletAnalyzr > Docs > Developers. These scripts are intended to help developers perform bullet comparisons on large datasets, as well as perform one-off tasks such as viewing the signal for a single land. 

Because these scripts are not intended for end users, they are not located within the package structure (E.g., the R and inst folders). The Docs folder is only available to end users if they clone the GitHub repository.

## BulletAnalyzr > Docs > Developers (Root Directory)

| Script | Description |
|--------|-------------|
| `bullet-codes.R` | Extracts standardized bullet codes from scan filenames and filepaths across datasets (Hamby, Houston, CTS, DFSC, Phoenix, etc.). |
| `bullet-scan-rotation-script.R` | Rotates all x3p scans in a directory by a specified angle (default 90°). |
| `dat-to-x3p.R` | Reads space-delimited `.dat` files from a directory and converts each into an x3p object. |
| `learning-workflow.R` | Loads and compares data structures from bulletAnalyzrApp() comparison pipeline snapshots to understand workflow stages. |
| `view-pipeline.R` | Interactive visualization of each bullet analysis step (read, preprocess, crosscut, grooves, signal). Supports manual groove selection and reading groove/crosscut values from a CSV. Does not save groove locations if they are adjusted. If you want to save new groove locations, use `inst/scripts/manual_groove_selection.R`. |


## BulletAnalyzr > Docs > Developers > bullet-studies

| Script | Description |
|--------|-------------|
| `list-bullet-scans.R` | Inventories x3p scans from the LSS shared drive, organizing them by study and producing CSV metadata files. These example filepaths and filenames were used to develope the standardized bullet codes used in `bullet-codes.R` |

## BulletAnalyzr > Docs > Developers > comparisons

| Script | Description |
|--------|-------------|
| `align-signals.R` | Aligns two bullet land signals and calculates comparison features (CCF, striation marks, etc.). Supports optional groove CSV files. Includes `align_all_land_pairs()` to compare all lands of a single bullet and `plot_alignment_matrix()` to visualize results in a 6x6 grid. |
| `analyze-houston-comparisons.R` | Analyzes Houston bullet study comparison scores and generates plots replicating Vanderplas et al. 2020 Figure 6. |
| `auto-bullet-comparison-pipeline.R` | Automated end-to-end pipeline comparing two bullets with automatic crosscut and groove detection. |
| `compare-example-hamby44.R` | Example showing how to run the manual comparison pipeline on Hamby Set 44 scans. |
| `compare-houston.R` | Batch comparison script for the Houston Set Final dataset. Generates all pairwise bullet comparisons. |
| `compare-phoenix.R` | Batch comparison script for the Phoenix Test dataset. Generates all pairwise bullet comparisons. |
| `comparison-utils.R` | Shared helper functions for comparison pipelines (e.g., `cond_x3p_m_to_mum`, `make_pairs_df`, `make_outfile`, `extract_signals`, `align_signals`, `extract_features`, `calculate_rf_scores`, `calculate_bullet_scores`, `run_phase_test`). |
| `manual-bullet-comparison-pipeline.R` | Pipeline comparing two bullets using manually-specified groove locations from CSV files, with parallel processing support. |
