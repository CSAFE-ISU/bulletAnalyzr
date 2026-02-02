# Developer Scripts

Utility and pipeline scripts for bullet scan analysis.

## Root

| Script | Description |
|--------|-------------|
| `view-pipeline.R` | Interactive visualization of each bullet analysis step (read, preprocess, crosscut, grooves, signal). Supports manual groove selection and reading groove/crosscut values from a CSV. |
| `parse-filenames.R` | Extracts standardized bullet codes from scan filenames across datasets (Hamby, Houston, CTS, DFSC, Phoenix, etc.). |
| `data-wrangling.R` | Loads and validates data structures from bullet comparison pipeline snapshots to understand workflow stages. |
| `bullet-scan-rotation-script.R` | Rotates all x3p scans in a directory by a specified angle (default 90°). |
| `dat-to-x3p.R` | Reads space-delimited `.dat` files from a directory and converts each into an x3p object. |

## bullet-studies/

| Script | Description |
|--------|-------------|
| `list_bullet_scans.R` | Inventories x3p scans from the LSS shared drive, organizing them by study and producing CSV metadata files. |

## comparisons/

| Script | Description |
|--------|-------------|
| `auto-bullet-comparison-pipeline.R` | Automated end-to-end pipeline comparing two bullets (read, rotate, preprocess, crosscut, grooves, signal, align, features, match score). |
| `manual-bullet-comparison-pipeline.R` | Interactive pipeline comparing two bullets using manually-specified groove locations from CSV files, with parallel processing support. |
| `compare-example-hamby44.R` | Example showing how to run the manual comparison pipeline on Hamby Set 44 scans. |
| `compare-houston.R` | Compares all bullet pairs from the Houston Set Final dataset. |
