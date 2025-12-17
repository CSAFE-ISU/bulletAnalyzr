# Manual Groove Selection Procedure

## Overview

This document describes the procedure for manually selecting and recording groove locations from bullet x3p scan files. The collected data will be used to evaluate and improve the automatic groove detection algorithm.

## Purpose

The BulletAnalyzr application automatically detects groove locations, but these automatic selections may not always be optimal. By manually reviewing and recording groove locations across a dataset, we can:

1. Create a ground truth dataset for algorithm evaluation
2. Identify patterns in when/why automatic detection fails
3. Potentially improve the automatic detection algorithm
4. Understand the variability in groove locations

## Tool Description

The `manual_groove_selection.R` script provides an interactive tool that:

1. Reads an x3p bullet scan file
2. Extracts a crosscut profile at an optimal location
3. Displays the profile with automatically detected groove locations
4. Allows you to accept the automatic values or enter manual corrections
5. Saves all selections to a CSV file with metadata

## CSV Output Format

The output CSV file contains the following columns:

- `filename`: Name of the x3p file (e.g., "Land1.x3p")
- `filepath`: Full path to the x3p file
- `left_groove`: Position of the left groove boundary (microns)
- `right_groove`: Position of the right groove boundary (microns)
- `crosscut_y`: Y-coordinate where the crosscut was taken (microns)
- `manual_selection`: TRUE if manually adjusted, FALSE if automatic values accepted
- `timestamp`: When this entry was recorded

## Installation

The script uses packages already in the BulletAnalyzr dependency list:

```r
# These should already be installed if you've installed BulletAnalyzr
library(x3ptools)
library(bulletxtrctr)
library(ggplot2)
```

## Usage

### Option 1: Interactive Mode in RStudio

This is the recommended approach for careful manual review:

```r
# Open RStudio and source the script
source("inst/scripts/manual_groove_selection.R")

# Process a single file
process_file("examples/Hamby-44/Barrel_1/Bullet_1/Land1.x3p", "groove_data.csv")

# Process an entire directory (all x3p files in a folder)
process_directory("examples/Hamby-44/Barrel_1/Bullet_1/", "groove_data.csv")
```

### Option 2: Command Line Mode

For batch processing or scripting:

```bash
# Process a single file
Rscript inst/scripts/manual_groove_selection.R examples/Hamby-44/Barrel_1/Bullet_1/Land1.x3p groove_data.csv

# Process all files in a directory
Rscript inst/scripts/manual_groove_selection.R examples/Hamby-44/Barrel_1/Bullet_1/ groove_data.csv
```

## Workflow for Data Collection

### Step 1: Organize Your Data

Decide which bullet scans you want to review. For example:
- All bullets in `examples/Hamby-44/`
- A specific subset of problematic scans
- A random sample for validation

### Step 2: Run the Tool

```r
# In RStudio
source("inst/scripts/manual_groove_selection.R")

# Process your target directory
process_directory("examples/Hamby-44/Barrel_1/Bullet_1/", "groove_selections.csv")
```

### Step 3: Review Each File

For each x3p file, the tool will:

1. Display a plot showing the crosscut profile
2. Show red dashed lines indicating automatic groove locations
3. Prompt you for input

You have three options:

- **Press ENTER**: Accept the automatic groove locations (fastest option)
- **Enter `left,right`**: Manually specify groove positions in microns (e.g., `450,2800`)
- **Enter `s`**: Skip this file entirely

### Step 4: Guidelines for Manual Selection

When deciding whether to manually adjust grooves:

**Accept automatic values when:**
- The grooves appear correctly placed at the transition between land and groove
- Both grooves are clearly identified
- The land area looks complete and reasonable

**Manually adjust when:**
- A groove line cuts into obvious land area
- A groove line is in the middle of a groove instead of at the boundary
- One or both groove lines are clearly misplaced
- The automatic detection seems to have failed

**Visual tips:**
- The land area (between grooves) should look relatively flat with striations
- The groove areas (outside the red lines) should show steep slopes or deeper cuts
- Try to maximize the land area while excluding the grooves

### Step 5: Working in Batches

The script appends to the CSV file, so you can work in sessions:

```r
# Day 1: Process first bullet
process_directory("examples/Hamby-44/Barrel_1/Bullet_1/", "groove_data.csv")

# Day 2: Process second bullet (appends to same file)
process_directory("examples/Hamby-44/Barrel_1/Bullet_2/", "groove_data.csv")

# Continue for additional bullets...
```

If you need to re-process a file (e.g., you made a mistake), just run it again - the script will replace the old entry for that filename.

## Example Session

```r
source("inst/scripts/manual_groove_selection.R")
process_directory("examples/Hamby-44/Barrel_1/Bullet_1/", "hamby44_grooves.csv")
```

Output:
```
Found 6 x3p files
Output will be saved to: hamby44_grooves.csv

[ 1 / 6 ]
=== Processing: Land1.x3p ===
Finding optimal crosscut location...
Crosscut location: 2458.3
Automatic groove locations: 423.5 , 2876.2

--- Manual Groove Selection ---
Automatic left groove: 423.5
Automatic right groove: 2876.2

Options:
  1. Press ENTER to accept automatic grooves
  2. Enter custom values as: left,right (e.g., 500,2500)
  3. Enter 's' to skip this file

Your choice: [ENTER pressed]

Saved to hamby44_grooves.csv
Left groove: 423.5
Right groove: 2876.2

[ 2 / 6 ]
=== Processing: Land2.x3p ===
...
```

## Data Analysis

After collecting groove locations, you can analyze the results:

```r
# Load the collected data
groove_data <- read.csv("groove_data.csv")

# Summary statistics
summary(groove_data)

# How many required manual adjustment?
table(groove_data$manual_selection)

# Distribution of groove positions
hist(groove_data$left_groove)
hist(groove_data$right_groove)

# Compare automatic vs manual adjustments
manual_only <- groove_data[groove_data$manual_selection == TRUE, ]
```

## Troubleshooting

**Problem**: Plot window doesn't appear
- **Solution**: Make sure you're in RStudio or have X11/graphics device configured

**Problem**: "Package not found" error
- **Solution**: Install missing packages: `install.packages(c("x3ptools", "bulletxtrctr", "ggplot2"))`

**Problem**: Script crashes on a specific file
- **Solution**: Note the filename and skip it. You can always process it separately later.

**Problem**: I made a mistake on a file
- **Solution**: Just run `process_file()` again on that file - it will overwrite the previous entry.

## Tips for Efficient Data Collection

1. **Work in good lighting** - You need to clearly see the plot details
2. **Take breaks** - Groove selection requires attention to detail
3. **Be consistent** - Develop criteria for what "correct" looks like and stick to it
4. **Document problems** - Keep notes about any files that were particularly difficult
5. **Batch by source** - Process all lands from one bullet together (they often have similar characteristics)

## Questions to Discuss with Stephanie

Before starting the data collection, consider:

1. **Sample size**: How many bullets/lands should we process?
2. **Data sources**: Which datasets should we prioritize? (Hamby-44, Houston, others?)
3. **Criteria**: What specific guidelines should we follow for "correct" groove placement?
4. **Quality control**: Should we have multiple people annotate the same files for inter-rater reliability?
5. **Edge cases**: How should we handle damaged bullets, missing grooves, or unclear boundaries?

## Next Steps

Once data is collected:

1. Share the CSV file with the team
2. Analyze patterns in manual vs automatic selections
3. Identify systematic biases or failure modes in automatic detection
4. Use the data to refine the `cc_locate_grooves()` algorithm or adjust parameters
5. Consider creating a validation test set for future algorithm improvements
