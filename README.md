# BulletAnalyzr Quick Start Guide

## What is BulletAnalyzr?

BulletAnalyzr is a forensic tool that uses 3D imaging and advanced algorithms to compare bullets and determine if they were fired from the same gun. It's **free, open-source**, and currently in **alpha testing** (research and lab testing only - not yet validated for casework).

### Key Features

- Interactive 3D visualization of bullet scans
- Automated crosscut and groove detection
- Manual refinement controls with intuitive sliders
- Comprehensive comparison reports
- Works with x3p format 3D scans

## Installation (60-80 minutes total)

### 1. Install Required Software (20-30 minutes)

1. Install **R** from https://cran.r-project.org/
2. Install **RStudio** from https://posit.co/download/rstudio-desktop/
3. Download **BulletAnalyzr** from https://github.com/CSAFE-ISU/bulletAnalyzr
   - Click the green "Code" button then "Download ZIP"
   - Unzip the downloaded file anywhere on your computer

### 2. Install R Packages (45-50 minutes)

1. Open RStudio
2. In the Console (after the `>` symbol), paste and run:

```r
# Install packages from CRAN
cran_packages <- c("bsicons", "bslib", "curl", "devtools", "dplyr", "DT", 
                   "ggplot2", "pagedown", "randomForest", "rgl", "sessioninfo", 
                   "shiny", "shinyBS", "shinycssloaders", "shinyjs")

for (pkg in cran_packages) {
  install.packages(pkg)
}

# Install packages from GitHub
github_packages <- c("heike/bulletxtrctr", "heike/x3ptools")

for (pkg in github_packages) {
  devtools::install_github(pkg)
}
```

## Launch BulletAnalyzr

1. Navigate to your unzipped `bulletAnalyzr-main` folder
2. Double-click `rstudio.Rproj` to open the project in RStudio
3. In RStudio's Files tab, open the `app` folder
4. Click `server.R` to open it
5. Click **Run App** in the top-right of the editor
6. Click **Begin** on the home screen

## Basic Workflow (15-20 minutes with example data)

### Step 1: Upload Bullets

1. Click **Browse** and navigate to `bulletAnalyzr-main/examples/Hamby-44/Barrel 1/Bullet 1`
2. Select all 6 `.x3p` files (each is one land engraved area)
3. Give the bullet a name (e.g., "Bullet 1")
4. Click **Add Bullet to Comparison List**
5. Repeat for Bullet 2 (`bulletAnalyzr-main/examples/Hamby-44/Barrel 1/Bullet 2`)

### Step 2: Adjust Crosscut Locations

Crosscuts appear as light grey lines on the 3D bullet renderings.

1. Select Bullet 1 from the drop-down
2. Use sliders to adjust crosscut positions if needed
3. Click **Finalize Crosscut**
4. Repeat for Bullet 2
5. Click **Compare Bullets**

### Step 3: Adjust Groove Placements

Grooves need to be removed before analysis.

1. Select Bullet 1 and Land 1 from the drop-downs
2. Red vertical lines show groove locations on the crosscut profile plot
3. Adjust sliders to keep as much land as possible while removing grooves
4. Click **Save Grooves**
5. Repeat for all lands on both bullets (12 total)
6. Click **Next Step**

### Step 4: Review Results

The report shows:

- **Phase test score** and **probability of false identification** (top)
- **Bullet-to-bullet score matrix** (0 = no similarity, 1 = perfect match)
- **Land-to-land score matrix** showing individual land comparisons
- Profile plots and signal plots for each land
- Detailed feature comparisons for top matches

Click **Download Report** to save a copy.

## Important Usage Notes

⚠️ **Not for casework**: BulletAnalyzr is in alpha testing and has not been validated for use in casework or court testimony. Use only for:

- Research purposes
- Internal lab testing and evaluation

## Citation

If you use BulletAnalyzr in research, please cite:

> Eric Hare, Heike Hofmann, Alicia Carriquiry. "Algorithmic approaches to match degraded land impressions." *Law, Probability and Risk*, Volume 16, Issue 4, December 2017, Pages 203-221. https://doi.org/10.1093/lpr/mgx018

> Eric Hare, Heike Hofmann, and Alicia Carriquiry. "Automatic Matching of Bullet Land Impressions." *The Annals of Applied Statistics*, Volume 11, Number 4, 2017, pp. 2332-56. http://www.jstor.org/stable/26362188

## License

GPL-3 (free use, modification, and distribution)

## Support

For more detailed instructions, see the full [user guide](docs/bulletAnalyzr-user-guide.pdf)