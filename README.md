
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bulletAnalyzr

<!-- badges: start -->

<!-- badges: end -->

## Overview

BulletAnalyzr is an innovative tool that integrates advanced 3D imaging
technology with sophisticated algorithms to revolutionize bullet
analysis in forensics. The application streamlines the process of
comparing fired bullets to determine whether they were fired from the
same firearm. Using high-resolution surface scans, the application
automates key steps in bullet comparison while providing forensic
examiners with intuitive controls to review and adjust the analysis.

## Key Features

- **Interactive Visualization** - View 3D renderings of bullet surfaces
  (x3p format) and examine crosscut profiles with adjustable parameters
- **Automated 3D Analysis** - Processes bullet surface scans,
  automatically identifies crosscut locations, and detects groove
  boundaries
- **Manual Refinement Controls** - Fine-tune automated decisions using
  intuitive sliders at each step of the analysis
- **Comprehensive Reporting** - Generate and export detailed comparison
  reports in a professional, shareable format

BulletAnalyzr bridges the gap between sophisticated algorithmic analysis
and practical forensic workflows, making advanced bullet comparison
techniques easily accessible to examiners.

## Installation

***Total estimated time:** 1-2 hours*

**Install R** from <https://cran.r-project.org/>

**Install RStudio** from <https://posit.co/download/rstudio-desktop/>

**Download bulletAnalyzr**

- Go to <https://github.com/CSAFE-ISU/bulletAnalyzr>
- Click the green Code button and select Download Zip
  <p align="center">

  <img src="www/readme-download-bulletanalyzr.png" alt="Green code button" width="50%">
  <br> <em>Click the green Code button and select Download Zip.</em>
  </p>
- Double-click on the downloaded file to unzip it. You may save the
  unzipped folder anywhere on your computer

**Install R packages**

- Open RStudio
- Install packages from the Comprehensive R Archive Network (CRAN) and
  GitHub by copying and pasting the following lines of code into the
  console.

``` r
# Install packages from CRAN
cran_packages <- c("bsicons", "bslib", "curl", "devtools", "dplyr", "DT", "ggplot2", "pagedown", 
                   "randomForest", "rgl", "sessioninfo", "shiny","shinyBS", 
                   "shinycssloaders", "shinyjs")
for (pkg in cran_packages) {
  install.packages(pkg)
}

# Install packages from GitHub
github_packages <- c("heike/bulletxtrctr", "heike/x3ptools")
for (pkg in github_packages) {
  devtools::install_github(pkg)
}
```

## Walkthrough

***Total estimated time:** 15-20 minutes*

| Step             | Estimated Time |
|------------------|----------------|
| Launch app       | ~1 minute      |
| Upload bullets   | 1-2 minutes    |
| Adjust crosscuts | 3-5 minutes    |
| Adjust grooves   | 3-5 minutes    |
| View results     | 5-10 minutes   |

The app includes 3d scans from the Hamby-Brundage bullet set \#44
provided by CSAFE so you can practice the workflow. Here’s a
step-by-step example:

### Launch the app (~1 minute)

- Open RStudio
- Open the BulletAnalyzr R project
  - Click Open Project in the top-right corner
  - Select the bulletAnalyzer-main folder that you unzipped during
    installation
  - Select the RStudio project File named rstudio.Rproj
- In the bottom-right panel of RStudio, navigate to the Files tab and
  double-click the app folder to open it
  <p align="center">

  <img src="www/readme-app-folder.png" alt="App folder" width="50%">
  <br> <em>Navigate to the app folder and double-click to open.</em>
  </p>
- Open the server.R file
- Click Run App
  <p align="center">

  <img src="www/readme-run-app.png" alt="Run app" width="50%"> <br>
  <em>Click Run App to launch the app.</em>
  </p>
- The start page will appear
- Click Begin
  <p align="center">

  <img src="www/readme-welcome.png" alt="Begin button" width="80%"> <br>
  <em>Click the Begin button.</em>
  </p>

### Upload the bullets (1-2 minutes)

- Upload the first bullet.
  - Click Browse and navigate to:
    bulletAnalyzr-main/examples/Hamby-44/barrel 1/Bullet 1
  - Select all 6 files in this folder. Each x3p file is an image of a
    bullet land engraved area.
  - Give the bullet a name (e.g., Bullet 1).
  - Add it to the Comparison List.
- Upload the second bullet.
  - Repeat the same process for the Bullet 2 images:
    bulletAnalyzr-main/examples/Hamby-44/barrel 1/Bullet 2.

### Adjust the crosscut location (3-5 minutes)

BulletAnalyzr attempts to identify suitable crosscut locations. The
crosscuts are displayed as light grey lines on the lands.

- Adjust the crosscuts for Bullet 1
  - Select Bullet 1 from the drop-down menu if it isn’t already selected
  - Use the sliders to adjust the crosscuts if needed.
  - Click Finalize Crosscut when satisfied.
- Adjust the crosscuts for Bullet 2
  - Select Bullet 2 from the drop-down menu.
  - Use the sliders to adjust the crosscuts if needed.
  - Click Finalize Crosscut when satisfied.
- When both bullets are ready, click Compare Bullets.

### Adjust the groove placements (3-5 minutes)

In order to capture the full land, the scans also contain parts of the
grooves. BulletAnalyzr needs to remove the grooves before further
processing. The app attempts to locate the grooves on the crosscut
profile, but manual adjustment is sometimes required.

- Adjust the groove on land 1 of Bullet 1.
  - Select Bullet 1 and Land 1 from the drop-down menus.
  - The vertical red lines on the crosscut profile plot indicate the
    left and right groove locations. Everything to the left of the left
    groove line and everything to the right of the right groove line
    will be discarded.
    <p align="center">

    <img src="www/readme-grooves1.png" alt="Groove locations" width="50%">
    <br> <em>The vertical red lines on the crosscut profile plot
    indicate the left and right groove locations.</em>
    </p>
  - Adjust the groove locations using the slider bars to keep as much of
    the land as possible.  
    <p align="center">

    <img src="www/readme-grooves2.png" alt="Adjust groove locations" width="50%">
    <br> <em>Adjust the left and right groove locations if needed.</em>
    </p>
  - Click Save Grooves when satisfied.
- Repeat for all lands on both bullets.
- Once grooves are defined, click Next Step.

### Comparison results report (5-10 minutes)

- The output page will display the comparison results.
- Each section at the bottom can be expanded to show more detailed
  information.
- Click Download Report to download a copy of the report.

## License

BulletAnalyzr is released under the GPL-3 license, allowing free use,
modification, and distribution of the software.

## Citation

If you use BulletAnalyzr in your work, please cite the following
publications:

Eric Hare, Heike Hofmann, Alicia Carriquiry. Algorithmic approaches to
match degraded land impressions, *Law, Probability and Risk*, Volume 16,
Issue 4, December 2017, Pages 203–221,
<https://doi.org/10.1093/lpr/mgx018>

Eric Hare, Heike Hofmann and Alicia Carriquiry. Automatic Matching of
Bullet Land Impressions, *The Annals of Applied Statistics*, Volume 11,
Number 4, 2017, pp. 2332–56. JSTOR,
<http://www.jstor.org/stable/26362188>.
