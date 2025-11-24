# BulletAnalyzr

BulletAnalyzr is a forensic tool that uses 3D imaging and advanced algorithms to compare bullets and determine if they were fired from the same gun. The application scans bullet surfaces at high resolution, identifying unique patterns left by the gun barrel to establish whether two bullets share a common source.

**[View the full User Guide](docs/BulletAnalyzr-user-guide.pdf)** for detailed instructions and screenshots.

## TABLE OF CONTENTS

- [Requirements](#requirements)
- [Disclaimer and Permitted Use](#disclaimer-and-permitted-use)
- [Key Features](#key-features)
- [Installation](#installation)
- [Launch BulletAnalyzr](#launch-bulletanalyzr)
- [Compare Bullets](#compare-bullets)
- [Citing BulletAnalyzr](#citing-bulletanalyzr)
- [License](#license)
- [Contact](#contact)

## REQUIREMENTS

BulletAnalyzr version 1.0.0 has the following requirements. **Failure to meet these requirements makes any potential results unreliable.**

- Bullets must be **9 mm** in caliber
- Bullets must display **right-twist** rifling
- Bullets must have exactly **6 land engraved areas**
- Bullets **cannot** have missing land engraved areas

## DISCLAIMER AND PERMITTED USE

> **Disclaimer:** The software is in testing and has not yet been fully validated. CSAFE is not liable for its accuracy or reliability.

### Permitted Uses
- ✅ **Research:** Use the software for academic or scientific research related to forensic bullet comparison
- ✅ **Testing:** Use the software for internal testing within forensic labs to evaluate its performance

### Discouraged Uses
- ❌ **Casework and Court Testimony:** The statistical model implemented by this software has not yet been applied to the full range of bullets that could be encountered in casework or investigations. Therefore, it is not ready for use in casework or court testimony at this time.

> **Note:** Users should follow the permitted use guidelines and treat findings as preliminary and unsuitable for formal conclusions.

## KEY FEATURES

- **Interactive Visualization:** Upload and view 3D renderings of your bullet scans (x3p format) and examine crosscut profiles with adjustable parameters
- **Automated Analysis:** The software automatically processes your bullet scans, identifying crosscut locations, removing grooves, extracting signals, and measuring similarities between signals
- **Manual Refinement Controls:** Fine-tune automated decisions using intuitive sliders at each step of the analysis
- **Comprehensive Reporting:** Generate and export detailed comparison reports in a professional, shareable format
- **Free and Open-Source:** BulletAnalyzr is an open-source and free-to-use tool

## INSTALLATION

**Total Estimated Time: 90-110 minutes**

**[View the full User Guide](docs/BulletAnalyzr-user-guide.pdf)** for detailed instructions and screenshots.

### Install R and RStudio

1. Install R from https://cran.r-project.org/
2. Install RStudio from https://posit.co/download/rstudio-desktop/
3. **(Windows only)** Install Rtools from https://cran.rstudio.com/bin/windows/Rtools/ (select the version corresponding to your version of R)

### Download Required Packages from GitHub

1. Go to https://github.com/heike/grooveFinder.
2. Click the green Code button and select Download Zip.
3. Unzip the downloaded file. The unzipped package can be saved anywhere on your computer. 
4. Repeat steps 1-3 for `x3ptools` (https://github.com/heike/x3ptools), `bulletxtrctr` (https://github.com/heike/bulletxtrctr), and `BulletAnalyzr` (https://github.com/CSAFE-ISU/bulletAnalyzr).

### Install Packages in RStudio

**Install CRAN packages** by running the following code in the RStudio Console:

```r
# Install packages from CRAN
cran_packages <- c("bsicons", "bslib", "curl", "devtools", "dplyr", "DT", "ggplot2", 
                   "pagedown", "randomForest", "rgl", "sessioninfo", "shiny", "shinyBS", 
                   "shinycssloaders", "shinyjs")
for (pkg in cran_packages) {
  install.packages(pkg)
}
```

**Install GitHub packages**

1. Double-click on the `.Rproj` file in downloaded folder for `grooveFinder`. This will open the package in RStudio. 
2. Click **Build > Install** in RStudio. 
3. Repeat steps 1-2 for `x3ptools`, `bulletxtrctr`, and `BulletAnalyzr`.

## LAUNCH BULLETANALYZR

Launch BulletAnalyzr by running the following code in RStudio:

```r
library(bulletAnalyzr)
bulletAnalyzrApp()
```

> **Tip:** The application includes sample 3D scans from the Hamby-Brundage bullet set #44 for practice (located in `examples/Hamby-44`).

## COMPARE BULLETS

**[View the full User Guide](docs/BulletAnalyzr-user-guide.pdf)** for detailed instructions and screenshots.

1. **Upload bullets:** Select the 6 x3p files (land engraved areas) for each bullet to compare
2. **Adjust crosscut locations:** Review and refine the automatically identified crosscut locations
3. **Adjust groove placements:** Fine-tune groove boundaries to maximize the land area retained for analysis
4. **Review results:** Examine the comparison report, including bullet-to-bullet scores, land-to-land score matrices, and statistical significance testing

## CITING BULLETANALYZR

If you use BulletAnalyzr in research, please cite:

> Eric Hare, Heike Hofmann, Alicia Carriquiry. "Algorithmic approaches to match degraded land impressions." *Law, Probability and Risk*, Volume 16, Issue 4, December 2017, Pages 203-221. https://doi.org/10.1093/lpr/mgx018

> Eric Hare, Heike Hofmann, and Alicia Carriquiry. "Automatic Matching of Bullet Land Impressions." *The Annals of Applied Statistics*, Volume 11, Number 4, 2017, pp. 2332-56. http://www.jstor.org/stable/26408049

## LICENSE

BulletAnalyzr is provided under the [GNU General Public License version 3](https://github.com/CSAFE-ISU/bulletAnalyzr/blob/main/LICENSE.md).

## CONTACT

**Center for Statistics and Applications in Forensic Evidence**  
195 Durham Center  
613 Morrill Road  
Ames, Iowa 50011  
csafe@iastate.edu