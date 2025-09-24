# BulletAnalyzr Developer Workflow

## Tab Structure & Navigation
- Main tabsetPanel: `id="prevreport"`
- Tabs: "Welcome", "Upload Bullet", "Preview Bullet", "Comparison Report"

## 1. Application Initialization (Welcome Tab)

**Tab Panel:** "Welcome"

### UI Elements

- Welcome message and instructions
- Button: `actionButton("begin_button", "Begin")`

### Server Logic

- `observeEvent(input$begin_button)` → switches to "Upload Bullet" tab
- Uses `updateTabsetPanel(session, "prevreport", selected = "Upload Bullet")`

### Reactive Value Changes

- None

---

## 2. Bullet Upload & Processing (Upload Bullet Tab)

**Tab Panel:** "Upload Bullet"

### File Upload Section

#### UI Elements

- UI Output: `bul_x3pui`, `lpupload`
- File input: `fileInput("bul_x3p", "Select Bullet Land x3p files")`
- Text input: `textInput("bul_x3p_name", label="Bullet Name")`
- Button: `actionButton("up_bull", label = "Add Bullet to Comparison List")`

#### Server Processing Chain

**1. `observeEvent(input$bul_x3p)` - File upload trigger**

- Calls `identify_bullet()` to auto-name bullet
- Updates `input$bul_x3p_name` via `updateTextInput()`
- **Reactive Value Changes:**
  - `values$show_alert <- TRUE`

**2. `uploaded_bull()` reactive - File processing**

- Creates temp directory and copies files via `copy_to_tempdir()`
- Calls `read_bullet(temp_dir)`
- **Reactive Value Changes:**
  - Returns processed bullet data structure

**3. `output$lpupload` renderUI - Main processing pipeline**

- **Bullet Rotation:** Calls `rotate_bullet()` which checks `hinfo$sizeX < hinfo$sizeY` and rotates if needed
- **Resolution Matching:** Calls `downsample_bullet()` which compares `x3p_get_scale()` between bullets and down-samples as needed
- **Unit Conversion:** `cond_x3p_m_to_mum()` converts from meters to microns if scale < 0.1
- **Metadata Generation:** 
  - MD5 checksums: `tools::md5sum(bull$source)`
  - Filenames: `basename(bull$source)`
  - Land names: `identify_lands(bull$filename)`
  - Bullet name: `identify_bullet(bull$filename)`
- **3D Rendering:** `renderRglwidget()` for each land using `render_land()`
- **Reactive Value Changes:**
  - `values$show_alert <- FALSE` (when rotation/resolution alerts shown)
  - `bulldata$allbull` (modified when down-sampling previous bullets)
  - `bulldata$cbull` ← current bullet data
  - `bulldata$cbull_export` ← export version of current bullet

**4. `observeEvent(input$up_bull)` - Add to comparison list**

- Calls `add_cbull_to_allbull()` to move data from `bulldata$cbull` to `bulldata$allbull`
- Calls `disable("up_bull")`
- **Reactive Value Changes:**
  - `bulldata$allbull` ← updated with new bullet (removes existing if same name)
  - `bulldata$allbull_export` ← export version updated

### Comparison Selection

#### UI Elements

- UI Output: `bull_sel`
- Checkbox group: `checkboxGroupInput("bullcompgroup")`
- Button: `actionButton("doprocess", label = "Compare Bullets")`

#### Server Logic

- `observeEvent(input$doprocess)` → triggers crosscut optimization via `get_default_cc_wrapper()`
- **Reactive Value Changes:**
  - `values$show_alert <- FALSE`
  - `bulldata$preCC` ← bullets with optimized crosscuts (if `interactive_cc = TRUE`)
  - `bulldata$preCC_export` ← export version
  - OR `bulldata$postCC` ← bullets (if `interactive_cc = FALSE`)
  - OR `bulldata$postCC_export` ← export version
- Switches to "Comparison Report" tab

---

## 3. Bullet Preview (Preview Bullet Tab)

**Tab Panel:** "Preview Bullet"

### UI Elements

- UI Output: `prevSelUI`, `lpreview`
- Dropdown: `selectInput("prev_bul", "Preview Bullet")`

### Server Logic

- `output$prevSelUI` renders bullet selection UI based on `bulldata$allbull`
- `output$lpreview` calls `filter_preview_bullet()` and renders 3D previews using `renderRglwidget()` with prefix "x3prglprev"

### Reactive Value Changes

- None (read-only operations)

---

## 4. Interactive Crosscut Adjustment (Comparison Report Tab)

**Tab Panel:** "Comparison Report"

### Crosscut Control Section

#### UI Elements

- UI Output: `CCBull1`, `CCBull2`, `CCBullLand`
- Dropdown: `selectInput("cc_bulsel", "Select Bullet")`
- Dynamic sliders: `sliderInput(paste("CCsl",x))` for each land via `render_ccsl()`
- Button: `actionButton("saveCC", label = "Finalise CrossCut")`
- Button: `actionButton("doprocessCC", label = "Compare Bullets")`

#### Server Logic Chain

**1. `output$CCBull1` - Renders bullet selection dropdown**

- Lists unique bullets from `bulldata$preCC`
- **Reactive Value Changes:** None

**2. `output$CCBull2` - Renders crosscut sliders dynamically**

- Calls `filter_selected_bullet()` to get selected bullet
- Uses `get_max_microns()` to calculate Y coordinate ranges
- Generates sliders via `render_ccsl()` for each land
- **Reactive Value Changes:** None

**3. `output$CCBullLand` - Renders 3D visualizations with crosscut lines**

- Uses `renderRglwidget()` with prefix "CC_Sel_"
- Calls `render_land()` with crosscut parameter for visualization
- **Reactive Value Changes:** None

**4. `observeEvent(input$saveCC)` - Save crosscut adjustments**

- Calls `update_cc_from_slider_wrapper()` to extract slider values
- **Reactive Value Changes:**
  - `bulldata$preCC` ← updated with manual crosscut positions from sliders
  - `bulldata$preCC_export` ← updated export version

**5. `observeEvent(input$doprocessCC)` - Trigger comparison**

- **Reactive Value Changes:**
  - `bulldata$postCC` ← data from `bulldata$preCC`
  - `bulldata$postCC_export` ← export version
  - `bulldata$preCC <- NULL` (reset)
  - `bulldata$preCC_export <- NULL` (reset)

---

## 5. Bullet Comparison Analysis (Triggered by postCC)

**Reactive Chain:** `observeEvent(bulldata$postCC)`

### Processing Steps

1. **Extract Crosscut Data:** `get_ccdata_wrapper()` - uses `try_x3p_crosscut()` with fallback logic
2. **Groove Detection:** `get_grooves_wrapper()` - calls `cc_locate_grooves()` with method="middle", adjust=30
3. **Signal Extraction:** `get_signals_wrapper()` - calls `cc_get_signature()` with span parameters
4. **Signal Alignment:** `get_aligned_signals_wrapper()` - performs pairwise `sig_align()` operations
5. **Feature Extraction:** `get_features_wrapper()` - calls `extract_feature_ccf()`, `sig_cms_max()`, and `extract_features_all()`
6. **Random Forest Scoring:** `predict(rtrees, newdata = features, type = "prob")[,2]`
7. **Bullet Scoring:** `get_bullet_scores_wrapper()` - calls `compute_average_scores()`
8. **Phase Testing:** `bullet_to_land_predict()` for same-source determination
9. **Image Rendering:** `render_crosscut_snap()` for each bullet land

### Reactive Value Changes

- `bulldata$comparison` ← complete analysis results containing:
  - `bullets` (processed bullet data with rendered images)
  - `comparisons` (pairwise land comparisons with alignment data)
  - `features_scaled` (extracted and scaled features)
  - `bullet_scores` (final scoring results with nested data)
- `bulldata$comparison_export` ← export versions via `get_report_data_wrapper()`

---

## 6. Report Generation (Comparison Report Tab)

### Report Selection

#### UI Elements

- UI Output: `reportSelUI` (from reportServer module)
- Dropdown: `selectInput("comp_bul1", "Compare Bullet")`
- Dropdown: `selectInput("comp_bul2", "With Bullet")`
- Button: `screenshotButton()` for report download

#### Module Structure (reportServer)

The report functionality is implemented as a Shiny module with:
- `reportSidebarUI("report1")` - renders download UI
- `reportMainUI("report1")` - renders main report content
- `reportServer("report1", ...)` - handles report logic

#### Report Outputs

- `output$bull_comp_score` - Phase test score text
- `output$bull_comp_test` - Phase test probability text  
- `output$bull_comp` - Bullet score matrix heatmap with `geom_tile()`
- `output$land_comp` - Land score matrix heatmap for selected bullet pair
- `output$land_visCC` - Crosscut comparison plots using `facet_grid(bullet ~ land)`
- `output$land_visSig` - Signal comparison plots showing raw and LOESS-smoothed signals

### Dynamic Report Sections

- `output$report` - Main report renderer that:
  - Calls `filter_selected_bullets()` and `get_bsldata()` for data preparation
  - Generates collapsible panels via `bsCollapsePanel()`
  - Creates dynamic outputs for top-scoring land comparisons:
    - **Data Tables:** `make_temptable()` creates feature comparison tables
    - **RGL Images:** `filter_x3pimg()` and `renderImage()` with prefixes "rglWinL", "rglWinR"  
    - **Groove Plots:** `filter_grooves_ccdata()` and `groove_plot()` with prefixes "GroovePlotL", "GroovePlotR"
    - **Signal Plots:** `filter_sig_plot_data()` and `plot_signal()` with prefix "SigPlot"

### Reactive Value Changes

- None (read-only rendering operations)

---

## 7. Phase Test Analysis

**Reactive:** `observe()` with comparison data requirements

### Processing

- Calls `bulletxtrctr:::phase_test(land1 = d$landA, land2 = d$landB, d$ccf)`
- Uses `tryCatch()` for error handling
- Handles both data.frame and phase.test class results

### Reactive Value Changes

- `phase$test_results` ← phase test statistical results

---

## Key Reactive Values Summary

### `bulldata` - Main Data Store
| Slot | Updated When | Trigger | Helper Functions |
|------|--------------|---------|------------------|
| `allbull` | File uploaded and added to comparison | `observeEvent(input$up_bull)` | `add_cbull_to_allbull()` |
| `allbull_export` | File uploaded and added to comparison | `observeEvent(input$up_bull)` | `make_export_df()` |
| `cbull` | File uploaded and processed | `output$lpupload` renderUI | Various preprocessing functions |
| `cbull_export` | File uploaded and processed | `output$lpupload` renderUI | `make_export_df()` |
| `preCC` | Compare button clicked (interactive mode) | `observeEvent(input$doprocess)` | `get_default_cc_wrapper()` |
| `preCC_export` | Compare button clicked (interactive mode) | `observeEvent(input$doprocess)` | `make_export_df()` |
| `preCC` | Crosscut positions saved | `observeEvent(input$saveCC)` | `update_cc_from_slider_wrapper()` |
| `preCC_export` | Crosscut positions saved | `observeEvent(input$saveCC)` | `make_export_df()` |
| `postCC` | Comparison triggered | `observeEvent(input$doprocessCC)` | Direct assignment |
| `postCC_export` | Comparison triggered | `observeEvent(input$doprocessCC)` | `make_export_df()` |
| `comparison` | Full analysis complete | `observeEvent(bulldata$postCC)` | Multiple wrapper functions |
| `comparison_export` | Full analysis complete | `observeEvent(bulldata$postCC)` | `get_report_data_wrapper()` |

### `phase` - Phase Test Results
| Slot | Updated When | Trigger |
|------|--------------|---------|
| `test_results` | Comparison data available and bullets selected | `observe()` with comparison requirements |

### `values` - UI State
| Slot | Updated When | Trigger |
|------|--------------|---------|
| `show_alert` | File upload or processing alerts | Various file processing events via helper functions |

---

## Key Helper Functions

### Data Processing
- `copy_to_tempdir()` - Creates temporary directories for file handling
- `identify_bullet()`, `identify_lands()` - Auto-generate names from filenames
- `cond_x3p_m_to_mum()` - Conditional unit conversion
- `rotate_bullet()`, `downsample_bullet()` - Preprocessing with user alerts
- `make_export_df()` - Creates test-friendly export versions

### Filtering & Selection  
- `filter_preview_bullet()`, `filter_selected_bullet()` - Bullet selection helpers
- `filter_selected_bullets()` - Bullet pair filtering for reports
- `filter_grooves_ccdata()`, `filter_sig_plot_data()`, `filter_x3pimg()` - Report data filtering

### Analysis Wrappers
- `get_default_cc_wrapper()` - Crosscut optimization with interactive/non-interactive modes
- `get_ccdata_wrapper()`, `get_grooves_wrapper()`, `get_signals_wrapper()` - Core analysis steps
- `get_aligned_signals_wrapper()`, `get_features_wrapper()` - Signal processing and feature extraction
- `get_bullet_scores_wrapper()`, `get_report_data_wrapper()` - Final scoring and report preparation

### Visualization
- `render_land()` - 3D land rendering with crosscut lines
- `render_crosscut_snap()` - Static image generation for reports
- `render_ccsl()` - Dynamic crosscut slider creation
- `parse_rglui()` - RGL widget UI generation
- `groove_plot()`, `plot_signal()` - Specialized plotting functions

### Report Generation
- `get_bsldata()`, `get_rf_order()` - Report data preparation
- `make_temptable()` - Feature comparison table generation
- `get_panel_name()` - Dynamic panel naming
- `show_modal()` - User alert system

---

## Export Values (for testing)
All reactive values have `_export` variants created by `make_export_df()` and exposed via `exportTestValues()` for shinytest2 testing. The export function removes large objects (x3p, x3pimg) and converts file paths to filenames for consistent testing across environme
