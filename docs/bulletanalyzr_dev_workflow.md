# BulletAnalyzr Developer Workflow

## Tab Structure & Navigation
- Main tabsetPanel: `id="prevreport"`
- Tabs: "Welcome", "Upload Bullet", "Preview Bullet", "Comparison Report"

## 1. Application Initialization (Welcome Tab)

**Tab Panel:** "Welcome"

### UI Elements

- Welcome message and instructions
- Button: `actionButton("confirm_autonomous", "Begin")`

### Server Logic

- `observeEvent(input$confirm_autonomous)` → switches to "Upload Bullet" tab
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

- Creates temp directory and copies files
- Calls `read_bullet(temp_dir)`
- **Reactive Value Changes:**
  - Returns processed bullet data structure

**3. `output$lpupload` renderUI - Main processing pipeline**

- Conditional rotation: checks `hinfo$sizeX < hinfo$sizeY`
- Resolution matching: compares `x3p_get_scale()` between bullets
- Unit conversion: `cond_x3p_m_to_mum()`
- Generates MD5 checksums: `tools::md5sum()`
- Renders 3D previews: `renderRglwidget()` for each land
- **Reactive Value Changes:**
  - `values$show_alert <- FALSE` (when rotation/resolution alerts shown)
  - `bulldata$allbull$x3p` (modified when down-sampling previous bullets)
  - `bulldata$cbull` ← current bullet data
  - `bulldata$cbull_export` ← export version of current bullet

**4. `observeEvent(input$up_bull)` - Add to comparison list**

- Moves data from `bulldata$cbull` to `bulldata$allbull`
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

- `observeEvent(input$doprocess)` → triggers crosscut optimization
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

- `output$prevSelUI` renders bullet selection UI
- `output$lpreview` renders 3D previews of selected bullet
- Uses `renderRglwidget()` with prefix "x3prglprev"

### Reactive Value Changes

- None (read-only operations)

---

## 4. Interactive Crosscut Adjustment (Comparison Report Tab)

**Tab Panel:** "Comparison Report"

### Crosscut Control Section

#### UI Elements

- UI Output: `CCBull1`, `CCBull2` `CCBullLand`
- Dropdown: `selectInput("cc_bulsel", "Select Bullet")`
- Dynamic sliders: `sliderInput(paste("CCsl",x))` for each land
- Button: `actionButton("saveCC", label = "Finalise CrossCut")`
- Button: `actionButton("doprocessCC", label = "Compare Bullets")`

#### Server Logic Chain

**1. `output$CCBull1` - Renders bullet selection dropdown**

- **Reactive Value Changes:** None

**2. `output$CCBull2` - Renders crosscut sliders dynamically**

- **Reactive Value Changes:** None

**3. `output$CCBullLand` - Renders 3D visualizations with crosscut lines**

- Uses `renderRglwidget()` with prefix "CC_Sel_"
- Calls `x3p_add_hline()` for crosscut visualization
- **Reactive Value Changes:** None

**4. `observeEvent(input$saveCC)` - Save crosscut adjustments**

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

- Extracts crosscut data: `mapply(try_x3p_crosscut)`
- Groove detection: `cc_locate_grooves()`
- Signal extraction: `cc_get_signature()`
- Signal alignment: `sig_align()`
- Feature extraction: `extract_features_all()`
- Random forest scoring: `predict(rtrees)`
- Bullet scoring: `compute_average_scores()`
- Phase testing: `bullet_to_land_predict()`
- Image rendering: `render_land()` for each bullet

### Reactive Value Changes

- `bulldata$comparison` ← complete analysis results containing:
  - `bullets` (processed bullet data with images)
  - `comparisons` (pairwise land comparisons)
  - `features_scaled` (extracted features)
  - `bullet_scores` (final scoring results)
- `bulldata$comparison_export` ← export versions of all comparison data

---

## 6. Report Generation (Comparison Report Tab)

### Report Selection

#### UI Elements

- UI Output: `reportSelUI`, `reportDownUI`, `reportUI`
- Dropdown: `selectInput("comp_bul1", "Compare Bullet")`
- Dropdown: `selectInput("comp_bul2", "With Bullet")`
- Button: `screenshotButton(id = "reportUI")`

#### Report Outputs

- `output$bull_comp_score` - Phase test score text
- `output$bull_comp_test` - Phase test probability text
- `output$bull_comp` - Bullet score matrix plot
- `output$land_comp` - Land score matrix plot
- `output$land_visCC` - Crosscut comparison plots
- `output$land_visSig` - Signal comparison plots

### Dynamic Report Sections

- `output$reportUI` - Main report renderer
  - Generates collapsible panels: `bsCollapsePanel()`
  - Creates dynamic outputs for each land comparison:
    - RGL images: `renderImage()` with prefixes "rglWinL", "rglWinR"
    - Groove plots: `renderPlot()` with prefixes "GroovePlotL", "GroovePlotR"
    - Signal plots: `renderPlot()` with prefix "SigPlot"
    - Data tables: `datatable()` for feature comparisons

### Reactive Value Changes

- None (read-only rendering operations)

---

## 7. Phase Test Analysis

**Reactive:** `observe()` with `req(bulldata$comparison)`

### Processing

- Calls `bulletxtrctr:::phase_test()`
- Handles both data.frame and phase.test class results

### Reactive Value Changes

- `phase$test_results` ← phase test statistical results

---

## Key Reactive Values Summary

### `bulldata` - Main Data Store
| Slot | Updated When | Trigger |
|------|--------------|---------|
| `allbull` | File uploaded and added to comparison | `observeEvent(input$up_bull)` |
| `allbull_export` | File uploaded and added to comparison | `observeEvent(input$up_bull)` |
| `cbull` | File uploaded and processed | `output$lpupload` renderUI |
| `cbull_export` | File uploaded and processed | `output$lpupload` renderUI |
| `preCC` | Compare button clicked (interactive mode) | `observeEvent(input$doprocess)` |
| `preCC_export` | Compare button clicked (interactive mode) | `observeEvent(input$doprocess)` |
| `preCC` | Crosscut positions saved | `observeEvent(input$saveCC)` |
| `preCC_export` | Crosscut positions saved | `observeEvent(input$saveCC)` |
| `postCC` | Comparison triggered | `observeEvent(input$doprocessCC)` |
| `postCC_export` | Comparison triggered | `observeEvent(input$doprocessCC)` |
| `comparison` | Full analysis complete | `observeEvent(bulldata$postCC)` |
| `comparison_export` | Full analysis complete | `observeEvent(bulldata$postCC)` |

### `phase` - Phase Test Results
| Slot | Updated When | Trigger |
|------|--------------|---------|
| `test_results` | Comparison data available and bullets selected | `observe()` with comparison requirements |

### `values` - UI State
| Slot | Updated When | Trigger |
|------|--------------|---------|
| `show_alert` | File upload or processing alerts | Various file processing events |

---

## Export Values (for testing)
All reactive values have `_export` variants created by `make_export_df()` and exposed via `exportTestValues()` for shinytest2 testing. These are updated simultaneously with their main counterparts.