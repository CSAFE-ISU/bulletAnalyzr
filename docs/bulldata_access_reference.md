# BulletAnalyzr Data Access Reference

This document provides a comprehensive reference for where each `bulldata` reactive value is accessed throughout the Shiny application, as well as the data flow between these values.

## Reactive Value Access Patterns

### `bulldata$allbull`

**Read Access:**

- `output$prevSelUI` - gets unique bullet names for dropdown: `unique(allbull$bullet)`
- `output$bull_sel` - gets unique bullet names for checkbox group: `unique(bulldata$allbull$bullet)`
- `observeEvent(input$doprocess)` - assigns to local `bullets` variable for processing
- `downsample_bullet()` function - checks if `nrow(allbull) > 0` and gets reference resolution

**Write Access:**

- `observeEvent(input$add_to_list_button)` - calls `add_cbull_to_allbull()` to append current bullet
- `downsample_bullet()` function - updates x3p data when downsampling previous bullets

### `bulldata$cbull`

**Read Access:**

- `observeEvent(input$add_to_list_button)` - requires `nrow(bulldata$cbull) > 0`
- `output$lpupload` - gets number of rows and x3p data for rendering: `bulldata$cbull$x3p[[cidx]]`, `bulldata$cbull$land_names[x]`
- `add_cbull_to_allbull()` function - source data for adding to allbull
- `downsample_bullet()` function - gets current bullet resolution
- `reportServer` module - gets scale and instrument info: `bullet_data$cbull$x3p[[1]]`

**Write Access:**

- `observeEvent(input$upload_button)` - assigns processed bullet data after reading, rotating, downsampling

### `bulldata$preCC`

**Read Access:**

- `output$CCBull1` - requires preCC exists for dropdown UI
- `output$CCBull2` - requires preCC exists and filters by selected bullet
- `output$CCBullLand` - requires preCC exists and filters by selected bullet
- `observeEvent(input$saveCC)` - reads for updating crosscut values
- `observeEvent(input$doprocessCC)` - assigns to local `bullets` variable
- `output$report` - checks `is.null(bulldata$preCC)` to show report
- `output$report_panels` - checks `is.null(bulldata$preCC)` to show panels
- `output$reportDownUI` - checks `is.null(bulldata$preCC)` for download button

**Write Access:**

- `observeEvent(input$doprocess)` - gets crosscut results from `get_default_cc_wrapper()`
- `observeEvent(input$saveCC)` - updates crosscut values from sliders
- `observeEvent(input$doprocessCC)` - sets to NULL after moving data to postCC

### `bulldata$postCC`

**Read Access:**

- `observeEvent(bulldata$postCC)` - triggers when postCC is populated to start processing pipeline

**Write Access:**

- `observeEvent(input$doprocess)` - gets crosscut results when `interactive_cc = FALSE`
- `observeEvent(input$doprocessCC)` - receives data from preCC when `interactive_cc = TRUE`

### `bulldata$comparison`

**Read Access:**

- `output$reportSelUI` - requires comparison exists, gets unique bullets from `bullet_scores`
- `output$report` - requires comparison exists for rendering report elements
- `output$report_panels` - requires comparison exists, accesses `bullets`, `bullet_scores`, `comparisons`
- `output$reportDownUI` - requires comparison exists for download button
- `reportServer` module - accesses multiple components:
  - `bullet_data$comparison$bullet_scores` for matrices and filtering
  - `bullet_data$comparison$bullets` for crosscut and signal plots
  - `bullet_data$comparison$comparisons` for signal data
- `observe()` in phase test section - gets `bullet_scores` for phase test calculation

**Write Access:**

- `observeEvent(bulldata$postCC)` - assigns processed results from `get_report_data_wrapper()`

### Export Variables (for testing)

All the `*_export` variants are write-only and used for testing:

- `bulldata$allbull_export`
- `bulldata$cbull_export` 
- `bulldata$preCC_export`
- `bulldata$postCC_export`
- `bulldata$comparison_export`

These are updated whenever their corresponding main variables are modified and are exported via `exportTestValues()`.

## Data Flow Pipeline

### 1. `bulldata$allbull` → `bulldata$preCC` or `bulldata$postCC`

**Location:** `observeEvent(input$doprocess)` in server.R

- **Trigger:** "Compare Bullets" button on Upload Bullet tab
- **Source:** `bullets <- bulldata$allbull`
- **Destination:** Via `get_default_cc_wrapper()`:
  - If `interactive_cc = TRUE` → `bulldata$preCC`
  - If `interactive_cc = FALSE` → `bulldata$postCC`

### 2. `bulldata$preCC` → `bulldata$postCC`

**Location:** `observeEvent(input$doprocessCC)` in server.R

- **Trigger:** "Compare Bullets" button on Comparison Report tab (after crosscut adjustment)
- **Source:** `bullets <- bulldata$preCC`
- **Destination:** `bulldata$postCC <- bullets`
- **Cleanup:** `bulldata$preCC <- NULL` (reset after transfer)

### 3. `bulldata$postCC` → `bulldata$comparison`

**Location:** `observeEvent(bulldata$postCC)` in server.R

- **Trigger:** Automatic when `bulldata$postCC` gets populated
- **Process:** Complete analysis pipeline:
  1. Extract crosscut data (`get_ccdata_wrapper`)
  2. Find groove locations (`get_grooves_wrapper`)
  3. Extract signals (`get_signals_wrapper`)
  4. Align signals (`get_aligned_signals_wrapper`)
  5. Get features (`get_features_wrapper`)
  6. Predict RF scores
  7. Calculate bullet scores (`get_bullet_scores_wrapper`)
  8. Generate report data (`get_report_data_wrapper`)
- **Destination:** `bulldata$comparison <- report_results$comparison`

## Complete Data Flow Summary

### Interactive Mode (`interactive_cc = TRUE`)
```
bulldata$allbull 
    ↓ (Compare Bullets button - Upload tab)
bulldata$preCC
    ↓ (Compare Bullets button - Report tab)
bulldata$postCC
    ↓ (Automatic processing pipeline)
bulldata$comparison
```

### Non-Interactive Mode (`interactive_cc = FALSE`)
```
bulldata$allbull 
    ↓ (Compare Bullets button - Upload tab)
bulldata$postCC
    ↓ (Automatic processing pipeline)
bulldata$comparison
```

The key distinction is that `preCC` is used for interactive crosscut adjustment, while `postCC` triggers the full analysis pipeline that produces the final comparison results.
