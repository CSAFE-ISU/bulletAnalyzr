# Quick Start Guide - Manual Groove Selection

## TL;DR

```r
# In RStudio
source("inst/scripts/manual_groove_selection.R")

# Process a single bullet (all 6 lands)
process_directory("examples/Hamby-44/barrel 1/Bullet 1/", "groove_data.csv")

# For each land:
# - Look at the plot
# - Press ENTER to accept automatic grooves (red dashed lines)
# - OR type "left,right" values to override (e.g., "450,2800")
# - OR type "s" to skip
```

## What This Tool Does

1. Opens an x3p bullet scan file
2. Shows you a crosscut profile with automatic groove detection (red dashed lines)
3. Lets you accept or manually correct the groove locations
4. Saves everything to a CSV file

## Quick Example

```r
# Open RStudio, navigate to bulletAnalyzr project
source("inst/scripts/manual_groove_selection.R")

# Process one bullet's worth of lands
process_directory("examples/Hamby-44/barrel 1/Bullet 1/", "test_grooves.csv")
```

You'll see:
```
=== Processing: Barrel_1-Bullet_1-Land_1.x3p ===
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

Your choice: 
```

**Decision making:**
- Grooves look good? → Press ENTER
- Grooves look wrong? → Type new values like `400,2900`
- Not sure / problematic? → Type `s` to skip

## Typical Workflow for Data Collection

### Day 1: Get familiar with the tool
```r
source("inst/scripts/manual_groove_selection.R")

# Try one bullet to get the hang of it
process_directory("examples/Hamby-44/barrel 1/Bullet 1/", "practice.csv")
```

### Day 2+: Collect real data
```r
source("inst/scripts/manual_groove_selection.R")

# Process all bullets systematically
process_directory("examples/Hamby-44/barrel 1/Bullet 1/", "hamby44_grooves.csv")
process_directory("examples/Hamby-44/barrel 1/Bullet 2/", "hamby44_grooves.csv")
# ... continue for other bullets
```

## Understanding the Plot

```
     |                                        
  30 |     /\              /\    /\           <- Striations
     |    /  \            /  \  /  \          
  20 |   /    \    /\    /    \/    \         
     |  /      \  /  \  /              
  10 | /        \/    \/               
     |/                                
   0 +--------------------------------
     0    500   1000  1500  2000  2500
          ^                        ^
       Left groove            Right groove
       (red line)            (red line)
```

**What you're looking for:**
- The area between red lines = **land** (the useful part)
- The area outside red lines = **grooves** (to be excluded)
- Good placement = red lines at the transitions where slope changes

## Tips

1. **Work in batches** - Do all 6 lands from one bullet at once
2. **Be consistent** - Use the same criteria for "good enough"
3. **When in doubt, accept auto** - Only override if clearly wrong
4. **Take breaks** - This requires focus
5. **Can always redo** - Running the same file again replaces the old entry

## Output CSV Structure

```csv
filename,filepath,left_groove,right_groove,crosscut_y,manual_selection,timestamp
Land_1.x3p,/full/path/Land_1.x3p,423.5,2876.2,2458.3,FALSE,2024-12-16 14:23:45
Land_2.x3p,/full/path/Land_2.x3p,410.0,2890.5,2460.1,TRUE,2024-12-16 14:25:12
...
```

## Common Issues

**Q: Plot doesn't show up**
A: Make sure you're in RStudio with graphics enabled

**Q: I made a mistake**
A: Just run the same file again - it will overwrite

**Q: Should I adjust this one?**
A: If you're unsure, press ENTER to accept automatic. Only override when clearly wrong.

**Q: File crashes/errors**
A: Type `s` to skip it and make a note. Come back to it later.

## For Stephanie

**Files Created:**
- `inst/scripts/manual_groove_selection.R` - The main tool
- `inst/scripts/README_GROOVE_SELECTION.md` - Full documentation
- `inst/scripts/QUICKSTART.md` - This quick reference

**Questions to discuss:**
1. Which dataset(s) should we annotate? (Hamby-44, Houston, others?)
2. How many bullets total? (Affects time commitment)
3. Should multiple people annotate the same files for reliability checks?
4. What are the criteria for "correct" groove placement?
5. Timeline for data collection during the break?

**Estimated time:**
- ~2-3 minutes per land (including review and decision)
- ~12-18 minutes per bullet (6 lands)
- ~2-3 hours for 10 bullets (60 lands)

**Next steps after data collection:**
1. Share the CSV file with the team
2. Analyze where automatic detection fails
3. Compute statistics (% requiring manual adjustment, etc.)
4. Use insights to improve the algorithm
