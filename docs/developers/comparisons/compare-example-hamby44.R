# Compare Example - Hamby Set 44
#
# Simple example showing how to run the manual comparison pipeline on a single
# pair of bullets from the Hamby Set 44 dataset. Sources the manual pipeline
# and bullet codes scripts, then compares Barrel 1 Bullet 1 vs Bullet 2.
#
# Usage:
#   source("docs/developers/comparisons/compare-example-hamby44.R")

source("docs/developers/comparisons/manual-bullet-comparison-pipeline.R")
source("docs/developers/bullet-codes.R")

compare_bullets(
  bullet1_dir = "examples/Hamby Set 44 Final/Barrel 1/Bullet 1",
  bullet2_dir = "examples/Hamby Set 44 Final/Barrel 1/Bullet 2"
)
