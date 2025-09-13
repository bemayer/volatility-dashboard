# ============================================================================
# BASIC STRUCTURE TEST
# Verify that all modules load correctly
# ============================================================================

cat("Testing Volatility Modeling Dashboard structure...\n")

# Test 1: Check if all R files can be sourced
cat("\n1. Testing R module loading:\n")

tryCatch({
  source("R/data_source.R")
  cat("  ✓ data_source.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ data_source.R failed:", e$message, "\n")
})

tryCatch({
  source("R/volatility_measures.R")
  cat("  ✓ volatility_measures.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ volatility_measures.R failed:", e$message, "\n")
})

tryCatch({
  source("R/evaluation_metrics.R")
  cat("  ✓ evaluation_metrics.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ evaluation_metrics.R failed:", e$message, "\n")
})

tryCatch({
  source("R/modeling_engine.R")
  cat("  ✓ modeling_engine.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ modeling_engine.R failed:", e$message, "\n")
})

tryCatch({
  source("R/ui_components.R")
  cat("  ✓ ui_components.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ ui_components.R failed:", e$message, "\n")
})

# Test 2: Check core functions
cat("\n2. Testing core functions:\n")

tryCatch({
  # Test sample data generation
  sample_data <- generate_sample_data("bitcoin", 100)
  cat("  ✓ Sample data generation works\n")
}, error = function(e) {
  cat("  ✗ Sample data generation failed:", e$message, "\n")
})

tryCatch({
  # Test volatility measures
  returns <- rnorm(100, 0, 0.02)
  target_vol <- calculate_target_volatility(returns, "squared")
  cat("  ✓ Volatility measure calculation works\n")
}, error = function(e) {
  cat("  ✗ Volatility measure calculation failed:", e$message, "\n")
})

tryCatch({
  # Test evaluation metrics
  actual <- abs(rnorm(50, 0, 0.02))
  predicted <- abs(rnorm(50, 0, 0.02))
  metrics <- calculate_all_metrics(actual, predicted)
  cat("  ✓ Evaluation metrics calculation works\n")
}, error = function(e) {
  cat("  ✗ Evaluation metrics calculation failed:", e$message, "\n")
})

# Test 3: Check required packages availability
cat("\n3. Testing package availability:\n")

required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly",
  "dplyr", "quantmod", "xts", "zoo", "moments"
)

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✓", pkg, "available\n")
  } else {
    cat("  ✗", pkg, "NOT available - install with:",
        "install.packages('", pkg, "')\n", sep = "")
  }
}

cat("\n4. Directory structure:\n")
dirs <- c("R", "ui", "www", "data", "docs", ".github/workflows")
for (dir in dirs) {
  if (dir.exists(dir)) {
    files_count <- length(list.files(dir, recursive = TRUE))
    cat("  ✓", dir, "(", files_count, "files )\n")
  } else {
    cat("  ✗", dir, "missing\n")
  }
}

cat("\n=== Test Complete ===\n")
cat("If all tests pass (✓), the dashboard structure is ready!\n")
cat("To run the dashboard: shiny::runApp('app.R')\n")
