# ============================================================================
# PARQUET FILE ANALYSIS SCRIPT
# Diagnostic tool to understand Deribit orderbook structure
# ============================================================================

library(arrow)
library(dplyr)

cat("📊 Deribit Parquet File Analysis\n")
cat("================================\n\n")

# Read the local Parquet file
file_path <- "./deribit_orderbook.parquet"

cat("📁 Reading file:", file_path, "\n\n")

# Method 1: Get metadata without loading all data
cat("🔍 FILE METADATA:\n")
cat("─────────────────\n")

# Open as dataset for efficient querying
ds <- open_dataset(file_path)

# Get schema
schema_info <- schema(ds)
cat("Columns:", length(names(ds)), "\n")
cat("Column names:\n")
print(names(ds))
cat("\n")

# Read a small sample to understand structure
cat("📋 SAMPLE DATA (first 10 rows):\n")
cat("────────────────────────────────\n")
sample_data <- ds %>% head(10) %>% collect()
print(sample_data)
cat("\n")

# Get total row count
cat("📏 DATA DIMENSIONS:\n")
cat("───────────────────\n")
total_rows <- ds %>% count() %>% collect() %>% pull(n)
cat("Total rows:", format(total_rows, big.mark = ","), "\n\n")

# Analyze key columns
cat("🔑 KEY COLUMN ANALYSIS:\n")
cat("───────────────────────\n")

# Check if instrument_type column exists
if ("instrument_type" %in% names(ds)) {
  cat("✅ instrument_type column found\n")

  instrument_dist <- ds %>%
    group_by(instrument_type) %>%
    summarise(count = n()) %>%
    collect() %>%
    arrange(desc(count))

  cat("\nInstrument Type Distribution:\n")
  print(instrument_dist)
  cat("\n")
} else {
  cat("❌ instrument_type column NOT found\n")
  cat("Available columns:", paste(names(ds), collapse = ", "), "\n\n")
}

# Check currency column
if ("currency" %in% names(ds)) {
  cat("✅ currency column found\n")

  currency_dist <- ds %>%
    group_by(currency) %>%
    summarise(count = n()) %>%
    collect() %>%
    arrange(desc(count))

  cat("\nCurrency Distribution:\n")
  print(currency_dist)
  cat("\n")
} else {
  cat("❌ currency column NOT found\n\n")
}

# Analyze mark_iv column
if ("mark_iv" %in% names(ds)) {
  cat("📊 MARK_IV ANALYSIS:\n")
  cat("────────────────────\n")

  iv_stats <- ds %>%
    summarise(
      total = n(),
      iv_not_null = sum(!is.na(mark_iv), na.rm = TRUE),
      iv_valid = sum(!is.na(mark_iv) & mark_iv > 0 & mark_iv < 200, na.rm = TRUE),
      iv_min = min(mark_iv, na.rm = TRUE),
      iv_max = max(mark_iv, na.rm = TRUE),
      iv_mean = mean(mark_iv, na.rm = TRUE)
    ) %>%
    collect()

  cat("Total rows:", format(iv_stats$total, big.mark = ","), "\n")
  cat("mark_iv NOT NULL:", format(iv_stats$iv_not_null, big.mark = ","),
      sprintf("(%.1f%%)\n", 100 * iv_stats$iv_not_null / iv_stats$total))
  cat("mark_iv VALID (0-200):", format(iv_stats$iv_valid, big.mark = ","),
      sprintf("(%.1f%%)\n", 100 * iv_stats$iv_valid / iv_stats$total))
  cat("mark_iv range:", round(iv_stats$iv_min, 2), "-", round(iv_stats$iv_max, 2), "\n")
  cat("mark_iv mean:", round(iv_stats$iv_mean, 2), "\n\n")
}

# Test the OPTIMIZED query strategy
cat("🚀 TESTING OPTIMIZED QUERY STRATEGY:\n")
cat("─────────────────────────────────────\n")

test_currency <- "BTC"
test_limit <- 10000

cat(sprintf("Query: Get %s most recent %s options with valid IV\n\n",
            format(test_limit, big.mark = ","), test_currency))

# Start timing
start_time <- Sys.time()

if ("instrument_type" %in% names(ds) && "currency" %in% names(ds)) {

  # Optimized query with predicate pushdown
  result <- ds %>%
    filter(
      instrument_type == 'option',
      currency == test_currency,
      !is.na(mark_iv),
      mark_iv > 0,
      mark_iv < 200
    ) %>%
    select(
      timestamp, datetime, instrument_name,
      underlying_price, mid_price, mark_iv
    ) %>%
    collect() %>%
    arrange(desc(timestamp)) %>%
    head(test_limit)

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("✅ Query completed\n")
  cat(sprintf("⏱️  Execution time: %.2f seconds\n", elapsed))
  cat(sprintf("📦 Rows returned: %s\n", format(nrow(result), big.mark = ",")))
  cat(sprintf("📅 Date range: %s to %s\n",
              min(result$datetime), max(result$datetime)))

  if (nrow(result) < test_limit) {
    cat(sprintf("\n⚠️  WARNING: Only %s rows found (requested %s)\n",
                format(nrow(result), big.mark = ","),
                format(test_limit, big.mark = ",")))
    cat("Consider increasing sample_size or adjusting filters\n")
  }

} else {
  cat("❌ Cannot test: missing instrument_type or currency columns\n")
}

cat("\n")
cat("✅ Analysis complete!\n")
