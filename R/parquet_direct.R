# Direct Parquet reading in R

#' Process Deribit data from S3 or local
#' @param raw_data Data frame from S3 streaming (already filtered by Arrow)
#' @param price_type Type of price to use
#'   ("price" for underlying, "mid_price" for option)
#' @param volatility_measure Volatility measure
#'   ("rolling_std" or "implied_vol")
#' @param rolling_window Window size for rolling volatility (if applicable)
#' @return Processed data list
process_deribit_data <- function(raw_data,
                                 price_type = "price",
                                 volatility_measure = "rolling_std",
                                 rolling_window = 20) {
  df <- raw_data

  cat(paste("Processing Deribit data:", nrow(df), "rows\n"))

  # Convert datetime from timestamp if needed
  if ("datetime" %in% names(df)) {
    df$datetime <- as.POSIXct(df$datetime)
  } else if ("timestamp" %in% names(df)) {
    df$datetime <- as.POSIXct(df$timestamp / 1000, origin = "1970-01-01")
  }

  # Select price series
  if (price_type == "price") {
    if (!"underlying_price" %in% names(df)) {
      stop("underlying_price column not found in data")
    }
    df$price <- df$underlying_price
  } else if (price_type == "mid_price") {
    if ("mid_price" %in% names(df)) {
      df$price <- df$mid_price
    } else if ("mark_price" %in% names(df)) {
      df$price <- df$mark_price
    } else {
      stop("Option price column not found in data")
    }
  }

  # Remove rows with invalid prices
  df <- df[!is.na(df$price) & df$price > 0, ]

  if (nrow(df) == 0) {
    stop("No valid price data after filtering")
  }

  cat(paste("After price filtering:", nrow(df), "rows\n"))

  # Sort by datetime (should already be sorted from Arrow, but ensure)
  df <- df[order(df$datetime), ]

  # Calculate returns
  returns <- diff(log(df$price))

  if (length(returns) == 0) {
    stop("Insufficient data to calculate returns")
  }

  cat(paste("Returns calculated:", length(returns), "observations\n"))

  # Create result list matching expected format
  result <- list(
    dates = df$datetime[-1],
    prices = df$price[-1],
    returns = as.numeric(returns),
    return_dates = df$datetime[-1],
    price_column = price_type,
    deribit_measure = volatility_measure
  )

  # Add raw IV data if using implied volatility
  if (volatility_measure == "implied_vol" && "mark_iv" %in% names(df)) {
    result$raw_iv <- df$mark_iv[-1]
    cat(paste("Added IV data:", length(result$raw_iv), "values\n"))
  }

  return(result)
}

#' Read Deribit Parquet file directly
#' @param file_path Path to the parquet file
#' @param sample_size Number of rows to sample (NULL for all)
#' @param columns Specific columns to read
#' @param price_type Type of price to use
#'   ("price" for underlying, "option_price" for option)
#' @return Processed data list
read_deribit_parquet <- function(file_path = "deribit_orderbook.parquet",
                                 sample_size = 10000,
                                 columns = NULL,
                                 price_type = "price") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Install with: install.packages('arrow')")
  }

  tryCatch({
    cat("Reading Parquet file directly...\n")

    # Default columns if not specified
    if (is.null(columns)) {
      columns <- c(
        "instrument_name", "timestamp", "datetime", "underlying_price",
        "mid_price", "mark_iv", "best_bid_price", "best_ask_price",
        "spread", "currency"
      )
    }

    # Read parquet file
    df <- arrow::read_parquet(file_path, col_select = columns)

    cat(paste("Loaded", nrow(df), "rows\n"))

    # Sample if requested
    if (!is.null(sample_size) && nrow(df) > sample_size) {
      cat(paste("Sampling", sample_size, "rows...\n"))
      df <- df[sample(nrow(df), sample_size), ]
    }

    # Filter for options with valid IV
    if ("mark_iv" %in% names(df)) {
      df <- df[
        !is.na(df$mark_iv) & df$mark_iv > 0 & df$mark_iv < 200,
      ]
      cat(paste("After IV filtering:", nrow(df), "rows\n"))
    }

    # Convert datetime
    if ("datetime" %in% names(df)) {
      df$datetime <- as.POSIXct(df$datetime)
    } else if ("timestamp" %in% names(df)) {
      df$datetime <- as.POSIXct(df$timestamp / 1000, origin = "1970-01-01")
    }

    # Sort by datetime
    df <- df[order(df$datetime), ]

    # Focus on one instrument with most data
    if ("instrument_name" %in% names(df)) {
      instruments <- table(df$instrument_name)
      main_instrument <- names(instruments)[which.max(instruments)]
      df <- df[df$instrument_name == main_instrument, ]
      cat(
        paste(
          "Using instrument:", main_instrument,
          "with", nrow(df), "observations\n"
        )
      )
    }

    # Calculate returns based on price type
    if (price_type == "option_price" && "mid_price" %in% names(df)) {
      prices <- df$mid_price
      price_column_name <- "Option_Price"
    } else {
      prices <- df$underlying_price
      price_column_name <- "Underlying_Price"
    }

    returns <- c(NA, diff(log(prices)))

    # Remove first NA
    valid_idx <- !is.na(returns)
    returns <- returns[valid_idx]
    dates <- df$datetime[valid_idx]
    prices <- prices[valid_idx]

    # Prepare result
    result <- list(
      dates = dates,
      prices = prices,
      returns = returns,
      return_dates = dates,
      price_column = price_column_name,
      data_source = "Deribit_Parquet_Direct",
      original_data = df[valid_idx, ]
    )

    # Add implied volatility if available
    if ("mark_iv" %in% names(df)) {
      result$implied_volatility <- df$mark_iv[valid_idx]
      result$iv_dates <- dates
    }

    cat("✓ Parquet data processed successfully\n")
    return(result)

  }, error = function(e) {
    stop("Error reading Parquet file: ", e$message)
  })
}

#' Quick Parquet preview
#' @param file_path Path to parquet file
preview_parquet <- function(file_path = "deribit_orderbook.parquet") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required")
  }

  # Read just metadata and first few rows
  pf <- arrow::open_dataset(file_path)
  schema <- pf$schema

  cat("Parquet file schema:\n")
  for (i in 1:length(schema$names)) {
    cat(paste(" ", i, schema$names[i], ":", schema$field(i - 1)$type, "\n"))
  }

  # Read first 100 rows
  sample_df <- pf %>%
    dplyr::slice_head(n = 100) %>%
    dplyr::collect()

  cat("\nFirst few rows:\n")
  print(head(sample_df))

  return(sample_df)
}
