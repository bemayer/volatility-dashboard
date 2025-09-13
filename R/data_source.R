# ============================================================================
# DATA SOURCE MODULE
# Handles Yahoo Finance integration and CSV uploads
# ============================================================================

#' Download data from Yahoo Finance
#' @param symbol Stock symbol (e.g., "BTC-USD")
#' @param from Start date
#' @param to End date
#' @return xts object with OHLCV data
get_yahoo_data <- function(symbol, from, to) {
  tryCatch(
    {
      env <- new.env()
      suppressWarnings({
        getSymbols(symbol,
          env = env, src = "yahoo", from = from, to = to,
          auto.assign = TRUE
        )
      })

      data <- env[[ls(env)[1]]]

      if (is.null(data) || nrow(data) == 0) {
        stop("No data retrieved for symbol: ", symbol)
      }

      return(data)
    },
    error = function(e) {
      stop("Failed to download data for ", symbol, ": ", e$message)
    }
  )
}

#' Process raw price data to returns
#' @param prices xts object with price data
#' @param price_column Name of the price column to use
#' @return list with dates, prices, and returns
process_price_data <- function(prices, price_column = NULL) {
  # If no specific column specified, use Close price
  if (is.null(price_column)) {
    price_col_names <- names(prices)
    close_cols <- grep("Close|Adjusted", price_col_names, value = TRUE)
    if (length(close_cols) > 0) {
      price_column <- close_cols[1]
    } else {
      price_column <- price_col_names[ncol(prices)] # Use last column
    }
  }

  clean_prices <- prices[, price_column]
  clean_prices <- na.omit(clean_prices)

  if (nrow(clean_prices) < 10) {
    stop("Insufficient data points after cleaning")
  }

  # Calculate returns
  returns <- diff(log(clean_prices))
  returns <- na.omit(returns)

  return(list(
    dates = index(clean_prices),
    prices = as.numeric(clean_prices),
    returns = as.numeric(returns),
    return_dates = index(returns),
    price_column = price_column
  ))
}

#' Read and process CSV file
#' @param file_path Path to CSV file
#' @param date_col Name of date column
#' @param price_col Name of price column
#' @param header Whether file has header
#' @param sep Column separator
#' @return Processed data list
read_csv_data <- function(file_path, date_col, price_col, header = TRUE,
                          sep = ",") {
  tryCatch(
    {
      # Read CSV
      data <- read.csv(file_path,
        header = header, sep = sep,
        stringsAsFactors = FALSE
      )

      if (!date_col %in% names(data)) {
        stop("Date column '", date_col, "' not found in data")
      }

      if (!price_col %in% names(data)) {
        stop("Price column '", price_col, "' not found in data")
      }

      # Process dates
      data[[date_col]] <- as.Date(data[[date_col]])
      data <- data[order(data[[date_col]]), ]

      # Clean price data
      data[[price_col]] <- as.numeric(data[[price_col]])
      data <- data[!is.na(data[[date_col]]) & !is.na(data[[price_col]])]

      if (nrow(data) < 10) {
        stop("Insufficient valid data points")
      }

      # Create xts object
      prices_xts <- xts(data[[price_col]], order.by = data[[date_col]])
      names(prices_xts) <- price_col

      # Process to returns
      return(process_price_data(prices_xts, price_col))
    },
    error = function(e) {
      stop("Error processing CSV file: ", e$message)
    }
  )
}

#' Get popular financial assets metadata
#' @return List of popular assets with descriptions
get_popular_assets <- function() {
  list(
    "BTC-USD" = list(
      name = "Bitcoin",
      description = "Cryptocurrency - Bitcoin to USD",
      category = "Crypto"
    ),
    "^GSPC" = list(
      name = "S&P 500",
      description = "Stock Index - Standard & Poor's 500",
      category = "Equity Index"
    ),
    "GLD" = list(
      name = "Gold ETF",
      description = "SPDR Gold Shares ETF",
      category = "Commodity"
    ),
    "^VIX" = list(
      name = "VIX",
      description = "CBOE Volatility Index",
      category = "Volatility"
    ),
    "EURUSD=X" = list(
      name = "EUR / USD",
      description = "Euro to US Dollar Exchange Rate",
      category = "FX"
    ),
    "CL=F" = list(
      name = "Crude Oil",
      description = "Crude Oil Futures",
      category = "Commodity"
    ),
    "^TNX" = list(
      name = "10-Year Treasury",
      description = "US 10-Year Treasury Yield",
      category = "Bond"
    ),
    "AAPL" = list(
      name = "Apple",
      description = "Apple Inc. Stock",
      category = "Equity"
    ),
    "TSLA" = list(
      name = "Tesla",
      description = "Tesla Inc. Stock",
      category = "Equity"
    ),
    "MSFT" = list(
      name = "Microsoft",
      description = "Microsoft Corporation Stock",
      category = "Equity"
    )
  )
}

#' Validate Yahoo Finance symbol
#' @param symbol Symbol to validate
#' @return Boolean indicating if symbol is valid
validate_symbol <- function(symbol) {
  tryCatch(
    {
      test_data <- get_yahoo_data(symbol, Sys.Date() - 10, Sys.Date())
      return(nrow(test_data) > 0)
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Generate sample data for testing
#' @param type Type of sample data ("bitcoin", "sp500", "random")
#' @param n Number of observations
#' @return Processed data list
generate_sample_data <- function(type = "bitcoin", n = 1000) {
  set.seed(42) # For reproducibility

  dates <- seq(
    from = Sys.Date() - n, to = Sys.Date() - 1, by = "days"
  )

  if (type == "bitcoin") {
    # Bitcoin - like high volatility
    returns <- rnorm(n, mean = 0.001, sd = 0.05)
    # Add volatility clustering
    volatility <- rep(0.05, n)
    for (i in 2:n) {
      volatility[i] <- 0.8 * volatility[i - 1] +
        0.2 * abs(returns[i - 1]) + 0.01
      returns[i] <- rnorm(1, mean = 0.001, sd = volatility[i])
    }
  } else if (type == "sp500") {
    # S&P 500 - like moderate volatility
    returns <- rnorm(n, mean = 0.0003, sd = 0.015)
  } else {
    # Random walk
    returns <- rnorm(n, mean = 0, sd = 0.02)
  }

  # Generate prices from returns
  prices <- cumprod(1 + returns) * 100

  return(list(
    dates = dates,
    prices = prices,
    returns = returns[-1], # Remove first return (NA)
    return_dates = dates[-1],
    price_column = "Sample_Price"
  ))
}
