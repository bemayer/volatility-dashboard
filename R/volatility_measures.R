# ============================================================================
# VOLATILITY MEASURES MODULE
# Calculates different target volatility measures
# ============================================================================

#' Calculate absolute returns
#' @param returns Vector of returns
#' @return Vector of absolute returns
calculate_absolute_returns <- function(returns) {
  abs(returns)
}

#' Calculate squared returns
#' @param returns Vector of returns
#' @return Vector of squared returns
calculate_squared_returns <- function(returns) {
  returns^2
}

#' Calculate rolling RMS volatility
#' @param returns Vector of returns
#' @param window Rolling window size
#' @param min_periods Minimum number of periods required
#' @return Vector of rolling RMS values
calculate_rolling_rms <- function(returns, window, min_periods = 1) {
  if (length(returns) < window) {
    stop("Data length is less than window size")
  }

  n <- length(returns)
  result <- rep(NA, n)

  for (i in window:n) {
    start_idx <- max(1, i - window + 1)
    window_data <- returns[start_idx:i]

    if (length(window_data) >= min_periods) {
      result[i] <- sqrt(mean(window_data^2, na.rm = TRUE))
    }
  }

  result
}

#' Calculate rolling standard deviation
#' @param returns Vector of returns
#' @param window Rolling window size
#' @param min_periods Minimum number of periods required
#' @return Vector of rolling standard deviation values
calculate_rolling_std <- function(returns, window, min_periods = 1) {
  if (length(returns) < window) {
    stop("Data length is less than window size")
  }

  n <- length(returns)
  result <- rep(NA, n)

  for (i in window:n) {
    start_idx <- max(1, i - window + 1)
    window_data <- returns[start_idx:i]

    if (length(window_data) >= min_periods) {
      result[i] <- sd(window_data, na.rm = TRUE)
    }
  }

  result
}

#' Calculate realized volatility from intraday data
#' @param intraday_returns Matrix or list of intraday returns (each row/day)
#' @return Vector of daily realized volatility
calculate_realized_volatility <- function(intraday_returns) {
  if (is.matrix(intraday_returns)) {
    sqrt(rowSums(intraday_returns^2, na.rm = TRUE))
  } else if (is.list(intraday_returns)) {
    sapply(intraday_returns, function(x) sqrt(sum(x^2, na.rm = TRUE)))
  } else {
    stop("intraday_returns must be a matrix or list")
  }
}

#' Calculate bi-power variation
#' @param intraday_returns Matrix or list of intraday returns
#' @return Vector of daily bi-power variation
calculate_bipower_variation <- function(intraday_returns) {
  mu1 <- sqrt(2 / pi)  # E[|Z|] for standard normal Z

  if (is.matrix(intraday_returns)) {
    result <- numeric(nrow(intraday_returns))
    for (i in seq_len(nrow(intraday_returns))) {
      returns <- intraday_returns[i, ]
      returns <- returns[!is.na(returns)]
      if (length(returns) > 1) {
        bpv <- (pi / 2) * sum(abs(returns[-1]) *
                                abs(returns[-length(returns)]),
                              na.rm = TRUE)
        result[i] <- bpv / mu1^2
      } else {
        result[i] <- NA
      }
    }
    sqrt(result)
  } else {
    stop("Bi-power variation requires matrix input")
  }
}

#' Main function to calculate target volatility measure
#' @param returns Vector of returns
#' @param measure_type Type of measure ("absolute", "squared", "rolling_std")
#' @param window Window size for rolling measures
#' @param dates Optional dates vector
#' @return List with volatility values and metadata
calculate_target_volatility <- function(returns, measure_type,
                                        window = NULL, dates = NULL) {

  if (is.null(dates)) {
    dates <- seq_along(returns)
  }

  result <- switch(measure_type,
    "absolute" = {
      vol <- calculate_absolute_returns(returns)
      list(
        values = vol,
        dates = dates,
        name = "Absolute Returns |r_t|",
        formula = "|r_t|",
        description = "Absolute value of returns"
      )
    },

    "squared" = {
      vol <- calculate_squared_returns(returns)
      list(
        values = vol,
        dates = dates,
        name = "Squared Returns r_t^2",
        formula = "r_t^2",
        description = "Squared returns (realized variance proxy)"
      )
    },

    "rolling_std" = {
      if (is.null(window)) window <- 20
      vol <- calculate_rolling_std(returns, window)
      list(
        values = vol,
        dates = dates,
        name = paste0(window, "-Day Rolling Std"),
        formula = paste0("sqrt(1/(", window, "-1) * Σ(r_t - r̄)²)"),
        description = paste("Rolling standard deviation over", window,
                            "periods"),
        window = window
      )
    },

    stop("Unknown measure type: ", measure_type)
  )

  # Add summary statistics
  valid_values <- result$values[!is.na(result$values)]
  if (length(valid_values) > 0) {
    result$statistics <- list(
      mean = mean(valid_values),
      median = median(valid_values),
      std = sd(valid_values),
      min = min(valid_values),
      max = max(valid_values),
      n_obs = length(valid_values),
      n_missing = sum(is.na(result$values))
    )
  }

  result
}

#' Annualize volatility
#' @param volatility Vector of volatility values
#' @param frequency Frequency of data ("daily" = 252, "weekly" = 52,
#'   "monthly" = 12)
#' @return Annualized volatility
annualize_volatility <- function(volatility, frequency = "daily") {
  scaling_factor <- switch(frequency,
    "daily" = sqrt(252),
    "weekly" = sqrt(52),
    "monthly" = sqrt(12),
    "quarterly" = sqrt(4),
    "annual" = 1,
    sqrt(252)  # Default to daily
  )

  volatility * scaling_factor
}

#' Get available volatility measures
#' @return List of available measures with descriptions
get_available_measures <- function() {
  list(
    "absolute" = list(
      name = "Absolute Returns",
      formula = "|r_t|",
      description = "Absolute value of daily returns",
      requires_window = FALSE
    ),
    "squared" = list(
      name = "Squared Returns",
      formula = "r_t²",
      description = "Squared daily returns (realized variance proxy)",
      requires_window = FALSE
    ),
    "rolling_std" = list(
      name = "Rolling Standard Deviation",
      formula = "\\sqrt{\\frac{1}{n-1} \\sum (r_t - \\bar{r})^2}",
      description = "Rolling sample standard deviation",
      requires_window = TRUE,
      default_window = 20
    )
  )
}

#' Validate volatility measure parameters
#' @param measure_type Measure type
#' @param window Window size (if required)
#' @param data_length Length of available data
#' @return List with validation results
validate_measure_params <- function(measure_type, window = NULL,
                                    data_length = NULL) {
  measures <- get_available_measures()

  if (!measure_type %in% names(measures)) {
    return(list(valid = FALSE,
                message = paste("Unknown measure type:", measure_type)))
  }

  measure_info <- measures[[measure_type]]

  if (measure_info$requires_window) {
    if (is.null(window)) {
      return(list(valid = FALSE,
                  message = "Window size required for this measure"))
    }

    if (window < 2) {
      return(list(valid = FALSE, message = "Window size must be at least 2"))
    }

    if (!is.null(data_length) && window > data_length) {
      return(list(valid = FALSE, message = "Window size exceeds data length"))
    }
  }

  return(list(valid = TRUE, message = "Parameters valid"))
}
