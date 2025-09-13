# ============================================================================
# MODELING ENGINE MODULE
# Comprehensive volatility modeling framework
# ============================================================================

# ============================================================================
# 1. NAIVE MODELS
# ============================================================================

#' Fit naive models (Historical Average and Previous Value)
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @return List of naive model predictions
fit_naive_models <- function(train, full) {
  debug_log(" * * *  FITTING NAIVE MODELS  * * * ")

  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]

  debug_log(paste("train_target length:", length(train_target)))
  debug_log(paste("test_length:", test_length))
  debug_log(paste(
    "train_target range:", min(train_target, na.rm = TRUE),
    "to", max(train_target, na.rm = TRUE)
  ))

  results <- list()

  # Historical Average - use FULL DATASET average
  if (!is.null(full_target)) {
    hist_mean <- mean(full_target, na.rm = TRUE)
    debug_log(paste("Historical Average (full dataset):", hist_mean))
  } else {
    hist_mean <- mean(train_target, na.rm = TRUE)
    debug_log(paste("Historical Average (training only):", hist_mean))
  }

  # Historical Average: constant forecast (no time - varying)
  forecasts_hist <- rep(hist_mean, test_length)

  results[["Historical_Average"]] <- list(
    predictions = forecasts_hist,
    parameters = list(mean = hist_mean),
    model_type = "Naive",
    description = "Historical average of full dataset"
  )

  # Random Walk  -  use actual previous observations from test data
  forecasts_rw <- numeric(test_length)

  if (!is.null(test_target) && length(test_target) >= test_length) {
    # Use actual previous observations
    last_train_obs <- tail(train_target[!is.na(train_target)], 1)
    debug_log(paste(
      "Random Walk starting from last training obs:", last_train_obs
    ))

    # First forecast uses last training observation
    forecasts_rw[1] <- last_train_obs

    # Subsequent forecasts use previous actual test observations
    for (h in 2:test_length) {
      forecasts_rw[h] <- test_target[h - 1] # Use actual previous observation
    }
  } else {
    # Fallback: use last training observation
    last_obs <- tail(train_target[!is.na(train_target)], 1)
    debug_log(paste("Random Walk fallback - last training obs:", last_obs))
    forecasts_rw <- rep(last_obs, test_length)
  }

  results[["Previous_Value"]] <- list(
    predictions = forecasts_rw,
    parameters = list(last_value = forecasts_rw[1]),
    model_type = "Naive",
    description = "Previous value (uses actual previous observations)"
  )

  debug_log(paste(
    "Naive models predictions - Historical:", hist_mean,
    "Random Walk start:", forecasts_rw[1]
  ))
  results
}

# ============================================================================
# 2. MOVING AVERAGE MODELS
# ============================================================================

#' Fit simple moving average models
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param windows Vector of window sizes
#' @return List of MA model predictions
fit_moving_average_models <- function(train, full,
                                      windows = c(5, 10, 20, 30)) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]

  results <- list()

  for (window in windows) {
    if (window <= length(train_target)) {
      # MA forecasts using actual previous data
      forecasts <- numeric(test_length)
      ma_value <- mean(tail(train_target, window), na.rm = TRUE)

      if (!is.null(test_target) && length(test_target) >= test_length) {
        # Use actual rolling window approach with real data
        # Combine training   +  test data for rolling calculations
        combined_data <- c(train_target, test_target)
        train_length <- length(train_target)

        for (h in 1:test_length) {
          # For each forecast, use the window ending just before the
          # prediction point
          end_idx <- train_length + h - 1
          start_idx <- max(1, end_idx - window + 1)
          forecasts[h] <- mean(
            combined_data[start_idx:end_idx],
            na.rm = TRUE
          )
        }
      } else {
        # Fallback: use last window from training
        forecasts <- rep(ma_value, test_length)
      }

      model_name <- paste0("MA_", window)
      results[[model_name]] <- list(
        predictions = forecasts,
        parameters = list(window = window),
        model_type = "Moving Average",
        description = paste("Simple moving average with window", window)
      )
    }
  }

  return(results)
}

#' Fit adaptive moving average with optimization
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param max_window Maximum window size to consider
#' @return List with adaptive MA model
fit_adaptive_moving_average <- function(train, full, max_window = 50) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  if (length(train_target) < max_window) {
    max_window <- floor(length(train_target) * 0.8)
  }

  windows <- 2:max_window
  best_window <- 5
  best_error <- Inf

  # Simple optimization: choose window that minimizes in - sample error
  for (window in windows) {
    if (window <= length(train_target)) {
      # Calculate rolling MA for in - sample evaluation
      n_train <- length(train_target)
      errors <- numeric()

      for (i in window:n_train) {
        ma_forecast <- mean(
          train_target[(i - window + 1):i],
          na.rm = TRUE
        )
        if (i < n_train) {
          actual <- train_target[i + 1]
          if (!is.na(actual) && !is.na(ma_forecast)) {
            errors <- c(errors, (actual - ma_forecast)^2)
          }
        }
      }

      if (length(errors) > 0) {
        mse <- mean(errors)
        if (mse < best_error) {
          best_error <- mse
          best_window <- window
        }
      }
    }
  }

  # Generate adaptive MA forecasts using actual data
  adaptive_forecasts <- numeric(test_length)

  if (!is.null(test_target) && length(test_target) >= test_length) {
    # Use actual rolling window approach with optimized window
    combined_data <- c(train_target, test_target)
    train_length <- length(train_target)

    for (h in 1:test_length) {
      # Use optimized window size with actual data
      end_idx <- train_length + h - 1
      start_idx <- max(1, end_idx - best_window + 1)
      adaptive_forecasts[h] <- mean(
        combined_data[start_idx:end_idx],
        na.rm = TRUE
      )
    }
  } else {
    # Fallback: use last window from training
    ma_value <- mean(tail(train_target, best_window), na.rm = TRUE)
    adaptive_forecasts <- rep(ma_value, test_length)
  }

  return(list(
    "Adaptive_MA" = list(
      predictions = adaptive_forecasts,
      parameters = list(
        optimal_window = best_window,
        optimization_error = best_error
      ),
      model_type = "Adaptive Moving Average",
      description = paste("Adaptive MA with optimized window", best_window)
    )
  ))
}

# ============================================================================
# 3. EWMA MODELS
# ============================================================================

#' Calculate EWMA volatility series
#' @param target_series Target volatility series
#' @param lambda EWMA decay parameter
#' @return Vector of EWMA volatility estimates
calculate_ewma_series <- function(target_series, lambda) {
  n <- length(target_series)
  ewma_values <- numeric(n)

  # Initialize with first observation
  ewma_values[1] <- target_series[1]

  # Recursive calculation
  for (t in 2:n) {
    ewma_values[t] <- lambda * ewma_values[t - 1] +
      (1 - lambda) * target_series[t - 1]
  }

  ewma_values
}

#' Fit EWMA models
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param lambdas Vector of lambda values
#' @param optimize Whether to optimize lambda
#' @return List of EWMA model predictions
fit_ewma_models <- function(train, full, lambdas = c(0.94, 0.97),
                            optimize = FALSE) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  results <- list()

  # Fixed lambda models (RiskMetrics)
  for (lambda in lambdas) {
    ewma_series <- calculate_ewma_series(train_target, lambda)
    last_ewma <- tail(ewma_series, 1)

    # Generate EWMA forecasts using actual data
    forecasts <- numeric(test_length)

    if (!is.null(test_target) && length(test_target) >= test_length) {
      # Use actual EWMA updating with real data
      combined_data <- c(train_target, test_target)
      train_length <- length(train_target)

      # Start with EWMA value from end of training period
      current_ewma <- last_ewma

      for (h in 1:test_length) {
        # EWMA forecast uses current EWMA value
        forecasts[h] <- current_ewma

        # Update EWMA with actual observed value (if not last forecast)
        if (h < test_length) {
          actual_value <- combined_data[train_length + h]
          current_ewma <- lambda * current_ewma +
            (1 - lambda) * actual_value
        }
      }
    } else {
      # Fallback: static forecast
      forecasts <- rep(last_ewma, test_length)
    }

    model_name <- paste0(
      "EWMA_lambda_", gsub("\\.", "", sprintf("%.3f", lambda))
    )
    results[[model_name]] <- list(
      predictions = forecasts,
      parameters = list(lambda = lambda, last_value = last_ewma),
      model_type = "EWMA",
      description = paste("EWMA with lambda  = ", lambda)
    )
  }

  # Optimized EWMA
  if (optimize) {
    lambda_range <- seq(0.90, 0.999, by = 0.001)
    best_lambda <- 0.94
    best_error <- Inf

    for (lambda in lambda_range) {
      ewma_series <- calculate_ewma_series(train_target, lambda)

      # Calculate in - sample forecast error
      errors <- numeric()
      for (t in 2:(length(train_target) - 1)) {
        forecast <- lambda * ewma_series[t] +
          (1 - lambda) * train_target[t]
        actual <- train_target[t + 1]
        if (!is.na(actual) && !is.na(forecast)) {
          errors <- c(errors, (actual - forecast)^2)
        }
      }

      if (length(errors) > 0) {
        mse <- mean(errors)
        if (mse < best_error) {
          best_error <- mse
          best_lambda <- lambda
        }
      }
    }

    # Fit with optimal lambda
    ewma_series <- calculate_ewma_series(train_target, best_lambda)
    last_ewma <- tail(ewma_series, 1)

    # Generate multi - step ahead forecasts
    long_run_avg <- mean(train_target, na.rm = TRUE)
    forecasts <- numeric(test_length)

    for (h in 1:test_length) {
      # EWMA forecast converges to long - run average
      forecasts[h] <- long_run_avg +
        (last_ewma - long_run_avg) * (best_lambda^h)
    }

    results[["EWMA_Optimized"]] <- list(
      predictions = forecasts,
      parameters = list(
        lambda = best_lambda,
        last_value = last_ewma,
        long_run_avg = long_run_avg,
        optimization_error = best_error
      ),
      model_type = "EWMA Optimized",
      description = paste(
        "Optimized EWMA with lambda  = ", round(best_lambda, 4)
      )
    )
  }

  return(results)
}

# ============================================================================
# 4. GARCH MODELS
# ============================================================================

#' Fit GARCH models
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param garch_specs List of GARCH specifications
#' @return List of GARCH model predictions
fit_garch_models <- function(train, full, garch_specs) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  returns <- train$returns
  results <- list()

  for (spec_name in names(garch_specs)) {
    tryCatch(
      {
        # Fit GARCH model
        fit <- ugarchfit(
          spec = garch_specs[[spec_name]], data = returns,
          solver = "hybrid"
        )

        if (convergence(fit) == 0) {
          # Generate forecasts
          forecast <- ugarchforecast(fit, n.ahead = test_length)
          sigma_forecast <- as.numeric(sigma(forecast))

          # For squared returns target, use variance; for others, use volatility
          predictions <- sigma_forecast^2

          results[[spec_name]] <- list(
            predictions = predictions,
            parameters = coef(fit),
            model_type = "GARCH",
            description = paste("GARCH model:", spec_name),
            fit_object = fit,
            convergence = convergence(fit),
            log_likelihood = likelihood(fit)
          )
        } else {
          warning(paste("GARCH model", spec_name, "did not converge"))
        }
      },
      error = function(e) {
        warning(paste(
          "Error fitting GARCH model", spec_name, ":", e$message
        ))
      }
    )
  }

  results
}

#' Create GARCH specifications
#' @param model_type Type of GARCH model
#'   ("sGARCH", "eGARCH", "gjrGARCH", "fiGARCH")
#' @param garch_order Vector c(p, q) for GARCH order
#' @param distribution Distribution assumption
#' @param include_mean Whether to include mean in the model
#' @return ugarchspec object
create_garch_spec <- function(model_type = "sGARCH", garch_order = c(1, 1),
                              distribution = "norm", include_mean = TRUE) {
  ugarchspec(
    variance.model = list(model = model_type, garchOrder = garch_order),
    mean.model = list(armaOrder = c(0, 0), include.mean = include_mean),
    distribution.model = distribution
  )
}

# ============================================================================
# 5. MACHINE LEARNING MODELS (Simplified)
# ============================================================================

#' Prepare data for ML models
#' @param target_series Target volatility series
#' @param lags Number of lags to use as features
#' @return List with X (features) and y (targets)
prepare_ml_data <- function(target_series, lags = 20) {
  n <- length(target_series)
  if (n <= lags) {
    stop("Insufficient data for specified lags")
  }

  # Create lagged features
  x_matrix <- matrix(NA, nrow = n - lags, ncol = lags)
  y <- numeric(n - lags)

  for (i in 1:(n - lags)) {
    x_matrix[i, ] <- target_series[i:(i + lags - 1)]
    y[i] <- target_series[i + lags]
  }

  return(list(X = x_matrix, y = y))
}

#' Fit simple neural network (placeholder  -  requires keras / tensorflow)
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param architecture Vector specifying layer sizes
#' @param lags Number of input lags
#' @return List with neural network predictions
fit_neural_network <- function(train, full, architecture = c(50, 25, 1),
                               lags = 20) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  # This is a simplified placeholder
  # In production, this would use keras / tensorflow

  tryCatch(
    {
      # Prepare data
      ml_data <- prepare_ml_data(train_target, lags)

      # Simple linear model as placeholder
      x_train <- ml_data$X
      y_train <- ml_data$y

      # Use last window for prediction
      last_window <- tail(train_target, lags)

      # Simple average as placeholder prediction
      prediction_value <- mean(y_train, na.rm = TRUE)

      model_name <- paste("MLP", paste(architecture, collapse = " - "), sep = "_")

      # Generate time - varying NN forecasts
      nn_forecasts <- numeric(test_length)
      for (h in 1:test_length) {
        # Simple NN decay pattern with mean reversion
        nn_forecasts[h] <- prediction_value * (0.98^h) +
          mean(train_target, na.rm = TRUE) * (1 - 0.98^h)
      }

      result <- list()
      result[[model_name]] <- list(
        predictions = nn_forecasts,
        parameters = list(
          architecture = architecture,
          lags = lags,
          training_samples = nrow(X_train)
        ),
        model_type = "Neural Network",
        description = paste(
          "MLP with architecture", paste(architecture, collapse = " - ")
        )
      )

      result
    },
    error = function(e) {
      warning(paste("Neural network fitting failed:", e$message))
      list()
    }
  )
}

# ============================================================================
# 6. HAR - RV MODELS
# ============================================================================

#' Create HAR features
#' @param rv_series Realized volatility series
#' @param daily_lag Lag for daily component
#' @param weekly_window Window for weekly component
#' @param monthly_window Window for monthly component
#' @return Data frame with HAR features
create_har_features <- function(rv_series, daily_lag = 1,
                                weekly_window = 5,
                                monthly_window = 22) {
  n <- length(rv_series)

  # Daily component (lagged)
  rv_daily <- c(rep(NA, daily_lag), rv_series[1:(n - daily_lag)])

  # Weekly component (rolling mean)
  rv_weekly <- rep(NA, n)
  for (i in weekly_window:n) {
    rv_weekly[i] <- mean(
      rv_series[(i - weekly_window + 1):i],
      na.rm = TRUE
    )
  }
  rv_weekly <- c(rep(NA, 1), rv_weekly[1:(n - 1)]) # Lag by 1

  # Monthly component (rolling mean)
  rv_monthly <- rep(NA, n)
  for (i in monthly_window:n) {
    rv_monthly[i] <- mean(
      rv_series[(i - monthly_window + 1):i],
      na.rm = TRUE
    )
  }
  rv_monthly <- c(rep(NA, 1), rv_monthly[1:(n - 1)]) # Lag by 1

  data.frame(
    rv_daily = rv_daily,
    rv_weekly = rv_weekly,
    rv_monthly = rv_monthly
  )
}

#' Fit HAR - RV model
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param extended Whether to include extended features
#' @return List with HAR model predictions
fit_har_models <- function(train, full, extended = FALSE) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  results <- list()

  tryCatch(
    {
      # Create HAR features
      har_features <- create_har_features(train_target)

      # Prepare data for regression
      y_train <- train_target
      x_train <- har_features

      # Remove missing values
      complete_cases <- complete.cases(cbind(y_train, x_train))
      y_clean <- y_train[complete_cases]
      x_clean <- x_train[complete_cases, ]

      if (nrow(x_clean) > 10) { # Minimum data requirement
        # Fit linear regression
        har_data <- data.frame(y = y_clean, x_clean)
        har_model <- lm(y ~ rv_daily + rv_weekly + rv_monthly, data = har_data)

        # Generate prediction using last available features
        last_features <- tail(x_clean, 1)
        prediction <- predict(har_model, newdata = last_features)

        if (!is.na(prediction)) {
          har_forecasts <- numeric(test_length)
          for (h in 1:test_length) {
            # HAR with mean reversion
            har_forecasts[h] <- as.numeric(prediction) * (0.95^h) +
              mean(train_target, na.rm = TRUE) * (1 - 0.95^h)
          }

          results[["HAR_RV"]] <- list(
            predictions = har_forecasts,
            parameters = coef(har_model),
            model_type = "HAR - RV",
            description = "Heterogeneous AutoRegressive Realized Volatility",
            r_squared = summary(har_model)$r.squared,
            model_object = har_model
          )
        }
      }
    },
    error = function(e) {
      warning(paste("HAR model fitting failed:", e$message))
    }
  )

  results
}

# ============================================================================
# MAIN MODELING FUNCTION
# ============================================================================

#' Main volatility modeling function
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param models List of models to fit
#' @return List of all model results
run_volatility_models <- function(train, full, models) {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  train_returns <- train$returns
  debug_log(" * * *  INSIDE run_volatility_models  * * * ")
  debug_log(paste("train_returns length:", length(train_returns)))
  debug_log(paste("train_target length:", length(train_target)))
  debug_log(paste("test_length:", test_length))
  debug_log(paste(
    "models structure:", paste(names(models), collapse = ", ")
  ))
  debug_log(paste(
    "model families:", paste(models$families, collapse = ", ")
  ))

  all_results <- list()

  # Naive models
  if ("naive" %in% models$families) {
    debug_log("Fitting naive models...")
    naive_results <- fit_naive_models(train, full)
    debug_log(paste("Naive models fitted:", length(naive_results)))
    all_results <- c(all_results, naive_results)
  }

  # Moving average models
  if ("moving_average" %in% models$families) {
    debug_log("Fitting moving average models...")
    ma_results <- fit_moving_average_models(train, full, models$ma_windows)
    debug_log(paste("MA models fitted:", length(ma_results)))
    all_results <- c(all_results, ma_results)

    if (models$adaptive_ma) {
      debug_log("Fitting adaptive MA...")
      adaptive_results <- fit_adaptive_moving_average(train, full)
      all_results <- c(all_results, adaptive_results)
    }
  }

  # EWMA models
  if ("ewma" %in% models$families) {
    debug_log("Fitting EWMA models...")
    ewma_results <- fit_ewma_models(
      train, full, models$ewma_lambdas, models$ewma_optimize
    )
    debug_log(paste("EWMA models fitted:", length(ewma_results)))
    all_results <- c(all_results, ewma_results)
  }

  # GARCH models
  if ("garch" %in% models$families && !is.null(models$garch_specs)) {
    debug_log("Fitting GARCH models...")
    garch_results <- fit_garch_models(train, full, models$garch_specs)
    debug_log(paste("GARCH models fitted:", length(garch_results)))
    all_results <- c(all_results, garch_results)
  }

  # Neural network models
  if ("neural_network" %in% models$families) {
    debug_log("Fitting neural network models...")
    nn_results <- fit_neural_network(
      train, full, models$nn_architecture, models$nn_lags
    )
    debug_log(paste("NN models fitted:", length(nn_results)))
    all_results <- c(all_results, nn_results)
  }

  # HAR models
  if ("har" %in% models$families) {
    debug_log("Fitting HAR models...")
    har_results <- fit_har_models(train, full, models$har_extended)
    debug_log(paste("HAR models fitted:", length(har_results)))
    all_results <- c(all_results, har_results)
  }

  debug_log(paste(" * * *  TOTAL MODELS FITTED:", length(all_results), " * * * "))
  debug_log(paste(
    "Model names:", paste(names(all_results), collapse = ", ")
  ))
  all_results
}
