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
  debug_log("*** FITTING NAIVE MODELS ***")

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

  # Historical Average: constant forecast (no time-varying)
  forecasts_hist <- rep(hist_mean, test_length)

  results[["Historical_Average"]] <- list(
    predictions = forecasts_hist,
    parameters = list(mean = hist_mean),
    model_type = "Naive",
    description = "Historical average of full dataset"
  )

  # Random Walk - use actual previous observations from test data
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
      forecasts_rw[h] <- test_target[h - 1]
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
                                      windows = c(7, 14, 20, 30, 60)) {
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
        # Combine training + test data for rolling calculations
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

  # Simple optimization: choose window that minimizes in-sample error
  for (window in windows) {
    if (window <= length(train_target)) {
      # Calculate rolling MA for in-sample evaluation
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
      (1 - lambda) * target_series[t]
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

    # Generate EWMA forecasts
    forecasts <- numeric(test_length)

    # Debug information
    debug_log <- function(msg) {
      if (exists("debug_log", envir = .GlobalEnv)) {
        do.call(get("debug_log", envir = .GlobalEnv), list(msg))
      }
    }

    debug_log(paste("EWMA Debug - lambda:", lambda))
    debug_log(paste("EWMA Debug - last_ewma:", last_ewma))
    debug_log(paste("EWMA Debug - test_length:", test_length))
    debug_log(paste("EWMA Debug - test_target is null:", is.null(test_target)))
    if (!is.null(test_target)) {
      debug_log(paste("EWMA Debug - test_target length:", length(test_target)))
    }

    # Check if we have valid last_ewma
    if (is.na(last_ewma) || is.null(last_ewma)) {
      # Use mean of training data as fallback
      last_ewma <- mean(train_target, na.rm = TRUE)
      debug_log(paste("EWMA Debug - Using mean fallback:", last_ewma))
    }

    if (!is.null(test_target) && length(test_target) >= test_length &&
        !any(is.na(test_target))) {
      # Dynamic EWMA updating with real data (like in thesis)
      current_ewma <- last_ewma

      for (h in 1:test_length) {
        # Use current EWMA as forecast
        forecasts[h] <- current_ewma
        # Update EWMA with actual observed value for next forecast
        if (h <= length(test_target)) {
          actual_value <- test_target[h]
          if (!is.na(actual_value)) {
            current_ewma <- lambda * current_ewma + (1 - lambda) * actual_value
          }
        }
      }
      debug_log("EWMA Debug - Used rolling forecasting")
    } else {
      # Static forecast using last EWMA value
      forecasts <- rep(last_ewma, test_length)
      debug_log("EWMA Debug - Used static forecast (fallback)")
    }

    debug_log(paste(
      "EWMA Debug - forecasts range:",
      min(forecasts, na.rm = TRUE), "to",
      max(forecasts, na.rm = TRUE)
    ))

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

    # Optimize lambda using in-sample forecasting
    for (lambda in lambda_range) {
      ewma_series <- calculate_ewma_series(train_target, lambda)

      # Calculate one-step ahead forecast errors
      errors <- numeric()
      for (t in 2:length(train_target)) {
        forecast <- ewma_series[t - 1] # Use previous EWMA value as forecast
        actual <- train_target[t]
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

    # Fit with optimal lambda and create dynamic forecasts
    debug_log(paste("EWMA Optimized - best_lambda:", best_lambda))
    debug_log(paste("EWMA Optimized - best_error:", best_error))

    ewma_series <- calculate_ewma_series(train_target, best_lambda)
    last_ewma <- tail(ewma_series, 1)

    debug_log(paste("EWMA Optimized - last_ewma:", last_ewma))

    # Check if we have valid last_ewma
    if (is.na(last_ewma) || is.null(last_ewma)) {
      # Use mean of training data as fallback
      last_ewma <- mean(train_target, na.rm = TRUE)
      debug_log(paste("EWMA Optimized - Using mean fallback:", last_ewma))
    }

    # Dynamic EWMA prediction function (like in thesis)
    if (!is.null(test_target) && length(test_target) >= test_length &&
        !any(is.na(test_target))) {
      forecasts <- numeric(test_length)
      current_ewma <- last_ewma

      for (h in 1:test_length) {
        # Use current EWMA as forecast
        forecasts[h] <- current_ewma
        # Update EWMA with actual value for next forecast
        if (h <= length(test_target)) {
          actual_value <- test_target[h]
          if (!is.na(actual_value)) {
            current_ewma <- best_lambda * current_ewma +
              (1 - best_lambda) * actual_value
          }
        }
      }
      debug_log("EWMA Optimized - Used dynamic updating")
    } else {
      # Static forecast with last EWMA value
      forecasts <- rep(last_ewma, test_length)
      debug_log("EWMA Optimized - Used static forecast")
    }

    debug_log(paste(
      "EWMA Optimized - forecasts range:",
      min(forecasts, na.rm = TRUE), "to",
      max(forecasts, na.rm = TRUE)
    ))

    results[["EWMA_Optimized"]] <- list(
      predictions = forecasts,
      parameters = list(
        lambda = best_lambda,
        last_value = last_ewma,
        optimization_error = best_error
      ),
      model_type = "EWMA Optimized",
      description = paste(
        "Optimized EWMA with lambda = ", round(best_lambda, 3)
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
#' @param target_type Type of target volatility
#'   ("squared", "rolling_std", "absolute")
#' @return List of GARCH model predictions
fit_garch_models <- function(train, full, garch_specs,
                              target_type = "squared") {
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
          # Rolling GARCH forecasting using out.sample and n.roll
          predictions <- numeric(test_length)

          if (!is.null(test_target) && length(test_target) >= test_length) {
            # Proper rolling forecast with out.sample approach
            tryCatch(
              {
                # Combine train and test returns for out.sample approach
                test_returns <- full$returns[
                  (length(returns) + 1):length(full$returns)
                ]
                combined_returns <- c(returns, test_returns)

                # Refit model with combined data using out.sample
                combined_fit <- ugarchfit(
                  spec = garch_specs[[spec_name]],
                  data = combined_returns,
                  out.sample = test_length,
                  solver = "hybrid"
                )

                if (convergence(combined_fit) == 0) {
                  # Rolling forecast
                  if (test_length > 1) {
                    forecast <- ugarchforecast(
                      combined_fit,
                      n.ahead = 1,
                      n.roll = test_length - 1
                    )
                    # Adjust scale based on target type
                    if (target_type == "rolling_std") {
                      # Standard deviation
                      predictions <- as.numeric(sigma(forecast))
                    } else {
                      # Variance
                      predictions <- as.numeric(sigma(forecast))^2
                    }
                  } else {
                    forecast <- ugarchforecast(combined_fit, n.ahead = 1)
                    # Adjust scale based on target type
                    if (target_type == "rolling_std") {
                      # Standard deviation
                      predictions <- as.numeric(sigma(forecast))
                    } else {
                      # Variance
                      predictions <- as.numeric(sigma(forecast))^2
                    }
                  }

                  debug_log(
                    paste(
                      "GARCH", spec_name,
                      "- Used rolling forecast with out.sample"
                    )
                  )
                } else {
                  # If combined fit fails, use original fit for static forecast
                  forecast <- ugarchforecast(
                    fit, n.ahead = min(test_length, 100)
                  )
                  sigma_forecast <- as.numeric(sigma(forecast))

                  if (length(sigma_forecast) < test_length) {
                    sigma_forecast <- c(
                      sigma_forecast,
                      rep(
                        tail(sigma_forecast, 1),
                        test_length - length(sigma_forecast)
                      )
                    )
                  }

                  # Adjust scale based on target type
                  if (target_type == "rolling_std") {
                    predictions <- sigma_forecast
                  } else {
                    predictions <- sigma_forecast^2
                  }
                  debug_log(
                    paste(
                      "GARCH", spec_name,
                      "- Used static forecast (combined fit failed)"
                    )
                  )
                }
              },
              error = function(e) {
                debug_log(
                  paste(
                    "GARCH", spec_name,
                    "out.sample forecast failed:", e$message
                  )
                )

                # Fallback to static forecast with original fit
                forecast <- ugarchforecast(
                  fit, n.ahead = min(test_length, 100)
                )
                sigma_forecast <- as.numeric(sigma(forecast))

                if (length(sigma_forecast) < test_length) {
                  sigma_forecast <- c(
                    sigma_forecast,
                    rep(
                      tail(sigma_forecast, 1),
                      test_length - length(sigma_forecast)
                    )
                  )
                }

                # Adjust scale based on target type
                if (target_type == "rolling_std") {
                  predictions <<- sigma_forecast
                } else {
                  predictions <<- sigma_forecast^2
                }
                debug_log(
                  paste(
                    "GARCH", spec_name,
                    "- Used static forecast (error fallback)"
                  )
                )
              }
            )
          } else {
            # Fallback: static forecast
            forecast <- ugarchforecast(fit, n.ahead = min(test_length, 100))
            sigma_forecast <- as.numeric(sigma(forecast))

            if (length(sigma_forecast) < test_length) {
              sigma_forecast <- c(
                sigma_forecast,
                rep(
                  tail(sigma_forecast, 1),
                  test_length - length(sigma_forecast)
                )
              )
            }

            # Adjust scale based on target type
            if (target_type == "rolling_std") {
              predictions <- sigma_forecast  # Standard deviation
            } else {
              predictions <- sigma_forecast^2  # Variance
            }
            debug_log(
              paste("GARCH", spec_name, "- Used static forecasting (fallback)")
            )
          }

          debug_log(paste(
            "GARCH", spec_name, "- Final predictions range:",
            min(predictions, na.rm = TRUE), "to",
            max(predictions, na.rm = TRUE)
          ))

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

#' Create additional GARCH specifications (FIGARCH, EGARCH, GJR-GARCH)
#' @return List of additional GARCH specs like in thesis
create_additional_garch_specs <- function() {
  specs <- list()

  # FIGARCH models (long memory) - exactly like in thesis
  figarch_distributions <- c("norm", "snorm", "std", "sstd", "ged")
  for (dist in figarch_distributions) {
    spec_name <- paste0("FIGARCH_", dist)
    specs[[spec_name]] <- ugarchspec(
      variance.model = list(model = "fiGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = dist
    )
  }

  # EGARCH models (asymmetric) - exactly like in thesis
  egarch_distributions <- c("norm", "snorm", "std", "sstd", "ged")
  for (dist in egarch_distributions) {
    spec_name <- paste0("EGARCH11_", dist)
    specs[[spec_name]] <- ugarchspec(
      variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = dist
    )
  }

  # GJR-GARCH models (asymmetric) - exactly like in thesis
  gjr_distributions <- c("norm", "snorm", "std", "sstd", "ged")
  for (dist in gjr_distributions) {
    spec_name <- paste0("GJR11_", dist)
    specs[[spec_name]] <- ugarchspec(
      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = dist
    )
  }

  return(specs)
}

# ============================================================================
# 5. MACHINE LEARNING MODELS
# ============================================================================

#' Prepare data for neural network models (like in thesis)
#' @param returns Return series for features
#' @param target_volatility Target volatility series for training
#' @param lags Number of lags to use as features
#' @return List with X (features) and y (targets)
prepare_nn_data <- function(returns, target_volatility, lags = 20) {
  n <- length(returns)
  if (n <= lags) {
    stop("Insufficient data for specified lags")
  }

  x_matrix <- matrix(NA, nrow = n - lags, ncol = lags)
  y <- numeric(n - lags)

  for (i in 1:(n - lags)) {
    x_matrix[i, ] <- as.numeric(returns[i:(i + lags - 1)]^2)
    # Use actual target volatility
    y[i] <- as.numeric(target_volatility[i + lags])
  }

  return(list(X = x_matrix, y = y))
}

#' Prepare data for LSTM models (like in thesis)
#' @param returns Return series for features
#' @param target_volatility Target volatility series for training
#' @param lags Number of lags to use as features
#' @return List with X (3D array) and y (targets)
prepare_lstm_data <- function(returns, target_volatility, lags = 20) {
  n <- length(returns)
  if (n <= lags) {
    stop("Insufficient data for specified lags")
  }

  x_array <- array(NA, dim = c(n - lags, lags, 1))
  y <- numeric(n - lags)

  for (i in 1:(n - lags)) {
    x_array[i, , 1] <- as.numeric(returns[i:(i + lags - 1)]^2)
    # Use actual target volatility
    y[i] <- as.numeric(target_volatility[i + lags])
  }

  return(list(X = x_array, y = y))
}

#' Fit simple neural network
#' @param train List with target and returns from training period
#' @param full List with target and returns from full dataset
#' @param architecture Vector specifying layer sizes
#' @param lags Number of input lags
#' @param target_type Type of target volatility
#'   ("squared", "rolling_std", "absolute")
#' @return List with neural network predictions
fit_neural_network <- function(train, full, architecture = c(50, 25, 1),
                               lags = 20, target_type = "squared") {
  train_returns <- train$returns
  full_returns <- full$returns
  test_length <- length(full_returns) - length(train_returns)
  test_returns <- full_returns[(length(train_returns) + 1):length(full_returns)]

  results <- list()

  # Check if keras/tensorflow is available
  keras_available <- requireNamespace("keras3", quietly = TRUE) &&
    requireNamespace("tensorflow", quietly = TRUE)

  if (!keras_available) {
    warning("Keras/TensorFlow not available.")
    return(results)
  }

  tryCatch(
    {
      library(keras3, quietly = TRUE)
      library(tensorflow, quietly = TRUE)

      # Set random seed for reproducibility
      tensorflow::set_random_seed(6)

# ============================================================================
# MLP MODEL
# ============================================================================

      # Prepare training data for MLP using actual target volatility
      train_target <- train$target
      train_data <- prepare_nn_data(train_returns, train_target, lags = lags)
      x_train <- train_data$X
      y_train <- train_data$y

      # Scale the data (like in thesis)
      x_mean <- mean(x_train)
      x_sd <- sd(x_train)
      x_train_scaled <- (x_train - x_mean) / x_sd

      y_mean <- mean(y_train)
      y_sd <- sd(y_train)
      y_train_scaled <- (y_train - y_mean) / y_sd

      # Build MLP model (exactly like in thesis)
      mlp_model <- keras_model_sequential(input_shape = c(lags)) %>%
        layer_dense(units = architecture[1], activation = "relu") %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = architecture[2], activation = "relu") %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = 1)

      suppressWarnings({
        mlp_model %>% compile(
          optimizer = optimizer_adam(learning_rate = 0.001),
          loss = "mse",
          metrics = c("mae")
        )
      })

      # Train MLP model
      history_mlp <- mlp_model %>% fit(
        x_train_scaled, y_train_scaled,
        epochs = 100,
        batch_size = 32,
        validation_split = 0.2,
        verbose = 0
      )

      # Rolling forecasting for MLP
      mlp_forecasts <- numeric(test_length)
      combined_returns <- c(train_returns, test_returns)

      for (h in 1:test_length) {
        # Prepare test data for current forecast (pseudo-out-of-sample)
        available_returns <- combined_returns[
          1:(length(train_returns) + h - 1)
        ]
        if (length(available_returns) >= lags) {
          # Use last 'lags' returns as features
          features <- tail(available_returns, lags)^2
          features_scaled <- (features - x_mean) / x_sd

          # Predict with MLP
          pred_scaled <- predict(
            mlp_model,
            matrix(features_scaled, nrow = 1),
            verbose = 0
          )
          mlp_forecasts[h] <- as.numeric(pred_scaled) * y_sd + y_mean
        } else {
          mlp_forecasts[h] <- y_mean # Fallback
        }
      }

      # Adjust MLP predictions scale based on target type
      if (target_type == "rolling_std") {
        # Predictions are already in std dev form, just ensure non-negative
        mlp_forecasts_adjusted <- pmax(mlp_forecasts, 0)
      } else {
        # For variance targets, square the predictions
        mlp_forecasts_adjusted <- mlp_forecasts^2  # Convert to variance
      }

      results[["MLP_50_25_1"]] <- list(
        predictions = mlp_forecasts_adjusted,
        parameters = list(
          architecture = architecture,
          lags = lags,
          epochs = 100,
          batch_size = 32
        ),
        model_type = "Neural Network",
        description = "MLP (50-25-1)"
      )

# ============================================================================
# LSTM MODEL
# ============================================================================

      # Prepare training data for LSTM using actual target volatility
      lstm_train_data <- prepare_lstm_data(train_returns, train_target, lags = lags)
      x_train_lstm <- lstm_train_data$X
      y_train_lstm <- lstm_train_data$y

      x_train_lstm_scaled <- (x_train_lstm - x_mean) / x_sd
      y_train_lstm_scaled <- (y_train_lstm - y_mean) / y_sd

      # Build LSTM model
      lstm_model <- keras_model_sequential(input_shape = c(lags, 1)) %>%
        layer_lstm(units = architecture[1], return_sequences = TRUE) %>%
        layer_dropout(rate = 0.2) %>%
        layer_lstm(units = architecture[2]) %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = 1)

      suppressWarnings({
        lstm_model %>% compile(
          optimizer = optimizer_adam(learning_rate = 0.001),
          loss = "mse",
          metrics = c("mae")
        )
      })

      # Train LSTM model
      history_lstm <- lstm_model %>% fit(
        x_train_lstm_scaled, y_train_lstm_scaled,
        epochs = 100,
        batch_size = 32,
        validation_split = 0.2,
        verbose = 0
      )

      # Rolling forecasting for LSTM
      lstm_forecasts <- numeric(test_length)

      for (h in 1:test_length) {
        # Prepare test data for current forecast (pseudo-out-of-sample)
        available_returns <- combined_returns[
          1:(length(train_returns) + h - 1)
        ]
        if (length(available_returns) >= lags) {
          # Use last 'lags' returns as features (3D array for LSTM)
          features <- tail(available_returns, lags)^2
          features_scaled <- (features - x_mean) / x_sd
          features_3d <- array(features_scaled, dim = c(1, lags, 1))

          # Predict with LSTM
          pred_scaled <- predict(lstm_model, features_3d, verbose = 0)
          lstm_forecasts[h] <- as.numeric(pred_scaled) * y_sd + y_mean
        } else {
          lstm_forecasts[h] <- y_mean # Fallback
        }
      }

      # Adjust LSTM predictions scale based on target type
      if (target_type == "rolling_std") {
        # Predictions are already in std dev form, just ensure non-negative
        lstm_forecasts_adjusted <- pmax(lstm_forecasts, 0)
      } else {
        # For variance targets, square the predictions
        lstm_forecasts_adjusted <- lstm_forecasts^2  # Convert to variance
      }

      results[["LSTM_50_25_1"]] <- list(
        predictions = lstm_forecasts_adjusted,
        parameters = list(
          architecture = architecture,
          lags = lags,
          epochs = 100,
          batch_size = 32
        ),
        model_type = "Neural Network",
        description = "LSTM (50-25-1)"
      )
    },
    error = function(e) {
      warning(paste("Neural network fitting failed:", e$message))
    }
  )

  return(results)
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

#' Create HAR extended features (like in thesis)
#' @param returns Return series
#' @param rv_series Realized volatility series
#' @return Data frame with extended HAR features
create_har_extended_features <- function(returns, rv_series) {
  har_base <- create_har_features(rv_series)

  n <- length(rv_series)

  # Extended features exactly like in thesis
  neg_ret <- ifelse(returns < 0, 1, 0)
  neg_ret <- c(NA, neg_ret[1:(n - 1)]) # Lag by 1

  neg_shock <- ifelse(returns < 0, returns^2, 0)
  neg_shock <- c(NA, neg_shock[1:(n - 1)]) # Lag by 1

  high_vol <- rv_series > quantile(rv_series, 0.75, na.rm = TRUE)
  high_vol <- c(NA, high_vol[1:(n - 1)]) # Lag by 1

  extended_features <- data.frame(
    rv_daily = har_base$rv_daily,
    rv_weekly = har_base$rv_weekly,
    rv_monthly = har_base$rv_monthly,
    neg_ret = neg_ret,
    neg_shock = neg_shock,
    high_vol = as.numeric(high_vol)
  )

  extended_features
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
      # HAR-RV Standard model
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

        # Rolling HAR forecasting
        har_forecasts <- numeric(test_length)

        if (!is.null(test_target) && length(test_target) >= test_length) {
          # Rolling forecast with expanding window
          combined_target <- c(train_target, test_target)

          for (h in 1:test_length) {
            # Pseudo-out-of-sample: use only training model (no refitting)
            # Create features using data up to prediction point
            # (h-1 test observations)
            available_target <- combined_target[
              1:(length(train_target) + h - 1)
            ]
            current_features <- create_har_features(available_target)
            last_features <- tail(
              current_features[complete.cases(current_features), ],
              1
            )

            if (nrow(last_features) > 0) {
              har_forecasts[h] <- predict(har_model, newdata = last_features)
            } else {
              # Fallback to unconditional mean
              har_forecasts[h] <- mean(train_target, na.rm = TRUE)
            }
          }
        } else {
          # Fallback: static forecast using last available features
          last_features <- tail(x_clean, 1)
          prediction <- predict(har_model, newdata = last_features)
          har_forecasts <- rep(as.numeric(prediction), test_length)
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

      # HAR-RV Extended model (if requested)
      if (extended && !is.null(train$returns) && !is.null(full$returns)) {
        tryCatch(
          {
            # Create extended HAR features
            train_returns <- train$returns
            full_returns <- full$returns
            combined_returns <- c(
              train_returns,
              full_returns[(length(train_returns) + 1):length(full_returns)]
            )
            combined_target <- c(train_target, test_target)

            har_ext_features <- create_har_extended_features(train_returns, train_target)

            # Prepare data for extended regression
            y_train_ext <- train_target
            x_train_ext <- har_ext_features

            # Remove missing values
            complete_cases_ext <- complete.cases(
              cbind(y_train_ext, x_train_ext)
            )
            y_clean_ext <- y_train_ext[complete_cases_ext]
            x_clean_ext <- x_train_ext[complete_cases_ext, ]

            if (nrow(x_clean_ext) > 10) { # Minimum data requirement
              # Fit extended linear regression
              har_ext_data <- data.frame(y = y_clean_ext, x_clean_ext)
              har_ext_model <- lm(y ~ ., data = har_ext_data)

              # Rolling HAR extended forecasting
              har_ext_forecasts <- numeric(test_length)

              if (!is.null(test_target) && length(test_target) >= test_length) {
                for (h in 1:test_length) {
                  # Create extended features using data up to prediction point
                  available_returns <- combined_returns[
                    1:(length(train_returns) + h - 1)
                  ]
                  available_target <- combined_target[
                    1:(length(train_target) + h - 1)
                  ]

                  if (length(available_returns) >= length(train_returns) &&
                    length(available_target) >= length(train_target)) {
                    current_ext_features <- create_har_extended_features(
                      available_returns,
                      available_target
                    )
                    last_ext_features <- tail(
                      current_ext_features[complete.cases(current_ext_features), ],
                      1
                    )

                    if (nrow(last_ext_features) > 0) {
                      har_ext_forecasts[h] <- predict(
                        har_ext_model,
                        newdata = last_ext_features
                      )
                    } else {
                      har_ext_forecasts[h] <- mean(train_target, na.rm = TRUE)
                    }
                  } else {
                    har_ext_forecasts[h] <- mean(train_target, na.rm = TRUE)
                  }
                }
              } else {
                # Static forecast using last available features
                last_ext_features <- tail(x_clean_ext, 1)
                prediction_ext <- predict(
                  har_ext_model,
                  newdata = last_ext_features
                )
                har_ext_forecasts <- rep(
                  as.numeric(prediction_ext),
                  test_length
                )
              }

              results[["HAR_RV_Extended"]] <- list(
                predictions = har_ext_forecasts,
                parameters = coef(har_ext_model),
                model_type = "HAR-RV Extended",
                description = "HAR-RV with asymmetric components",
                r_squared = summary(har_ext_model)$r.squared,
                model_object = har_ext_model
              )
            }
          },
          error = function(e) {
            warning(paste("HAR extended model fitting failed:", e$message))
          }
        )
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
#' @param target_type Type of target volatility measure
#' @return List of all model results
run_volatility_models <- function(train, full, models, target_type = "squared") {
  train_target <- train$target
  full_target <- full$target
  test_length <- length(full_target) - length(train_target)
  test_target <- full_target[
    (length(train_target) + 1):length(full_target)
  ]
  train_returns <- train$returns
  debug_log("*** INSIDE run_volatility_models ***")
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
    garch_results <- fit_garch_models(train, full, models$garch_specs, target_type)
    debug_log(paste("GARCH models fitted:", length(garch_results)))
    all_results <- c(all_results, garch_results)
  }

  # Neural network models
  if ("neural_network" %in% models$families) {
    debug_log("Fitting neural network models...")
    nn_results <- fit_neural_network(
      train, full, models$nn_architecture, models$nn_lags, target_type
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
