# ============================================================================
# EVALUATION METRICS MODULE
# Exact metrics from thesis: MSE, RMSE, MAE, MAPE, QLIKE, LogLoss
# ============================================================================

#' Calculate Mean Squared Error
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return MSE value
calculate_mse <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have same length")
  }

  # Remove missing values
  valid_idx <- !is.na(actual) & !is.na(predicted) & is.finite(actual) &
    is.finite(predicted)
  actual_clean <- actual[valid_idx]
  predicted_clean <- predicted[valid_idx]

  if (length(actual_clean) == 0) {
    return(NA)
  }

  mean((actual_clean - predicted_clean)^2)
}

#' Calculate Root Mean Squared Error
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return RMSE value
calculate_rmse <- function(actual, predicted) {
  mse <- calculate_mse(actual, predicted)
  if (is.na(mse)) {
    return(NA)
  }
  sqrt(mse)
}

#' Calculate Mean Absolute Error
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return MAE value
calculate_mae <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have same length")
  }

  # Remove missing values
  valid_idx <- !is.na(actual) & !is.na(predicted) & is.finite(actual) &
    is.finite(predicted)
  actual_clean <- actual[valid_idx]
  predicted_clean <- predicted[valid_idx]

  if (length(actual_clean) == 0) {
    return(NA)
  }

  mean(abs(actual_clean - predicted_clean))
}

#' Calculate Mean Absolute Percentage Error
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return MAPE value (in percentage)
calculate_mape <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have same length")
  }

  # Remove missing values and zero actual values
  valid_idx <- !is.na(actual) & !is.na(predicted) & is.finite(actual) &
    is.finite(predicted) & actual != 0
  actual_clean <- actual[valid_idx]
  predicted_clean <- predicted[valid_idx]

  if (length(actual_clean) == 0) {
    return(NA)
  }

  mean(abs((actual_clean - predicted_clean) / actual_clean)) * 100
}

#' Calculate Quasi - Maximum Likelihood Error (QLIKE)
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return QLIKE value
calculate_qlike <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have same length")
  }

  # Remove missing values and ensure positive values
  valid_idx <- !is.na(actual) & !is.na(predicted) & is.finite(actual) &
    is.finite(predicted) & actual > 0 & predicted > 0
  actual_clean <- actual[valid_idx]
  predicted_clean <- predicted[valid_idx]

  if (length(actual_clean) == 0) {
    return(NA)
  }

  # For volatility forecasting: log(predicted) + actual / predicted
  mean(log(predicted_clean) + actual_clean / predicted_clean)
}

#' Calculate Log Loss (Logarithmic Loss)
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return LogLoss value
calculate_log_loss <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have same length")
  }

  # Remove missing values and ensure positive values
  valid_idx <- !is.na(actual) & !is.na(predicted) & is.finite(actual) &
    is.finite(predicted) & actual > 0 & predicted > 0
  actual_clean <- actual[valid_idx]
  predicted_clean <- predicted[valid_idx]

  if (length(actual_clean) == 0) {
    return(NA)
  }

  # Log Loss = mean(log(predicted) + actual / predicted - log(actual) - 1)
  mean(log(predicted_clean) + actual_clean / predicted_clean -
         log(actual_clean) - 1)
}

#' Calculate all metrics for a single model
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return Named list of all metrics
calculate_all_metrics <- function(actual, predicted) {
  list(
    MSE = calculate_mse(actual, predicted),
    RMSE = calculate_rmse(actual, predicted),
    MAE = calculate_mae(actual, predicted),
    MAPE = calculate_mape(actual, predicted),
    QLIKE = calculate_qlike(actual, predicted),
    LogLoss = calculate_log_loss(actual, predicted)
  )
}

#' Calculate metrics for multiple models
#' @param actual Vector of actual values
#' @param model_results List of model results with predictions
#' @return Data frame with metrics for all models
evaluate_all_models <- function(actual, model_results) {
  if (length(model_results) == 0) {
    return(data.frame())
  }

  metrics_list <- list()

  for (model_name in names(model_results)) {
    model_result <- model_results[[model_name]]

    if (!is.null(model_result$predictions)) {
      predicted <- model_result$predictions

      # Calculate all metrics
      metrics <- calculate_all_metrics(actual, predicted)

      # Create row for results table
      metrics_row <- data.frame(
        Model = model_name,
        Type = model_result$model_type,
        MSE = metrics$MSE,
        RMSE = metrics$RMSE,
        MAE = metrics$MAE,
        MAPE = metrics$MAPE,
        QLIKE = metrics$QLIKE,
        LogLoss = metrics$LogLoss,
        stringsAsFactors = FALSE
      )

      metrics_list[[model_name]] <- metrics_row
    }
  }

  if (length(metrics_list) == 0) {
    return(data.frame())
  }

  # Combine all results
  results_df <- do.call(rbind, metrics_list)
  rownames(results_df) <- NULL

  # Sort by RMSE (ascending)
  results_df <- results_df[order(results_df$RMSE, na.last = TRUE), ]

  results_df
}

#' Diebold - Mariano test for forecast accuracy comparison
#' @param errors1 Forecast errors from model 1
#' @param errors2 Forecast errors from model 2
#' @param alternative Alternative hypothesis ("two.sided", "less", "greater")
#' @return List with test statistic and p - value
diebold_mariano_test <- function(errors1, errors2,
                                 alternative = "two.sided") {
  # Calculate loss differential
  d <- errors1^2 - errors2^2
  d_bar <- mean(d, na.rm = TRUE)
  n <- length(d)

  if (n < 2) {
    return(list(
      statistic = NA, p_value = NA, message = "Insufficient data"
    ))
  }

  # Calculate variance of loss differential
  gamma0 <- var(d, na.rm = TRUE)

  if (gamma0 <= 0) {
    return(list(statistic = NA, p_value = NA, message = "Zero variance"))
  }

  # DM test statistic
  dm_stat <- d_bar / sqrt(gamma0 / n)

  # Calculate p - value
  if (alternative == "two.sided") {
    p_value <- 2 * (1 - pnorm(abs(dm_stat)))
  } else if (alternative == "less") {
    p_value <- pnorm(dm_stat)
  } else if (alternative == "greater") {
    p_value <- 1 - pnorm(dm_stat)
  } else {
    stop("Invalid alternative hypothesis")
  }

  return(list(
    statistic = dm_stat,
    p_value = p_value,
    d_bar = d_bar,
    variance = gamma0
  ))
}

#' Perform pairwise Diebold - Mariano tests
#' @param actual Vector of actual values
#' @param model_results List of model results
#' @param alpha Significance level
#' @return Matrix of p - values for pairwise comparisons
pairwise_dm_tests <- function(actual, model_results, alpha = 0.05) {
  model_names <- names(model_results)
  n_models <- length(model_names)

  if (n_models < 2) {
    return(NULL)
  }

  # Initialize results matrix
  p_value_matrix <- matrix(NA,
    nrow = n_models, ncol = n_models,
    dimnames = list(model_names, model_names)
  )

  # Calculate forecast errors for all models
  errors_list <- list()
  for (i in 1:n_models) {
    model_name <- model_names[i]
    predicted <- model_results[[model_name]]$predictions
    errors_list[[model_name]] <- actual - predicted
  }

  # Pairwise tests
  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      model1 <- model_names[i]
      model2 <- model_names[j]

      dm_result <- diebold_mariano_test(
        errors_list[[model1]], errors_list[[model2]]
      )

      p_value_matrix[i, j] <- dm_result$p_value
      p_value_matrix[j, i] <- dm_result$p_value # Symmetric matrix
    }
  }

  return(p_value_matrix)
}

#' Format metrics for display
#' @param metrics_df Data frame of metrics
#' @param digits Number of digits for rounding
#' @return Data frame with formatted metrics
format_metrics_table <- function(metrics_df, digits = 6) {
  if (nrow(metrics_df) == 0) {
    return(metrics_df)
  }

  formatted_df <- metrics_df

  # Format numeric columns
  numeric_cols <- c("MSE", "RMSE", "MAE", "MAPE", "QLIKE", "LogLoss")

  for (col in numeric_cols) {
    if (col %in% names(formatted_df)) {
      if (col == "MSE") {
        # Scientific notation for MSE
        formatted_df[[col]] <- formatC(
          formatted_df[[col]],
          format = "e", digits = 2
        )
      } else if (col == "MAPE") {
        # Percentage format for MAPE
        formatted_df[[col]] <- paste0(
          formatC(formatted_df[[col]], format = "f", digits = 2), "%"
        )
      } else {
        # Fixed point notation for others
        formatted_df[[col]] <- formatC(
          formatted_df[[col]],
          format = "f", digits = digits
        )
      }
    }
  }

  formatted_df
}

#' Get metric descriptions
#' @return List of metric descriptions with formulas
get_metric_descriptions <- function() {
  list(
    MSE = list(
      name = "Mean Squared Error",
      formula = "1 / T Σ(σ_t - σ̂_t)²",
      description = "Average of squared forecast errors"
    ),
    RMSE = list(
      name = "Root Mean Squared Error",
      formula = "√(MSE)",
      description = "Square root of MSE, in same units as data"
    ),
    MAE = list(
      name = "Mean Absolute Error",
      formula = "1 / T Σ|σ_t - σ̂_t|",
      description = "Average of absolute forecast errors"
    ),
    MAPE = list(
      name = "Mean Absolute Percentage Error",
      formula = "100 / T Σ|σ_t - σ̂_t| / σ_t",
      description = "Average percentage error (scale - free)"
    ),
    QLIKE = list(
      name = "Quasi - Maximum Likelihood",
      formula = "1 / T Σ[ln(σ̂_t) + σ_t / σ̂_t]",
      description = "Likelihood - based loss function"
    ),
    LogLoss = list(
      name = "Logarithmic Loss",
      formula = "1 / T Σ[ln(σ̂_t) + σ_t / σ̂_t - ln(σ_t) - 1]",
      description = "Log - likelihood loss function"
    )
  )
}

#' Validate metrics calculation inputs
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return List with validation results
validate_metrics_inputs <- function(actual, predicted) {
  errors <- c()
  warnings <- c()

  if (length(actual) != length(predicted)) {
    errors <- c(errors, "Actual and predicted vectors must have same length")
  }

  if (length(actual) == 0) {
    errors <- c(errors, "Input vectors cannot be empty")
  }

  if (all(is.na(actual))) {
    errors <- c(errors, "All actual values are missing")
  }

  if (all(is.na(predicted))) {
    errors <- c(errors, "All predicted values are missing")
  }

  # Check for negative values (problematic for QLIKE and LogLoss)
  if (any(actual <= 0, na.rm = TRUE)) {
    warnings <- c(
      warnings,
      paste(
        "Negative or zero actual values detected",
        "- QLIKE and LogLoss may be invalid"
      )
    )
  }

  if (any(predicted <= 0, na.rm = TRUE)) {
    warnings <- c(
      warnings,
      paste(
        "Negative or zero predicted values detected",
        "- QLIKE and LogLoss may be invalid"
      )
    )
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}
