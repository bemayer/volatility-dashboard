# ============================================================================
# UI COMPONENTS MODULE
# Reusable UI components for the dashboard
# ============================================================================

#' Safe notification helper with type validation
#' @param message Notification message
#' @param type Notification type (default, message, warning, error)
#' @param duration Duration in seconds (optional)
#' @param id Notification ID (optional)
safe_notification <- function(message, type = "default",
                              duration = NULL, id = NULL) {
  # Validate type parameter
  valid_types <- c("default", "message", "warning", "error")
  if (!type %in% valid_types) {
    type <- "default"
  }

  # Build notification parameters
  params <- list(
    ui = message,
    type = type
  )

  if (!is.null(duration)) {
    params$duration <- duration
  }

  if (!is.null(id)) {
    params$id <- id
  }

  # Call showNotification safely
  do.call(showNotification, params)
}

#' Debug logging function for troubleshooting
#' @param message Debug message
#' @param level Log level (INFO, WARNING, ERROR)
debug_log <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] [", level, "] ", message, "\n"))
  flush.console()
}

#' Create a metric info box
#' @param title Box title
#' @param value Main value to display
#' @param subtitle Additional information
#' @param icon Icon name
#' @param color Box color
#' @return Shiny valueBox
metric_info_box <- function(title, value, subtitle = NULL,
                            icon = "info", color = "blue") {
  valueBox(
    value = value,
    subtitle = if (!is.null(subtitle)) paste(title, "-", subtitle) else title,
    icon = icon(icon),
    color = color,
    width = NULL
  )
}

#' Create a formula display box
#' @param title Formula title
#' @param formula LaTeX formula
#' @param description Description text
#' @return Shiny box with formula
formula_box <- function(title, formula, description = NULL) {
  box(
    title = title,
    status = "info",
    solidHeader = TRUE,
    width = 12,
    div(
      style = "text-align: center; padding: 20px;",
      h4(style = "margin-bottom: 20px;", title),
      div(
        style = paste("background: #f8f9fa; padding: 15px;",
                      "border-radius: 5px; margin: 10px 0;"),
        withMathJax(paste0("$$", formula, "$$"))
      ),
      if (!is.null(description)) {
        p(style = "font-style: italic; color: #666;", description)
      }
    )
  )
}

#' Create a model card for displaying model information
#' @param model_name Name of the model
#' @param model_type Type/category of the model
#' @param parameters Model parameters
#' @param description Model description
#' @param actions Action buttons (Edit, Remove)
#' @return Shiny div with model card
model_card <- function(model_name, model_type, parameters = NULL,
                       description = NULL, actions = TRUE) {

  param_text <- if (!is.null(parameters)) {
    param_names <- names(parameters)
    param_values <- sapply(parameters, function(x) {
      if (is.numeric(x)) round(x, 4) else as.character(x)
    })
    paste(param_names, "=", param_values, collapse = ", ")
  } else {
    ""
  }

  div(
    class = "model-card",
    fluidRow(
      column(8,
        h5(style = "margin: 0; color: #2c3e50;", model_name),
        p(style = "margin: 5px 0; color: #7f8c8d;", model_type),
        if (!is.null(description)) {
          p(style = "margin: 5px 0; font-size: 12px; color: #95a5a6;",
            description)
        },
        if (param_text != "") {
          p(style = paste("margin: 5px 0; font-size: 11px;",
                          "font-family: monospace; color: #34495e;"),
            param_text)
        }
      ),
      if (actions) {
        column(4, style = "text-align: right;",
          actionButton(
            paste0("remove_", gsub("[^A-Za-z0-9]", "_", model_name)),
            "Remove",
            size = "xs",
            class = "btn-outline-danger"
          )
        )
      }
    )
  )
}

#' Create sortable table header
#' @param column_name Column name
#' @param display_name Display name
#' @param current_sort Current sort column
#' @param sort_direction Current sort direction
#' @return Shiny span with sortable header
sortable_header <- function(column_name, display_name,
                            current_sort = NULL, sort_direction = "asc") {

  arrow <- if (!is.null(current_sort) && current_sort == column_name) {
    if (sort_direction == "asc") "↑" else "↓"
  } else {
    "↕"
  }

  span(
    style = "cursor: pointer; user-select: none;",
    display_name, " ", arrow
  )
}

#' Create parameter input based on type
#' @param param_name Parameter name
#' @param param_type Parameter type ("numeric", "integer", "choice", "logical")
#' @param default_value Default value
#' @param choices Choices for selection (if type = "choice")
#' @param min Minimum value (for numeric/integer)
#' @param max Maximum value (for numeric/integer)
#' @return Shiny input control
create_parameter_input <- function(param_name, param_type, default_value = NULL,
                                   choices = NULL, min = NULL, max = NULL) {
  input_id <- paste0("param_", gsub("[^A-Za-z0-9]", "_", param_name))

  switch(param_type,
    "numeric" = numericInput(
      input_id,
      param_name,
      value = default_value,
      min = min,
      max = max,
      step = if (is.null(min) || is.null(max)) 0.01 else (max - min) / 100
    ),

    "integer" = numericInput(
      input_id,
      param_name,
      value = default_value,
      min = min,
      max = max,
      step = 1
    ),

    "choice" = selectInput(
      input_id,
      param_name,
      choices = choices,
      selected = default_value
    ),

    "logical" = checkboxInput(
      input_id,
      param_name,
      value = default_value %||% FALSE
    ),

    "text" = textInput(
      input_id,
      param_name,
      value = default_value %||% ""
    ),

    # Default to text input
    textInput(input_id, param_name, value = default_value %||% "")
  )
}

#' Create progress indicator
#' @param message Progress message
#' @param value Progress value (0-100)
#' @return Shiny div with progress bar
progress_indicator <- function(message, value = NULL) {
  if (is.null(value)) {
    # Indeterminate progress
    div(
      style = "text-align: center; padding: 20px;",
      div(class = "spinner-border text-primary", role = "status"),
      br(), br(),
      p(message, style = "color: #666;")
    )
  } else {
    # Determinate progress
    div(
      style = "padding: 20px;",
      p(message, style = "color: #666; margin-bottom: 10px;"),
      div(
        class = "progress",
        div(
          class = "progress-bar progress-bar-striped progress-bar-animated",
          role = "progressbar",
          style = paste0("width: ", value, "%;"),
          paste0(value, "%")
        )
      )
    )
  }
}

#' Create alert box
#' @param message Alert message
#' @param type Alert type ("info", "warning", "danger", "success")
#' @return Shiny div with alert
alert_box <- function(message, type = "info") {
  class_name <- paste0("alert alert-", type)

  icon_name <- switch(type,
    "info" = "info-circle",
    "warning" = "exclamation-triangle",
    "danger" = "exclamation-circle",
    "success" = "check-circle",
    "info-circle"
  )

  div(
    class = class_name,
    role = "alert",
    icon(icon_name), " ", message
  )
}

#' Create collapsible help section
#' @param title Help section title
#' @param content Help content (HTML)
#' @param collapsed Whether to start collapsed
#' @return Shiny box with collapsible help
help_section <- function(title, content, collapsed = TRUE) {
  box(
    title = paste("Help:", title),
    status = "info",
    solidHeader = FALSE,
    collapsible = TRUE,
    collapsed = collapsed,
    width = 12,
    div(
      style = "padding: 10px;",
      HTML(content)
    )
  )
}

#' Create export buttons
#' @param prefix Prefix for button IDs
#' @return Shiny div with export buttons
export_buttons <- function(prefix = "export") {
  div(
    style = "margin: 10px 0;",
    downloadButton(
      paste0(prefix, "_csv"),
      "Export CSV",
      class = "btn-outline-secondary btn-sm",
      style = "margin-right: 5px;"
    ),
    downloadButton(
      paste0(prefix, "_png"),
      "Export PNG",
      class = "btn-outline-secondary btn-sm"
    )
  )
}

#' Null coalescing operator
#' @param lhs Left-hand side value
#' @param rhs Right-hand side value (default)
#' @return lhs if not NULL, otherwise rhs
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

#' Create data summary table
#' @param data_summary List with data summary statistics
#' @return Shiny DT table
create_data_summary_table <- function(data_summary) {
  if (is.null(data_summary)) {
    return(div("No data summary available"))
  }

  summary_df <- data.frame(
    Statistic = c("Observations", "Mean", "Std Dev", "Minimum", "Maximum",
                  "Skewness", "Kurtosis", "Missing Values"),
    Value = c(
      data_summary$n_obs %||% "N/A",
      round(data_summary$mean %||% 0, 6),
      round(data_summary$std %||% 0, 6),
      round(data_summary$min %||% 0, 6),
      round(data_summary$max %||% 0, 6),
      round(data_summary$skewness %||% 0, 4),
      round(data_summary$kurtosis %||% 0, 4),
      data_summary$n_missing %||% 0
    ),
    stringsAsFactors = FALSE
  )

  DT::datatable(
    summary_df,
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(className = "dt-right", targets = 1)
      )
    ),
    rownames = FALSE,
    class = "table-condensed"
  )
}

#' Create model parameter validation
#' @param params List of parameters to validate
#' @param model_type Type of model
#' @return List with validation results
validate_model_parameters <- function(params, model_type) {
  errors <- c()
  warnings <- c()

  # Common validations
  if (model_type %in% c("GARCH", "EGARCH", "GJR-GARCH")) {
    if (!is.null(params$p) && (params$p < 1 || params$p > 5)) {
      errors <- c(errors, "GARCH p order must be between 1 and 5")
    }
    if (!is.null(params$q) && (params$q < 1 || params$q > 5)) {
      errors <- c(errors, "GARCH q order must be between 1 and 5")
    }
  }

  if (model_type == "EWMA") {
    if (!is.null(params$lambda) && (params$lambda <= 0 || params$lambda >= 1)) {
      errors <- c(errors, "EWMA lambda must be between 0 and 1")
    }
  }

  if (model_type == "Moving Average") {
    if (!is.null(params$window) && (params$window < 1 || params$window > 100)) {
      errors <- c(errors, "MA window must be between 1 and 100")
    }
  }

  if (model_type == "Neural Network") {
    if (!is.null(params$layers)) {
      for (i in seq_along(params$layers)) {
        if (params$layers[i] < 1 || params$layers[i] > 1000) {
          errors <- c(errors,
                      paste("Layer", i, "size must be between 1 and 1000"))
        }
      }
    }
    if (!is.null(params$learning_rate) &&
          (params$learning_rate <= 0 || params$learning_rate > 1)) {
      errors <- c(errors, "Learning rate must be between 0 and 1")
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}
