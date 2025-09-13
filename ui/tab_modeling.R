# ============================================================================
# VOLATILITY MODELING TAB  -  UI AND SERVER
# Dynamic model configuration and results analysis
# ============================================================================

#' Modeling tab UI
#' @param id Module ID
#' @return Shiny tabItem
modeling_tab_ui < -function(id) {
  ns < -NS(id)

  tabItem(
    tabName = "modeling",

    # Status check
    conditionalPanel(
      condition = paste0("!output['", ns("data_ready"), "']"),
      fluidRow(
        box(
          title = "Data Required",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text - align: center; padding: 40px;",
            icon("exclamation - triangle", "fa - 3x",
              style = "color: #f39c12;"
            ),
            br(), br(),
            h4("Processed Data Required", style = "color: #e67e22;"),
            p(
              paste(
                "Please load data and apply configuration in the",
                "Data Selection tab first."
              ),
              style = "color: #d68910;"
            )
          )
        )
      )
    ),

    # Main modeling interface
    conditionalPanel(
      condition = paste0("output['", ns("data_ready"), "']"),

      # Model Configuration Section
      fluidRow(
        box(
          title = "Model Configuration",
          status = "primary",
          solidHeader = TRUE,
          width = 8,

          # Active models display
          h4("Active Models"),
          div(
            id = ns("active_models_container"),
            style = paste(
              "min - height: 100px; border: 1px dashed #ddd;",
              "padding: 20px; border - radius: 5px;"
            ),
            div(
              id = ns("no_models_message"),
              style = "text - align: center; color: #999;",
              icon("cog", "fa - 2x"), br(), br(),
              "No models configured yet. Click 'Add Model' to get started."
            ),

            # Model cards display
            uiOutput(ns("model_cards"))
          ),
          br(),

          # Action buttons
          fluidRow(
            column(
              6,
              actionButton(
                ns("add_model"),
                "Add New Model",
                class = "btn - success",
                icon = icon("plus"),
                style = "width: 100%;"
              )
            ),
            column(
              6,
              actionButton(
                ns("run_models"),
                "Run All Models",
                class = "btn - primary btn - lg",
                icon = icon("play"),
                style = "width: 100%;"
              )
            )
          )
        ),

        # Quick Model Presets
        box(
          title = "Quick Presets",
          status = "info",
          solidHeader = TRUE,
          width = 4,
          p("Add common model combinations quickly:"),
          actionButton(
            ns("preset_basic"), "Basic Models",
            class = "btn - outline - info",
            style = "width: 100%; margin: 2px;"
          ),
          tags$small("Naive   +  MA   +  EWMA"),
          br(), br(),
          actionButton(
            ns("preset_garch"), "GARCH Suite",
            class = "btn - outline - info",
            style = "width: 100%; margin: 2px;"
          ),
          tags$small("GARCH(1,1) variants with different distributions"),
          br(), br(),
          actionButton(
            ns("preset_advanced"), "Advanced Models",
            class = "btn - outline - info",
            style = "width: 100%; margin: 2px;"
          ),
          tags$small("ML   +  HAR - RV   +  Extended GARCH"),
          br(), br(),
          actionButton(
            ns("preset_thesis"), "Thesis Replication",
            class = "btn - outline - warning",
            style = "width: 100%; margin: 2px;"
          ),
          tags$small("All models from the original thesis"),
          br(), br(),
          h5("Current Setup:"),
          verbatimTextOutput(
            ns("model_summary"),
            placeholder = TRUE
          )
        )
      ),

      # Results Section
      conditionalPanel(
        condition = paste0("output['", ns("results_available"), "']"),
        fluidRow(
          box(
            title = "Model Performance Comparison",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                8,
                h5("Performance Ranking")
              ),
              column(4,
                style = "text - align: right;",
                export_buttons("performance")
              )
            ),

            # Sortable results table
            DT::dataTableOutput(ns("performance_table"))
          )
        ),
        fluidRow(
          # Performance visualization
          column(
            6,
            box(
              title = "Performance Metrics Visualization",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              selectInput(
                ns("chart_metric"),
                "Select metric:",
                choices = c("RMSE", "MAE", "MAPE", "QLIKE", "LogLoss"),
                selected = "RMSE"
              ),
              plotlyOutput(ns("performance_chart"), height = "350px")
            )
          ),

          # Statistical significance
          column(
            6,
            box(
              title = "Statistical Significance Tests",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              p("Diebold - Mariano test results (p - values):"),
              DT::dataTableOutput(ns("dm_tests"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Forecast Visualization",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                4,
                selectInput(
                  ns("viz_model"),
                  "Select model to visualize:",
                  choices = NULL
                )
              ),
              column(
                4,
                checkboxInput(
                  ns("show_all_forecasts"),
                  "Show all model forecasts",
                  value = FALSE
                )
              ),
              column(
                4,
                checkboxInput(
                  ns("annualize_viz"),
                  "Annualize volatility",
                  value = TRUE
                )
              )
            ),
            plotlyOutput(ns("forecast_plot"), height = "450px")
          )
        )
      ),

      # No results message
      conditionalPanel(
        condition = paste0("!output['", ns("results_available"), "']"),
        fluidRow(
          box(
            title = "Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              style = "text - align: center; padding: 40px;",
              icon("chart - line", "fa - 3x", style = "color: #bdc3c7;"),
              br(), br(),
              h4("No Results Yet", style = "color: #7f8c8d;"),
              p(
                paste(
                  "Configure and run models to see performance comparison",
                  "and forecasts."
                ),
                style = "color: #95a5a6;"
              )
            )
          )
        )
      )
    ),

    # Help section
    help_section(
      "Volatility Modeling",
      "
      <h5>Model Families< / h5>
      <ul>
        <li><strong>Naive:< / strong> Simple historical averages
        and random walk< / li>
        <li><strong>Moving Average:< / strong> Rolling window averages
        with configurable periods< / li>
        <li><strong>EWMA:< / strong> Exponentially weighted moving averages
        (RiskMetrics methodology)< / li>
        <li><strong>GARCH:< / strong> Generalized autoregressive conditional
        heteroscedasticity models< / li>
        <li><strong>Neural Networks:< / strong> Multi - layer perceptrons
        and LSTM networks< / li>
        <li><strong>HAR - RV:< / strong> Heterogeneous autoregressive
        realized volatility models< / li>
      < / ul>

      <h5>Performance Metrics< / h5>
      <ul>
        <li><strong>RMSE:< / strong> Root mean squared error
        (primary ranking metric)< / li>
        <li><strong>MAE:< / strong> Mean absolute error< / li>
        <li><strong>MAPE:< / strong> Mean absolute percentage error< / li>
        <li><strong>QLIKE:< / strong> Quasi - maximum likelihood error< / li>
        <li><strong>LogLoss:< / strong> Logarithmic loss function< / li>
      < / ul>

      <h5>Model Configuration Tips< / h5>
      <ul>
        <li>Start with basic models to establish benchmarks< / li>
        <li>GARCH models require sufficient data
        (>500 observations recommended)< / li>
        <li>Neural networks may overfit with limited data< / li>
        <li>Compare statistical significance using
        Diebold - Mariano tests< / li>
      < / ul>
      "
    )
  )
}

#' Modeling tab server
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param values Reactive values
modeling_tab_server < -function(input, output, session, values) {
  # Reactive values for this module
  modeling_data < -reactiveValues(
    selected_models = list(),
    model_results = NULL,
    performance_metrics = NULL
  )

  # ====================================================================
  # DATA READINESS CHECK
  # ====================================================================

  output$data_ready < -reactive({
    !is.null(values$processed_data) &&
      !is.null(values$processed_data$target_volatility) &&
      !is.null(values$processed_data$returns)
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  output$results_available < -reactive({
    !is.null(modeling_data$model_results) &&
      length(modeling_data$model_results) > 0
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)

  # ====================================================================
  # MODEL CONFIGURATION  -  ADD MODEL MODAL
  # ====================================================================

  observeEvent(input$add_model, {
    showModal(
      modalDialog(
        title = "Add New Model",
        size = "l",
        fluidRow(
          column(
            6,
            selectInput(
              session$ns("modal_model_family"),
              "Model Family:",
              choices = list(
                "Naive Models" = "naive",
                "Moving Average" = "moving_average",
                "EWMA" = "ewma",
                "GARCH" = "garch",
                "Neural Network" = "neural_network",
                "HAR - RV" = "har"
              )
            )
          ),
          column(
            6,
            textInput(
              session$ns("modal_model_name"),
              "Model Name:",
              placeholder = "Auto - generated if empty"
            )
          )
        ),

        # Dynamic parameter inputs based on model family
        uiOutput(session$ns("modal_parameters")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("modal_add_model"),
            "Add Model",
            class = "btn - primary"
          )
        )
      )
    )
  })

  # Dynamic parameter UI based on selected model family
  output$modal_parameters < -renderUI({
    req(input$modal_model_family)

    family < -input$modal_model_family
    ns < -session$ns

    switch(family,
      "naive" = div(
        h5("Naive Model Configuration"),
        radioButtons(
          ns("naive_type"), "Type:",
          choices = list(
            "Historical Average" = "historical",
            "Random Walk" = "random_walk"
          ),
          selected = "historical"
        ),
        p(
          "No additional parameters required.",
          style = "color: #666; font - style: italic;"
        )
      ),
      "moving_average" = div(
        h5("Moving Average Configuration"),
        checkboxInput(
          ns("ma_adaptive"),
          "Adaptive MA (optimize window)",
          FALSE
        ),
        conditionalPanel(
          condition = paste0("!input['", ns("ma_adaptive"), "']"),
          numericInput(
            ns("ma_window"), "Window Size:",
            value = 20, min = 2, max = 100, step = 1
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("ma_adaptive"), "']"),
          numericInput(
            ns("ma_max_window"), "Maximum Window:",
            value = 50, min = 5, max = 100, step = 1
          )
        )
      ),
      "ewma" = div(
        h5("EWMA Configuration"),
        checkboxInput(ns("ewma_optimize"), "Optimize Lambda", FALSE),
        conditionalPanel(
          condition = paste0("!input['", ns("ewma_optimize"), "']"),
          numericInput(
            ns("ewma_lambda"), "Lambda:",
            value = 0.94, min = 0.01, max = 0.999, step = 0.001
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("ewma_optimize"), "']"),
          p(
            "Lambda will be optimized in range [0.90, 0.999]",
            style = "color: #666; font - style: italic;"
          )
        )
      ),
      "garch" = div(
        h5("GARCH Configuration"),
        fluidRow(
          column(
            6,
            selectInput(
              ns("garch_type"), "GARCH Type:",
              choices = list(
                "Standard GARCH" = "sGARCH",
                "EGARCH" = "eGARCH",
                "GJR - GARCH" = "gjrGARCH",
                "FIGARCH" = "fiGARCH"
              ),
              selected = "sGARCH"
            )
          ),
          column(
            6,
            selectInput(
              ns("garch_distribution"), "Distribution:",
              choices = list(
                "Normal" = "norm",
                "Student - t" = "std",
                "Skewed Normal" = "snorm",
                "Skewed t" = "sstd",
                "GED" = "ged"
              ),
              selected = "norm"
            )
          )
        ),
        fluidRow(
          column(
            6,
            numericInput(
              ns("garch_p"), "GARCH Order (p):",
              value = 1, min = 1, max = 5, step = 1
            )
          ),
          column(
            6,
            numericInput(
              ns("garch_q"), "ARCH Order (q):",
              value = 1, min = 1, max = 5, step = 1
            )
          )
        ),
        checkboxInput(ns("garch_mean"), "Include Mean", TRUE)
      ),
      "neural_network" = div(
        h5("Neural Network Configuration"),
        selectInput(
          ns("nn_type"), "Network Type:",
          choices = list(
            "Multi - Layer Perceptron" = "mlp",
            "LSTM" = "lstm"
          ),
          selected = "mlp"
        ),
        fluidRow(
          column(
            6,
            textInput(
              ns("nn_architecture"), "Architecture (comma - separated):",
              value = "50,25", placeholder = "e.g., 64,32,16"
            )
          ),
          column(
            6,
            numericInput(
              ns("nn_lags"), "Input Lags:",
              value = 20, min = 5, max = 100, step = 1
            )
          )
        ),
        fluidRow(
          column(
            6,
            numericInput(
              ns("nn_dropout"), "Dropout Rate:",
              value = 0.2, min = 0, max = 0.8, step = 0.1
            )
          ),
          column(
            6,
            numericInput(
              ns("nn_learning_rate"), "Learning Rate:",
              value = 0.001, min = 0.0001, max = 0.1, step = 0.0001
            )
          )
        ),
        div(
          style = paste(
            "background: #fff3cd; padding: 10px;",
            "border - radius: 5px; margin - top: 10px;"
          ),
          p(
            strong("Note:"),
            "Neural networks require substantial computational time and data.",
            style = "margin: 0; font - size: 12px;"
          )
        )
      ),
      "har" = div(
        h5("HAR - RV Configuration"),
        checkboxInput(
          ns("har_extended"),
          "Extended HAR (include asymmetric terms)",
          FALSE
        ),
        fluidRow(
          column(
            4,
            numericInput(
              ns("har_daily_lag"), "Daily Lag:",
              value = 1, min = 1, max = 5, step = 1
            )
          ),
          column(
            4,
            numericInput(
              ns("har_weekly_window"), "Weekly Window:",
              value = 5, min = 3, max = 10, step = 1
            )
          ),
          column(
            4,
            numericInput(
              ns("har_monthly_window"), "Monthly Window:",
              value = 22, min = 15, max = 30, step = 1
            )
          )
        )
      ),
      div("Select a model family to configure parameters.")
    )
  })

  # Add model to configuration
  observeEvent(input$modal_add_model, {
    req(input$modal_model_family)

    family < -input$modal_model_family

    # Generate model configuration based on family
    model_config < -switch(family,
      "naive" = list(
        family = "naive",
        type = input$naive_type %||% "historical",
        name = input$modal_model_name %||% paste(
          "Naive",
          stringr::str_to_title(input$naive_type %||% "Historical")
        )
      ),
      "moving_average" = list(
        family = "moving_average",
        adaptive = input$ma_adaptive %||% FALSE,
        window = if (input$ma_adaptive) {
          NULL
        } else {
          (input$ma_window %||% 20)
        },
        max_window = if (input$ma_adaptive) {
          (input$ma_max_window %||% 50)
        } else {
          NULL
        },
        name = input$modal_model_name %||% {
          if (input$ma_adaptive) {
            "Adaptive MA"
          } else {
            paste0("MA(", input$ma_window %||% 20, ")")
          }
        }
      ),
      "ewma" = list(
        family = "ewma",
        optimize = input$ewma_optimize %||% FALSE,
        lambda = if (input$ewma_optimize) {
          NULL
        } else {
          (input$ewma_lambda %||% 0.94)
        },
        name = input$modal_model_name %||% {
          if (input$ewma_optimize) {
            "EWMA Optimized"
          } else {
            paste0("EWMA(λ = ", input$ewma_lambda %||% 0.94, ")")
          }
        }
      ),
      "garch" = list(
        family = "garch",
        type = input$garch_type %||% "sGARCH",
        distribution = input$garch_distribution %||% "norm",
        p = input$garch_p %||% 1,
        q = input$garch_q %||% 1,
        include_mean = input$garch_mean %||% TRUE,
        name = input$modal_model_name %||% paste0(
          input$garch_type %||% "sGARCH", "(",
          input$garch_p %||% 1, ",", input$garch_q %||% 1, ") - ",
          input$garch_distribution %||% "norm"
        )
      ),
      "neural_network" = list(
        family = "neural_network",
        type = input$nn_type %||% "mlp",
        architecture = as.numeric(
          strsplit(input$nn_architecture %||% "50,25", ",")[[1]]
        ),
        lags = input$nn_lags %||% 20,
        dropout = input$nn_dropout %||% 0.2,
        learning_rate = input$nn_learning_rate %||% 0.001,
        name = input$modal_model_name %||% paste0(
          toupper(input$nn_type %||% "mlp"), "(",
          paste(
            c(
              as.numeric(
                strsplit(input$nn_architecture %||% "50,25", ",")[[1]]
              ),
              1
            ),
            collapse = " - "
          ),
          ")"
        )
      ),
      "har" = list(
        family = "har",
        extended = input$har_extended %||% FALSE,
        daily_lag = input$har_daily_lag %||% 1,
        weekly_window = input$har_weekly_window %||% 5,
        monthly_window = input$har_monthly_window %||% 22,
        name = input$modal_model_name %||% ifelse(
          input$har_extended,
          "HAR - RV Extended",
          "HAR - RV"
        )
      )
    )

    # Validate model configuration
    validation < -validate_model_parameters(model_config, family)

    if (!validation$valid) {
      safe_notification(
        paste(
          "Model configuration error:",
          paste(validation$errors, collapse = ", ")
        ),
        type = "error"
      )
      return()
    }

    # Add unique ID
    model_config$id < -paste0(
      "model_", length(modeling_data$selected_models) + 1, "_",
      gsub("[ ^ A - Za - z0 - 9]", "_", Sys.time())
    )

    # Add to selected models
    modeling_data$selected_models[[model_config$id]] < -model_config

    # Update UI
    update_models_ui()

    removeModal()
    safe_notification(
      paste("Added model:", model_config$name),
      type = "message"
    )
  })

  # ====================================================================
  # PRESET CONFIGURATIONS
  # ====================================================================

  observeEvent(input$preset_basic, {
    add_preset_models("basic")
  })

  observeEvent(input$preset_garch, {
    add_preset_models("garch")
  })

  observeEvent(input$preset_advanced, {
    add_preset_models("advanced")
  })

  observeEvent(input$preset_thesis, {
    add_preset_models("thesis")
  })

  add_preset_models < -function(preset_type) {
    new_models < -list()

    if (preset_type == "basic") {
      new_models < -list(
        list(
          family = "naive", type = "historical",
          name = "Historical Average"
        ),
        list(
          family = "naive", type = "random_walk",
          name = "Random Walk"
        ),
        list(family = "moving_average", window = 10, name = "MA(10)"),
        list(family = "moving_average", window = 20, name = "MA(20)"),
        list(family = "ewma", lambda = 0.94, name = "EWMA(λ = 0.94)"),
        list(family = "ewma", lambda = 0.97, name = "EWMA(λ = 0.97)")
      )
    } else if (preset_type == "garch") {
      distributions < -c("norm", "std", "snorm")
      new_models < -lapply(distributions, function(dist) {
        list(
          family = "garch", type = "sGARCH", distribution = dist,
          p = 1, q = 1, name = paste0("GARCH(1,1) - ", dist)
        )
      })
    } else if (preset_type == "advanced") {
      new_models < -list(
        list(
          family = "neural_network", type = "mlp",
          architecture = c(50, 25), name = "MLP(50 - 25 - 1)"
        ),
        list(family = "har", extended = FALSE, name = "HAR - RV"),
        list(
          family = "garch", type = "eGARCH", distribution = "std",
          p = 1, q = 1, name = "EGARCH(1,1) - std"
        )
      )
    } else if (preset_type == "thesis") {
      # Comprehensive model set from thesis
      new_models < -list(
        # Naive
        list(
          family = "naive", type = "historical",
          name = "Historical Average"
        ),
        # EWMA
        list(family = "ewma", optimize = TRUE, name = "EWMA Optimized"),
        # MA Adaptive
        list(
          family = "moving_average", adaptive = TRUE,
          name = "MA Adaptive"
        ),
        # GARCH suite
        list(
          family = "garch", type = "sGARCH", distribution = "norm",
          p = 1, q = 1, name = "GARCH(1,1) - norm"
        ),
        list(
          family = "garch", type = "sGARCH", distribution = "std",
          p = 1, q = 1, name = "GARCH(1,1) - std"
        ),
        list(
          family = "garch", type = "eGARCH", distribution = "std",
          p = 1, q = 1, name = "EGARCH(1,1) - std"
        ),
        # HAR
        list(family = "har", extended = FALSE, name = "HAR - RV"),
        # Neural Network
        list(
          family = "neural_network", type = "mlp",
          architecture = c(50, 25), name = "MLP(50 - 25 - 1)"
        )
      )
    }

    # Add unique IDs and add to selected models
    for (i in seq_along(new_models)) {
      model_config < -new_models[[i]]
      model_config$id < -paste0(
        "preset_", preset_type, "_", i, "_",
        gsub("[ ^ A - Za - z0 - 9]", "_", Sys.time())
      )
      modeling_data$selected_models[[model_config$id]] < -model_config
    }

    update_models_ui()

    safe_notification(
      paste(
        "Added", length(new_models), "models from", preset_type, "preset"
      ),
      type = "message"
    )
  }

  # ====================================================================
  # UI UPDATES
  # ====================================================================

  # Update models UI display
  update_models_ui < -function() {
    models < -modeling_data$selected_models

    if (length(models) == 0) {
      # Show no models message
      shinyjs::show("no_models_message")
      output$model_cards < -renderUI(NULL)
    } else {
      # Hide no models message
      shinyjs::hide("no_models_message")

      # Create model cards
      cards < -lapply(names(models), function(model_id) {
        model < -models[[model_id]]

        # Create parameter summary
        params < -model[!names(model) %in% c("id", "name", "family")]

        model_card(
          model$name,
          stringr::str_to_title(gsub("_", " ", model$family)),
          params,
          get_model_description(model),
          actions = TRUE
        )
      })

      # Clear existing cards and render new ones
      output$model_cards < -renderUI({
        div(cards)
      })
    }
  }

  # Get model description
  get_model_description < -function(model) {
    switch(model$family,
      "naive" = "Simple benchmark model",
      "moving_average" = "Rolling window average",
      "ewma" = "Exponentially weighted moving average",
      "garch" = "Conditional heteroscedasticity model",
      "neural_network" = "Machine learning approach",
      "har" = "Heterogeneous autoregressive model",
      "Volatility model"
    )
  }

  # Model summary text
  output$model_summary < -renderText({
    models < -modeling_data$selected_models

    if (length(models) == 0) {
      return("No models configured")
    }

    families < -table(sapply(models, function(m) m$family))
    family_text < -paste(
      names(families), families,
      sep = ":", collapse = ", "
    )

    paste0("Total: ", length(models), " models\n", family_text)
  })

  # ====================================================================
  # MODEL EXECUTION
  # ====================================================================

  observeEvent(input$run_models, {
    debug_log("=== MODEL EXECUTION STARTED ===")
    debug_log("Checking requirements...")
    req(values$processed_data, modeling_data$selected_models)

    debug_log(paste(
      "Number of selected models:",
      length(modeling_data$selected_models)
    ))
    if (length(modeling_data$selected_models) == 0) {
      debug_log("No models configured to run", "WARNING")
      safe_notification("No models configured to run", type = "warning")
      return()
    }

    debug_log("Showing progress modal...")
    # Show progress modal
    showModal(modalDialog(
      title = "Running Models",
      progress_indicator("Fitting volatility models..."),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        debug_log("Starting model execution tryCatch block...")
        # Prepare data
        debug_log("Accessing processed_data from values...")
        processed_data < -values$processed_data
        debug_log(paste(
          "processed_data structure:",
          paste(names(processed_data), collapse = ", ")
        ))

        # Prepare train and full data lists for unified interface
        train_data < -list(
          target = processed_data$target_volatility$values[
            processed_data$train_indices
          ],
          returns = processed_data$returns[processed_data$train_indices]
        )

        full_data < -list(
          target = processed_data$target_volatility$values,
          returns = processed_data$returns
        )

        debug_log(paste("train_data target length:", length(train_data$target)))
        debug_log(paste("full_data target length:", length(full_data$target)))

        # Convert model configurations to modeling engine format
        debug_log("Preparing model specifications...")
        model_specs < -prepare_model_specifications(
          modeling_data$selected_models
        )
        debug_log(paste(
          "Number of model specs prepared:",
          length(model_specs)
        ))

        # Run models with unified interface
        debug_log("=== CALLING run_volatility_models ===")
        model_results < -run_volatility_models(
          train_data, full_data, model_specs
        )
        debug_log(paste(
          "Models executed, results count:",
          length(model_results)
        ))

        # Evaluate performance
        debug_log("Evaluating model performance...")
        test_target < -full_data$target[
          (length(train_data$target) + 1):length(full_data$target)
        ]
        performance_metrics < -evaluate_all_models(test_target, model_results)
        debug_log("Performance evaluation completed")

        # Store results
        debug_log("Storing results in modeling_data...")
        modeling_data$model_results < -model_results
        modeling_data$performance_metrics < -performance_metrics

        # Update visualization choices
        debug_log("Updating visualization choices...")
        model_choices < -setNames(
          names(model_results),
          sapply(model_results, function(m) m$description %||% "Unknown")
        )
        updateSelectInput(
          session, "viz_model",
          choices = model_choices, selected = names(model_results)[1]
        )

        debug_log("Removing modal and showing success notification...")
        removeModal()
        safe_notification(
          paste("Successfully fit", length(model_results), "models"),
          type = "message"
        )

        debug_log("=== MODEL EXECUTION COMPLETED SUCCESSFULLY ===")
      },
      error = function(e) {
        debug_log(paste(
          "=== ERROR in model execution:", e$message, "==="
        ), "ERROR")
        debug_log(paste("Error class:", class(e)), "ERROR")
        debug_log(paste(
          "Full error:", capture.output(print(e))
        ), "ERROR")
        removeModal()
        safe_notification(
          paste("Error running models:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Convert model configurations to modeling engine format
  prepare_model_specifications < -function(selected_models) {
    families < -unique(sapply(selected_models, function(m) m$family))

    # Initialize model specs
    model_specs < -list(families = families)

    # Configure each family
    if ("moving_average" %in% families) {
      ma_models < -selected_models[
        sapply(selected_models, function(m) m$family == "moving_average")
      ]
      model_specs$ma_windows < -sapply(
        ma_models, function(m) m$window %||% 20
      )
      model_specs$adaptive_ma < -any(
        sapply(ma_models, function(m) m$adaptive %||% FALSE)
      )
    }

    if ("ewma" %in% families) {
      ewma_models < -selected_models[
        sapply(selected_models, function(m) m$family == "ewma")
      ]
      model_specs$ewma_lambdas < -sapply(
        ewma_models, function(m) m$lambda %||% 0.94
      )
      model_specs$ewma_optimize < -any(
        sapply(ewma_models, function(m) m$optimize %||% FALSE)
      )
    }

    if ("garch" %in% families) {
      garch_models < -selected_models[
        sapply(selected_models, function(m) m$family == "garch")
      ]
      model_specs$garch_specs < -list()

      for (i in seq_along(garch_models)) {
        model < -garch_models[[i]]
        spec_name < -model$name

        model_specs$garch_specs[[spec_name]] < -create_garch_spec(
          model_type = model$type %||% "sGARCH",
          garch_order = c(model$p %||% 1, model$q %||% 1),
          distribution = model$distribution %||% "norm",
          include_mean = model$include_mean %||% TRUE
        )
      }
    }

    if ("neural_network" %in% families) {
      nn_models < -selected_models[
        sapply(selected_models, function(m) m$family == "neural_network")
      ]
      if (length(nn_models) > 0) {
        model < -nn_models[[1]] # Use first NN model for now
        model_specs$nn_architecture < -c(model$architecture, 1)
        model_specs$nn_lags < -model$lags %||% 20
      }
    }

    if ("har" %in% families) {
      har_models < -selected_models[
        sapply(selected_models, function(m) m$family == "har")
      ]
      model_specs$har_extended < -any(
        sapply(har_models, function(m) m$extended %||% FALSE)
      )
    }

    model_specs
  }

  # ====================================================================
  # RESULTS DISPLAY
  # ====================================================================

  # Performance table with sorting
  output$performance_table < -DT::renderDataTable({
    req(modeling_data$performance_metrics)

    metrics_df < -modeling_data$performance_metrics

    DT::datatable(
      metrics_df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(2, "asc")), # Sort by RMSE ascending
        columnDefs = list(
          list(className = "dt - right", targets = 2:7),
          list(targets = 0, visible = TRUE, searchable = TRUE),
          list(targets = 1, visible = TRUE, searchable = TRUE)
        )
      ),
      rownames = FALSE,
      class = "table - striped table - hover"
    ) %>%
      DT::formatRound(
        columns = c("RMSE", "MAE", "QLIKE", "LogLoss"),
        digits = 6
      ) %>%
      DT::formatString(
        columns = "MSE", suffix = "", prefix = ""
      ) %>%
      DT::formatPercentage(columns = "MAPE", digits = 2) %>%
      DT::formatStyle(
        columns = "RMSE",
        backgroundColor = DT::styleInterval(
          cuts = quantile(
            metrics_df$RMSE,
            probs = c(0.33, 0.67), na.rm = TRUE
          ),
          values = c("#d4edda", "#fff3cd", "#f8d7da")
        )
      )
  })

  # Performance chart
  output$performance_chart < -renderPlotly({
    req(modeling_data$performance_metrics, input$chart_metric)

    metrics_df < -modeling_data$performance_metrics
    selected_metric < -input$chart_metric

    # Prepare data for plotting
    plot_data < -metrics_df[order(metrics_df[[selected_metric]]), ]

    p < -plot_ly(
      plot_data,
      x = ~ reorder(Model, get(selected_metric)),
      y = ~ get(selected_metric),
      type = "bar",
      marker = list(
        color = viridis::viridis(nrow(plot_data)),
        line = list(color = "white", width = 1)
      ),
      hovertemplate = paste0(
        "<b>%{x}< / b><br>",
        selected_metric, ": %{y:.6f}<br>",
        "<extra>< / extra>"
      )
    ) %>%
      layout(
        title = paste(selected_metric, "by Model"),
        xaxis = list(title = "Model", tickangle = 45),
        yaxis = list(title = selected_metric),
        margin = list(b = 100)
      )

    p
  })

  # Diebold - Mariano tests
  output$dm_tests < -DT::renderDataTable({
    req(modeling_data$model_results, values$processed_data)

    tryCatch(
      {
        test_target < -values$processed_data$target_volatility$values[
          values$processed_data$test_indices
        ]
        dm_results < -pairwise_dm_tests(
          test_target,
          modeling_data$model_results
        )

        if (!is.null(dm_results)) {
          DT::datatable(
            dm_results,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              columnDefs = list(
                list(className = "dt - center", targets = "_all")
              )
            )
          ) %>%
            DT::formatRound(
              columns = seq_len(ncol(dm_results)),
              digits = 4
            ) %>%
            DT::formatStyle(
              columns = seq_len(ncol(dm_results)),
              backgroundColor = DT::styleInterval(
                cuts = c(0.01, 0.05, 0.1),
                values = c("#d4edda", "#fff3cd", "#ffeaa7", "#ffffff")
              )
            )
        } else {
          DT::datatable(data.frame(
            Message = "Insufficient models for pairwise comparison"
          ))
        }
      },
      error = function(e) {
        DT::datatable(data.frame(
          Error = paste("Could not compute DM tests:", e$message)
        ))
      }
    )
  })

  # Forecast visualization
  output$forecast_plot < -renderPlotly({
    req(modeling_data$model_results, values$processed_data)

    # Show full dataset with train / test split line
    all_dates < -values$processed_data$dates
    all_target < -values$processed_data$target_volatility$values
    train_indices < -values$processed_data$train_indices
    test_indices < -values$processed_data$test_indices

    # Split line date (end of training period)
    split_date < -all_dates[max(train_indices)]

    # Start with full actual values
    plot_data < -data.frame(
      Date = all_dates,
      Actual = all_target
    )

    debug_log(paste(" * * *  ACTUAL VOLATILITY DATA DEBUG  * * * "))
    debug_log(paste(
      "all_target range:", min(all_target, na.rm = TRUE),
      "to", max(all_target, na.rm = TRUE)
    ))
    debug_log(paste("Annualize checkbox:", input$annualize_viz))

    # Apply annualization if requested
    if (input$annualize_viz) {
      plot_data$Actual < -annualize_volatility(plot_data$Actual, "daily")
      debug_log(paste(
        "After annualization:", min(plot_data$Actual, na.rm = TRUE),
        "to", max(plot_data$Actual, na.rm = TRUE)
      ))
    }

    p < -plot_ly() %>%
      add_trace(
        data = plot_data,
        x = ~Date, y = ~Actual,
        type = "scatter", mode = "lines",
        name = "Actual",
        line = list(color = "#2c3e50", width = 2)
      )

    # Add train / test split line
    p < -p %>% add_vline(
      x = as.numeric(as.POSIXct(split_date)),
      line = list(color = "red", width = 2, dash = "dash"),
      annotation = list(
        text = "Train / Test Split",
        textangle = -90,
        showarrow = FALSE,
        xanchor = "right"
      )
    )

    # Add model forecasts (only on test period)
    test_dates < -values$processed_data$dates[test_indices]

    if (input$show_all_forecasts) {
      # Show all models
      colors < -viridis::viridis(length(modeling_data$model_results))

      for (i in seq_along(modeling_data$model_results)) {
        model_name < -names(modeling_data$model_results)[i]
        predictions < -modeling_data$model_results[[model_name]]$predictions

        debug_log(paste(" * * *  FORECAST VISUALIZATION DEBUG  * * * "))
        debug_log(paste("Model:", model_name))
        debug_log(paste(
          "Original predictions range:",
          min(predictions, na.rm = TRUE),
          "to", max(predictions, na.rm = TRUE)
        ))
        debug_log(paste("Annualize checkbox:", input$annualize_viz))

        if (input$annualize_viz) {
          predictions < -annualize_volatility(predictions, "daily")
          debug_log(paste(
            "After annualization:",
            min(predictions, na.rm = TRUE),
            "to", max(predictions, na.rm = TRUE)
          ))
        }

        p < -p %>% add_trace(
          x = test_dates, y = predictions,
          type = "scatter", mode = "lines",
          name = model_name,
          line = list(color = colors[i], width = 1, dash = "dot")
        )
      }
    } else if (!is.null(input$viz_model) && input$viz_model != "") {
      # Show selected model only
      selected_predictions < -modeling_data$model_results[[
        input$viz_model
      ]]$predictions

      debug_log(paste(" * * *  SINGLE FORECAST VISUALIZATION DEBUG  * * * "))
      debug_log(paste("Selected Model:", input$viz_model))
      debug_log(paste(
        "Original predictions range:",
        min(selected_predictions, na.rm = TRUE),
        "to", max(selected_predictions, na.rm = TRUE)
      ))
      debug_log(paste("Annualize checkbox:", input$annualize_viz))

      if (input$annualize_viz) {
        selected_predictions < -annualize_volatility(
          selected_predictions, "daily"
        )
        debug_log(paste(
          "After annualization:",
          min(selected_predictions, na.rm = TRUE),
          "to", max(selected_predictions, na.rm = TRUE)
        ))
      }

      p < -p %>% add_trace(
        x = test_dates, y = selected_predictions,
        type = "scatter", mode = "lines",
        name = paste("Forecast:", input$viz_model),
        line = list(color = "#e74c3c", width = 2)
      )
    }

    # Layout
    p < -p %>% layout(
      title = "Volatility Forecast vs Actual",
      xaxis = list(title = "Date"),
      yaxis = list(
        title = if (input$annualize_viz) {
          "Annualized Volatility"
        } else {
          "Volatility"
        }
      ),
      hovermode = "x unified",
      legend = list(x = 0.02, y = 0.98)
    )

    p
  })

  # ====================================================================
  # DYNAMIC REMOVE BUTTON HANDLERS
  # ====================================================================

  # Handle all remove button clicks dynamically
  observeEvent(input,
    {
      # Get all input names that start with "remove_"
      remove_inputs < -names(input)[startsWith(names(input), "remove_")]

      for (remove_id in remove_inputs) {
        # Check if this button was clicked
        if (!is.null(input[[remove_id]]) && input[[remove_id]] > 0) {
          # Extract model name from button ID
          model_name < -gsub(" ^ remove_", "", remove_id)
          model_name < -gsub("_", " ", model_name) # Convert underscores back

          debug_log(paste("Remove button clicked for model:", model_name))

          # Find and remove the model
          current_models < -modeling_data$selected_models
          model_to_remove < -NULL

          # Find the model by name
          for (model_id in names(current_models)) {
            if (gsub(
              "[ ^ A - Za - z0 - 9]", "_", current_models[[model_id]]$name
            ) == gsub("[ ^ A - Za - z0 - 9]", "_", model_name)) {
              model_to_remove < -model_id
              break
            }
          }

          if (!is.null(model_to_remove)) {
            debug_log(paste("Removing model with ID:", model_to_remove))

            # Remove the model from the list
            modeling_data$selected_models[[model_to_remove]] < -NULL

            # Update the UI
            update_models_ui()

            # Show success notification
            safe_notification(
              paste(
                "Removed model:",
                current_models[[model_to_remove]]$name %||% model_name
              ),
              type = "message"
            )

            debug_log(paste(
              "Model removed successfully. Remaining models:",
              length(modeling_data$selected_models)
            ))
          } else {
            debug_log(paste(
              "Could not find model to remove:", model_name
            ), "WARNING")
          }
        }
      }
    },
    ignoreInit = TRUE
  )
}
