# ============================================================================
# DATA TAB  -  UI AND SERVER
# Data selection, upload, and preprocessing interface
# ============================================================================

#' Data tab UI
#' @param id Module ID
#' @return Shiny tabItem
data_tab_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "data",

    # Header section
    fluidRow(
      box(
        title = "Data Selection & Upload",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          # Popular assets section
          column(
            4,
            h4("Popular Assets"),
            p("Quick access to commonly analyzed financial instruments:"),

            # Asset buttons
            div(
              style = "margin: 10px 0;",
              actionButton(ns("load_btc"), "Bitcoin (BTC-USD)",
                class = "btn-outline-primary",
                style = "margin: 2px; width: 100%;"
              ),
              actionButton(ns("load_sp500"), "S&P 500 (^GSPC)",
                class = "btn-outline-primary",
                style = "margin: 2px; width: 100%;"
              ),
              actionButton(ns("load_gold"), "Gold ETF (GLD)",
                class = "btn-outline-primary",
                style = "margin: 2px; width: 100%;"
              ),
              actionButton(ns("load_vix"), "VIX (^VIX)",
                class = "btn-outline-primary",
                style = "margin: 2px; width: 100%;"
              )
            ),
            br(),
            p("Or use the custom symbol search →",
              style = "color: #666; font-size: 12px;"
            )
          ),

          # Custom symbol section
          column(
            4,
            h4("Custom Symbol"),
            p("Enter any Yahoo Finance symbol:"),
            textInput(
              ns("custom_symbol"),
              "Symbol:",
              placeholder = "e.g., AAPL, EURUSD=X, CL=F"
            ),
            dateRangeInput(
              ns("date_range"),
              "Date Range:",
              start = Sys.Date() - 365 * 3, # 3 years ago
              end = Sys.Date() - 1,
              max = Sys.Date() - 1
            ),
            actionButton(
              ns("load_custom"),
              "Load Data",
              class = "btn-success",
              style = "width: 100%;"
            )
          ),

          # CSV upload section
          column(
            4,
            h4("CSV Upload"),
            p("Upload your own data file:"),
            fileInput(
              ns("csv_file"),
              "Choose CSV File:",
              accept = c(".csv", ".txt")
            ),
            fluidRow(
              column(
                6,
                selectInput(
                  ns("date_column"),
                  "Date Column:",
                  choices = NULL
                )
              ),
              column(
                6,
                selectInput(
                  ns("price_column"),
                  "Price Column:",
                  choices = NULL
                )
              )
            ),
            checkboxInput(ns("csv_header"), "Header row", TRUE),
            actionButton(
              ns("process_csv"),
              "Process CSV",
              class = "btn-info",
              style = "width: 100%;"
            )
          )
        )
      )
    ),

    # Data configuration section
    conditionalPanel(
      condition = paste0("output['", ns("data_loaded"), "']"),
      fluidRow(
        # Target volatility selection
        column(
          6,
          box(
            title = "Target Volatility Configuration",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            radioButtons(
              ns("target_measure"),
              "Select target volatility measure:",
              choices = list(
                "Absolute Returns \\(|r_t|\\)" = "absolute",
                "Squared Returns \\(r_t^2\\)" = "squared",
                "Rolling Standard Deviation \\(\\sigma_t^2\\)" = "rolling_std"
              ),
              selected = "squared"
            ),
            conditionalPanel(
              condition = paste0(
                "input['", ns("target_measure"),
                "'] == 'rolling_std'"
              ),
              numericInput(
                ns("rolling_window"),
                "Rolling window size:",
                value = 20,
                min = 2,
                max = 100,
                step = 1
              )
            )
          )
        ),

        # Data splitting
        column(
          6,
          box(
            title = "Data Splitting",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            sliderInput(
              ns("train_ratio"),
              "Training data percentage:",
              min = 50,
              max = 90,
              value = 80,
              step = 5,
              post = "%"
            ),
            uiOutput(ns("split_info"))
          )
        )
      )
    ),

    # Data preview section
    conditionalPanel(
      condition = paste0("output['", ns("data_loaded"), "']"),
      fluidRow(
        box(
          title = "Data Preview & Statistics",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          tabsetPanel(
            tabPanel(
              "Price Chart",
              div(style = "margin-top: 15px;",
                plotlyOutput(ns("price_plot"), height = "400px")
              )
            ),
            tabPanel(
              "Returns Chart",
              div(style = "margin-top: 15px;",
                plotlyOutput(ns("returns_plot"), height = "400px")
              )
            ),
            tabPanel(
              "Data Summary",
              div(style = "margin-top: 15px;",
                fluidRow(
                column(
                  6,
                  h5("Price Data Summary"),
                  DT::dataTableOutput(ns("price_summary"))
                ),
                column(
                  6,
                  h5("Returns Summary"),
                  DT::dataTableOutput(ns("returns_summary"))
                )
              )
            )
            ),
            tabPanel(
              "Raw Data",
              div(style = "margin-top: 15px;",
                DT::dataTableOutput(ns("raw_data_table"))
              )
            )
          )
        )
      )
    ),

    # Status section
    conditionalPanel(
      condition = paste0("!output['", ns("data_loaded"), "']"),
      fluidRow(
        box(
          title = "Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text-align: center; padding: 40px;",
            icon("database", "fa-3x", style = "color: #bdc3c7;"),
            br(), br(),
            h4("No Data Loaded", style = "color: #7f8c8d;"),
            p(
              paste0(
                "Select a popular asset, enter a custom symbol, ",
                "or upload a CSV file to get started."
              ),
              style = "color: #95a5a6;"
            )
          )
        )
      )
    ),

    # Help section
    help_section(
      "Data Selection",
      "
      <h5>Getting Started</h5>
      <ul>
        <li><strong>Popular Assets:</strong> Click any button to quickly load common financial instruments</li>
        <li><strong>Custom Symbol:</strong> Enter any Yahoo Finance symbol (stocks, indices, currencies, commodities)</li>
        <li><strong>CSV Upload:</strong> Upload your own data with Date and Price columns</li>
      </ul>

      <h5>Target Volatility Measures</h5>
      <ul>
        <li><strong>Absolute Returns:</strong> \\(|r_t|\\) - Simple absolute value of returns (basic volatility proxy)</li>
        <li><strong>Squared Returns:</strong> \\(r_t^2\\) - Classical realized variance proxy (most common)</li>
        <li><strong>Rolling RMS:</strong> \\(\\sqrt{\\frac{1}{n} \\sum_{i=1}^n r_i^2}\\) - Root Mean Square of returns over rolling window (alternative volatility estimator)</li>
        <li><strong>Rolling Standard Deviation:</strong> \\(\\sqrt{\\frac{1}{n-1} \\sum_{i=1}^n (r_i - \\bar{r})^2}\\) - Sample standard deviation (traditional volatility measure)</li>
      </ul>

      <h5>Key Differences</h5>
      <ul>
        <li><strong>RMS vs Standard Deviation:</strong> RMS assumes zero mean (\\(\\bar{r} = 0\\)), while Standard Deviation subtracts the sample mean. For financial returns (often close to zero mean), RMS and Std Dev are very similar.</li>
        <li><strong>Squared vs Absolute:</strong> Squared returns penalize large deviations more heavily, while absolute returns treat all deviations equally.</li>
        <li><strong>Rolling vs Fixed:</strong> Rolling measures use a moving window, providing time-varying volatility estimates.</li>
      </ul>

      <h5>Data Requirements</h5>
      <ul>
        <li>Minimum 100 observations recommended</li>
        <li>At least 50 observations for test set</li>
        <li>Missing values are automatically handled</li>
      </ul>
      "
    )
  )
}

#' Data tab server
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param values Reactive values for data sharing
data_tab_server <- function(input, output, session, values) {
  # Reactive values for this module
  local_data <- reactiveValues(
    raw_data = NULL,
    symbol = NULL,
    data_source = NULL
  )

  # ====================================================================
  # DATA LOADING FUNCTIONS
  # ====================================================================

  # Popular asset buttons
  observeEvent(input$load_btc, {
    load_yahoo_data("BTC-USD", "Bitcoin")
  })

  observeEvent(input$load_sp500, {
    load_yahoo_data("^GSPC", "S&P 500")
  })

  observeEvent(input$load_gold, {
    load_yahoo_data("GLD", "Gold ETF")
  })

  observeEvent(input$load_vix, {
    load_yahoo_data("^VIX", "VIX")
  })

  # Custom symbol loading
  observeEvent(input$load_custom, {
    req(input$custom_symbol)
    load_yahoo_data(input$custom_symbol, input$custom_symbol)
  })

  # Generic Yahoo Finance loading function
  load_yahoo_data <- function(symbol, name) {
    debug_log(paste("Starting data load for symbol:", symbol, "name:", name))

    showModal(modalDialog(
      title = "Loading Data",
      progress_indicator(paste("Downloading", name, "data...")),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        debug_log(paste("Modal shown, preparing to download data"))

        # Use date range from input or default
        from_date <- input$date_range[1] %||% (Sys.Date() - 365 * 3)
        to_date <- input$date_range[2] %||% (Sys.Date() - 1)
        debug_log(paste("Date range:", from_date, "to", to_date))

        # Download data
        debug_log(paste("Calling get_yahoo_data for", symbol))
        raw_yahoo <- get_yahoo_data(symbol, from_date, to_date)
        debug_log(paste("Yahoo data retrieved, rows:", nrow(raw_yahoo)))

        debug_log("Processing price data...")
        processed_data <- process_price_data(raw_yahoo)
        debug_log(paste(
          "Data processed, returns:",
          length(processed_data$returns)
        ))

        # Store data
        debug_log("Storing data in local_data...")
        local_data$raw_data <- processed_data
        local_data$symbol <- symbol
        local_data$data_source <- "Yahoo Finance"

        # Update values for other modules
        debug_log("Updating values$raw_data for other modules...")
        values$raw_data <- processed_data

        debug_log("Removing modal and showing success notification...")
        removeModal()
        safe_notification(
          paste(
            "Successfully loaded", name, "data with",
            length(processed_data$returns), "observations"
          ),
          type = "message"
        )
        debug_log(paste("Data loading completed successfully for", name))
      },
      error = function(e) {
        debug_log(paste("ERROR in data loading:", e$message), "ERROR")
        removeModal()
        safe_notification(
          paste("Failed to load data:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  }

  # CSV upload handling
  observe({
    req(input$csv_file)

    tryCatch(
      {
        # Read CSV to get column names
        sample_data <- read.csv(input$csv_file$datapath,
          nrows = 5,
          header = input$csv_header
        )
        col_names <- names(sample_data)

        updateSelectInput(session, "date_column",
          choices = col_names,
          selected = col_names[1]
        )
        updateSelectInput(session, "price_column",
          choices = col_names,
          selected = col_names[length(col_names)]
        )
      },
      error = function(e) {
        safe_notification(paste("Error reading CSV:", e$message),
          type = "error"
        )
      }
    )
  })

  # Process CSV
  observeEvent(input$process_csv, {
    req(input$csv_file, input$date_column, input$price_column)

    showModal(modalDialog(
      title = "Processing CSV",
      progress_indicator("Processing uploaded data..."),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        processed_data <- read_csv_data(
          input$csv_file$datapath,
          input$date_column,
          input$price_column,
          input$csv_header
        )

        # Store data
        local_data$raw_data <- processed_data
        local_data$symbol <- "CSV Data"
        local_data$data_source <- "CSV Upload"

        # Update values for other modules
        values$raw_data <- processed_data

        removeModal()
        safe_notification(
          paste(
            "Successfully processed CSV with",
            length(processed_data$returns), "observations"
          ),
          type = "message"
        )
      },
      error = function(e) {
        removeModal()
        safe_notification(
          paste("Failed to process CSV:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # ====================================================================
  # DATA CONFIGURATION
  # ====================================================================

  # Automatic configuration function
  apply_configuration <- function() {
    req(local_data$raw_data, input$target_measure)

    tryCatch(
      {
        # Calculate target volatility
        window <- if (input$target_measure %in%
                        c("rolling_rms", "rolling_std")) {
          input$rolling_window
        } else {
          NULL
        }

        target_vol <- calculate_target_volatility(
          local_data$raw_data$returns,
          input$target_measure,
          window,
          local_data$raw_data$return_dates
        )

        # Split data
        n_total <- length(target_vol$values)
        n_train <- floor(n_total * input$train_ratio / 100)

        train_indices <- 1:n_train
        test_indices <- (n_train + 1):n_total

        # Store processed data
        values$processed_data <- list(
          target_volatility = target_vol,
          returns = local_data$raw_data$returns,
          dates = local_data$raw_data$return_dates,
          train_indices = train_indices,
          test_indices = test_indices,
          split_info = list(
            n_total = n_total,
            n_train = n_train,
            n_test = length(test_indices),
            train_ratio = input$train_ratio
          )
        )

        debug_log("Split applied successfully")
      },
      error = function(e) {
        debug_log(paste("Configuration error:", e$message), "ERROR")
      }
    )
  }

  # Apply configuration automatically when data is loaded (with default 80%)
  observeEvent(local_data$raw_data, {
    if (!is.null(local_data$raw_data)) {
      apply_configuration()
    }
  })

  # Apply configuration automatically when target measure changes
  observeEvent(input$target_measure, {
    apply_configuration()
  })

  # Apply configuration automatically when rolling window changes
  observeEvent(input$rolling_window, {
    apply_configuration()
  })

  # Apply configuration automatically when train ratio changes
  observeEvent(input$train_ratio, {
    apply_configuration()
  })

  # ====================================================================
  # OUTPUTS
  # ====================================================================

  # Data loaded indicator
  output$data_loaded <- reactive({
    !is.null(local_data$raw_data)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # Target formula display
  output$target_formula <- renderUI({
    req(input$target_measure)

    measures <- get_available_measures()
    selected_measure <- measures[[input$target_measure]]

    if (!is.null(selected_measure)) {
      formula_box(
        selected_measure$name,
        selected_measure$formula,
        selected_measure$description
      )
    }
  })

  # Split information
  output$split_info <- renderUI({
    req(local_data$raw_data)

    n_total <- length(local_data$raw_data$returns)
    n_train <- floor(n_total * input$train_ratio / 100)
    n_test <- n_total - n_train

    div(
      p(strong("Data split preview:")),
      p(paste("• Total observations:", n_total)),
      p(paste(
        "• Training set:", n_train,
        paste0("(", input$train_ratio, "%)")
      )),
      p(paste(
        "• Test set:", n_test,
        paste0("(", 100 - input$train_ratio, "%)")
      ))
    )
  })

  # Price plot
  output$price_plot <- renderPlotly({
    req(local_data$raw_data)

    plot_data <- data.frame(
      Date = local_data$raw_data$dates,
      Price = local_data$raw_data$prices
    )

    p <- plot_ly(plot_data,
      x = ~Date, y = ~Price,
      type = "scatter", mode = "lines",
      line = list(color = "#3498db", width = 2)
    ) %>%
      layout(
        title = paste("Price Chart  - ", local_data$symbol),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price"),
        hovermode = "x unified"
      )

    p
  })

  # Returns plot
  output$returns_plot <- renderPlotly({
    req(local_data$raw_data)

    plot_data <- data.frame(
      Date = local_data$raw_data$return_dates,
      Returns = local_data$raw_data$returns
    )

    p <- plot_ly(plot_data,
      x = ~Date, y = ~Returns,
      type = "scatter", mode = "lines",
      line = list(color = "#e74c3c", width = 1)
    ) %>%
      layout(
        title = paste("Returns Chart  - ", local_data$symbol),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Returns"),
        hovermode = "x unified"
      )

    p
  })

  # Data summaries
  output$price_summary <- DT::renderDataTable({
    req(local_data$raw_data)

    prices <- local_data$raw_data$prices
    summary_data <- list(
      n_obs = length(prices),
      mean = mean(prices, na.rm = TRUE),
      std = sd(prices, na.rm = TRUE),
      min = min(prices, na.rm = TRUE),
      max = max(prices, na.rm = TRUE),
      n_missing = sum(is.na(prices))
    )

    create_data_summary_table(summary_data)
  })

  output$returns_summary <- DT::renderDataTable({
    req(local_data$raw_data)

    returns <- local_data$raw_data$returns
    summary_data <- list(
      n_obs = length(returns),
      mean = mean(returns, na.rm = TRUE),
      std = sd(returns, na.rm = TRUE),
      min = min(returns, na.rm = TRUE),
      max = max(returns, na.rm = TRUE),
      skewness = moments::skewness(returns, na.rm = TRUE),
      kurtosis = moments::kurtosis(returns, na.rm = TRUE),
      n_missing = sum(is.na(returns))
    )

    create_data_summary_table(summary_data)
  })

  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    req(local_data$raw_data)

    display_data <- data.frame(
      Date = local_data$raw_data$return_dates,
      Price = local_data$raw_data$prices[-1], # Remove first price
      Returns = local_data$raw_data$returns
    )

    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt - right", targets = 1:2)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("Price", "Returns"), digits = 6)
  })

  local_data
}
