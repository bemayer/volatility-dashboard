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

    # Header section - Two columns: Yahoo Finance (left) + Deribit (right)
    fluidRow(
      # LEFT COLUMN: Traditional Assets - Yahoo Finance
      column(
        6,
        box(
          title = "Traditional Assets",
          status = "primary",
          solidHeader = TRUE,
          width = 12,

          h4("Popular Assets"),
          p("Quick access to commonly analyzed financial instruments:"),

          # Asset buttons (WITHOUT VIX)
          div(
            style = "margin: 10px 0;",
            actionButton(
              ns("load_btc"),
              "Bitcoin (BTC-USD)",
              class = "btn-outline-primary",
              style = "margin: 2px; width: 100%;"
            ),
            actionButton(
              ns("load_sp500"),
              "S&P 500 (^GSPC)",
              class = "btn-outline-primary",
              style = "margin: 2px; width: 100%;"
            ),
            actionButton(
              ns("load_gold"),
              "Gold ETF (GLD)",
              class = "btn-outline-primary",
              style = "margin: 2px; width: 100%;"
            )
          ),
          br(),

          h4("Custom Symbol"),
          p("Enter any Yahoo Finance symbol:"),
          textInput(
            ns("custom_symbol"),
            "Symbol:",
            placeholder = "e.g., AAPL, TSLA, EURUSD=X"
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
        )
      ),

      # RIGHT COLUMN: Crypto Options - Deribit
      column(
        6,
        box(
          title = "Crypto Options - Deribit",
          status = "primary",
          solidHeader = TRUE,
          width = 12,

          radioButtons(
            ns("currency"),
            "Currency:",
            choices = list(
              "Bitcoin (BTC)" = "BTC",
              "Ethereum (ETH)" = "ETH"
            ),
            selected = "BTC",
            inline = TRUE
          ),

          numericInput(
            ns("sample_size"),
            "Number of recent observations:",
            value = 10000,
            min = 1000,
            max = 200000,
            step = 1000
          ),

          radioButtons(
            ns("price_type"),
            "Price Type:",
            choices = list(
              "Underlying Price" = "price",
              "Option Mid Price" = "mid_price"
            ),
            selected = "price",
            inline = TRUE
          ),

          radioButtons(
            ns("volatility_measure"),
            "Volatility Measure:",
            choices = list(
              "Realized (from returns)" = "rolling_std",
              "Implied (from options)" = "implied_vol"
            ),
            selected = "rolling_std",
            inline = TRUE
          ),

          conditionalPanel(
            condition = paste0(
              "input['", ns("volatility_measure"), "'] == 'rolling_std'"
            ),
            numericInput(
              ns("rolling_window_size"),
              "Rolling window:",
              value = 20,
              min = 2,
              max = 100,
              step = 1
            )
          ),

          actionButton(
            ns("load_s3_data"),
            "📥 Load Options Data",
            class = "btn-success",
            style = "width: 100%; margin-top: 10px;"
          ),

          br(), br(),

          fluidRow(
            column(
              6,
              actionButton(
                ns("preview_s3_data"),
                "👁️ Preview",
                class = "btn-outline-info",
                style = "width: 100%;"
              )
            ),
            column(
              6,
              downloadButton(
                ns("download_s3_data"),
                "⬇️ Download",
                class = "btn-outline-secondary",
                style = "width: 100%;"
              )
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
            status = "info",
            solidHeader = TRUE,
            width = 12,
            radioButtons(
              ns("target_measure"),
              "Select target volatility measure:",
              choices = list(
                "Squared Returns \\(r_t^2\\)" = "squared",
                "Rolling Standard Deviation \\(\\sigma_t^2\\)" =
                  "rolling_std"
              ),
              selected = "rolling_std"
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
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text-align: center; padding: 40px;",
            icon("database", "fa-3x", style = "color: #bdc3c7;"),
            br(), br(),
            h4("No Data Loaded", style = "color: #7f8c8d;"),
            p(
              paste(
                "Click 'Load Deribit Data from S3' to load the",
                "most recent consecutive observations."
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
        <li><strong>Popular Assets:</strong> Click any button to
            quickly load common financial instruments</li>
        <li><strong>Custom Symbol:</strong> Enter any Yahoo Finance
            symbol (stocks, indices, currencies, commodities)</li>
        <li><strong>CSV Upload:</strong> Upload your own data with
            Date and Price columns</li>
      </ul>

      <h5>Target Volatility Measures</h5>
      <ul>
        <li><strong>Squared Returns:</strong> \\(r_t^2\\) -
            Classical realized variance proxy (most common)</li>
        <li><strong>Rolling Standard Deviation:</strong>
            \\(\\sqrt{\\frac{1}{n-1} \\sum_{i=1}^n
            (r_i - \\bar{r})^2}\\) - Sample standard deviation
            (traditional volatility measure)</li>
      </ul>

      <h5>Key Differences</h5>
      <ul>
        <li><strong>Squared Returns:</strong> Represent realized
            variance, suitable for GARCH modeling and theoretical
            applications</li>
        <li><strong>Rolling Standard Deviation:</strong> Provides
            time-varying volatility estimates with adjustable
            smoothing window</li>
        <li><strong>Scale Consideration:</strong> Squared returns are
            in variance units (σ²), while rolling std is in
            volatility units (σ)</li>
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
  # YAHOO FINANCE DATA LOADING
  # ====================================================================

  # Load Bitcoin (BTC-USD)
  observeEvent(input$load_btc, {
    load_yahoo_symbol("BTC-USD", "Bitcoin")
  })

  # Load S&P 500
  observeEvent(input$load_sp500, {
    load_yahoo_symbol("^GSPC", "S&P 500")
  })

  # Load Gold ETF
  observeEvent(input$load_gold, {
    load_yahoo_symbol("GLD", "Gold ETF")
  })

  # Load custom symbol
  observeEvent(input$load_custom, {
    req(input$custom_symbol, input$date_range)

    symbol <- trimws(toupper(input$custom_symbol))
    if (symbol == "") {
      safe_notification("Please enter a valid symbol", type = "warning")
      return()
    }

    load_yahoo_symbol(
      symbol,
      symbol,
      from = input$date_range[1],
      to = input$date_range[2]
    )
  })

  # Helper function to load Yahoo Finance data
  load_yahoo_symbol <- function(symbol, display_name,
                                 from = NULL, to = NULL) {
    showModal(modalDialog(
      title = paste("Loading", display_name),
      progress_indicator(
        paste(
          "Downloading data for", symbol, "from Yahoo Finance..."
        )
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch({
      # Source data loading functions
      source("R/data_source.R", local = TRUE)

      # Use default date range if not provided
      if (is.null(from)) from <- Sys.Date() - 365 * 3  # 3 years
      if (is.null(to)) to <- Sys.Date() - 1

      debug_log(paste("[YAHOO] Loading", symbol, "from", from, "to", to))

      # Download data from Yahoo Finance
      raw_prices <- get_yahoo_data(symbol, from, to)

      # Process to returns
      processed_data <- process_price_data(raw_prices)

      debug_log(
        paste("[YAHOO] Loaded", length(processed_data$returns), "observations")
      )

      # Store data
      local_data$raw_data <- processed_data
      local_data$symbol <- display_name
      local_data$data_source <- "Yahoo Finance"
      local_data$deribit_measure <- NULL  # Clear Deribit measure

      # Update global values
      values$raw_data <- processed_data
      values$processed_data <- NULL
      values$target_volatility <- NULL
      values$model_results <- NULL
      values$selected_models <- list()

      removeModal()
      safe_notification(
        paste(
          "Successfully loaded", display_name, "with",
          length(processed_data$returns), "observations"
        ),
        type = "message"
      )
    }, error = function(e) {
      debug_log(paste("[YAHOO ERROR]:", e$message), "ERROR")
      removeModal()
      safe_notification(
        paste("Failed to load", display_name, ":", e$message),
        type = "error",
        duration = 10
      )
    })
  }

  # ====================================================================
  # S3 DATA LOADING FUNCTIONS
  # ====================================================================

  # View S3 metadata
  observeEvent(input$view_s3_metadata, {
    showModal(modalDialog(
      title = "Reading S3 Metadata",
      progress_indicator("Connecting to S3..."),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        source("R/s3_streaming.R", local = TRUE)
        metadata <- get_s3_metadata()

        removeModal()

        showModal(modalDialog(
          title = "S3 File Metadata",
          size = "l",
          easyClose = TRUE,
          fluidRow(
            column(
              12,
              h4("File Information"),
              p(
                paste("Total Rows:", format(metadata$total_rows, big.mark = ","))
              ),
              p(paste("Total Columns:", metadata$num_columns)),
              br(),
              h5("Columns:"),
              DT::renderDataTable({
                col_info <- data.frame(
                  Column = metadata$columns,
                  Type = as.character(metadata$column_types)
                )
                DT::datatable(col_info,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE
                )
              })
            )
          )
        ))
      },
      error = function(e) {
        removeModal()
        safe_notification(
          paste("Failed to read S3 metadata:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Load S3 data with optimized Arrow query
  observeEvent(input$load_s3_data, {
    req(
      input$sample_size,
      input$price_type,
      input$volatility_measure,
      input$currency
    )

    currency_name <- if (input$currency == "BTC") "Bitcoin" else "Ethereum"

    debug_log(paste(
      "[DERIBIT] Starting optimized load:",
      input$currency, input$price_type, input$volatility_measure
    ))

    showModal(modalDialog(
      title = paste("Loading", currency_name, "Options Data"),
      progress_indicator(paste(
        "Querying", format(input$sample_size, big.mark = ","),
        "most recent", currency_name, "options from Parquet..."
      )),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        # Source the S3 streaming and processing functions
        source("R/s3_streaming.R", local = TRUE)
        source("R/parquet_direct.R", local = TRUE)

        debug_log("[DERIBIT] Calling query_deribit_optimized()...")

        # Use optimized Arrow query with predicate pushdown AND
        # hourly aggregation. This ensures ONE observation per hour
        # (aggregating all options) for valid time series
        s3_data <- query_deribit_optimized(
          s3_url = "https://deribit-data-bucket.s3.amazonaws.com/deribit_orderbook.parquet",
          currency = input$currency,
          instrument_type = "option",
          sample_size = input$sample_size,
          aggregate_by_hour = TRUE  # Critical for time series modeling!
        )

        debug_log(paste("[DERIBIT] Query completed, rows:", nrow(s3_data)))

        # Process data according to selected options
        processed_data <- process_deribit_data(
          s3_data,
          price_type = input$price_type,
          volatility_measure = input$volatility_measure,
          rolling_window = if (input$volatility_measure == "rolling_std") {
            input$rolling_window_size
          } else {
            NULL
          }
        )

        debug_log(paste(
          "[DERIBIT] Data processed, returns:",
          length(processed_data$returns)
        ))

        # Store data
        debug_log("[DERIBIT] Storing data in local_data...")
        local_data$raw_data <- processed_data
        local_data$symbol <- paste("Deribit", currency_name, "Options")
        local_data$data_source <- "Deribit"
        local_data$deribit_measure <- input$volatility_measure

        # Update values for other modules and reset all computed data
        debug_log("[DERIBIT] Updating values$raw_data for other modules...")
        values$raw_data <- processed_data

        # Clear all computed data when new raw data is loaded
        values$processed_data <- NULL
        values$target_volatility <- NULL
        values$model_results <- NULL
        values$selected_models <- list()

        debug_log(
          "[DERIBIT] Removing modal and showing success notification..."
        )
        removeModal()
        safe_notification(
          paste(
            "Successfully loaded", currency_name, "options data with",
            length(processed_data$returns), "observations"
          ),
          type = "message"
        )
        debug_log("[DERIBIT] Data loading completed successfully")
      },
      error = function(e) {
        debug_log(paste("[DERIBIT ERROR]:", e$message), "ERROR")
        removeModal()
        safe_notification(
          paste("Failed to load options data:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Reactive values for preview pagination
  preview_state <- reactiveValues(
    current_page = 1,
    loaded_rows = 0,
    page_size = 1000,
    data = NULL,
    currency = NULL
  )

  # Helper function to load preview page
  load_preview_page <- function(page_num) {
    req(preview_state$currency)

    currency_name <- if (preview_state$currency == "BTC") {
      "Bitcoin"
    } else {
      "Ethereum"
    }

    tryCatch({
      source("R/s3_streaming.R", local = TRUE)

      # Calculate offset
      offset <- (page_num - 1) * preview_state$page_size

      # Query data for this page (raw data for preview - no aggregation needed)
      preview_data <- query_deribit_optimized(
        s3_url = "https://deribit-data-bucket.s3.amazonaws.com/deribit_orderbook.parquet",
        currency = preview_state$currency,
        instrument_type = "option",
        sample_size = preview_state$page_size,
        offset = offset
      )

      # Extract metadata
      loaded_rows <- attr(preview_data, "loaded_rows")
      start_row <- attr(preview_data, "start_row")
      end_row <- attr(preview_data, "end_row")
      returned_rows <- attr(preview_data, "returned_rows")

      # Update state
      preview_state$current_page <- page_num
      preview_state$loaded_rows <- if (!is.null(loaded_rows)) loaded_rows else nrow(preview_data)
      preview_state$data <- preview_data

      # Update modal
      showModal(modalDialog(
        title = paste(currency_name, "Options Data Preview"),
        size = "l",
        easyClose = TRUE,
        div(
          style = "margin-top: 15px;",
          p(HTML(paste0(
            "<strong>Page ", preview_state$current_page, "</strong>",
            " (showing ", format(returned_rows, big.mark = ","), " rows",
            " from ", format(start_row, big.mark = ","),
            " to ", format(end_row, big.mark = ","),
            " of ", format(preview_state$loaded_rows, big.mark = ","),
            " loaded <strong>", preview_state$currency, " options</strong>)"
          ))),
          DT::dataTableOutput("data_tab-preview_table")
        ),
        footer = tagList(
          div(
            style = paste(
              "display: flex; justify-content: space-between;",
              "align-items: center;"
            ),
            div(
              actionButton(
                "data_tab-preview_prev",
                "← Previous",
                class = "btn-primary",
                disabled = if (preview_state$current_page == 1) {
                  "disabled"
                } else {
                  NULL
                }
              )
            ),
            div(
              style = "text-align: center;",
              sprintf("Page %d", preview_state$current_page)
            ),
            div(
              actionButton(
                "data_tab-preview_next",
                "Next →",
                class = "btn-primary",
                disabled = if (returned_rows < preview_state$page_size) {
                  "disabled"
                } else {
                  NULL
                }
              )
            )
          ),
          modalButton("Close")
        )
      ))

    }, error = function(e) {
      removeModal()
      safe_notification(
        paste("Failed to load preview page:", e$message),
        type = "error",
        duration = 10
      )
    })
  }

  # Preview S3 data - initial load
  observeEvent(input$preview_s3_data, {
    req(input$currency)

    preview_state$currency <- input$currency
    currency_name <- if (input$currency == "BTC") "Bitcoin" else "Ethereum"

    showModal(modalDialog(
      title = paste("Preview", currency_name, "Options Data"),
      progress_indicator("Loading preview data..."),
      footer = NULL,
      easyClose = FALSE
    ))

    # Load first page
    load_preview_page(1)
  })

  # Previous page button
  observeEvent(input$preview_prev, {
    if (preview_state$current_page > 1) {
      showModal(modalDialog(
        title = "Loading...",
        progress_indicator("Loading previous page..."),
        footer = NULL,
        easyClose = FALSE
      ))
      load_preview_page(preview_state$current_page - 1)
    }
  })

  # Next page button
  observeEvent(input$preview_next, {
    if (preview_state$current_page < preview_state$total_pages) {
      showModal(modalDialog(
        title = "Loading...",
        progress_indicator("Loading next page..."),
        footer = NULL,
        easyClose = FALSE
      ))
      load_preview_page(preview_state$current_page + 1)
    }
  })

  # Go to page button
  # Note: Go to page functionality removed due to memory optimization
  # (we no longer count total pages to avoid scanning entire dataset)

  # Render preview table
  output$preview_table <- DT::renderDataTable({
    req(preview_state$data)

    DT::datatable(
      preview_state$data,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        scrollY = "500px",
        lengthMenu = c(10, 25, 50, 100, 500, 1000),
        dom = 'lftip'
      ),
      rownames = FALSE
    )
  })

  # Download S3 data handler
  output$download_s3_data <- downloadHandler(
    filename = function() {
      currency_name <- if (input$currency == "BTC") "bitcoin" else "ethereum"
      paste0("deribit_", currency_name, "_options_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$currency, input$sample_size)

      tryCatch({
        source("R/s3_streaming.R", local = TRUE)

        # Query data
        download_data <- query_deribit_optimized(
          s3_url = "https://deribit-data-bucket.s3.amazonaws.com/deribit_orderbook.parquet",
          currency = input$currency,
          instrument_type = "option",
          sample_size = input$sample_size
        )

        # Write to CSV
        write.csv(download_data, file, row.names = FALSE)
      }, error = function(e) {
        safe_notification(
          paste("Failed to download data:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
  )

  # ====================================================================
  # DATA CONFIGURATION
  # ====================================================================

  # Automatic configuration function
  apply_configuration <- function() {
    req(local_data$raw_data, input$target_measure)

    tryCatch(
      {
        debug_log("[CONFIG] Starting data configuration...")

        # Determine if this is Deribit data
        is_deribit <- !is.null(local_data$deribit_measure)

        if (is_deribit) {
          debug_log(
            paste("[CONFIG] Using Deribit measure:", local_data$deribit_measure)
          )

          # For Deribit data, use the measure selected during loading
          deribit_measure <- local_data$deribit_measure

          # Source the required function
          source("R/volatility_measures.R", local = TRUE)

          # Calculate target volatility based on Deribit measure
          if (deribit_measure == "rolling_std") {
            window <- input$rolling_window_size %||% 20
            debug_log(
              paste("[CONFIG] Measure:", deribit_measure, "| Window:", window)
            )

            target_vol <- calculate_target_volatility(
              local_data$raw_data$returns,
              "rolling_std",
              window,
              local_data$raw_data$return_dates
            )
          } else if (deribit_measure == "implied_vol") {
            debug_log(
              "[CONFIG] Using implied volatility from Deribit data"
            )

            if (is.null(local_data$raw_data$raw_iv)) {
              stop(
                paste(
                  "Deribit data with implied volatility",
                  "required for measure: implied_vol"
                )
              )
            }

            # Use IV directly as target volatility
            target_vol <- list(
              values = local_data$raw_data$raw_iv,
              dates = local_data$raw_data$return_dates,
              measure = "implied_vol",
              description = "Implied Volatility from Options"
            )
          }
        } else {
          # Regular Yahoo/CSV data
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
        }

        debug_log(
          "[CONFIG] Data source:",
          local_data$data_source %||% "Unknown"
        )
        debug_log(
          paste(
            "[CONFIG] Returns length:",
            length(local_data$raw_data$returns)
          )
        )
        debug_log(
          "[CONFIG] Calling calculate_target_volatility_enhanced..."
        )

        # Split data
        n_total <- length(target_vol$values)
        n_train <- floor(n_total * input$train_ratio / 100)

        train_indices <- 1:n_train
        test_indices <- (n_train + 1):n_total

        debug_log(
          paste(
            "[CONFIG] Data split: total=", n_total,
            "train=", n_train, "test=", length(test_indices)
          )
        )

        # Store processed data
        values$processed_data <- list(
          target_volatility = target_vol,
          target_type = if (is_deribit) {
            local_data$deribit_measure
          } else {
            input$target_measure
          },
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

        debug_log("[CONFIG] values$processed_data structure created")

        # Clear model results when data configuration changes
        values$model_results <- NULL
        values$selected_models <- list()

        debug_log("[CONFIG] ✓ Configuration completed successfully!")
        debug_log(
          paste(
            "[CONFIG] processed_data is NULL?",
            is.null(values$processed_data)
          )
        )
        debug_log(
          paste(
            "[CONFIG] target_volatility is NULL?",
            is.null(values$processed_data$target_volatility)
          )
        )
        debug_log(
          paste(
            "[CONFIG] returns is NULL?",
            is.null(values$processed_data$returns)
          )
        )

        # Show success notification to user
        safe_notification(
          paste(
            "Data configured successfully:",
            n_total, "observations,",
            n_train, "training,",
            length(test_indices), "test"
          ),
          type = "message"
        )
      },
      error = function(e) {
        debug_log(paste("[CONFIG ERROR]:", e$message), "ERROR")
        safe_notification(
          paste("Error configuring data:", e$message),
          type = "error",
          duration = 10
        )
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
