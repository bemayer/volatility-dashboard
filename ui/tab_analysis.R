# ============================================================================
# STATISTICAL ANALYSIS TAB - UI AND SERVER
# Comprehensive statistical analysis of financial time series
# ============================================================================

#' Statistical Analysis tab UI
#' @param id Module ID
#' @return Shiny tabItem
analysis_tab_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "analysis",

    # Status check
    conditionalPanel(
      condition = paste0("!output['", ns("data_available"), "']"),
      fluidRow(
        box(
          title = "Data Required",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text-align: center; padding: 40px;",
            icon("exclamation-triangle", "fa-3x",
              style = "color: #f39c12;"
            ),
            br(), br(),
            h4("No Data Available", style = "color: #e67e22;"),
            p("Please load and configure data in the Data Selection tab first.",
              style = "color: #d68910;"
            )
          )
        )
      )
    ),

    # Main analysis content
    conditionalPanel(
      condition = paste0("output['", ns("data_available"), "']"),

      # Descriptive Statistics Section
      fluidRow(
        box(
          title = "Descriptive Statistics",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          DT::dataTableOutput(ns("descriptive_stats"))
        ),
        box(
          title = "Distribution Overview",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          tabsetPanel(
            tabPanel(
              "Histogram",
              plotlyOutput(ns("histogram_plot"), height = "300px")
            ),
            tabPanel(
              "Q-Q Plot",
              plotlyOutput(ns("qq_plot"), height = "300px")
            ),
            tabPanel(
              "Density",
              plotlyOutput(ns("density_plot"), height = "300px")
            )
          )
        )
      ),

      # Statistical Tests Section
      fluidRow(
        box(
          title = "Normality Tests",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          h5("Test Results"),
          DT::dataTableOutput(ns("normality_tests")),
          br(),
          div(
            style = paste0(
              "background: #fff3cd; padding: 10px; ",
              "border-radius: 5px;"
            ),
            p(strong("Interpretation:"), style = "margin: 0;"),
            p("p-value < 0.05 suggests rejection of normality hypothesis",
              style = "margin: 5px 0 0 0; font-size: 12px;"
            )
          )
        ),
        box(
          title = "Stationarity Tests",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          h5("Test Results"),
          DT::dataTableOutput(ns("stationarity_tests")),
          br(),
          div(
            style = paste0(
              "background: #fff3cd; padding: 10px; ",
              "border-radius: 5px;"
            ),
            p(strong("ADF Test:"), "p-value < 0.05 suggests stationarity",
              style = "margin: 0; font-size: 12px;"
            ),
            p(strong("KPSS Test:"), "p-value > 0.05 suggests stationarity",
              style = "margin: 5px 0 0 0; font-size: 12px;"
            )
          )
        )
      ),

      # Volatility Analysis Section
      fluidRow(
        box(
          title = "Volatility Clustering Tests",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          h5("ARCH-LM Test for Heteroscedasticity"),
          DT::dataTableOutput(ns("arch_tests")),
          br(),
          div(
            style = paste0(
              "background: #d1ecf1; padding: 10px; ",
              "border-radius: 5px;"
            ),
            p(strong("ARCH-LM Test:"),
              paste0(
                "p-value < 0.05 suggests presence of ARCH effects ",
                "(volatility clustering)"
              ),
              style = "margin: 0; font-size: 12px;"
            )
          )
        ),
        box(
          title = "Autocorrelation Analysis",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          tabsetPanel(
            tabPanel(
              "Returns ACF",
              plotlyOutput(ns("returns_acf"), height = "250px")
            ),
            tabPanel(
              "Squared Returns ACF",
              plotlyOutput(ns("squared_returns_acf"), height = "250px")
            ),
            tabPanel(
              "Absolute Returns ACF",
              plotlyOutput(ns("abs_returns_acf"), height = "250px")
            )
          )
        )
      ),

      # Time Series Visualization
      fluidRow(
        box(
          title = "Time Series Analysis",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          tabsetPanel(
            tabPanel(
              "Returns Time Series",
              div(style = "margin-top: 15px;",
                plotlyOutput(ns("returns_timeseries"), height = "400px")
              )
            ),
            tabPanel(
              "Volatility Evolution",
              div(style = "margin-top: 15px;",
                plotlyOutput(ns("volatility_timeseries"), height = "400px")
              )
            ),
            tabPanel(
              "Rolling Statistics",
              div(style = "margin-top: 15px;",
                fluidRow(
                  column(
                    6,
                    numericInput(ns("rolling_window_viz"), "Rolling Window:",
                      value = 30, min = 5, max = 252, step = 1
                    )
                  )
                ),
                plotlyOutput(ns("rolling_stats"), height = "400px")
              )
            )
          )
        )
      )
    ),

    # Help Section
    help_section(
      "Statistical Analysis",
      "
      <h5>Descriptive Statistics</h5>
      <ul>
        <li><strong>Mean:</strong> Average return</li>
        <li><strong>Std Dev:</strong> Volatility measure</li>
        <li><strong>Skewness:</strong> Asymmetry
        (negative = left tail)</li>
        <li><strong>Kurtosis:</strong> Tail thickness
        (>3 = heavy tails)</li>
        <li><strong>Min/Max:</strong> Extreme values</li>
      </ul>

      <h5>Statistical Tests</h5>
      <ul>
  <li><strong>Jarque-Bera:</strong> Tests normality assumption</li>
  <li><strong>Anderson-Darling:</strong>
        Alternative normality test</li>
        <li><strong>ADF:</strong> Tests for unit root
  (non-stationarity)</li>
        <li><strong>KPSS:</strong> Tests stationarity (complementary to ADF)
        </li>
        <li><strong>ARCH-LM:</strong> Tests for volatility clustering</li>
      </ul>

      <h5>Interpretation Guidelines</h5>
      <ul>
        <li><strong>Normal Returns:</strong> Rare in financial data, expect
        rejection</li>
        <li><strong>Stationary Returns:</strong>
        Expected for return series</li>
        <li><strong>ARCH Effects:</strong>
        Common in financial returns, justifies
        GARCH models</li>
        <li><strong>Autocorrelation:</strong> Should be minimal in efficient
        markets</li>
      </ul>
      "
    )
  )
}

#' Statistical Analysis tab server
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param values Reactive values
analysis_tab_server <- function(input, output, session, values) {
  # ====================================================================
  # REACTIVE DATA PREPARATION
  # ====================================================================

  # Check if data is available
  output$data_available <- reactive({
    !is.null(values$processed_data) && !is.null(values$processed_data$returns)
  })
  outputOptions(output, "data_available", suspendWhenHidden = FALSE)

  # Get analysis data
  analysis_data <- reactive({
    req(values$processed_data)

    returns <- values$processed_data$returns
    target_vol <- values$processed_data$target_volatility$values
    dates <- values$processed_data$dates

    # Remove missing values
    valid_idx <- !is.na(returns) & !is.na(target_vol)

    list(
      returns = returns[valid_idx],
      target_volatility = target_vol[valid_idx],
      dates = dates[valid_idx],
      n_obs = sum(valid_idx)
    )
  })

  # ====================================================================
  # DESCRIPTIVE STATISTICS
  # ====================================================================

  output$descriptive_stats <- DT::renderDataTable({
    req(analysis_data())

    data <- analysis_data()
    returns <- data$returns
    target_vol <- data$target_volatility

    # Calculate statistics
    stats_df <- data.frame(
      Variable = c("Returns", "Target Volatility"),
      Observations = c(length(returns), length(target_vol)),
      Mean = c(
        mean(returns, na.rm = TRUE),
        mean(target_vol, na.rm = TRUE)
      ),
      `Std Dev` = c(
        sd(returns, na.rm = TRUE),
        sd(target_vol, na.rm = TRUE)
      ),
      Minimum = c(
        min(returns, na.rm = TRUE),
        min(target_vol, na.rm = TRUE)
      ),
      Maximum = c(
        max(returns, na.rm = TRUE),
        max(target_vol, na.rm = TRUE)
      ),
      Skewness = c(
        moments::skewness(returns, na.rm = TRUE),
        moments::skewness(target_vol, na.rm = TRUE)
      ),
      Kurtosis = c(
        moments::kurtosis(returns, na.rm = TRUE),
        moments::kurtosis(target_vol, na.rm = TRUE)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      stats_df,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt - right", targets = 2:7)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 3:8, digits = 6)
  })

  # ====================================================================
  # DISTRIBUTION PLOTS
  # ====================================================================

  output$histogram_plot <- renderPlotly({
    req(analysis_data())

    returns <- analysis_data()$returns

    p <- plot_ly(
      x = ~returns, type = "histogram", nbinsx = 50,
      marker = list(color = "#3498db", opacity = 0.7)
    ) %>%
      layout(
        title = "Returns Distribution",
        xaxis = list(title = "Returns"),
        yaxis = list(title = "Frequency"),
        showlegend = FALSE
      )

    p
  })

  output$qq_plot <- renderPlotly({
    req(analysis_data())

    returns <- analysis_data()$returns
    n <- length(returns)

    # Calculate quantiles
    sorted_returns <- sort(returns)
    theoretical_quantiles <- qnorm((1:n - 0.5) / n)

    plot_data <- data.frame(
      Theoretical = theoretical_quantiles,
      Sample = sorted_returns
    )

    p <- plot_ly(plot_data,
      x = ~Theoretical, y = ~Sample,
      type = "scatter", mode = "markers",
      marker = list(color = "#e74c3c", size = 4)
    ) %>%
      add_trace(
        x = range(theoretical_quantiles),
        y = range(theoretical_quantiles),
        type = "scatter", mode = "lines", name = "Normal Line",
        line = list(color = "#2c3e50", dash = "dash")
      ) %>%
      layout(
        title = "Q - Q Plot vs Normal Distribution",
        xaxis = list(title = "Theoretical Quantiles"),
        yaxis = list(title = "Sample Quantiles"),
        showlegend = FALSE
      )

    p
  })

  output$density_plot <- renderPlotly({
    req(analysis_data())

    returns <- analysis_data()$returns

    # Kernel density estimation
    density_est <- density(returns, na.rm = TRUE)

    # Normal overlay
    x_seq <- seq(min(returns, na.rm = TRUE),
      max(returns, na.rm = TRUE),
      length.out = 100
    )
    normal_density <- dnorm(x_seq,
      mean = mean(returns, na.rm = TRUE),
      sd = sd(returns, na.rm = TRUE)
    )

    p <- plot_ly() %>%
      add_trace(
        x = density_est$x, y = density_est$y,
        type = "scatter", mode = "lines",
        name = "Empirical",
        line = list(color = "#3498db", width = 3)
      ) %>%
      add_trace(
        x = x_seq, y = normal_density,
        type = "scatter", mode = "lines",
        name = "Normal",
        line = list(
          color = "#e74c3c", dash = "dash",
          width = 2
        )
      ) %>%
      layout(
        title = "Density Comparison",
        xaxis = list(title = "Returns"),
        yaxis = list(title = "Density"),
        legend = list(x = 0.7, y = 0.9)
      )

    p
  })

  # ====================================================================
  # STATISTICAL TESTS
  # ====================================================================

  output$normality_tests <- DT::renderDataTable({
    req(analysis_data())

    returns <- analysis_data()$returns

    # Jarque - Bera test
    jb_test <- moments::jarque.test(returns)

    # Anderson - Darling test
    ad_test <- nortest::ad.test(returns)

    tests_df <- data.frame(
      Test = c("Jarque - Bera", "Anderson - Darling"),
      Statistic = c(jb_test$statistic, ad_test$statistic),
      `P - Value` = c(jb_test$p.value, ad_test$p.value),
      `H0 Rejected` = c(jb_test$p.value < 0.05, ad_test$p.value < 0.05),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      tests_df,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt - right", targets = 1:2)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2:3, digits = 6) %>%
      DT::formatStyle(
        "H0 Rejected",
        backgroundColor = DT::styleEqual(TRUE, "#ffebee")
      )
  })

  output$stationarity_tests <- DT::renderDataTable({
    req(analysis_data())

    returns <- analysis_data()$returns

    # ADF test
    adf_test <- tseries::adf.test(returns)

    # KPSS test
    kpss_test <- tseries::kpss.test(returns, null = "Level")

    tests_df <- data.frame(
      Test = c("Augmented Dickey - Fuller", "KPSS"),
      Statistic = c(adf_test$statistic, kpss_test$statistic),
      `P - Value` = c(adf_test$p.value, kpss_test$p.value),
      Interpretation = c(
        ifelse(adf_test$p.value < 0.05, "Stationary", "Non - Stationary"),
        ifelse(kpss_test$p.value > 0.05, "Stationary", "Non - Stationary")
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      tests_df,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt - right", targets = 1:2)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2:3, digits = 6) %>%
      DT::formatStyle(
        "Interpretation",
        backgroundColor = DT::styleEqual("Stationary", "#e8f5e8")
      )
  })

  output$arch_tests <- DT::renderDataTable({
    req(analysis_data())

    returns <- analysis_data()$returns

    tryCatch(
      {
        # ARCH - LM test using FinTS package
        arch_test_5 <- FinTS::ArchTest(returns, lags = 5)
        arch_test_10 <- FinTS::ArchTest(returns, lags = 10)

        tests_df <- data.frame(
          `Lags` = c(5, 10),
          `LM Statistic` = c(arch_test_5$statistic, arch_test_10$statistic),
          `P - Value` = c(arch_test_5$p.value, arch_test_10$p.value),
          `ARCH Effects` = c(
            ifelse(arch_test_5$p.value < 0.05, "Present", "Not Detected"),
            ifelse(arch_test_10$p.value < 0.05, "Present", "Not Detected")
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        DT::datatable(
          tests_df,
          options = list(
            dom = "t",
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            columnDefs = list(
              list(className = "dt - right", targets = 1:2)
            )
          ),
          rownames = FALSE
        ) %>%
          DT::formatRound(columns = 2:3, digits = 6) %>%
          DT::formatStyle(
            "ARCH Effects",
            backgroundColor = DT::styleEqual("Present", "#fff3cd")
          )
      },
      error = function(e) {
        DT::datatable(
          data.frame(Error = "Could not compute ARCH tests"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    )
  })

  # ====================================================================
  # AUTOCORRELATION PLOTS
  # ====================================================================

  create_acf_plot <- function(series, title, max_lag = 20) {
    acf_result <- acf(series, lag.max = max_lag, plot = FALSE)

    plot_data <- data.frame(
      Lag = 0:max_lag,
      ACF = as.numeric(acf_result$acf),
      Significant = abs(as.numeric(acf_result$acf)) >
        1.96 / sqrt(length(series))
    )

    confidence_level <- 1.96 / sqrt(length(series))

    p <- plot_ly(plot_data,
      x = ~Lag, y = ~ACF, type = "bar",
      marker = list(color = ~ ifelse(Significant,
                    "#e74c3c", "#3498db"
      ))
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Lag"),
        yaxis = list(title = "Autocorrelation"),
        showlegend = FALSE,
        shapes = list(
          # Upper confidence bound
          list(
            type = "line", x0 = min(plot_data$Lag),
            x1 = max(plot_data$Lag),
            y0 = confidence_level, y1 = confidence_level,
            line = list(color = "red", dash = "dash", width = 2)
          ),
          # Lower confidence bound
          list(
            type = "line", x0 = min(plot_data$Lag),
            x1 = max(plot_data$Lag),
            y0 = -confidence_level, y1 = -confidence_level,
            line = list(color = "red", dash = "dash", width = 2)
          ),
          # Zero line
          list(
            type = "line", x0 = min(plot_data$Lag),
            x1 = max(plot_data$Lag),
            y0 = 0, y1 = 0,
            line = list(color = "black", width = 1)
          )
        )
      )

    return(p)
  }

  output$returns_acf <- renderPlotly({
    req(analysis_data())
    create_acf_plot(analysis_data()$returns, "Returns Autocorrelation")
  })

  output$squared_returns_acf <- renderPlotly({
    req(analysis_data())
    returns <- analysis_data()$returns
    create_acf_plot(returns^2, "Squared Returns Autocorrelation")
  })

  output$abs_returns_acf <- renderPlotly({
    req(analysis_data())
    returns <- analysis_data()$returns
    create_acf_plot(abs(returns), "Absolute Returns Autocorrelation")
  })

  # ====================================================================
  # TIME SERIES PLOTS
  # ====================================================================

  output$returns_timeseries <- renderPlotly({
    req(analysis_data())

    data <- analysis_data()

    plot_data <- data.frame(
      Date = data$dates,
      Returns = data$returns
    )

    p <- plot_ly(plot_data,
      x = ~Date, y = ~Returns,
      type = "scatter", mode = "lines",
      line = list(color = "#e74c3c", width = 1)
    ) %>%
      layout(
        title = "Returns Time Series",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Returns"),
        hovermode = "x unified"
      )

    p
  })

  output$volatility_timeseries <- renderPlotly({
    req(analysis_data())

    data <- analysis_data()

    plot_data <- data.frame(
      Date = data$dates,
      Volatility = data$target_volatility
    )

    p <- plot_ly(plot_data,
      x = ~Date, y = ~Volatility,
      type = "scatter", mode = "lines",
      line = list(color = "#9b59b6", width = 1.5)
    ) %>%
      layout(
        title = "Target Volatility Time Series",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Volatility"),
        hovermode = "x unified"
      )

    p
  })

  output$rolling_stats <- renderPlotly({
    req(analysis_data(), input$rolling_window_viz)

    data <- analysis_data()
    returns <- data$returns
    window <- input$rolling_window_viz

    if (length(returns) < window) {
      return(plotly_empty())
    }

    # Calculate rolling statistics
    n <- length(returns)
    rolling_mean <- rep(NA, n)
    rolling_vol <- rep(NA, n)

    for (i in window:n) {
      window_data <- returns[(i - window + 1):i]
      rolling_mean[i] <- mean(window_data, na.rm = TRUE)
      rolling_vol[i] <- sd(window_data, na.rm = TRUE)
    }

    plot_data <- data.frame(
      Date = data$dates,
      `Rolling Mean` = rolling_mean,
      `Rolling Volatility` = rolling_vol,
      check.names = FALSE
    )

    p <- plot_ly() %>%
      add_trace(
        data = plot_data, x = ~Date, y = ~`Rolling Mean`,
        type = "scatter", mode = "lines",
        name = "Rolling Mean", line = list(color = "#3498db")
      ) %>%
      add_trace(
        data = plot_data, x = ~Date, y = ~`Rolling Volatility`,
        type = "scatter", mode = "lines",
        name = "Rolling Volatility", yaxis = "y2",
        line = list(color = "#e74c3c")
      ) %>%
      layout(
        title = paste("Rolling Statistics (", window, " - period window)"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Rolling Mean", side = "left"),
        yaxis2 = list(
          title = "Rolling Volatility", side = "right",
          overlaying = "y"
        ),
        hovermode = "x unified",
        legend = list(x = 0.7, y = 0.9)
      )

    p
  })
}
