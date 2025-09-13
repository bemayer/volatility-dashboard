# ============================================================================
# PROFESSIONAL VOLATILITY MODELING DASHBOARD
# A comprehensive toolkit for financial time series volatility analysis
# ============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(quantmod)
library(rugarch)
library(xts)
library(zoo)
library(moments)
library(forecast)
library(ggplot2)
library(viridis)

# Source all modules
source("R/data_source.R")
source("R/volatility_measures.R")
source("R/modeling_engine.R")
source("R/evaluation_metrics.R")
source("R/ui_components.R")

# Source UI components
source("ui/tab_data.R")
source("ui/tab_analysis.R")
source("ui/tab_modeling.R")

# ============================================================================
# MAIN APPLICATION
# ============================================================================

# Define UI
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Volatility Modeling Dashboard",
    titleWidth = 300
  ),

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Data Selection", tabName = "data", icon = icon("database")),
      menuItem(
        "Statistical Analysis", tabName = "analysis",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Volatility Modeling", tabName = "modeling", icon = icon("cogs")
      )
    )
  ),

  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content - wrapper, .right - side {
          background - color: #f8f9fa;
        }
        .box.box - solid.box - primary > .box - header {
          background: linear - gradient(45deg, #3c8dbc, #5cb85c);
          color: white;
        }
        .box.box - solid.box - warning > .box - header {
          background: linear - gradient(45deg, #f39c12, #e67e22);
          color: white;
        }
        .box.box - solid.box - success > .box - header {
          background: linear - gradient(45deg, #5cb85c, #18bc9c);
          color: white;
        }
        .box.box - solid.box - info > .box - header {
          background: linear - gradient(45deg, #3c8dbc, #3498db);
          color: white;
        }
        .main - header .navbar {
          background: linear - gradient(45deg, #2c3e50, #34495e) !important;
        }
        .skin - blue .main - sidebar {
          background: linear - gradient(180deg, #2c3e50, #34495e) !important;
        }
        .model - card {
          background: white;
          border - radius: 8px;
          padding: 15px;
          margin: 5px 0;
          box - shadow: 0 2px 4px rgba(0,0,0,0.1);
          border - left: 4px solid #3c8dbc;
        }
        .metric - highlight {
          background: linear - gradient(45deg, #e8f5e8, #f0f8f0);
          border - radius: 5px;
          padding: 5px;
        }
      "))
    ),

    # Include MathJax for formula rendering
    withMathJax(),

    tabItems(
      data_tab_ui("data_tab"),
      analysis_tab_ui("analysis_tab"),
      modeling_tab_ui("modeling_tab")
    )
  )
)

# Define server
server <- function(input, output, session) {

  # Reactive values for data sharing between modules
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    target_volatility = NULL,
    model_results = NULL,
    selected_models = list()
  )

  # Data tab server
  data_server <- callModule(data_tab_server, "data_tab", values)

  # Analysis tab server
  analysis_server <- callModule(analysis_tab_server, "analysis_tab", values)

  # Modeling tab server
  modeling_server <- callModule(modeling_tab_server, "modeling_tab", values)

  # Welcome message
  observe({
    showModal(modalDialog(
      title = "Welcome to Volatility Modeling Dashboard",
      HTML("
        <div style = 'text - align: center;'>
          <h4>Professional Financial Volatility Analysis< / h4>
          <br>
          <p><strong>Features:< / strong>< / p>
          <ul style = 'text - align: left; margin: 20px;'>
            <li>📊 Interactive data selection from Yahoo Finance< / li>
            <li>📈 Comprehensive statistical analysis< / li>
            <li>🔧 Dynamic model configuration< / li>
            <li>📋 Sortable performance comparison< / li>
            <li>📁 Export capabilities< / li>
          < / ul>
          <br>
          <p>
            <em>Start by selecting your data in the Data Selection tab.< / em>
          < / p>
        < / div>
      "),
      easyClose = TRUE,
      footer = modalButton("Get Started")
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
