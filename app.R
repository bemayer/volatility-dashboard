# ============================================================================
# VOLATILITY MODELING DASHBOARD
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
        "Statistical Analysis",
        tabName = "analysis",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Volatility Modeling",
        tabName = "modeling",
        icon = icon("cogs")
      )
    )
  ),

  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* ==========================================================================
           GLOBAL STYLES & BASE
           ========================================================================== */

        * {
          scrollbar-width: thin;
          scrollbar-color: #95a5a6 #ecf0f1;
        }

        html {
          scroll-behavior: smooth;
        }

        .content-wrapper, .right-side {
          background-color: #ecf0f1;
        }

        /* ==========================================================================
           BOX ALIGNMENT - Equal Heights
           ========================================================================== */

        /* Make all rows use flexbox for equal height boxes */
        .row {
          display: flex;
          flex-wrap: wrap;
        }

        /* Make columns stretch to full height */
        .row > [class*='col-'] {
          display: flex;
          flex-direction: column;
        }

        /* Make boxes stretch to fill column height */
        .row > [class*='col-'] > .box {
          flex: 1;
          display: flex;
          flex-direction: column;
        }

        /* Make box body flex to fill remaining space */
        .box-body {
          flex: 1;
        }

        /* ==========================================================================
           BOX HEADER GRADIENTS - Semantic Colors
           ========================================================================== */

        /* Primary: Core workflow boxes (blue) */
        .box.box-solid.box-primary > .box-header {
          background: linear-gradient(
            135deg, #3498db 0%, #2980b9 100%
          );
          color: white;
          font-weight: 600;
        }

        /* Success: Results and completed states (green) */
        .box.box-solid.box-success > .box-header {
          background: linear-gradient(
            135deg, #27ae60 0%, #229954 100%
          );
          color: white;
          font-weight: 600;
        }

        /* Info: Optional/informational content (light blue) */
        .box.box-solid.box-info > .box-header {
          background: linear-gradient(
            135deg, #3498db 0%, #5dade2 100%
          );
          color: white;
          font-weight: 600;
        }

        /* Warning: Attention required (orange) */
        .box.box-solid.box-warning > .box-header {
          background: linear-gradient(
            135deg, #f39c12 0%, #e67e22 100%
          );
          color: white;
          font-weight: 600;
        }

        /* Danger: Errors (red) */
        .box.box-solid.box-danger > .box-header {
          background: linear-gradient(
            135deg, #e74c3c 0%, #c0392b 100%
          );
          color: white;
          font-weight: 600;
        }

        /* Make box titles larger and more prominent */
        .box-header > .box-title {
          font-size: 18px;
          font-weight: 700;
        }

        /* ==========================================================================
           TYPOGRAPHY - Consistent Headings
           ========================================================================== */

        /* Standardize h4 headings (section titles inside boxes) */
        .box-body h4 {
          font-size: 16px;
          font-weight: 700;
          color: #2c3e50;
          margin-top: 0;
          margin-bottom: 12px;
        }

        /* Standardize h5 headings (subsection titles) */
        .box-body h5 {
          font-size: 14px;
          font-weight: 700;
          color: #34495e;
          margin-top: 8px;
          margin-bottom: 8px;
        }

        /* ==========================================================================
           LAYOUT - Spacing & Margins
           ========================================================================== */

        /* Reduce gaps between columns */
        .row {
          margin-left: -10px;
          margin-right: -10px;
        }

        .row > [class*='col-'] {
          padding-left: 10px;
          padding-right: 10px;
        }

        /* Ensure consistent margins for content wrapper */
        .content-wrapper {
          padding: 15px;
        }

        /* Fix box borders to have rounded corners consistently */
        .box {
          border-radius: 8px !important;
          overflow: hidden;
        }

        .box.box-solid {
          border-radius: 8px !important;
        }

        /* ==========================================================================
           BOX ENHANCEMENTS - Hover Effects & Transitions
           ========================================================================== */

        .box {
          transition: box-shadow 0.3s ease, transform 0.2s ease;
          border-radius: 8px;
          overflow: hidden;
        }

        .box:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          transform: translateY(-2px);
        }

        .box-body {
          padding: 16px;
        }

        /* ==========================================================================
           NAVIGATION & HEADER
           ========================================================================== */

        .main-header .navbar {
          background: linear-gradient(
            135deg, #2c3e50 0%, #34495e 100%
          ) !important;
          border-bottom: 3px solid #3498db;
        }

        .skin-blue .main-sidebar {
          background: linear-gradient(
            180deg, #2c3e50 0%, #34495e 100%
          ) !important;
        }

        .skin-blue .sidebar-menu > li.active > a {
          border-left: 4px solid #3498db;
          background-color: rgba(52, 152, 219, 0.1);
        }

        .sidebar-menu > li > a {
          transition: all 0.3s ease;
        }

        .sidebar-menu > li > a:hover {
          background-color: rgba(52, 152, 219, 0.15);
        }

        /* ==========================================================================
           BUTTONS - Consistent Styling & Hover Effects
           ========================================================================== */

        .btn {
          transition: all 0.3s ease;
          border-radius: 6px;
          font-weight: 500;
          border-width: 2px;
        }

        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        }

        .btn:active {
          transform: translateY(0);
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        /* Primary button - Main actions */
        .btn-primary {
          background: linear-gradient(
            135deg, #3498db 0%, #2980b9 100%
          );
          border-color: #2980b9;
        }

        .btn-primary:hover {
          background: linear-gradient(
            135deg, #2980b9 0%, #21618c 100%
          );
          border-color: #21618c;
        }

        /* Success button - Loading/Confirm actions */
        .btn-success {
          background: linear-gradient(
            135deg, #27ae60 0%, #229954 100%
          );
          border-color: #229954;
        }

        .btn-success:hover {
          background: linear-gradient(
            135deg, #229954 0%, #1e8449 100%
          );
          border-color: #1e8449;
        }

        /* Info button - Secondary actions */
        .btn-info {
          background: linear-gradient(
            135deg, #3498db 0%, #5dade2 100%
          );
          border-color: #3498db;
        }

        .btn-info:hover {
          background: linear-gradient(
            135deg, #2980b9 0%, #3498db 100%
          );
        }

        /* ==========================================================================
           MODEL CARDS - Enhanced Design
           ========================================================================== */

        .model-card {
          background: white;
          border-radius: 8px;
          padding: 16px;
          margin: 8px 0;
          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
          border-left: 4px solid #3498db;
          transition: all 0.3s ease;
          position: relative;
        }

        .model-card:hover {
          box-shadow: 0 6px 16px rgba(0,0,0,0.15);
          transform: translateX(4px);
          border-left-width: 6px;
        }

        .model-card::before {
          content: '';
          position: absolute;
          top: 0;
          right: 0;
          width: 0;
          height: 100%;
          background: linear-gradient(
            90deg, transparent, rgba(52, 152, 219, 0.05)
          );
          transition: width 0.3s ease;
        }

        .model-card:hover::before {
          width: 100%;
        }

        /* ==========================================================================
           METRIC HIGHLIGHTS & INFO BOXES
           ========================================================================== */

        .metric-highlight {
          background: linear-gradient(
            135deg, #e8f5e8 0%, #d5f4e6 100%
          );
          border-radius: 8px;
          padding: 12px;
          border-left: 4px solid #27ae60;
          transition: all 0.3s ease;
        }

        .metric-highlight:hover {
          box-shadow: 0 4px 12px rgba(39, 174, 96, 0.2);
          transform: translateY(-2px);
        }

        /* Info boxes for interpretations */
        div[style*='#d1ecf1'] {
          border-radius: 8px;
          border-left: 4px solid #3498db;
          transition: all 0.3s ease;
        }

        div[style*='#fff3cd'] {
          border-radius: 8px;
          border-left: 4px solid #f39c12;
          transition: all 0.3s ease;
        }

        /* ==========================================================================
           DATA TABLES - Enhanced Styling
           ========================================================================== */

        .dataTables_wrapper {
          border-radius: 8px;
          overflow: hidden;
        }

        table.dataTable {
          border-radius: 8px;
        }

        table.dataTable thead th {
          background: linear-gradient(
            135deg, #34495e 0%, #2c3e50 100%
          );
          color: white;
          font-weight: 600;
          border-bottom: none;
        }

        table.dataTable tbody tr {
          transition: background-color 0.2s ease;
        }

        table.dataTable tbody tr:hover {
          background-color: rgba(52, 152, 219, 0.05);
        }

        table.dataTable tbody tr:nth-child(even) {
          background-color: #f8f9fa;
        }

        /* ==========================================================================
           FORMS & INPUTS - Consistent Styling
           ========================================================================== */

        .form-control {
          border-radius: 6px;
          border: 2px solid #ecf0f1;
          transition: all 0.3s ease;
        }

        .form-control:focus {
          border-color: #3498db;
          box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
        }

        .radio label, .checkbox label {
          transition: color 0.2s ease;
        }

        .radio label:hover, .checkbox label:hover {
          color: #3498db;
        }

        /* ==========================================================================
           PROGRESS INDICATORS & LOADING
           ========================================================================== */

        .progress {
          border-radius: 8px;
          overflow: hidden;
        }

        .progress-bar {
          transition: width 0.6s ease;
        }

        /* ==========================================================================
           EMPTY STATES & ICONS
           ========================================================================== */

        .fa-3x {
          transition: transform 0.3s ease, color 0.3s ease;
        }

        .fa-3x:hover {
          transform: scale(1.1);
        }

        /* ==========================================================================
           UTILITY CLASSES
           ========================================================================== */

        .text-secondary {
          color: #7f8c8d !important;
        }

        .text-muted {
          color: #95a5a6 !important;
        }

        /* Spacing utilities */
        .mb-8 { margin-bottom: 8px !important; }
        .mt-8 { margin-top: 8px !important; }
        .p-16 { padding: 16px !important; }

        /* ==========================================================================
           ANIMATIONS
           ========================================================================== */

        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }

        .box, .model-card {
          animation: fadeIn 0.4s ease;
        }

        /* ==========================================================================
           RESPONSIVE DESIGN TWEAKS
           ========================================================================== */

        @media (max-width: 768px) {
          .box:hover {
            transform: none;
          }

          .btn:hover {
            transform: none;
          }
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
}

# Run the application
shinyApp(ui = ui, server = server)
