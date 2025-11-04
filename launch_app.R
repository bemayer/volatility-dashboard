# Launch script for Volatility Modeling Dashboard
# This script attempts to load the app with proper library configuration

# Set working directory
setwd("C:/Users/mayer/Documents/Mémoire/Dashboard")

# Try to load required packages
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "dplyr",
                      "quantmod", "rugarch", "xts", "zoo", "moments",
                      "forecast", "ggplot2", "viridis")

# Check and report package availability
missing_packages <- c()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("ERROR: Missing required packages:\n")
  cat(paste("-", missing_packages, collapse = "\n"), "\n\n")
  cat("Please install them using:\n")
  cat("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n\n", sep = "")
  quit(status = 1)
}

# Launch the app
cat("Starting Volatility Modeling Dashboard...\n")
cat("The app will be available at: http://127.0.0.1:3838\n")
cat("Press Ctrl+C to stop the server\n\n")

shiny::runApp("app.R", port = 3838, host = "127.0.0.1", launch.browser = FALSE)
