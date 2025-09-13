# 🚀 Deployment Guide

This document provides comprehensive instructions for deploying the Volatility Modeling Dashboard in various environments.

## 📋 Table of Contents

1. [Prerequisites](#prerequisites)
2. [Local Development](#local-development)
3. [Shiny Server Deployment](#shiny-server-deployment)
4. [Docker Deployment](#docker-deployment)
5. [Cloud Deployment](#cloud-deployment)
6. [GitHub Pages (Static Demo)](#github-pages-static-demo)
7. [Troubleshooting](#troubleshooting)

## 🛠️ Prerequisites

### System Requirements

- **R Version**: 4.0.0 or higher
- **RAM**: Minimum 4GB, recommended 8GB+
- **Storage**: 2GB free space
- **Network**: Internet access for Yahoo Finance API

### Required R Packages

```r
# Core Shiny packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly"
))

# Financial data and modeling
install.packages(c(
  "quantmod", "rugarch", "xts", "zoo", "tseries", "FinTS"
))

# Statistics and visualization
install.packages(c(
  "dplyr", "ggplot2", "viridis", "moments", "nortest", "forecast"
))

# Optional: Machine Learning (requires TensorFlow)
install.packages(c("keras", "tensorflow"))
```

## 💻 Local Development

### Quick Start

```bash
# Clone the repository
git clone https://github.com/your-username/volatility-dashboard.git
cd volatility-dashboard

# Launch in R/RStudio
R -e "shiny::runApp('app.R', port = 3838)"
```

### RStudio Integration

1. Open `volatility-dashboard.Rproj` in RStudio
2. Install dependencies: `renv::restore()` (if using renv)
3. Click **Run App** button or use `Ctrl+Shift+Enter`

### Development Configuration

```r
# .Rprofile for development
options(
  shiny.port = 3838,
  shiny.host = "0.0.0.0",
  shiny.autoreload = TRUE,
  warn = 1
)
```

## 🖥️ Shiny Server Deployment

### Installation on Ubuntu/Debian

```bash
# Install R
sudo apt-get update
sudo apt-get install r-base r-base-dev

# Install Shiny Server
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.18.987-amd64.deb
sudo gdebi shiny-server-1.5.18.987-amd64.deb
```

### App Configuration

```bash
# Copy app to Shiny Server directory
sudo cp -R volatility-dashboard /srv/shiny-server/

# Set permissions
sudo chown -R shiny:shiny /srv/shiny-server/volatility-dashboard
sudo chmod -R 755 /srv/shiny-server/volatility-dashboard
```

### Shiny Server Configuration (`/etc/shiny-server/shiny-server.conf`)

```bash
# Define a server
server {
  listen 3838;
  
  # Define a location at the base URL
  location / {
    site_dir /srv/shiny-server;
    log_dir /var/log/shiny-server;
    
    # Host the directory of Shiny Apps stored in this directory
    directory_index on;
  }
  
  # Volatility Dashboard specific configuration
  location /volatility-dashboard {
    app_dir /srv/shiny-server/volatility-dashboard;
    log_dir /var/log/shiny-server;
    
    # Increase timeout for model fitting
    app_init_timeout 120;
    app_idle_timeout 300;
  }
}
```

### Service Management

```bash
# Start/stop/restart service
sudo systemctl start shiny-server
sudo systemctl stop shiny-server
sudo systemctl restart shiny-server

# Check status
sudo systemctl status shiny-server

# View logs
sudo tail -f /var/log/shiny-server.log
```

## 🐳 Docker Deployment

### Dockerfile

```dockerfile
FROM rocker/shiny-verse:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', 'shinydashboard', 'DT', 'plotly', \
    'dplyr', 'quantmod', 'rugarch', 'xts', 'zoo', \
    'moments', 'forecast', 'ggplot2', 'viridis', \
    'tseries', 'FinTS', 'nortest' \
))"

# Copy application files
COPY . /srv/shiny-server/volatility-dashboard/
RUN chown -R shiny:shiny /srv/shiny-server/

# Expose port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]
```

### Build and Run

```bash
# Build the image
docker build -t volatility-dashboard .

# Run the container
docker run -d \
  --name volatility-app \
  -p 3838:3838 \
  -v $(pwd)/logs:/var/log/shiny-server \
  volatility-dashboard

# View logs
docker logs -f volatility-app
```

### Docker Compose

```yaml
# docker-compose.yml
version: '3.8'

services:
  volatility-dashboard:
    build: .
    ports:
      - "3838:3838"
    volumes:
      - ./logs:/var/log/shiny-server
      - ./data:/srv/shiny-server/volatility-dashboard/data
    environment:
      - SHINY_LOG_LEVEL=TRACE
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - volatility-dashboard
    restart: unless-stopped
```

## ☁️ Cloud Deployment

### AWS EC2 with Shiny Server

```bash
# Launch EC2 instance (Ubuntu 20.04 LTS)
# Security group: Allow HTTP (80), HTTPS (443), Custom TCP (3838)

# Connect and install
ssh -i your-key.pem ubuntu@your-ec2-instance.com

# Install dependencies
sudo apt-get update
sudo apt-get install r-base r-base-dev nginx

# Install Shiny Server (see above)
# Deploy app (see above)

# Configure nginx reverse proxy
sudo nano /etc/nginx/sites-available/volatility-dashboard
```

### Nginx Configuration

```nginx
server {
    listen 80;
    server_name your-domain.com;
    
    location / {
        proxy_pass http://localhost:3838/volatility-dashboard/;
        proxy_redirect http://localhost:3838/ $scheme://$host/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
        proxy_read_timeout 20d;
        proxy_buffering off;
    }
}
```

### DigitalOcean App Platform

Create `app.yaml`:

```yaml
name: volatility-dashboard
services:
- environment_slug: r
  github:
    branch: main
    deploy_on_push: true
    repo: your-username/volatility-dashboard
  instance_count: 1
  instance_size_slug: basic-xxs
  name: volatility-app
  source_dir: /
  http_port: 3838
  run_command: R -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)"
```

### Heroku Deployment

Create `heroku.yml`:

```yaml
build:
  docker:
    web: Dockerfile.heroku
run:
  web: R -e "shiny::runApp('app.R', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"
```

## 📄 GitHub Pages (Static Demo)

For demonstration purposes only (not interactive):

1. Enable GitHub Pages in repository settings
2. The GitHub Action workflow will automatically deploy
3. Access at: `https://your-username.github.io/volatility-dashboard/`

Note: This creates a static landing page with installation instructions.

## 🔧 Troubleshooting

### Common Issues

#### Package Installation Errors

```r
# If rugarch fails to install
install.packages("rugarch", dependencies = TRUE, type = "binary")

# For Ubuntu/Debian systems
sudo apt-get install libxml2-dev libssl-dev libcurl4-openssl-dev
```

#### Memory Issues

```r
# Increase memory limits
options(java.parameters = "-Xmx4g")
memory.limit(8000)  # Windows only
```

#### Port Conflicts

```bash
# Find processes using port 3838
sudo lsof -i :3838

# Kill process
sudo kill -9 <PID>
```

#### Yahoo Finance API Issues

```r
# Test connectivity
library(quantmod)
getSymbols("AAPL", from = "2023-01-01", to = "2023-12-31")

# Alternative: Use CSV upload feature
```

### Performance Optimization

#### Shiny Server Tuning

```bash
# /etc/shiny-server/shiny-server.conf
preserve_logs true;
sanitize_errors false;
app_init_timeout 120;
app_idle_timeout 300;
```

#### R Configuration

```r
# Increase worker processes
options(
  shiny.maxRequestSize = 50*1024^2,  # 50MB upload limit
  shiny.sanitize.errors = FALSE,
  repos = c(CRAN = "https://cloud.r-project.org/")
)
```

### Monitoring and Logging

#### Application Logs

```bash
# Shiny Server logs
tail -f /var/log/shiny-server/*.log

# Application-specific logs
tail -f /var/log/shiny-server/volatility-dashboard-*.log
```

#### Health Check Endpoint

Add to `app.R`:

```r
# Simple health check
observe({
  if (input$health_check) {
    cat("Health check OK:", Sys.time(), "\n")
  }
})
```

### Security Considerations

1. **Authentication**: Implement user authentication for production
2. **HTTPS**: Always use SSL in production environments
3. **File Uploads**: Validate and sanitize uploaded files
4. **API Limits**: Monitor Yahoo Finance API usage
5. **Resource Limits**: Set appropriate memory and CPU limits

## 📊 Monitoring

### Basic Monitoring

```bash
# System resources
htop
df -h
free -h

# Shiny Server status
sudo systemctl status shiny-server

# Active connections
sudo ss -tulpn | grep :3838
```

### Advanced Monitoring

Consider integrating with:
- **Grafana + Prometheus**: System metrics
- **New Relic**: Application performance
- **Datadog**: Full-stack monitoring

## 🎯 Production Checklist

- [ ] SSL certificate configured
- [ ] Database backups scheduled (if applicable)
- [ ] Monitoring system in place
- [ ] Error tracking configured
- [ ] Performance baselines established
- [ ] Security headers configured
- [ ] Rate limiting implemented
- [ ] Documentation updated
- [ ] User training completed

---

For additional support, please refer to the [main README](README.md) or create an issue in the repository.