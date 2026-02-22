# ============================================================
#  Stock Health Dashboard — Tab-based (lazy rendering)
#  Stack: R + Shiny + quantmod + prophet + plotly
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(quantmod)
library(prophet)
library(dplyr)
library(DT)
library(rmarkdown)

COL_VERDE  <- "#008000"
COL_ROJO   <- "#FF4C61"
COL_AZUL   <- "#2D7DD2"
COL_OSCURO <- "#1A1F2E"
COL_GRIS   <- "#6c757d"

# Reusable layout for all Plotly charts
light_layout <- function(extra = list()) {
  base <- list(
    paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
    font = list(color = "#1C1E21", size = 11),
    xaxis = list(gridcolor = "#e9ecef", title = "", showspikes = FALSE, linecolor = "#dee2e6"),
    yaxis = list(gridcolor = "#e9ecef", linecolor = "#dee2e6"),
    legend = list(orientation = "h", y = -0.25, bgcolor = "rgba(255,255,255,0.8)"),
    hovermode = "x unified",
    margin = list(l = 45, r = 15, t = 25, b = 40)
  )
  modifyList(base, extra)
}

# Vectorized moving average - avoids stats::filter() which creates heavy ts objects
# Uses cumulative sums for O(n) performance instead of O(n*window)
roll_mean_vec <- function(x, n) {
  len <- length(x); out <- rep(NA_real_, len)
  if (len < n) return(out)
  cx <- cumsum(ifelse(is.na(x), 0, x)); cnt <- cumsum(!is.na(x))
  for (i in n:len) {
    if (cnt[i] - (if (i > n) cnt[i - n] else 0L) == n) {
      out[i] <- (cx[i] - (if (i > n) cx[i - n] else 0)) / n
    }
  }
  out
}

# Rolling standard deviation using E[X²] - E[X]²
roll_sd_vec <- function(x, n) {
  m1 <- roll_mean_vec(x, n); m2 <- roll_mean_vec(x^2, n)
  sqrt(pmax(m2 - m1^2, 0))
}

# RSI with Wilder's smoothing - vectorized to avoid row-by-row sapply
rsi_vec <- function(price, n = 14) {
  len <- length(price); out <- rep(NA_real_, len)
  if (len < n + 2) return(out)
  delta <- c(NA_real_, diff(price)); up <- pmax(delta, 0); dn <- pmax(-delta, 0)
  alpha <- 1 / n; seed <- n + 1
  au <- mean(up[2:seed], na.rm = TRUE); ad <- mean(dn[2:seed], na.rm = TRUE)
  for (i in (seed + 1):len) {
    au <- alpha * up[i] + (1 - alpha) * au
    ad <- alpha * dn[i] + (1 - alpha) * ad
    out[i] <- 100 - 100 / (1 + au / (ad + 1e-10))
  }
  out
}

# Timeout wrapper for Yahoo Finance - prevents shinyapps.io from hanging
get_stock_data <- function(ticker, period = "1y") {
  end <- Sys.Date()
  start <- switch(period, "3m"=end-90, "6m"=end-180, "1y"=end-365)
  result <- NULL
  tryCatch({
    # 10-second timeout to avoid cloud deployment hangs
    setTimeLimit(cpu = 10, elapsed = 10, transient = TRUE)
    result <- suppressWarnings(getSymbols(ticker, src="yahoo",
                                          from=start, to=end, auto.assign=FALSE))
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  }, error = function(e) {
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    NULL
  })
  result
}

# Full dataset with all technical indicators
build_df <- function(xts_data, ticker) {
  cl <- as.numeric(Cl(xts_data))
  ma20 <- roll_mean_vec(cl, 20); sd20 <- roll_sd_vec(cl, 20)
  data.frame(
    date = as.Date(index(xts_data)),
    open = as.numeric(Op(xts_data)), high = as.numeric(Hi(xts_data)),
    low = as.numeric(Lo(xts_data)), close = cl,
    volume = as.numeric(Vo(xts_data)), ticker = ticker,
    returns = c(NA_real_, diff(log(cl))),
    ma20 = ma20, ma50 = roll_mean_vec(cl, 50), ma200 = roll_mean_vec(cl, 200),
    rsi = rsi_vec(cl, 14), bb_upper = ma20 + 2 * sd20, bb_lower = ma20 - 2 * sd20,
    stringsAsFactors = FALSE
  )
}

# Lightweight dataset for portfolio comparison - only needs normalized prices
build_df_lite <- function(xts_data, ticker) {
  cl <- as.numeric(Cl(xts_data))
  data.frame(date = as.Date(index(xts_data)), close = cl, ticker = ticker,
             normalized = cl / cl[1] * 100, stringsAsFactors = FALSE)
}

# Automated technical diagnosis based on MA crossovers and RSI
diagnose_stock <- function(df) {
  last <- df[nrow(df), ]; price <- last$close; n <- nrow(df)
  # Trend: bullish if price > MA20 > MA50, bearish if reverse, else sideways
  trend <- if (!is.na(last$ma20) && !is.na(last$ma50)) {
    if (price > last$ma20 && last$ma20 > last$ma50) "bullish"
    else if (price < last$ma20 && last$ma20 < last$ma50) "bearish"
    else "sideways"
  } else "indeterminada"
  rsi_v <- last$rsi
  rsi_s <- if (!is.na(rsi_v)) {
    if (rsi_v > 70) "overbought" else if (rsi_v < 30) "oversold" else "neutral"
  } else "sin data"
  list(
    trend = trend, rsi_status = rsi_s,
    rsi_val = if (!is.na(rsi_v)) round(rsi_v, 1) else NA_real_,
    # 1-month = 21 trading days, 3-month = 63 trading days
    ret_1m = if (n >= 21) round((last$close/df$close[n-20]-1)*100, 2) else NA_real_,
    ret_3m = if (n >= 63) round((last$close/df$close[n-62]-1)*100, 2) else NA_real_,
    price = round(price, 2),
    vol_avg = round(mean(tail(df$volume, 20), na.rm=TRUE)/1e6, 2)
  )
}

# Prophet forecast - memory optimized for shinyapps.io
run_prophet <- function(df, horizon = 30) {
  # Only use last 180 days to reduce memory and training time
  train <- tail(df[!is.na(df$close), c("date","close")], 180)
  names(train) <- c("ds", "y")
  m <- prophet(train, daily.seasonality=FALSE, yearly.seasonality=FALSE,
               weekly.seasonality=TRUE, changepoint.prior.scale=0.05,
               # Reduced from default 1000 to save memory
               uncertainty.samples=50)
  fc <- predict(m, make_future_dataframe(m, periods=horizon))
  fc[, c("ds", "yhat", "yhat_lower", "yhat_upper")]
}

# ══════════════════════════════════════════════════════════════
# UI WITH TABS — prevents rendering all plots simultaneously
# ══════════════════════════════════════════════════════════════
ui <- page_navbar(
  title = "Stock Health Dashboard — by Alejandro Castro",
  theme = bs_theme(bootswatch="flatly", primary=COL_VERDE, 
                   base_font=font_google("Inter")),
  fillable = FALSE,
  
  sidebar = sidebar(
    width = 270, bg = "#f8f9fa", open = "always",
    
    h6("Search Stock", class="mb-1"),
    textInput("ticker", NULL, value="AAPL", placeholder="e.g., AAPL, TSLA"),
    selectInput("period", NULL, choices=c("3 months"="3m","6 months"="6m","1 year"="1y"),
                selected="6m"),
    actionButton(
      "fetch_btn",
      tagList(icon("database"), " Load Data"),
      class = "btn-success w-100"
    ),
    hr(class = "my-3")
    ,
    
    h6("Prophet Forecast", class="mb-1"),
    sliderInput("forecast_days", NULL, min=7, max=30, value=14, step=7),
    actionButton(
      "forecast_btn",
      tagList(icon("chart-line"), " Predict"),
      class = "btn-success w-100"
    ),
    hr(style="border-color:#dee2e6; margin:10px 0;"),
    
    h6("Portfolio Comparison", class="mb-1"),
    textInput("tickers_compare", NULL, value="AAPL,MSFT"),
    actionButton("compare_btn", "vs S&P 500", class="btn-warning w-100"),
    hr(style="border-color:#dee2e6; margin:10px 0;"),
    
    downloadButton("download_pdf", "⬇ Download Report", class="btn-info w-100")
    
  ),
  
  # Tab structure: only renders visible tab's plots (memory efficient)
  nav_panel(
    "Dashboard",
    uiOutput("kpi_cards"),
    br(),
    uiOutput("diagnostico_card"),
    br(),
    card(card_header("Price & Technical Indicators"), full_screen=TRUE,
         plotlyOutput("plot_precio", height="380px")),
    br(),
    fluidRow(
      column(6, card(card_header("Volume"),
                     plotlyOutput("plot_volumen", height="200px"))),
      column(6, card(card_header("RSI (14d)"),
                     plotlyOutput("plot_rsi", height="200px")))
    )
  ),
  
  nav_panel(
    "Forecast",
    uiOutput("forecast_panel")
  ),
  
  nav_panel(
    "Comparison",
    uiOutput("compare_panel")
  ),
  
  nav_panel(
    "Data",
    card(card_header("Historical Data"), full_screen=TRUE,
         DTOutput("tabla_data"))
  )
)

# ══════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # Reactive storage - all state in one object for clean management
  rv <- reactiveValues(df=NULL, diag=NULL, forecast=NULL, compare=NULL)
  
  # Load stock data
  observeEvent(input$fetch_btn, {
    ticker <- toupper(trimws(input$ticker))
    req(nchar(ticker) >= 1)
    
    withProgress(message=paste("Loading", ticker, "..."), {
      setProgress(0.2, detail="Yahoo Finance")
      raw <- get_stock_data(ticker, input$period)
      
      if (is.null(raw) || nrow(raw) < 30) {
        showNotification(paste("No data:", ticker), type="error", duration=5)
        return()
      }
      setProgress(0.75, detail="Indicators")
      rv$df <- build_df(raw, ticker)
      rv$diag <- diagnose_stock(rv$df)
      # Reset dependent data when loading new stock
      rv$forecast <- NULL
      rv$compare <- NULL
      # Explicit garbage collection for shinyapps.io memory management
      rm(raw); gc(verbose=FALSE)
      showNotification(paste0(ticker, " — ", nrow(rv$df), " days"),
                       type="message", duration=3)
    })
  })
  
  # KPI cards - dynamically colored based on performance
  output$kpi_cards <- renderUI({
    req(rv$diag); d <- rv$diag
    kpi <- function(label, val, col) {
      div(class="col-md-3 col-6",
          div(style=paste0("background:#f8f9fa;border:1px solid #dee2e6;",
                           "border-left:4px solid ",col,";border-radius:6px;",
                           "padding:10px 12px;text-align:center;"),
              h3(style=paste0("color:",col,";margin:0;font-weight:700;"), val),
              p(style="color:#6c757d;font-size:.78rem;margin:2px 0 0;", label)
          )
      )
    }
    # Returns: green if positive, red if negative
    rc <- function(v) if (!is.na(v) && v >= 0) COL_VERDE else COL_ROJO
    fluidRow(
      kpi("Price", paste0("$", d$price), COL_AZUL),
      kpi("1 month", if(is.na(d$ret_1m)) "N/A" else paste0(d$ret_1m,"%"), rc(d$ret_1m)),
      kpi("3 months", if(is.na(d$ret_3m)) "N/A" else paste0(d$ret_3m,"%"), rc(d$ret_3m)),
      kpi("Vol. 20d", paste0(d$vol_avg,"M"), COL_GRIS)
    )
  })
  
  # Diagnosis card - color changes based on bullish/bearish signal
  output$diagnostico_card <- renderUI({
    req(rv$diag); d <- rv$diag
    sc <- if (grepl("alcista", d$trend) && !grepl("sobrecomprado", d$rsi_status))
      COL_VERDE else if (grepl("bajista", d$trend)) COL_ROJO else "#FFD700"
    stxt <- if (grepl("alcista", d$trend) && !grepl("sobrecomprado", d$rsi_status))
      "Favorable" else if (grepl("bajista", d$trend)) "Caution" else "Neutral"
    div(style="background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;padding:12px 16px;",
        h6("Diagnosis", style="color:#2c3e50;margin:0 0 8px;font-weight:600;"),
        fluidRow(
          column(4, p(style="color:#6c757d;font-size:.78rem;margin:0;","Trend"),
                 h6(style="color:#2c3e50;margin:2px 0 0;", d$trend)),
          column(4, p(style="color:#6c757d;font-size:.78rem;margin:0;","RSI"),
                 h6(style="color:#2c3e50;margin:2px 0 0;",
                    if(is.na(d$rsi_val)) "N/A" else paste0(d$rsi_val," — ",d$rsi_status))),
          column(4, p(style="color:#6c757d;font-size:.78rem;margin:0;","Signal"),
                 h6(style=paste0("color:",sc,";margin:2px 0 0;"), stxt))
        )
    )
  })
  
  # Price plot - candlestick for ≤180 days, line for longer periods (memory optimization)
  output$plot_precio <- renderPlotly({
    req(rv$df); df <- rv$df
    fig <- if (nrow(df) <= 180) {
      plot_ly(df, x=~date, type="candlestick", open=~open, high=~high, low=~low, close=~close,
              increasing=list(line=list(color=COL_VERDE),fillcolor = COL_VERDE),
              decreasing=list(line=list(color=COL_ROJO),fillcolor = COL_ROJO))
    } else {
      plot_ly(df, x=~date, y=~close, type="scatter", mode="lines",
              line=list(color=COL_VERDE, width=1.5))
    }
    df_ma <- df[!is.na(df$ma20),]; df_ma50 <- df[!is.na(df$ma50),]
    df_bb <- df[!is.na(df$bb_upper),]
    fig %>%
      add_lines(data=df_ma, x=~date, y=~ma20, inherit=FALSE,
                name="MA 20", line=list(color="#FFD700", width=1.2)) %>%
      add_lines(data=df_ma50, x=~date, y=~ma50, inherit=FALSE,
                name="MA 50", line=list(color=COL_AZUL, width=1.2)) %>%
      add_lines(data=df_bb, x=~date, y=~bb_upper, inherit=FALSE,
                name="BB+", line=list(color="#9B59B6", width=1, dash="dot")) %>%
      add_lines(data=df_bb, x=~date, y=~bb_lower, inherit=FALSE,
                name="BB−", line=list(color="#9B59B6", width=1, dash="dot")) %>%
      layout(light_layout(list(yaxis=list(title="Precio (USD)"),
                               rangeslider=list(visible=FALSE))))
  })
  # Critical: suspendWhenHidden prevents rendering plots in inactive tabs
  outputOptions(output, "plot_precio", suspendWhenHidden=TRUE)
  
  output$plot_volumen <- renderPlotly({
    req(rv$df); df <- rv$df
    # Color bars based on up/down days
    col <- ifelse(df$close >= df$open, COL_VERDE, COL_ROJO)
    plot_ly(df, x=~date, y=~volume, type="bar", marker=list(color=col)) %>%
      layout(light_layout(list(yaxis=list(title="Volumen"), showlegend=FALSE)))
  })
  outputOptions(output, "plot_volumen", suspendWhenHidden=TRUE)
  
  output$plot_rsi <- renderPlotly({
    req(rv$df); df <- rv$df[!is.na(rv$df$rsi),]; dr <- range(df$date)
    plot_ly(df, x=~date, y=~rsi, type="scatter", mode="lines",
            line=list(color=COL_AZUL, width=1.5)) %>%
      add_lines(x=dr, y=c(70,70), inherit=FALSE,
                line=list(color=COL_ROJO, dash="dash", width=1)) %>%
      add_lines(x=dr, y=c(30,30), inherit=FALSE,
                line=list(color=COL_VERDE, dash="dash", width=1)) %>%
      layout(light_layout(list(yaxis=list(title="RSI", range=c(0,100)), showlegend=FALSE)))
  })
  outputOptions(output, "plot_rsi", suspendWhenHidden=TRUE)
  
  # Prophet forecast
  observeEvent(input$forecast_btn, {
    req(rv$df)
    withProgress(message="Prophet...", {
      setProgress(0.15)
      fc <- tryCatch(run_prophet(rv$df, input$forecast_days),
                     error=function(e) {showNotification(paste("Error:", e$message),
                                                         type="error"); NULL})
      if (!is.null(fc)) {
        rv$forecast <- fc; gc(verbose=FALSE)
        showNotification("~ Forecast ready", type="message", duration=3)
      }
    })
  })
  
  output$forecast_panel <- renderUI({
    req(rv$forecast)
    card(card_header("Prophet Forecast"), full_screen=TRUE,
         plotlyOutput("plot_forecast", height="350px"),
         p(class="text-muted small p-2",
           "Warning:For educational purposes only. Not financial advice."))
  })
  
  output$plot_forecast <- renderPlotly({
    req(rv$forecast, rv$df)
    fc <- rv$forecast; df <- rv$df; maxd <- max(df$date)
    # Only plot future predictions (after historical data)
    fut <- fc[as.Date(fc$ds) > maxd,]
    plot_ly() %>%
      add_ribbons(data=fut, x=~as.Date(ds), ymin=~yhat_lower, ymax=~yhat_upper,
                  fillcolor="rgba(45,125,210,0.15)", line=list(color="transparent"),
                  name="Intervalo") %>%
      add_lines(data=df, x=~date, y=~close, inherit=FALSE,
                name="Real", line=list(color=COL_VERDE, width=1.5)) %>%
      add_lines(data=fut, x=~as.Date(ds), y=~yhat, inherit=FALSE,
                name="Predicción", line=list(color=COL_AZUL, width=2)) %>%
      layout(light_layout(list(yaxis=list(title="Precio (USD)"))))
  })
  outputOptions(output, "plot_forecast", suspendWhenHidden=TRUE)
  
  # Portfolio comparison
  observeEvent(input$compare_btn, {
    tickers <- toupper(trimws(strsplit(input$tickers_compare,",")[[1]]))
    tickers <- tickers[nchar(tickers) > 0]
    req(length(tickers) > 0)
    # Limit to 2 user tickers + S&P 500 to avoid memory issues
    tickers <- unique(c(head(tickers, 2), "^GSPC"))
    
    withProgress(message="Portfolio...", {
      res <- list()
      for (i in seq_along(tickers)) {
        t <- tickers[i]
        setProgress(i/length(tickers), detail=t)
        raw <- tryCatch(get_stock_data(t,"1y"), error=function(e) NULL)
        if (!is.null(raw) && nrow(raw) > 10) res[[t]] <- build_df_lite(raw, t)
      }
      if (length(res)) {
        rv$compare <- res; gc(verbose=FALSE)
        showNotification("~ Comparison ready", type="message", duration=3)
      } else {
        showNotification("Could not load", type="error")
      }
    })
  })
  
  output$compare_panel <- renderUI({
    req(rv$compare)
    card(card_header("Ticker Performance vs S&P 500"), full_screen=TRUE,
         plotlyOutput("plot_compare", height="350px"))
  })
  
  output$plot_compare <- renderPlotly({
    req(rv$compare)
    cols <- c(COL_VERDE, COL_AZUL, "#FFD700", "#FF4C61", "#9B59B6")
    fig <- plot_ly()
    for (i in seq_along(rv$compare)) {
      t <- names(rv$compare)[i]; d <- rv$compare[[t]]; esp <- t == "^GSPC"
      fig <- fig %>% add_lines(data=d, x=~date, y=~normalized,
                               name=if(esp) "S&P 500" else t,
                               line=list(color=cols[((i-1) %% length(cols)) + 1],
                                         width=if(esp) 2.5 else 1.5,
                                         dash=if(esp) "dash" else "solid"))
    }
    fig %>% layout(light_layout(list(yaxis=list(title="Retorno normalizado (base 100)"))))
  })
  outputOptions(output, "plot_compare", suspendWhenHidden=TRUE)
  
  # Data table
  output$tabla_data <- renderDT({
    req(rv$df)
    rv$df %>%
      select(Fecha=date, Apertura=open, Máximo=high, Mínimo=low, Cierre=close, Volumen=volume) %>%
      mutate(across(where(is.numeric), ~round(.x,2))) %>%
      arrange(desc(Fecha)) %>%
      datatable(rownames=FALSE, options=list(pageLength=10, scrollX=TRUE, dom="ftip"),
                class="stripe hover")
  })
  outputOptions(output, "tabla_data", suspendWhenHidden=TRUE)
  
  # Report download (HTML, not PDF - works on shinyapps.io without LaTeX)
  output$download_pdf <- downloadHandler(
    filename = function() paste0("stock_report_", toupper(input$ticker), "_",
                                 format(Sys.Date(),"%Y%m%d"), ".html"),
    content = function(file) {
      req(rv$df, rv$diag)
      tmp <- tempfile(fileext=".Rmd")
      file.copy("report_template.Rmd", tmp, overwrite=TRUE)
      withProgress(message="Generating report...", {
        tryCatch(
          rmarkdown::render(tmp, output_file=file,
                            params=list(ticker=toupper(input$ticker),
                                        df=rv$df, diag=rv$diag, fc_data=rv$forecast),
                            envir=new.env(parent=globalenv()), quiet=TRUE),
          error=function(e) showNotification(
            paste("Error generating report:", e$message),
            type="error", duration=8)
        )
      })
    }
  )
}

shinyApp(ui, server)
