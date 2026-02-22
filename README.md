# financial-analysis-dashboard
Real-time stock analysis dashboard with technical indicators.

## Project Description

**Stock Health Dashboard** is a full-stack financial analytics web application that provides real-time technical analysis and price forecasting for publicly traded stocks. The dashboard was built to demonstrate end-to-end data science capabilities, from data acquisition and feature engineering to predictive modeling and deployment.
<img width="1366" height="606" alt="Captura de pantalla (263)" src="https://github.com/user-attachments/assets/81308f00-6f4c-4cc2-9bc9-12d5c98e4d3c" />

[You can try my app here!!](https://alecas.shinyapps.io/Stock_Health_Dashboard/)

### What It Does

The application delivers four core functionalities:

1. **Technical Analysis**: Real-time stock data from Yahoo Finance with automated calculation of key indicators including Moving Averages (MA20, MA50, MA200), Relative Strength Index (RSI), and Bollinger Bands. The system provides automated diagnosis based on MA crossovers and RSI thresholds.

2. **Prophet Forecasting**: Time series prediction using Facebook's Prophet model, optimized for financial data with configurable horizons (7-30 days). The forecast includes 80% confidence intervals and is trained on the most recent 180 days of data for memory efficiency.
<img width="1366" height="598" alt="Captura de pantalla (264)" src="https://github.com/user-attachments/assets/1ce09ad7-87e8-4354-a0eb-7ff3dba5b2f1" />

3. **Portfolio Comparison**: Multi-ticker performance, automatically compared against the S&P 500 index for relative performance evaluation.
<img width="1366" height="587" alt="Captura de pantalla (265)" src="https://github.com/user-attachments/assets/198986d1-635b-4652-bed6-8a994881d2d1" />

4. **Automated Reporting**: Professional HTML reports with dynamic content generation, including technical interpretations, statistical summaries, volatility analysis, and visual charts—all generated programmatically from R Markdown templates.
<img width="1366" height="569" alt="Captura de pantalla (268)" src="https://github.com/user-attachments/assets/c49901c8-5484-4209-8895-4d68a6e4af3e" />



### Why It Was Built

This project was created to showcase skills relevant to entry-level Data Scientist positions:
- **Real-time data processing** and API integration
- **Feature engineering** for financial time series
- **Machine learning deployment** in production environments
- **Technical communication** through automated report generation
- **Performance optimization** for cloud deployment constraints

### Technical Stack

- **Backend**: R + Shiny (reactive programming)
- **Data**: quantmod (Yahoo Finance API), dplyr (data manipulation)
- **Visualization**: plotly (interactive charts), ggplot2 (static reports)
- **ML**: prophet (time series forecasting)
- **Reporting**: rmarkdown + knitr (dynamic document generation)
- **Deployment**: shinyapps.io

### Key Challenges Solved

**1. Memory Management on Cloud Platform**

The initial deployment crashed on shinyapps.io's 1GB RAM limit when loading data. Root cause: Shiny attempted to render all 6+ Plotly charts simultaneously, consuming excessive memory.

**Solution**: Implemented tab-based architecture with `suspendWhenHidden=TRUE`, ensuring only visible charts are rendered. Added explicit garbage collection (`gc()`) after heavy operations. Result: 70% reduction in peak memory usage.

**2. Performance Optimization**

Standard R functions like `stats::filter()` for moving averages created heavy time-series objects and caused row-by-row operations.

**Solution**: Developed vectorized implementations using cumulative sums (`cumsum()`) for O(n) complexity instead of O(n×window). RSI calculation rewritten with Wilder's exponential smoothing in a single loop. Result: 10x faster indicator calculation.

**3. Yahoo Finance Reliability**

Yahoo Finance API has unpredictable response times, causing timeout issues in cloud environments.

**Solution**: Wrapped `getSymbols()` in a 10-second timeout handler with proper cleanup. Limited candlestick rendering to ≤180 days (switches to line charts for longer periods). Result: Consistent load times under 5 seconds.

**4. Report Generation Without LaTeX**

PDF generation via `rmarkdown` requires LaTeX, which isn't available on shinyapps.io's free tier.

**Solution**: Switched output format to self-contained HTML with embedded CSS. Reports are fully portable, can be converted to PDF via browser print function, and work across all deployment environments.


- **Tab-based UI**: Prevents simultaneous rendering bottlenecks (critical for free-tier deployment)

This project demonstrates practical problem-solving for real-world deployment constraints while maintaining production-quality code standards.
