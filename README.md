
# Momentum Strategy 
This repository contains my final submission for the ECOM155 portfolio project. The project uses historical monthly price data for 100 stocks (from January 2000 to November 2024) along with Fama-French factor data to implement momentum-based trading strategies.

## project overview 📉

- **Data Handling:** Load and preprocess the provided datasets (stock prices and Fama-French factors).
- **Signal Generation:** Compute past returns over multiple horizons (3, 6, 9, and 12 months), rank stocks, and form decile portfolios (winners vs. losers).
- **Visualization:** Generate charts for price trends, decile portfolio performance, cumulative returns, and spread dynamics.
- **Performance Evaluation:** Perform Fama-French regressions on monthly returns to evaluate the momentum strategy, reporting metrics like Sharpe Ratio, average return, geometric return, and standard deviation.

## package requirement📌
- zoo
- reshape2
- data.table
- plotly
- ggplot2
