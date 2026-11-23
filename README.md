# Do tokenized stocks track their underlying shares?

Tracking error and limits to arbitrage in xStocks on Kraken (14 tokens,
Aug 2025 to 9 Oct 2026). All analysis in R. Rules and thresholds were logged in
`prereg_log.md` before the results they govern; post-hoc changes are marked there.

## Data sources (both free to obtain)

| Data | Source | How obtained | File |
|---|---|---|---|
| Token trades (every trade, 14 xStocks + BTC checkpoints) | Kraken public REST API, `GET /0/public/Trades` | No account or key needed | `kraken_data/*.csv` (raw, never edited), `btc_data/btc_checkpoints.csv` |
| Stock 1-minute OHLCV bars (14 underlying tickers) | Databento | Downloaded with Databento's **free-trial credits** (no payment) | `ohlcv1m_14tickers.csv` |

Bar prices are not official closing or opening auction prints (a stated limitation).

**Data are not included in this repository.** Kraken trades can be re-downloaded
by running `00_kraken_download.R` (about an hour) and `21_btc_checkpoints.R`.
The Databento bars (`ohlcv1m_14tickers.csv`: OHLCV-1m, the 14 tickers,
2025-08-12 to 2026-10-09+) must be requested from Databento with your own account.
Minute-level files derived from them (`output/*.rds`) are also excluded.

## Run order

From the project folder (the RStudio project sets the working directory):

| Script | What it does | Writes |
|---|---|---|
| `00_kraken_download.R` | Long network job, **already done** (all `.done` files exist) | `kraken_data/` |
| `01_clean_stock.R` | Parse Databento bars, convert to New York time | `clean/databento_1m_ny.rds` |
| `02_clean_kraken.R` | Combine token trades, drop page-boundary duplicates | `clean/kraken_trades.rds` |
| `03_explore_coverage.R` | Exploratory design checks (needs internet for the Yahoo comparison) | console only |
| `04_data_quality.R` | Data appendix tables | `output/dq_*.csv`, `output/data_quality.txt` |
| `10_basis_mid.R` | Intraday gap: side-specific vs midpoint, half-spread, no-arbitrage band, persistence, figures | `output/mid_*.csv`, `band.csv`, `persistence.csv`, `fig_mid_monthly.png`, `fig_half_spread.png`, `mid_minute.rds` |
| `11_determinants.R` | Token-day panel regression of the absolute midpoint gap | `output/determinants*.{csv,txt,rds}` |
| `20_weekend_panel.R` | One row per token per closed window, with checkpoint prices | `output/weekend_panel.rds` |
| `21_btc_checkpoints.R` | Small network job (~125 calls), already done | `btc_data/btc_checkpoints.csv` |
| `22_weekend_tracker.R` | Main weekend regressions and scatter figure | `output/weekend_*.csv`, `weekend_regressions.txt`, `fig_weekend_scatter.png` |
| `23_weekend_robust.R` | Same-sample check, promised robustness, forecast benchmark, leave-one-out | `output/weekend_robust.{csv,txt}` |

R packages: data.table, ggplot2, scales, ragg, sandwich, lmtest, jsonlite,
quantmod (03 only).
