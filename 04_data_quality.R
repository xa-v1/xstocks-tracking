# 04_data_quality.R
# Data appendix tables: how complete and clean are the two datasets?
#
# Plain-English version:
#  A) Kraken trades, read straight from the raw files: rows downloaded,
#     duplicates at page boundaries, whether trade ids are consecutive (if so,
#     no trade was skipped), whether time ever runs backwards, first and last
#     trade, and the longest stretch with no trades.
#  B) Kraken activity: trades per day, by token and month.
#  C) Databento stock bars: trading days, how many of the 390 regular-session
#     minutes have a bar, and whether the 15:59 closing bar is present.
#
# Inputs:  kraken_data/*.csv (read only), clean/databento_1m_ny.rds
# Outputs: output/dq_kraken.csv, output/dq_kraken_month.csv,
#          output/dq_stock.csv, output/data_quality.txt

library(data.table)

LAST_DATE   <- as.Date("2026-10-09")
EARLY_CLOSE <- as.Date(c("2025-11-28", "2025-12-24"))

# ---- A) Kraken raw files ----
files <- list.files("kraken_data", pattern = "xUSD\\.csv$", full.names = TRUE)
read_one <- function(f) {
  x <- fread(f, header = FALSE, colClasses = "character",
             col.names = c("price", "vol", "time", "side", "type", "id"))
  x <- x[price != "price"]
  x[, symbol := sub("xUSD\\.csv$", "", basename(f))]
  x
}
raw <- rbindlist(lapply(files, read_one))
raw[, `:=`(t = as.numeric(time), idn = as.numeric(id))]

dq_k <- raw[, {
  u <- unique(.SD, by = "id")[order(idn)]
  list(raw_rows      = .N,
       dup_rows      = .N - nrow(u),
       trades        = nrow(u),
       missing_ids   = (max(u$idn) - min(u$idn) + 1) - nrow(u),
       time_backward = sum(diff(u$t) < 0),       # id order vs time order
       first_utc     = format(as.POSIXct(min(u$t), origin = "1970-01-01", tz = "UTC")),
       last_utc      = format(as.POSIXct(max(u$t), origin = "1970-01-01", tz = "UTC")),
       max_gap_hours = round(max(diff(sort(u$t))) / 3600, 1),
       share_buy     = round(mean(u$side == "b"), 3))
}, by = symbol][order(-trades)]
fwrite(dq_k, "output/dq_kraken.csv")

# ---- B) Trades per calendar day, by month ----
u <- unique(raw, by = c("symbol", "id"))
u[, d := as.Date(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))]
u[, month := format(d, "%Y-%m")]
dq_m <- u[, .(trades = .N, days = uniqueN(d)), by = .(symbol, month)]
dq_m[, per_day := round(trades / days)]     # per calendar day with any trade
dq_m <- dcast(dq_m, symbol ~ month, value.var = "per_day", fill = 0)
fwrite(dq_m, "output/dq_kraken_month.csv")

# ---- C) Databento regular-session bars ----
px  <- readRDS("clean/databento_1m_ny.rds")
reg <- px[mod >= 570 & mod <= 959 & date <= LAST_DATE & !(date %in% EARLY_CLOSE)]
dq_s <- reg[, .(bars = .N, has_1559 = any(mod == 959)), by = .(symbol, date)][
  , .(trading_days = .N, first = min(date), last = max(date),
      mean_bars = round(mean(bars)), days_under_380 = sum(bars < 380),
      share_has_1559 = round(mean(has_1559), 3)), by = symbol][order(symbol)]
fwrite(dq_s, "output/dq_stock.csv")

sink("output/data_quality.txt")
cat("04_data_quality.R run", format(Sys.time()), "\n\n== A) Kraken raw trade files ==\n")
print(dq_k)
cat("\n== B) Trades per calendar day (days with any trade), by month ==\n"); print(dq_m)
cat("\n== C) Databento regular-session bars (full days only, to", format(LAST_DATE), ") ==\n")
print(dq_s)
sink()
cat(readLines("output/data_quality.txt"), sep = "\n")
