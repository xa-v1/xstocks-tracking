# basis_minute.R
# Preliminary intraday basis: xStock token price vs the underlying stock,
# minute by minute, during regular US trading hours.
#
# RULES FIXED BEFORE LOOKING AT RESULTS (written 2026-11-10):
#  - Session: regular hours only, 09:30-15:59 ET bars (mod 570-959).
#  - Early-close days dropped (13:00 close; later bars are after-hours):
#    2025-11-28, 2025-12-24, 2026-07-02.  [early-close dates from memory,
#    unverified against the NYSE calendar]
#  - Matching: for each stock 1-minute bar, take the LAST token trade inside
#    that same minute. Compare it to the bar's close. So the two prices are at
#    most 60 seconds apart, and only minutes where the token actually traded
#    are used (no stale prices).
#  - Basis (%) = 100 * (token price / stock price - 1).
#  - Sample ends 2026-10-09 (last full day of Kraken data).
#  - Thresholds shown on the figure (0.5% and 1%) are ILLUSTRATIVE round
#    numbers, not estimated trading costs. Kraken xStocks fees are unverified.
#
# Inputs:  clean/kraken_trades.rds, clean/databento_1m_ny.rds
# Outputs: output/basis_minute.rds, output/basis_summary.csv

library(data.table)
dir.create("output", showWarnings = FALSE)

tr <- readRDS("clean/kraken_trades.rds")
px <- readRDS("clean/databento_1m_ny.rds")

early_close <- as.Date(c("2025-11-28", "2025-12-24", "2026-07-02"))

# Stock side: regular-session bars only.
stk <- px[mod >= 570 & mod <= 959 & !(date %in% early_close) &
          date <= as.Date("2026-10-09"),
          .(symbol, bar = as.numeric(ts), stock = close)]

# Token side: floor each trade time to its minute; keep the last trade
# in each minute (trades sorted by time, then take the final row).
tr <- tr[order(symbol, t)]
tr[, bar := floor(t / 60) * 60]
tok <- tr[, .(token = price[.N], n_trades = .N), by = .(symbol, bar)]

# Join: one row per minute where both the stock bar and a token trade exist.
b <- merge(stk, tok, by = c("symbol", "bar"))
b[, ts := as.POSIXct(bar, origin = "1970-01-01", tz = "UTC")]
b[, date := as.Date(format(ts, "%Y-%m-%d", tz = "America/New_York"))]
b[, basis := 100 * (token / stock - 1)]

saveRDS(b, "output/basis_minute.rds")

summ <- b[, .(minutes   = .N,
              days      = uniqueN(date),
              median    = median(basis),
              med_abs   = median(abs(basis)),
              p95_abs   = quantile(abs(basis), 0.95),
              p99_abs   = quantile(abs(basis), 0.99),
              share_gt_0.5 = mean(abs(basis) > 0.5),
              share_gt_1   = mean(abs(basis) > 1)),
          by = symbol][order(med_abs)]
fwrite(summ, "output/basis_summary.csv")
print(summ, digits = 3)
