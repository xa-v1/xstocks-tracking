# 03_explore_coverage.R
# EXPLORATORY checks used to design the analysis (not results):
#  A) Stock bars: how many of the 390 regular minutes have a bar, whether the
#     15:59 bar exists, and how the last bar compares to Yahoo's daily close
#     (needs internet: quantmod::getSymbols).
#  B) Token trades: when do tokens trade (weekday, ET hour), longest no-trade
#     gaps, and how stale the token's last trade is at each stock minute.
#
# Inputs: clean/databento_1m_ny.rds, clean/kraken_trades.rds
# (Split out of the former Untitled.R on 2026-11-23; code unchanged.)

library(data.table); library(quantmod)
px <- readRDS("clean/databento_1m_ny.rds")

# Regular session = minute 570 (9:30) through 959 (the 15:59 bar)
reg <- px[mod >= 570 & mod <= 959]

# 1) Bar density: how many of the 390 regular-session minutes have a bar
dens <- reg[, .(bars = .N), by = .(symbol, date)][
  , .(avg_bars = round(mean(bars)), min_bars = min(bars),
      days_under_300 = sum(bars < 300)), by = symbol]
print(dens[order(avg_bars)])

# 2) Last regular-session bar per ticker-day, and how stale it is
last_bar <- reg[order(ts), .SD[.N], by = .(symbol, date)]
print(last_bar[, .(share_last_bar_is_959 = round(mean(mod == 959), 3)),
               by = symbol])

# 3) Compare to Yahoo's daily close (unadjusted close column)
get_close <- function(s) {
  x <- getSymbols(s, from = "2025-08-12", to = "2026-10-30", auto.assign = FALSE)
  data.table(symbol = s, date = as.Date(index(x)), yahoo_close = as.numeric(Cl(x)))
}
yh <- rbindlist(lapply(unique(px$symbol), get_close))
m  <- merge(last_bar, yh, by = c("symbol", "date"))
m[, diff_pct := 100 * (close / yahoo_close - 1)]
print(m[, .(days = .N,
            median_abs = round(median(abs(diff_pct)), 3),
            p95_abs    = round(quantile(abs(diff_pct), 0.95), 3)),
        by = symbol][order(-p95_abs)])


library(data.table); options(width = 200)
tr <- readRDS("clean/kraken_trades.rds")
px <- readRDS("clean/databento_1m_ny.rds")

# ---- A) When do tokens trade? (New York time) ----
lt <- as.POSIXlt(as.POSIXct(tr$t, origin = "1970-01-01", tz = "UTC"),
                 tz = "America/New_York")
tr[, `:=`(wday = lt$wday, hr = lt$hour)]        # wday: 0 = Sunday ... 6 = Saturday
print(round(prop.table(table(factor(tr$wday, 0:6,
                                    c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")))), 3))
print(round(prop.table(table(tr$hr)), 3))        # share of trades by ET hour
print(tr[, .(weekend_share = round(mean(wday %in% c(0, 6)), 3)), by = symbol])

# Where is each token's longest no-trade gap?
print(tr[order(t), .(gap_hrs = round(max(diff(t)) / 3600),
                     gap_start_ET = as.POSIXct(t[which.max(diff(t))], origin = "1970-01-01",
                                               tz = "America/New_York")), by = symbol])

# ---- B) Coverage: for each regular-session stock minute, how stale
#          is the token's last trade? ----
# Match to the END of each 1-minute bar (bar stamp + 60 s), so we never use
# a token trade that happened after the stock bar closed.
grid <- px[mod >= 570 & mod <= 959 & ts < as.POSIXct("2026-10-10", tz = "UTC"),
           .(symbol, date, tq = as.numeric(ts) + 60)]
first_t <- tr[, .(first_t = min(t)), by = symbol]       # skip minutes before a
grid <- merge(grid, first_t, by = "symbol")[tq >= first_t]  # token even existed

trk <- tr[, .(symbol, tq = t, t_trade = t)]
setkey(trk, symbol, tq)
cov <- trk[grid, on = .(symbol, tq), roll = TRUE]       # last trade at or before tq
cov[, stale_min := (tq - t_trade) / 60]
cov[is.na(stale_min), stale_min := Inf]                 # no earlier trade at all
cov[, month := format(date, "%Y-%m")]

print(cov[, .(share_le_1  = round(mean(stale_min <= 1), 3),
              share_le_5  = round(mean(stale_min <= 5), 3),
              share_le_15 = round(mean(stale_min <= 15), 3),
              median_stale_min = round(median(stale_min), 1)),
          by = symbol][order(share_le_5)])

print(dcast(cov[, .(s5 = round(mean(stale_min <= 5), 2)), by = .(symbol, month)],
