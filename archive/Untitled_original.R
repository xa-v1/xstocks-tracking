library(data.table)

# fread is a fast CSV reader. Reading everything as text first means R
# can't silently mangle timestamps or tickers.
px <- fread("ohlcv1m_14tickers.csv", colClasses = "character")

cat("Rows:", nrow(px), "\n")
cat("Columns:", paste(names(px), collapse = ", "), "\n\n")
print(head(px, 4))

# Rows per ticker (should be roughly similar across the liquid names)
print(px[, .N, by = symbol][order(-N)])

# Earliest and latest timestamp (the column is probably ts_event, in UTC)
tcol <- intersect(c("ts_event", "ts_recv"), names(px))[1]
cat("\nTime column used:", tcol, "\n")
print(range(px[[tcol]]))

library(data.table)
dir.create("clean", showWarnings = FALSE)

# The CSV was read as text. Convert the columns we need to numbers.
px[, c("open","high","low","close","volume") :=
     lapply(.SD, as.numeric), .SDcols = c("open","high","low","close","volume")]

# Parse the UTC timestamp, then express it in New York time.
# R handles daylight saving automatically when given the zone name.
px[, ts := as.POSIXct(substr(ts_event, 1, 19),
                      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]
ny <- as.POSIXlt(px$ts, tz = "America/New_York")
px[, date := as.Date(format(px$ts, "%Y-%m-%d", tz = "America/New_York"))]
px[, mod  := ny$hour * 60 + ny$min]   # minute of day: 570 = 9:30, 960 = 16:00

saveRDS(px[, .(ts, date, mod, symbol, open, high, low, close, volume)],
        "clean/databento_1m_ny.rds")

# Check 1: time range in New York minutes (240 = 4:00am, 1199 = 7:59pm)
print(range(px$mod))

# Check 2: which bar holds the closing-auction volume spike? (SPY)
spy <- px[symbol == "SPY" & mod %in% 955:962,
          .(avg_volume = mean(volume)), by = mod][order(mod)]
print(spy)

# Check 3: same test at the open (9:30 = 570)
print(px[symbol == "SPY" & mod %in% 567:573,
         .(avg_volume = mean(volume)), by = mod][order(mod)])

# Check 4: number of distinct dates, and any weekend dates
cat("Distinct dates:", uniqueN(px$date), "\n")
print(table(weekdays(unique(px$date))))

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


###################

library(data.table)
options(width = 200)

files <- list.files("kraken_data", pattern = "xUSD\\.csv$", full.names = TRUE)

# Files have no guaranteed header, so read as text with our own names and
# drop any row that is really a header.
read_one <- function(f) {
  x <- fread(f, header = FALSE, colClasses = "character",
             col.names = c("price","vol","time","side","type","id"))
  x <- x[price != "price"]
  x[, symbol := sub("xUSD\\.csv$", "", basename(f))]
  x
}
tr <- rbindlist(lapply(files, read_one))
tr[, `:=`(price = as.numeric(price), vol = as.numeric(vol),
          t = as.numeric(time), idn = as.numeric(id))]

raw_n <- tr[, .(raw_rows = .N), by = symbol]
tr <- unique(tr, by = c("symbol", "id"))      # drop page-boundary duplicates

# Coverage summary per token
summ <- tr[, .(
  rows         = .N,
  first_utc    = as.POSIXct(min(t), origin = "1970-01-01", tz = "UTC"),
  last_utc     = as.POSIXct(max(t), origin = "1970-01-01", tz = "UTC"),
  missing_ids  = (max(idn) - min(idn) + 1) - .N,   # 0 means no skipped trades
  max_gap_hrs  = round(max(diff(sort(t))) / 3600, 1)
), by = symbol]
summ <- merge(raw_n, summ, by = "symbol")
summ[, dupes_dropped := raw_rows - rows]
print(summ[order(rows)])

# Trades per day by month, so we can see when each token became active
tr[, month := format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), "%Y-%m")]
per_month <- tr[, .N, by = .(symbol, month)]
per_month[, per_day := round(N / 30)]
print(dcast(per_month, symbol ~ month, value.var = "per_day", fill = 0))

dir.create("clean", showWarnings = FALSE)
saveRDS(tr[, .(symbol, t, price, vol, side, type, idn)], "clean/kraken_trades.rds")

###################

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
            symbol ~ month, value.var = "s5"))