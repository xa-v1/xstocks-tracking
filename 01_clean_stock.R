# 01_clean_stock.R
# Cleans the Databento 1-minute stock bars (ohlcv1m_14tickers.csv).
# Source: Databento, obtained with free-trial credits (see DATA.md).
#
# Plain-English version:
#  1. Read the CSV as text so nothing gets silently mangled, then inspect it.
#  2. Convert prices/volume to numbers, parse the UTC timestamp, and express
#     it in New York time (daylight saving handled by the zone name).
#  3. Save clean/databento_1m_ny.rds and run quick checks: time range, which
#     bar holds the closing and opening auction volume, weekday coverage.
#
# Input:  ohlcv1m_14tickers.csv   Output: clean/databento_1m_ny.rds
# (Split out of the former Untitled.R on 2026-11-23; code unchanged.)

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
