# 02_clean_kraken.R
# Combines the raw Kraken trade files (kraken_data/<PAIR>.csv) into one table.
#
# Plain-English version:
#  1. Read every token file as text with our own column names; drop any
#     stray header rows.
#  2. Convert to numbers, drop duplicate trade ids (page-boundary repeats).
#  3. Print a coverage summary (rows, first/last trade, skipped ids, longest
#     gap) and trades per day by month.
#  4. Save clean/kraken_trades.rds. Raw files in kraken_data/ are never modified.
#
# Input:  kraken_data/*.csv   Output: clean/kraken_trades.rds
# (Split out of the former Untitled.R on 2026-11-23; code unchanged.)

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
