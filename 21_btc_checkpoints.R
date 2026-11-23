# 21_btc_checkpoints.R
# Fetches the BTC/USD price (Kraken spot, XBTUSD) at each weekend/holiday
# checkpoint (the close C0 and Sunday-night K) used in 20_weekend_panel.R.
#
# How: Kraken's public Trades endpoint accepts a Unix time in seconds as
# `since` and returns trades from that moment forward. We ask for trades
# starting 5 minutes before each checkpoint and keep the last one at or before
# it. BTC trades constantly, so that price is seconds old (staleness is saved).
#
# About 125 calls at 1.2 s each, so roughly 3 minutes. Resumable: rows already
# in the output file are skipped. Run 20_weekend_panel.R first.
#
# Output: btc_data/btc_checkpoints.csv (raw download; do not edit by hand)

library(data.table); library(jsonlite)

OUT <- "btc_data/btc_checkpoints.csv"
dir.create("btc_data", showWarnings = FALSE)

panel <- readRDS("output/weekend_panel.rds")[type != "overnight"]
need  <- sort(unique(c(panel$t_C0, panel$t_K)))
if (file.exists(OUT)) need <- setdiff(need, fread(OUT)$t_check)
cat("Checkpoints to fetch:", length(need), "\n")

fetch <- function(since) {
  u <- paste0("https://api.kraken.com/0/public/Trades?pair=XBTUSD&count=1000&since=", since)
  for (attempt in 1:6) {
    Sys.sleep(1.2)
    r <- tryCatch(fromJSON(u), error = function(e) NULL)
    if (!is.null(r) && length(r$error) == 0) return(r)
    cat("  retry", attempt, if (!is.null(r)) paste(r$error, collapse = "; ") else "network error", "\n")
    Sys.sleep(15 * attempt)
  }
  stop("Gave up after repeated errors. Rerun the script: it resumes where it stopped.")
}

for (tc in need) {
  r <- fetch(floor(tc - 300))
  m <- r$result[[setdiff(names(r$result), "last")[1]]]
  t <- as.numeric(m[, 3]); p <- as.numeric(m[, 1])
  k <- max(which(t <= tc), -Inf)
  # If all 1000 trades came after tc (impossible here) or none before it, store NA.
  row <- data.table(t_check = tc,
                    btc   = if (is.finite(k)) p[k] else NA_real_,
                    stale_sec = if (is.finite(k)) tc - t[k] else NA_real_)
  fwrite(row, OUT, append = file.exists(OUT))
}
cat("Finished. Rows in file:", nrow(fread(OUT)), "\n")
