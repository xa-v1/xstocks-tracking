# 20_weekend_panel.R
# Builds one row per token per "closed window" (the gap between one trading
# day's close and the next trading day's open), with the token and stock
# prices at fixed checkpoints. Rules are in prereg_log.md (2026-11-18).
#
# Plain-English version:
#  1. Find the trading days from the stock bars. Two trading days more than one
#     calendar day apart means a weekend or holiday sat between them.
#  2. For each window, mark three clock times: the close (C0), Sunday-night
#     20:00 or its holiday equivalent (K), and the next 09:30 open (O1).
#  3. Look up the last token trade at or before each time, and how old it was.
#  4. Look up the stock's closing bar, opening bar and last pre-market bar.
#
# Inputs:  clean/kraken_trades.rds, clean/databento_1m_ny.rds
# Outputs: output/weekend_panel.rds (all windows, incl. overnight rows)

library(data.table)
dir.create("output", showWarnings = FALSE)

tr <- readRDS("clean/kraken_trades.rds")
px <- readRDS("clean/databento_1m_ny.rds")

LAST_DATE   <- as.Date("2026-10-09")   # last full day of Kraken data
EARLY_CLOSE <- as.Date(c("2025-11-28", "2025-12-24"))  # checked in bar counts
STALE_C0    <- 15    # minutes; token price at the close
STALE_K     <- 120   # minutes; token price at Sunday 20:00 (thin weekends)
STALE_P     <- 15    # minutes; token price just before the open
NY          <- "America/New_York"

# Turn a date + "HH:MM" New York clock time into Unix seconds.
# as.POSIXct with tz = NY applies daylight saving automatically.
ny_time <- function(d, hhmm) as.numeric(as.POSIXct(paste(d, hhmm), tz = NY))

# ---- 1) Trading days and windows ----
days <- sort(unique(px[mod >= 570 & mod <= 959 & date <= LAST_DATE]$date))
win <- data.table(d_prev = days[-length(days)], d_next = days[-1])
win[, cal_gap := as.numeric(d_next - d_prev)]
win[, type := fifelse(cal_gap == 1, "overnight",
              fifelse(cal_gap == 3 & weekdays(d_prev) == "Friday", "weekend",
                      "holiday"))]   # holiday = any longer or mid-week closure
win[, early := d_prev %in% EARLY_CLOSE]
win[, `:=`(t_C0 = ny_time(d_prev, fifelse(early, "13:00", "16:00")),
           t_K  = ny_time(d_next - 1, "20:00"),
           t_P  = ny_time(d_next, "09:30"))]
win[, window_id := format(d_prev)]
print(win[, .N, by = type])

# One row per token x window
syms  <- sort(unique(tr$symbol))
panel <- CJ(symbol = syms, window_id = win$window_id)[win, on = "window_id"]

# ---- 2) Token prices: last trade at or before each checkpoint ----
# Rolling join: for each (symbol, query time) take the most recent trade.
trk <- tr[, .(symbol, tq = t, t_trade = t, price)]
setkey(trk, symbol, tq)
tok_at <- function(times, max_stale) {
  q <- data.table(symbol = panel$symbol, tq = times)
  m <- trk[q, on = .(symbol, tq), roll = TRUE]
  stale <- (m$tq - m$t_trade) / 60
  list(price = fifelse(!is.na(stale) & stale <= max_stale, m$price, NA_real_),
       stale = stale)
}
x <- tok_at(panel$t_C0, STALE_C0); panel[, `:=`(tok_C0 = x$price, stale_C0 = x$stale)]
x <- tok_at(panel$t_K,  STALE_K);  panel[, `:=`(tok_K  = x$price, stale_K  = x$stale)]
x <- tok_at(panel$t_P,  STALE_P);  panel[, `:=`(tok_P  = x$price, stale_P  = x$stale)]

# Number of token trades between the close and K (how active the closed market was)
q <- panel[, .(symbol, lo = t_C0, hi = t_K)]
panel[, n_closed_trades := tr[q, on = .(symbol, t > lo, t <= hi), .N, by = .EACHI]$N]

# ---- 3) Stock prices from the 1-minute bars ----
close_bar <- px[(mod == 959 & !(date %in% EARLY_CLOSE)) |
                (mod == 779 &   date %in% EARLY_CLOSE),
                .(symbol, d_prev = date, stk_close = close)]
open_bar  <- px[mod == 570, .(symbol, d_next = date, stk_open = open)]
pre_bar   <- px[mod >= 240 & mod <= 569][order(ts), .SD[.N], by = .(symbol, date)][
                , .(symbol, d_next = date, stk_pre = close,
                    stale_pre = 570 - (mod + 1))]  # minutes from bar end to 09:30
panel <- close_bar[panel, on = .(symbol, d_prev)]
panel <- open_bar[panel,  on = .(symbol, d_next)]
panel <- pre_bar[panel,   on = .(symbol, d_next)]

# ---- 4) Returns, in log percent ----
panel[, `:=`(
  r_stk_gap   = 100 * log(stk_open / stk_close),  # what the stock did while closed
  r_stk_pre   = 100 * log(stk_pre  / stk_close),
  r_tok       = 100 * log(tok_K / tok_C0),        # token move, close -> Sunday 20:00
  r_tok_pre   = 100 * log(tok_P / tok_C0),        # token move, close -> 09:29
  basis_C0    = 100 * log(tok_C0 / stk_close),    # starting gap at the close
  drift_K     = 100 * log(tok_K / stk_close))]    # unhedged gap vs last tradable price

setcolorder(panel, c("window_id", "type", "symbol", "d_prev", "d_next"))
setorder(panel, d_prev, symbol)
saveRDS(panel, "output/weekend_panel.rds")

# ---- Data checks only (no results) ----
main <- panel[type != "overnight"]
cat("\nWeekend/holiday token-windows:", nrow(main),
    "| windows:", uniqueN(main$window_id), "\n")
cat("Usable (both r_tok and r_stk_gap present):",
    main[!is.na(r_tok) & !is.na(r_stk_gap), .N], "\n")
print(main[, .(windows = .N,
               missing_C0 = sum(is.na(tok_C0)), missing_K = sum(is.na(tok_K)),
               missing_stock = sum(is.na(r_stk_gap)),
               med_stale_K = round(median(stale_K, na.rm = TRUE)),
               med_closed_trades = median(n_closed_trades)), by = symbol])
cat("\nPossible splits / bad prints (|return| > 25%):\n")
print(panel[abs(r_stk_gap) > 25 | abs(r_tok) > 25,
            .(window_id, symbol, r_stk_gap, r_tok, stk_close, stk_open, tok_C0, tok_K)])
