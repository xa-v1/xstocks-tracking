# 23_weekend_robust.R
# Follow-up checks on the weekend / holiday test. Rules: prereg_log.md
# (2026-11-23, step 5). Run 20_weekend_panel.R and 21_btc_checkpoints.R first.
#
# Plain-English version:
#  1. Same-sample check: is the drop in significance when adding BTC and SPYx
#     controls caused by the controls, or by losing a third of the rows?
#  2. The four robustness checks promised in the 2026-11-18 log.
#  3. Replace the single trade at Sunday 20:00 with a buy/sell midpoint, to
#     remove bid/ask bounce (which would bias the slope toward zero).
#  4. Forecast benchmark: does the token's Sunday-night price predict Monday's
#     open better than simply assuming "no change from Friday's close"?
#  5. Drop one weekend at a time to see whether any single weekend drives it.
#
# Inputs:  output/weekend_panel.rds, btc_data/btc_checkpoints.csv,
#          clean/kraken_trades.rds
# Outputs: output/weekend_robust.txt, output/weekend_robust.csv

library(data.table); library(sandwich); library(lmtest)

# ---- CONFIG (same as 22_weekend_tracker.R; prereg_log.md) ----
DROP_TOKENS <- c("AMZN", "COIN", "INTC", "META", "MRVL")
STALE_K     <- 120          # minutes
NY          <- "America/New_York"
# ----------------------------------------------------------

p  <- readRDS("output/weekend_panel.rds")[type != "overnight" & !(symbol %in% DROP_TOKENS)]
tr <- readRDS("clean/kraken_trades.rds")
btc <- fread("btc_data/btc_checkpoints.csv")
p[, r_btc := 100 * log(btc$btc[match(t_K, btc$t_check)] / btc$btc[match(t_C0, btc$t_check)])]

# Last trade at or before each query time (optionally one side only).
# Returns price, or NA if older than max_stale minutes.
last_px <- function(times, max_stale, side_only = NULL) {
  x <- if (is.null(side_only)) tr else tr[side == side_only]
  x <- x[, .(symbol, tq = t, t_trade = t, price)]
  setkey(x, symbol, tq)
  m <- x[data.table(symbol = p$symbol, tq = times), on = .(symbol, tq), roll = TRUE]
  fifelse(!is.na(m$t_trade) & (m$tq - m$t_trade) / 60 <= max_stale, m$price, NA_real_)
}

# Alternative regressors (all log % vs Friday's stock close, like drift_K)
p[, drift_K30  := fifelse(stale_K <= 30, drift_K, NA_real_)]                   # (a)
t_mon4 <- as.numeric(as.POSIXct(paste(p$d_next, "04:00"), tz = NY))
p[, drift_M4   := 100 * log(last_px(t_mon4, STALE_K) / stk_close)]            # (b)
p[, drift_Kmid := 100 * log((last_px(t_K, STALE_K, "b") +
                             last_px(t_K, STALE_K, "s")) / 2 / stk_close)]    # 3)

# Fit one spec: slope on x, SE clustered by window.
fit <- function(data, xvar, controls = FALSE, label) {
  d <- copy(data)[, x := get(xvar)]
  d[, r_mkt := x[symbol == "SPY"][1], by = window_id]
  d <- d[!is.na(x) & !is.na(r_stk_gap)]
  if (controls) d <- d[!(symbol %in% c("SPY", "QQQ")) & !is.na(r_btc) & !is.na(r_mkt)]
  f <- if (controls) r_stk_gap ~ x + r_btc + r_mkt else r_stk_gap ~ x
  m <- lm(f, data = d)
  V <- vcovCL(m, cluster = d$window_id)
  b <- coef(m)[["x"]]; se <- sqrt(V["x", "x"])
  data.table(spec = label, regressor = xvar, controls, N = nrow(d),
             clusters = uniqueN(d$window_id), slope = b, se = se,
             p_slope0 = 2 * pt(-abs(b / se), df = uniqueN(d$window_id) - 1),
             t_slope1 = (b - 1) / se, r2 = summary(m)$r.squared)
}

# Rows used by regression 2, for the same-sample check
d2_rows <- copy(p)[, x := drift_K][, r_mkt := x[symbol == "SPY"][1], by = window_id][
  !is.na(x) & !is.na(r_stk_gap) & !(symbol %in% c("SPY", "QQQ")) & !is.na(r_btc) & !is.na(r_mkt)]
p_same <- p[d2_rows[, .(window_id, symbol)], on = .(window_id, symbol)]

res <- rbind(
  fit(p, "drift_K", FALSE, "Main 1: no controls"),
  fit(p, "drift_K", TRUE,  "Main 2: + BTC, SPYx"),
  fit(p_same, "drift_K", FALSE, "1) Main 1 on Main 2's rows"),
  fit(p, "drift_K30", FALSE, "2a) K staleness 30 min"),
  fit(p, "drift_K30", TRUE,  "2a) K staleness 30 min, controls"),
  fit(p, "drift_M4",  FALSE, "2b) K = Monday 04:00"),
  fit(p[d_prev >= as.Date("2025-11-01")], "drift_K", FALSE, "2c) drop windows before Nov 2025"),
  fit(p, "r_tok", FALSE, "2d) r_tok regressor"),
  fit(p, "r_tok", TRUE,  "2d) r_tok regressor, controls"),
  fit(p, "drift_Kmid", FALSE, "3) buy/sell midpoint at K"),
  fit(p, "drift_Kmid", TRUE,  "3) buy/sell midpoint at K, controls"))
fwrite(res, "output/weekend_robust.csv")

# ---- 4) Forecast benchmark (main sample, no controls) ----
d <- p[!is.na(drift_K) & !is.na(r_stk_gap)]
# Out-of-sample slope: fit on all other windows, predict this window
d[, pred_oos := {
  w <- window_id[1]
  m <- lm(r_stk_gap ~ drift_K, data = d[window_id != w])
  predict(m, newdata = .SD)
}, by = window_id]
d[, `:=`(e_naive = r_stk_gap^2, e_tok = (r_stk_gap - drift_K)^2,
         e_oos = (r_stk_gap - pred_oos)^2)]
mse_test <- function(a, b) {   # is mean(a - b) different from 0? (clustered)
  m <- lm(I(a - b) ~ 1, data = data.table(a, b))
  ct <- coeftest(m, vcov. = vcovCL(m, cluster = d$window_id))
  sprintf("diff = %.3f, SE = %.3f, p = %.3f", ct[1, 1], ct[1, 2], ct[1, 4])
}
fc <- c(sprintf("Rows %d, windows %d", nrow(d), uniqueN(d$window_id)),
        sprintf("MSE naive (open = Friday close): %.3f", mean(d$e_naive)),
        sprintf("MSE token one-for-one:           %.3f", mean(d$e_tok)),
        sprintf("MSE token x slope (out-of-sample): %.3f", mean(d$e_oos)),
        paste("naive - one-for-one:", mse_test(d$e_naive, d$e_tok)),
        paste("naive - out-of-sample:", mse_test(d$e_naive, d$e_oos)),
        sprintf("Same sign as stock gap: %.3f", mean(sign(d$drift_K) == sign(d$r_stk_gap))))

# ---- 5) Leave-one-window-out ----
lowo <- rbindlist(lapply(unique(d$window_id), function(w) {
  m <- lm(r_stk_gap ~ drift_K, data = d[window_id != w])
  data.table(dropped = w, n_rows = d[window_id == w, .N], slope = coef(m)[[2]])
}))
setorder(lowo, slope)

sink("output/weekend_robust.txt")
cat("weekend_robust.R run", format(Sys.time()), "\n\n== Specs ==\n")
print(res[, .(spec, N, clusters, slope = round(slope, 3), se = round(se, 3),
              p_slope0 = round(p_slope0, 3), t_slope1 = round(t_slope1, 2), r2 = round(r2, 3))])
cat("\n== Forecast benchmark ==\n"); cat(fc, sep = "\n")
cat("\n\n== Leave-one-window-out (slope of Main 1) ==\n")
cat(sprintf("range %.3f to %.3f, median %.3f\n", min(lowo$slope), max(lowo$slope), median(lowo$slope)))
cat("Most influential (lowest and highest slope when dropped):\n")
print(rbind(head(lowo, 3), tail(lowo, 3)), digits = 3)
sink()
cat(readLines("output/weekend_robust.txt"), sep = "\n")
