# 11_determinants.R
# What predicts the size of the intraday token-vs-stock gap? Panel regression
# at the token-day level. Rules: prereg_log.md (2026-11-23, step 4).
#
# Plain-English version:
#  1. For each token and trading day, summarise: the typical absolute midpoint
#     gap (outcome), how many token trades there were, how volatile the stock
#     was, the token's half-spread, and how much trading came in the first
#     30 minutes after the open.
#  2. Regress the gap on these, with token fixed effects (compare each token
#     with its own average) and, in the main spec, date fixed effects (compare
#     tokens with each other on the same day, which removes market-wide news).
#  3. Standard errors clustered by date, because all tokens react to the same
#     daily news, so the effective sample is closer to the number of days.
#
# Inputs:  output/mid_minute.rds (from 10_basis_mid.R), clean/kraken_trades.rds,
#          clean/databento_1m_ny.rds
# Outputs: output/determinants_panel.rds, output/determinants.txt,
#          output/determinants_table.csv

library(data.table); library(sandwich); library(lmtest)

# ---- CONFIG (fixed in prereg_log.md) ----
LAST_DATE   <- as.Date("2026-10-09")
EARLY_CLOSE <- as.Date(c("2025-11-28", "2025-12-24"))
EPISODE     <- as.Date(c("2025-10-15", "2025-11-21"))
MIN_MID     <- 10        # minimum midpoint minutes per token-day
# -----------------------------------------

mid <- readRDS("output/mid_minute.rds")
tr  <- readRDS("clean/kraken_trades.rds")
px  <- readRDS("clean/databento_1m_ny.rds")

# ---- 1) Outcome and half-spread from the midpoint minutes ----
dg <- mid[, .(n_mid = .N,
              abs_gap = median(abs(mid_gap)),
              half_spread = median((g_b - g_s) / 2)), by = .(symbol, date)]

# ---- 2) Token activity: regular-session trades and opening share ----
lt <- as.POSIXlt(as.POSIXct(tr$t, origin = "1970-01-01", tz = "UTC"), tz = "America/New_York")
tr[, `:=`(date = as.Date(format(lt, "%Y-%m-%d")), mod = lt$hour * 60 + lt$min)]
act <- tr[mod >= 570 & mod < 960, .(n_trades = .N, open_share = mean(mod < 600)),
          by = .(symbol, date)]

# ---- 3) Stock realized volatility from 1-minute bar closes ----
reg <- px[mod >= 570 & mod <= 959 & !(date %in% EARLY_CLOSE) & date <= LAST_DATE][order(symbol, ts)]
reg[, r := 100 * log(close / shift(close)), by = .(symbol, date)]
rv <- reg[, .(rv = sqrt(sum(r^2, na.rm = TRUE))), by = .(symbol, date)]

# ---- 4) Assemble the panel ----
d <- Reduce(function(a, b) merge(a, b, by = c("symbol", "date")), list(dg, act, rv))
d <- d[n_mid >= MIN_MID]
d[, `:=`(log_trades = log(n_trades),
         episode = as.integer(date >= EPISODE[1] & date <= EPISODE[2]),
         fdate = factor(date), fsym = factor(symbol))]
saveRDS(d, "output/determinants_panel.rds")

# ---- 5) Regressions ----
show <- function(f, cl, label, data = d) {
  m  <- lm(f, data = data)
  V  <- vcovCL(m, cluster = as.data.frame(data[, all.vars(cl), with = FALSE]), type = "HC1")
  ct <- coeftest(m, vcov. = V)
  keep <- !grepl("^fsym|^fdate|Intercept", rownames(ct))
  list(label = label, n = nobs(m), r2 = summary(m)$r.squared,
       days = uniqueN(data$date), ct = ct[keep, , drop = FALSE])
}
X  <- "log_trades + rv + half_spread + open_share"
fits <- list(
  show(as.formula(paste("abs_gap ~", X, "+ episode + fsym")), ~ date, "(1) token FE"),
  show(abs_gap ~ log_trades + rv + open_share + episode + fsym, ~ date, "(2) token FE, no half_spread"),
  show(as.formula(paste("abs_gap ~", X, "+ fsym + fdate")), ~ date, "(3) MAIN: token + date FE"),
  show(as.formula(paste("abs_gap ~", X, "+ fsym + fdate")), ~ date + symbol, "(4) as (3), 2-way cluster"),
  show(as.formula(paste("log(abs_gap) ~", X, "+ fsym + fdate")), ~ date, "(5) as (3), log outcome",
       data = d[abs_gap > 0])
)

# Tidy table: coefficient (SE) with stars, one column per spec
star <- function(p) fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.1, "*", "")))
tab <- rbindlist(lapply(fits, function(f) data.table(
  spec = f$label, term = rownames(f$ct),
  cell = sprintf("%.4f%s (%.4f)", f$ct[, 1], star(f$ct[, 4]), f$ct[, 2]))))
tab <- dcast(tab, term ~ spec, value.var = "cell", fill = "")
foot <- rbindlist(lapply(fits, function(f) data.table(spec = f$label,
  N = f$n, days = f$days, R2 = round(f$r2, 3))))
fwrite(tab, "output/determinants_table.csv")

sink("output/determinants.txt")
cat("determinants.R run", format(Sys.time()), "\n")
cat("Token-days:", nrow(d), "| tokens:", uniqueN(d$symbol), "| days:", uniqueN(d$date), "\n\n")
cat("== Descriptives (token-day) ==\n")
print(d[, lapply(.SD, function(x) round(quantile(x, c(.1, .5, .9)), 4)),
        .SDcols = c("abs_gap", "n_trades", "rv", "half_spread", "open_share")])
cat("\nToken-days kept per token:\n"); print(d[, .N, by = symbol][order(-N)])
for (f in fits) {
  cat("\n==", f$label, "| N =", f$n, "| days =", f$days, "| R2 =", round(f$r2, 3), "==\n")
  print(round(f$ct, 4))
}
sink()
cat(readLines("output/determinants.txt"), sep = "\n")
