# 10_basis_mid.R
# Intraday basis, take two: separate the token's bid/ask spread from genuine
# mispricing, test whether gaps exceed a no-arbitrage band, and measure how fast
# gaps close. Rules: prereg_log.md (2026-11-23).
#
# Plain-English version:
#  1. Every token trade is tagged buy (b) or sell (s). A buy trade happens at
#     the ask, a sell at the bid, so single trades sit above/below the "true"
#     price by half the spread. We measure the gap separately for each side.
#  2. Midpoint gap: average the latest buy gap and latest sell gap (each at
#     most 5 minutes old). The spread cancels out, leaving the actual mispricing.
#  3. Half-spread = (median buy gap - median sell gap) / 2 per token-month.
#     This is the token's trading cost, estimated from our own data.
#  4. Check that does not depend on minute matching: share of token trades
#     priced inside the stock's high-low range in the same minute.
#  5. No-arbitrage band: did an actual buy (sell) trade happen so far below
#     (above) the stock that the gap cleared estimated trading costs (fees and
#     the cost of exiting)? This is not "profit": trade size, slippage, latency
#     and stock-side costs are ignored. Several fee levels (fee unverified).
#  6. Persistence: 15-minute medians of the midpoint gap, AR(1) per token,
#     converted to a half-life in minutes.
#
# Inputs:  clean/kraken_trades.rds, clean/databento_1m_ny.rds
# Outputs (all in output/): mid_summary.csv, half_spread_month.csv,
#   mid_month.csv, inside_range.csv, band.csv, band_days.csv, persistence.csv,
#   fig_mid_monthly.png, fig_half_spread.png, basis_mid_log.txt, mid_minute.rds,
#   last_minute.rds, band_minute.rds (the last two feed 12_band_followups.R)

library(data.table); library(ggplot2)
dir.create("output", showWarnings = FALSE)

# ---- CONFIG (fixed in prereg_log.md 2026-11-23) ----
LAST_DATE   <- as.Date("2026-10-09")
EARLY_CLOSE <- as.Date(c("2025-11-28", "2025-12-24"))
EPISODE     <- as.Date(c("2025-10-15", "2025-11-21"))  # data-identified
MID_MAX_AGE <- 240          # seconds between bar starts: current minute + 4 earlier = 5 min
FEES        <- c(0, 0.10, 0.25)   # % per token trade; 0.10 is [secondary]
BLOCK_MIN   <- 15
# ----------------------------------------------------

tr <- readRDS("clean/kraken_trades.rds")
px <- readRDS("clean/databento_1m_ny.rds")

# Stock: regular-session 1-minute bars, full trading days only.
stk <- px[mod >= 570 & mod <= 959 & !(date %in% EARLY_CLOSE) & date <= LAST_DATE,
          .(symbol, bar = as.numeric(ts), date, mod, low, high, stock = close)]

tr <- tr[order(symbol, t)]
tr[, bar := floor(t / 60) * 60]      # minute the trade falls in (= bar start stamp)

# ---- 1) Inside-range check: every trade vs the same minute's high-low ----
ir <- merge(tr[, .(symbol, bar, price, side)], stk[, .(symbol, bar, date, low, high)],
            by = c("symbol", "bar"))
ir[, episode := date >= EPISODE[1] & date <= EPISODE[2]]
inside <- ir[, .(trades = .N, share_inside = mean(price >= low & price <= high)),
             by = .(symbol, episode, side)]
inside <- dcast(inside, symbol + episode ~ side, value.var = c("trades", "share_inside"))
fwrite(inside, "output/inside_range.csv")

# ---- 2) Side-specific gaps: last buy / last sell in each minute vs stock close ----
sg <- tr[, .(p = price[.N]), by = .(symbol, bar, side)]
sg <- merge(sg, stk, by = c("symbol", "bar"))
sg[, g := 100 * log(p / stock)]      # log percent
sg[, `:=`(month = format(date, "%Y-%m"),
          episode = date >= EPISODE[1] & date <= EPISODE[2])]

# Half-spread per token-month (the token's own trading cost)
hs <- dcast(sg[, .(med_g = median(g), n = .N), by = .(symbol, month, side)],
            symbol + month ~ side, value.var = c("med_g", "n"))
hs[, half_spread := (med_g_b - med_g_s) / 2]
fwrite(hs, "output/half_spread_month.csv")

# ---- 3) Midpoint gap at each minute where the token traded ----
# For each such minute, roll back to the latest buy-minute and latest
# sell-minute within MID_MAX_AGE seconds. Each gap was already measured
# against the stock in its own minute, so stock moves in between don't matter.
grid <- unique(sg[, .(symbol, bar, date, mod, month, episode)])
roll_side <- function(s) {
  x <- sg[side == s, .(symbol, bar, g_side = g)]
  setkey(x, symbol, bar)
  x[grid, on = .(symbol, bar), roll = MID_MAX_AGE]$g_side
}
grid[, `:=`(g_b = roll_side("b"), g_s = roll_side("s"))]
mid <- grid[!is.na(g_b) & !is.na(g_s)]
mid[, mid_gap := (g_b + g_s) / 2]
saveRDS(mid, "output/mid_minute.rds")   # used by 11_determinants.R

# Trade-price gap (last trade of any side) for comparison with the old measure
last_any <- merge(tr[, .(p = price[.N]), by = .(symbol, bar)], stk, by = c("symbol", "bar"))
last_any[, `:=`(g = 100 * log(p / stock), episode = date >= EPISODE[1] & date <= EPISODE[2])]
saveRDS(last_any[, .(symbol, bar, g_last = g)], "output/last_minute.rds")  # same-minute check

summ_mid <- mid[, .(minutes_mid = .N, med_mid = median(mid_gap),
                    med_abs_mid = median(abs(mid_gap)),
                    p95_abs_mid = quantile(abs(mid_gap), 0.95),
                    share_mid_gt_0.5 = mean(abs(mid_gap) > 0.5)),
                by = .(symbol, episode)]
summ_trd <- last_any[, .(minutes_trade = .N, med_abs_trade = median(abs(g)),
                         p95_abs_trade = quantile(abs(g), 0.95),
                         share_trade_gt_0.5 = mean(abs(g) > 0.5)),
                     by = .(symbol, episode)]
summ <- merge(summ_trd, summ_mid, by = c("symbol", "episode"), all = TRUE)
hs[, episode := month %in% c("2025-10", "2025-11")]   # month-level approximation of the episode
summ <- merge(summ, hs[, .(med_half_spread = median(half_spread, na.rm = TRUE)), by = .(symbol, episode)],
              by = c("symbol", "episode"))
setorder(summ, episode, med_abs_mid)
fwrite(summ, "output/mid_summary.csv")

# Monthly median midpoint gap (dividend / total-return drift check)
mm <- mid[, .(med_mid = median(mid_gap), minutes = .N), by = .(symbol, month)]
fwrite(mm, "output/mid_month.csv")

# ---- 4) No-arbitrage band using executable trade prices ----
# Entry leg = an actual trade. A gap "clears estimated costs" (not "is
# profitable") if it exceeds c = 2 * fee (in and out) + half-spread
# (crossing the spread again to exit). Stock-side costs set to zero.
# Two versions (prereg_log.md, 2026-11-23 later):
#  out_close (pre-registered): trade vs the stock bar CLOSE.
#  out_range (post-hoc, conservative): buy must be below the bar LOW, sell above
#    the bar HIGH, so ordinary stock movement inside the minute can't create a gap.
# Token-months with no half-spread (one side only) get the token's median.
hs[, h := pmax(fifelse(is.na(half_spread), median(half_spread, na.rm = TRUE), half_spread), 0),
   by = symbol]
bd <- merge(sg, hs[, .(symbol, month, h)], by = c("symbol", "month"))
bd[, `:=`(g_lo = 100 * log(p / low), g_hi = 100 * log(p / high))]
band <- rbindlist(lapply(FEES, function(f) {
  bd[, .(fee = f, c = 2 * f + h, side, g, g_lo, g_hi, symbol, date, episode, bar)][
     , `:=`(out_close = (side == "b" & g < -c)    | (side == "s" & g > c),
            out_range = (side == "b" & g_lo < -c) | (side == "s" & g_hi > c))]
}))
band_min <- band[, .(out = any(out_range), out_close = any(out_close),
                     out_buy  = any(out_range & side == "b"),   # buy token, sell stock
                     out_sell = any(out_range & side == "s")),  # sell token, buy stock
                 by = .(fee, symbol, episode, date, bar)]
saveRDS(band_min, "output/band_minute.rds")   # direction split and run lengths
band_tab <- band_min[, .(minutes = .N, share_minutes_out = mean(out),
                         share_minutes_out_close = mean(out_close)),
                     by = .(fee, symbol, episode)]
# n_out = flagged minutes per DAY, not how long a gap lasts. Durations in
# consecutive minutes are in 12_band_followups.R (band_runs.csv).
band_day <- band_min[, .(any_out = any(out), n_out = sum(out)), by = .(fee, symbol, episode, date)][
                     , .(days = .N, share_days_any_out = mean(any_out),
                         med_out_minutes_when_any = as.numeric(median(n_out[any_out]))),
                     by = .(fee, symbol, episode)]
band_tab <- merge(band_tab, band_day, by = c("fee", "symbol", "episode"))
setorder(band_tab, fee, episode, -share_minutes_out)
fwrite(band_tab, "output/band.csv")
# Which token-days are outside at the middle fee? (for inspection, not a result)
fwrite(band_min[fee == FEES[2] & out, .(n_out_minutes = .N), by = .(symbol, date)][order(-n_out_minutes)],
       "output/band_days.csv")

# ---- 5) Persistence: AR(1) on 15-minute block medians, within day ----
mid[, block := (mod - 570) %/% BLOCK_MIN]
blk <- mid[, .(y = median(mid_gap)), by = .(symbol, date, episode, block)]
setkey(blk, symbol, date, block)
blk[, y_lag := blk[.(symbol, date, block - 1), y]]   # same day, previous block only
ar1 <- function(d) {
  d <- d[!is.na(y_lag)]
  if (nrow(d) < 30) return(data.table(n = nrow(d), rho = NA_real_, se = NA_real_))
  m <- lm(y ~ y_lag, data = d)
  data.table(n = nrow(d), rho = coef(m)[2], se = sqrt(vcov(m)[2, 2]))
}
pers <- rbind(blk[, ar1(.SD), by = .(symbol, episode)],
              blk[, ar1(.SD), by = episode][, symbol := "ALL (pooled)"])
pers[, half_life_min := NA_real_]
pers[rho > 0 & rho < 1, half_life_min := BLOCK_MIN * log(0.5) / log(rho)]
setorder(pers, episode, symbol)
fwrite(pers, "output/persistence.csv")

# ---- 6) Figures ----
ink2 <- "#52514e"; grid_col <- "#e6e5e1"; blue <- "#2a78d6"; orange <- "#eb6834"
theme_paper <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
        plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.background = element_rect(fill = "white", colour = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))

mm[, m := as.Date(paste0(month, "-15"))]  # plot at mid-month
g1 <- ggplot(mm[minutes >= 50], aes(m, med_mid)) +
  annotate("rect", xmin = EPISODE[1], xmax = EPISODE[2], ymin = -Inf, ymax = Inf,
           fill = orange, alpha = 0.12) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.3) +
  geom_line(colour = blue) + geom_point(colour = blue, size = 1) +
  facet_wrap(~ symbol, ncol = 4) +
  labs(title = "Monthly median midpoint gap, token vs stock (log %)",
       subtitle = "Shaded: Oct-Nov 2025 wide-spread episode. Months with < 50 midpoint minutes dropped.",
       x = NULL, y = "Median midpoint gap (log %)") + theme_paper
ggsave("output/fig_mid_monthly.png", g1, width = 10, height = 7, dpi = 300, device = ragg::agg_png)

hs[, m := as.Date(paste0(month, "-15"))]  # plot at mid-month
g2 <- ggplot(hs[n_b >= 30 & n_s >= 30], aes(m, half_spread)) +
  annotate("rect", xmin = EPISODE[1], xmax = EPISODE[2], ymin = -Inf, ymax = Inf,
           fill = orange, alpha = 0.12) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.3) +
  geom_line(colour = blue) + geom_point(colour = blue, size = 1) +
  facet_wrap(~ symbol, ncol = 4) +
  labs(title = "Effective half-spread of each token, by month (%)",
       subtitle = "(median buy-trade gap - median sell-trade gap) / 2. Months with < 30 minutes per side dropped.",
       x = NULL, y = "Half-spread (%)") + theme_paper
ggsave("output/fig_half_spread.png", g2, width = 10, height = 7, dpi = 300, device = ragg::agg_png)

# ---- Log ----
sink("output/basis_mid_log.txt")
cat("basis_mid.R run", format(Sys.time()), "\n\n== Midpoint vs trade-price gaps ==\n")
print(summ, digits = 3)
cat("\n== Inside stock high-low range ==\n"); print(inside, digits = 3)
cat("\n== No-arbitrage band ==\n"); print(band_tab, digits = 3)
cat("\n== Persistence (15-min AR(1)) ==\n"); print(pers, digits = 3)
sink()
cat(readLines("output/basis_mid_log.txt"), sep = "\n")
