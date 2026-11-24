# 12_band_followups.R
# Follow-up checks on the intraday gap and the no-arbitrage band. All POST-HOC:
# chosen after seeing 10_basis_mid.R results. Rules: prereg_log.md (2026-11-24).
#
# Plain-English version:
#  1. Same minutes: compare the trade-price gap and the midpoint gap on exactly
#     the same minutes, so the two numbers differ only by how they are measured.
#  2. Direction: split band crossings into "buy token, sell stock" (a buy trade
#     far below the stock) and "sell token, buy stock" (a sell trade far above).
#  3. Duration: how many CONSECUTIVE minutes a gap stays outside the band. A
#     minute with no token trade, or a traded minute inside the band, ends it.
#  4. Heat maps: gap size and band crossings by time of day (15-min buckets).
#  5. Groups: ETFs vs crypto-linked stocks vs other single stocks (descriptive).
#
# Inputs:  output/mid_minute.rds, output/last_minute.rds, output/band_minute.rds,
#          output/persistence.csv (all from 10_basis_mid.R)
# Outputs (all in output/): same_minute.csv, band_direction.csv, band_runs.csv,
#   heat_gap.csv, heat_band.csv, group_tokens.csv, group_compare.csv,
#   fig_heat_gap.png, fig_heat_band.png, followups_log.txt

library(data.table); library(ggplot2)

# ---- CONFIG (fixed in prereg_log.md 2026-11-24) ----
FEE_MAIN  <- 0.10        # % per token trade [secondary]; middle of the fee grid
BLOCK_MIN <- 15          # heat-map bucket width, minutes from 09:30
MIN_CELL  <- 30          # heat-map cells with fewer minutes are left blank
ETF       <- c("SPY", "QQQ")
CRYPTO    <- c("COIN", "MSTR", "CRCL", "HOOD")
# ----------------------------------------------------

mid <- readRDS("output/mid_minute.rds")
lst <- readRDS("output/last_minute.rds")
bm  <- readRDS("output/band_minute.rds")
group_of <- function(s) fcase(s %in% ETF, "ETF", s %in% CRYPTO, "Crypto-linked",
                              default = "Other single stock")

# ---- 1) Same minutes: trade-price gap vs midpoint gap ----
# Every midpoint minute is a minute where the token traded, so it has a
# last-trade gap too. Midpoint minutes are the busier ones (both sides traded).
sm <- merge(mid[, .(symbol, bar, episode, mid_gap)], lst, by = c("symbol", "bar"))
stopifnot(nrow(sm) == nrow(mid))
same <- sm[, .(minutes = .N,
               med_abs_trade = median(abs(g_last)), med_abs_mid = median(abs(mid_gap)),
               p95_abs_trade = quantile(abs(g_last), 0.95),
               p95_abs_mid   = quantile(abs(mid_gap), 0.95),
               share_trade_gt_0.5 = mean(abs(g_last) > 0.5),
               share_mid_gt_0.5   = mean(abs(mid_gap) > 0.5)),
           by = .(symbol, episode)]
setorder(same, episode, med_abs_mid)
fwrite(same, "output/same_minute.csv")

# ---- 2) Direction of band crossings (conservative out_range test) ----
# Denominator = all minutes in the band test, as in band.csv.
dirn <- bm[, .(minutes = .N, share_out = mean(out),
               share_buy_token = mean(out_buy), share_sell_token = mean(out_sell),
               n_buy_token = sum(out_buy), n_sell_token = sum(out_sell)),
           by = .(fee, symbol, episode)]
setorder(dirn, fee, episode, -share_out)
fwrite(dirn, "output/band_direction.csv")

# ---- 3) Run lengths in consecutive minutes ----
# Within a token-day, in time order: a flagged minute continues a run only if
# the previous traded minute was also flagged AND exactly 60 s earlier.
setorder(bm, fee, symbol, date, bar)
run_lengths <- function(flag) {
  x <- bm[, .(fee, symbol, episode, date, bar, f = get(flag))]
  x[, cont := f & shift(f, fill = FALSE) & (bar - shift(bar, fill = -Inf)) == 60,
    by = .(fee, symbol, date)]
  x[, run := cumsum(f & !cont), by = .(fee, symbol, date)]
  x[f == TRUE, .(len = .N), by = .(fee, symbol, episode, date, run)]
}
run_stats <- function(r) r[, .(runs = .N, med_len = as.numeric(median(len)),
                               mean_len = mean(len), p90_len = as.numeric(quantile(len, 0.9)),
                               max_len = max(len), share_len1 = mean(len == 1))]
runs <- rbindlist(lapply(c(any = "out", buy_token = "out_buy", sell_token = "out_sell"),
                         run_lengths), idcol = "direction")
runs_tab <- rbind(runs[, run_stats(.SD), by = .(direction, fee, symbol, episode)],
                  runs[, run_stats(.SD), by = .(direction, fee, episode)][, symbol := "ALL (pooled)"])
setorder(runs_tab, direction, fee, episode, symbol)
fwrite(runs_tab, "output/band_runs.csv")

# ---- 4) Time-of-day heat maps (episode excluded) ----
bm[, mod := {lt <- as.POSIXlt(as.POSIXct(bar, origin = "1970-01-01", tz = "UTC"),
                             tz = "America/New_York"); lt$hour * 60 + lt$min}]
mid[, block := (mod - 570) %/% BLOCK_MIN]
bm[,  block := (mod - 570) %/% BLOCK_MIN]
heat_gap  <- mid[episode == FALSE, .(n = .N, value = median(abs(mid_gap))), by = .(symbol, block)]
heat_band <- bm[episode == FALSE & fee == FEE_MAIN, .(n = .N, value = 100 * mean(out)),
                by = .(symbol, block)]
fwrite(heat_gap, "output/heat_gap.csv"); fwrite(heat_band, "output/heat_band.csv")

# Shared styling (matches 10_basis_mid.R). Sequential single-hue fill,
# light = small, dark = large; blank cells = too few minutes.
theme_heat <- theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.background = element_rect(fill = "white", colour = NA),
        strip.text.y = element_text(angle = 0, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))
heat_plot <- function(h, title, subtitle, fill_lab) {
  h <- copy(h)[n < MIN_CELL, value := NA_real_]
  h[, `:=`(group = factor(group_of(symbol), c("ETF", "Crypto-linked", "Other single stock")),
           start = sprintf("%02d:%02d", (570 + block * BLOCK_MIN) %/% 60,
                           (570 + block * BLOCK_MIN) %% 60))]
  ggplot(h, aes(start, symbol, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    scale_fill_gradient(low = "#e8f0fb", high = "#0d3b78", na.value = "white",
                        name = fill_lab) +
    labs(title = title, subtitle = subtitle, x = "Start of 15-minute bucket (ET)", y = NULL) +
    theme_heat
}
ggsave("output/fig_heat_gap.png",
       heat_plot(heat_gap, "Typical token-vs-stock gap by time of day",
                 "Median absolute midpoint gap, log %. Oct-Nov 2025 episode excluded. Blank: < 30 minutes.",
                 "Median abs. gap (%)"),
       width = 10, height = 5.5, dpi = 300, device = ragg::agg_png)
ggsave("output/fig_heat_band.png",
       heat_plot(heat_band, "Gaps that clear estimated trading costs, by time of day",
                 sprintf("%% of minutes outside the conservative band at a %.2f%% fee. Episode excluded. Blank: < 30 minutes.", FEE_MAIN),
                 "% of minutes"),
       width = 10, height = 5.5, dpi = 300, device = ragg::agg_png)

# ---- 5) ETFs vs crypto-linked vs other (descriptive; no test) ----
pers <- fread("output/persistence.csv")[episode == FALSE, .(symbol, half_life_min)]
tok <- Reduce(function(a, b) merge(a, b, by = "symbol", all.x = TRUE), list(
  mid[episode == FALSE, .(med_abs_mid = median(abs(mid_gap))), by = symbol],
  dcast(bm[episode == FALSE & fee %in% c(0.10, 0.25), .(share = mean(out)), by = .(symbol, fee)],
        symbol ~ paste0("share_out_fee", fee), value.var = "share"),
  runs[direction == "any" & episode == FALSE & fee == FEE_MAIN,
       .(med_run_len = as.numeric(median(len))), by = symbol],
  pers))
tok[, group := group_of(symbol)]
setorder(tok, group, med_abs_mid)
fwrite(tok, "output/group_tokens.csv")
num <- setdiff(names(tok), c("symbol", "group"))
grp <- tok[, c(list(tokens = paste(symbol, collapse = " ")),
               lapply(.SD, median, na.rm = TRUE)), by = group, .SDcols = num]
fwrite(grp, "output/group_compare.csv")

# ---- Log ----
sink("output/followups_log.txt")
cat("12_band_followups.R run", format(Sys.time()), "(all POST-HOC, prereg_log.md 2026-11-24)\n")
cat("\n== 1) Same minutes: trade-price vs midpoint gap ==\n"); print(same, digits = 3)
cat("\n== 2) Direction of band crossings ==\n"); print(dirn, digits = 3)
cat("\n== 3) Run lengths (consecutive minutes), pooled ==\n")
print(runs_tab[symbol == "ALL (pooled)"], digits = 3)
cat("\n== 3) Run lengths, any direction, fee", FEE_MAIN, ", by token ==\n")
print(runs_tab[direction == "any" & fee == FEE_MAIN & symbol != "ALL (pooled)"], digits = 3)
cat("\n== 5) Per token (episode excluded) ==\n"); print(tok, digits = 3)
cat("\n== 5) Group medians ==\n"); print(grp, digits = 3)
sink()
cat(readLines("output/followups_log.txt"), sep = "\n")
