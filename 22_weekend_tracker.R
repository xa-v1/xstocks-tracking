# 22_weekend_tracker.R
# Weekend / holiday tracker: while the stock market is closed, how far do
# tokens drift from the last tradable stock price, and does that drift
# forecast where the stock opens? Rules: prereg_log.md (2026-11-18).
#
# Plain-English version:
#  1. Take the weekend/holiday rows from 20_weekend_panel.R and keep the
#     token-windows that have both a token move and a stock gap.
#  2. Attach the BTC move over the same hours, and the SPYx token's own move
#     as a "market" control.
#  3. Tracker table: one row per weekend, summarising all tokens.
#  4. Regressions: stock gap at the open on the token's closed-hours move.
#     Slope 0 = the token move tells us nothing; slope 1 = it passes through
#     one-for-one. Standard errors clustered by weekend.
#  5. Scatter figure for slides.
#
# Inputs:  output/weekend_panel.rds, btc_data/btc_checkpoints.csv (optional)
# Outputs: output/weekend_tracker.csv, output/weekend_by_token.csv,
#          output/weekend_regressions.txt, output/fig_weekend_scatter.png

library(data.table); library(sandwich); library(lmtest)
library(ggplot2); library(scales)

# ---- CONFIG (decide BEFORE running; log any change in prereg_log.md) ----
REGRESSOR   <- "drift_K"    # "r_tok" = token close->K move; "drift_K" = token at K vs stock close
DROP_TOKENS <- c("AMZN", "COIN", "INTC", "META", "MRVL")  # ~0 weekend trades (prereg_log.md)
CRYPTO      <- c("COIN", "MSTR", "CRCL", "HOOD")
# --------------------------------------------------------------------------

p <- readRDS("output/weekend_panel.rds")[type != "overnight" & !(symbol %in% DROP_TOKENS)]
p[, x := get(REGRESSOR)]

# BTC move over the same closed hours (close C0 -> K), if downloaded
if (file.exists("btc_data/btc_checkpoints.csv")) {
  btc <- fread("btc_data/btc_checkpoints.csv")
  p[, btc_C0 := btc$btc[match(t_C0, btc$t_check)]]
  p[, btc_K  := btc$btc[match(t_K,  btc$t_check)]]
  p[, r_btc  := 100 * log(btc_K / btc_C0)]
} else {
  cat("NOTE: no BTC file yet; run 21_btc_checkpoints.R. BTC specs skipped.\n")
  p[, r_btc := NA_real_]
}

# Market control: SPYx's own closed-hours move in the same window
p[, r_mkt := x[symbol == "SPY"][1], by = window_id]
p[, crypto := symbol %in% CRYPTO]

d <- p[!is.na(x) & !is.na(r_stk_gap)]
cat("Usable token-windows:", nrow(d), "| weekends:", uniqueN(d$window_id),
    "| tokens:", uniqueN(d$symbol), "\n")

# ---- 3) Tracker: one row per weekend ----
tracker <- d[, .(d_prev = d_prev[1], d_next = d_next[1], type = type[1],
                 tokens       = .N,
                 med_tok_move = median(x),
                 med_stk_gap  = median(r_stk_gap),
                 med_abs_miss = median(abs(r_stk_gap - x)),  # how far off the token was
                 same_sign    = mean(sign(x) == sign(r_stk_gap)),
                 btc_move     = r_btc[1]),
             by = window_id][order(d_prev)]
fwrite(tracker, "output/weekend_tracker.csv")

by_tok <- d[, .(windows       = .N,
                sd_tok_move   = sd(x),
                sd_stk_gap    = sd(r_stk_gap),
                corr          = cor(x, r_stk_gap),
                med_abs_miss  = median(abs(r_stk_gap - x)),
                med_abs_drift = median(abs(drift_K), na.rm = TRUE),
                share_drift_gt_0.5 = mean(abs(drift_K) > 0.5, na.rm = TRUE),
                med_stale_K   = median(stale_K)),
            by = symbol][order(-windows)]
fwrite(by_tok, "output/weekend_by_token.csv")
print(by_tok, digits = 3)

# ---- 4) Regressions, clustered by weekend ----
fit_show <- function(f, data, label) {
  m  <- lm(f, data = data)
  V  <- vcovCL(m, cluster = data$window_id)
  ct <- coeftest(m, vcov. = V)
  b  <- coef(m)["x"]; se <- sqrt(V["x", "x"])
  # data has no missing values in the model columns, so every row is used
  out <- c(paste0("== ", label, " =="),
           paste("N =", nobs(m), "| clusters =", uniqueN(data$window_id),
             "| R2 =", round(summary(m)$r.squared, 3)),
           capture.output(ct)[-(1:3)],
           sprintf("Test slope = 1: t = %.2f", (b - 1) / se), "")
  cat(out, sep = "\n"); out
}
res <- fit_show(r_stk_gap ~ x, d, "1) stock gap ~ token move")
if (any(!is.na(d$r_btc))) {
  d2 <- d[!(symbol %in% c("SPY", "QQQ")) & !is.na(r_btc) & !is.na(r_mkt)]
  res <- c(res,
    fit_show(r_stk_gap ~ x + r_btc + r_mkt, d2, "2) + BTC and SPYx controls"),
    fit_show(r_stk_gap ~ x * crypto + r_btc + r_mkt, d2, "3) crypto-linked interaction"))
}
writeLines(c(paste("Regressor:", REGRESSOR, "| dropped:", paste(DROP_TOKENS, collapse = ",")), res),
           "output/weekend_regressions.txt")

# ---- 5) Figure: token closed-hours move vs stock gap at the open ----
blue <- "#2a78d6"; orange <- "#eb6834"   # reference palette slots 1 and 2
ink  <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"
d[, group := factor(fifelse(crypto, paste0("Crypto-linked (", paste(setdiff(CRYPTO, DROP_TOKENS), collapse = ", "), ")"),
                            "Other stocks and ETFs"))]
lim <- quantile(abs(c(d$x, d$r_stk_gap)), 0.995)

fig <- ggplot(d, aes(x, r_stk_gap, colour = group)) +
  geom_hline(yintercept = 0, colour = grid_col) + geom_vline(xintercept = 0, colour = grid_col) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = ink2, linewidth = 0.5) +
  annotate("text", x = lim * 0.95, y = lim * 0.95, label = "token move = stock gap",
           hjust = 1, vjust = -0.6, angle = 45, size = 3.2, colour = ink2) +
  geom_point(size = 2.4, alpha = 0.75, stroke = 0) +
  scale_colour_manual(values = c(orange, blue), name = NULL) +
  scale_x_continuous(labels = function(v) paste0(v, "%")) +
  scale_y_continuous(labels = function(v) paste0(v, "%")) +
  coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
  labs(title = "Do weekend token moves predict where the stock opens?",
       subtitle = paste0("One dot per token per weekend/holiday. ",
                         if (REGRESSOR == "r_tok") "Token move: Friday close to Sunday 20:00 ET."
                         else "Token at Sunday 20:00 ET vs Friday stock close."),
       x = if (REGRESSOR == "r_tok") "Token move while market closed (log %)" else "Token Sunday 20:00 price vs Friday stock close (log %)",
       y = "Stock gap, close to next open (log %)",
       caption = paste0("Kraken xStocks trades vs Databento 1-min bars; ",
                        nrow(d), " token-weekends, ", uniqueN(d$window_id), " weekends.")) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", colour = ink, size = 15),
        plot.subtitle = element_text(colour = ink2),
        plot.caption = element_text(colour = ink2, hjust = 0, size = 9),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "top", legend.justification = "left",
        legend.text = element_text(colour = ink),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2, size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.4),
        plot.background = element_rect(fill = "white", colour = NA))
ggsave("output/fig_weekend_scatter.png", fig, width = 8, height = 8.4, dpi = 300,
       device = ragg::agg_png)
