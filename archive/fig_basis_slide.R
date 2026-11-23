# fig_basis_slide.R
# Slide figure: how far xStock token prices sit from the real stock during
# US trading hours. One row per token: dot = median absolute gap,
# bar = range up to the 95th percentile. Dashed line = 0.5%, an ILLUSTRATIVE
# threshold (not an estimated trading cost; Kraken fees unverified).
# Run basis_minute.R first.

library(data.table); library(ggplot2); library(scales)

b <- readRDS("output/basis_minute.rds")

s <- b[, .(med = median(abs(basis)),
           p95 = quantile(abs(basis), 0.95),
           out = mean(abs(basis) > 0.5),
           n   = .N), by = symbol]
s[, label := paste0(symbol, "x")]
s[, label := factor(label, levels = label[order(-med)])]  # smallest gap on top

blue  <- "#2a78d6"   # reference palette, categorical slot 1
ink   <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

p <- ggplot(s, aes(y = label)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = ink2, linewidth = 0.5) +
  annotate("text", x = 0.5, y = 14.65, label = "0.5% reference line",
           hjust = -0.05, size = 3.4, colour = ink2) +
  geom_segment(aes(x = 0.01, xend = p95, yend = label),
               colour = "#b9cfee", linewidth = 2.6, lineend = "round") +
  geom_point(aes(x = med), colour = blue, size = 3.4) +
  geom_text(aes(x = 4.2, label = percent(out, accuracy = 1)),
            hjust = 1, size = 3.6, colour = ink) +
  annotate("text", x = 4.2, y = 15.1, label = "minutes\n> 0.5%",
           hjust = 1, vjust = 0, size = 3.1, colour = ink2, lineheight = 0.9) +
  scale_x_log10(limits = c(0.01, 4.2),
                breaks = c(0.01, 0.03, 0.1, 0.3, 1, 3),
                labels = function(x) paste0(x, "%")) +
  scale_y_discrete() +
  coord_cartesian(clip = "off") +
  labs(title = "Absolute Gaps Between Tokenized Stocks and Underlying Stocks",
       subtitle = "Gap between xStock trade price and stock price, same minute (absolute %).\nDot = median, bar = 95th percentile.",
       x = "Absolute gap, % (log scale)", y = NULL,
       caption = ("")) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", colour = ink, size = 15),
        plot.subtitle = element_text(colour = ink2, margin = margin(b = 18)),
        plot.caption = element_text(colour = ink2, hjust = 0, size = 9),
        plot.title.position = "plot", plot.caption.position = "plot",
        axis.text.y = element_text(colour = ink, size = 11),
        axis.text.x = element_text(colour = ink2),
        axis.title.x = element_text(colour = ink2, size = 10, margin = margin(t = 8)),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.4),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(14, 18, 10, 14))

ggsave("output/fig_basis_slide.png", p, width = 10, height = 6.2, dpi = 300,
       device = ragg::agg_png)
