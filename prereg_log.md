# Pre-registration log

Rules written down BEFORE looking at the results they govern. Append only;
if a rule changes later, add a dated entry saying what changed and why.

## 2026-11-18: Weekend / holiday tracker (off-hours side question)

Written before any weekend returns were computed. Scripts: `weekend_panel.R`,
`btc_checkpoints.R`, `weekend_tracker.R`.

**Framing.** This is NOT an arbitrage test. While the stock market is closed the
stock leg cannot be traded, so no one can lock in a gap. What we measure is
(a) how far the token drifts away from the last tradable stock price while
unhedged, and (b) whether that drift forecasts where the stock opens.

**Windows.**
- Trading days = dates with regular-session (09:30-15:59 ET) bars in the
  Databento file. Holidays come from the data, not from a hand-typed calendar.
- A "closed window" = two consecutive trading days more than 1 calendar day
  apart (weekends, plus holiday-lengthened weekends and mid-week holidays).
  Consecutive-day pairs are kept as "overnight" rows but are not in the main test.
- Sample ends with the last window whose next open is on or before 2026-10-09.

**Checkpoints (all in America/New_York time).**
- C0 = close of the last trading day: 16:00, or 13:00 on early-close days.
  Early closes: 2025-11-28 and 2025-12-24 (confirmed in the bar data: ~210
  regular bars and the closing volume spike in the 12:59 bar). 2026-07-02 was a
  full day in the data.
- K = 20:00 on the calendar day before the next open (Sunday 20:00 for a plain
  weekend). Chosen because extended-hours stock trading has ended and overnight
  stock venues reportedly start around Sunday 20:00 [unverified]; token activity
  jumps at Sunday 20:00 in our data, consistent with that.
- O1 = 09:30 open of the next trading day.
- P = 09:29 pre-open (used only as a contrast; it contains pre-market stock info).

**Prices.**
- Stock close = close of the 15:59 bar (12:59 on early-close days). Stock open =
  open of the 09:30 bar. Stock pre-open = close of the last bar 04:00-09:29.
  These are Databento bar prices, NOT the official auction prints [limitation].
- Token at C0: last trade at or before C0, must be at most 15 minutes old.
- Token at K: last trade at or before K, must be at most 120 minutes old
  (weekend trading is thin, ~1 trade/hour/token on Saturdays).
- Token at P: last trade at or before 09:30, at most 15 minutes old.
- BTC (Kraken XBTUSD spot): last trade at or before C0 and K.

**Main specification.** Weekend + holiday windows only. Returns in log percent.
  r_stk_gap = 100 * log(stock open / stock close)
  r_tok     = 100 * log(token at K / token at C0)
  Regression 1: r_stk_gap ~ r_tok
  Regression 2: r_stk_gap ~ r_tok + r_btc + r_mkt, where r_mkt is SPYx's own
  r_tok in the same window (SPY and QQQ rows dropped in this spec).
  Regression 3: as 2, with r_tok interacted with a crypto-linked dummy
  (COIN, MSTR, CRCL, HOOD).
  Standard errors clustered by window (about 60 clusters). Test slope = 0
  (token move is uninformative) and slope = 1 (token move fully passes through).

**Hypothesis (from CLAUDE.md H3).** Some predictive content (slope > 0), but
much of the token's weekend move may be BTC / crypto sentiment. A null is an
acceptable result.

**Robustness (planned, not run yet).** K staleness 30 min; K = Monday 04:00;
drop Aug-Oct 2025 (thin early months); token-to-stock drift
100 * log(token at K / stock close) as the regressor instead of r_tok.

**Data checks.** Windows with |r_stk_gap| or |r_tok| above 25% are flagged and
printed as possible splits or bad prints before any regression.

## 2026-11-18 (later the same day): main spec changed before seeing any returns

Reason: data availability only. No weekend returns or regressions had been
computed. With the rules above only 197 of 882 token-weekends were usable.
- AMZN, COIN, INTC, META, MRVL dropped from the main sample: median of 0 to 4
  token trades over an entire closed window, in every month including Sept-Oct
  2026. Reported as a descriptive finding (liquidity vanishes when the stock
  cannot be hedged; cause not established, could be a venue restriction).
- Main regressor is now drift_K = 100 * log(token at K / stock close), i.e. the
  Sunday-20:00 token price as a forecast of the open relative to Friday's
  close. It does not need a fresh token price at the close. The token-to-token
  move r_tok (15-min freshness at C0) becomes a robustness check.
- K staleness rule unchanged (120 min). Crypto-linked group is now MSTR, CRCL, HOOD.

## 2026-11-23: Intraday basis revisited: spread vs gap, no-arbitrage band, persistence

Written before running `basis_mid.R`. **Disclosure:** the motivation is data-driven.
While planning, a look at `output/basis_minute.rds` showed that from roughly
2025-10-15 to 2025-11-21 buy trades print ~+0.5% and sell trades ~-0.5% vs the
stock, i.e. the trade-price "basis" in that period is mostly half the quoted
spread. The episode dates below were read off that output; they are
data-identified, not pre-specified. Everything else is fixed now, before results.

**Side-specific gaps.** For each token-minute and each side (b = buyer-initiated,
s = seller-initiated), take the last trade of that side in the minute and compare
it to the stock 1-min bar close of the same minute: g = 100*log(token/stock).
(Log percent from here on; the old `basis_minute.R` used simple percent.)

**Midpoint gap.** At each minute where the token traded, mid_gap = average of the
most recent buy gap and most recent sell gap, each measured against the stock in
its own minute and each at most 5 minutes old. Minutes without both sides inside
5 minutes get no mid_gap.

**Half-spread (effective, from trades).** Per token-month:
(median buy gap - median sell gap) / 2. Includes stock-move noise within a minute.

**Inside-range check.** Share of ALL token trades whose price lies inside the
stock's [low, high] for the same 1-min bar. Reported by side.

**Episode dummy.** episode = 2025-10-15 to 2025-11-21 inclusive (data-identified).

**Dividend / trend check.** Monthly median mid_gap per token, plotted. A steady
upward drift for SPY/QQQ/AAPL would suggest a total-return token design.
Interpretation only; no test.

**No-arbitrage band (executable prices).** A minute is "outside the band" if an
actual token trade could have been the entry leg of a profitable round trip at
the observed prices:
  buy-token arb:  buy gap  < -c      sell-token arb:  sell gap > +c
with c = 2 * fee + h, where fee is a per-trade token fee and h the token's
half-spread in that month (cost to exit when the gap closes). Stock-side costs
assumed zero (favours finding arbitrage). Fee grid: 0%, 0.10%, 0.25%.
0.10% is the Kraken Pro taker fee per a secondary source (Datawallet, Aug 2026)
[secondary, unverified on kraken.com]. Ignores that US persons cannot trade
xStocks, latency, and short-sale constraints on either leg. Report the share of
minutes and of token-days with at least one such minute, by token and episode.

**Persistence.** 15-minute blocks within the regular session (09:30-16:00 ET, 26
blocks/day). Block value = median mid_gap. AR(1) per token, lag only within the
same day (no overnight lag), episode excluded in the main spec and shown
separately. Half-life (minutes) = 15 * log(0.5) / log(rho) when 0 < rho < 1.
Days: same sample rules as `basis_minute.R` (end 2026-10-09). Early-close days:
2025-11-28 and 2025-12-24 only (2026-07-02 was a full day per the bar counts,
fixing the inconsistency in `basis_minute.R`).

## 2026-11-23 (later): changes after the first run of `basis_mid.R` (POST-HOC)

These changes were made AFTER seeing the first output. Both versions are reported.
1. Band test, stricter version added. The pre-registered test compares each
   token trade to the stock's 1-min bar CLOSE, so ordinary stock movement inside
   the minute shows up as a "gap" (fee = 0 flagged 24-54% of minutes, which is
   not credible as arbitrage). New conservative version: a buy trade counts only
   if it is below the stock's minute LOW by more than c; a sell trade only if it
   is above the minute HIGH by more than c. The pre-registered version stays in
   `band.csv` as `out_close`; the new one is `out_range`.
2. Missing half-spreads (a token-month with trades on only one side) are filled
   with that token's median half-spread across months. Mechanical fix; it only
   affected COIN.

## 2026-11-23: Determinants of the intraday gap (step 4)

Written before running `determinants.R`. Uses the midpoint gap from `basis_mid.R`.

**Unit.** Token-day (regular session, same sample rules as `basis_mid.R`). Keep
token-days with at least 10 midpoint minutes.

**Outcome.** abs_gap = median |mid_gap| over the day (log %). Robustness: log(abs_gap).

**Regressors (hypothesis 2: larger gaps for thinner, wider, more volatile).**
- log_trades: log of the token's regular-session trade count that day (expect < 0)
- rv: stock realized volatility that day, sqrt(sum of squared 1-min log
  returns) in %, from the Databento bars (expect > 0)
- half_spread: that day's median of (g_b - g_s)/2 over midpoint minutes (expect > 0).
  Caveat: built from the same token prices as the outcome, and noisy when spreads
  are tiny (see 2026-11-23 post-hoc note). Spec without it is also reported.
- open_share: share of the day's token trades in 09:30-10:00 ET (expect > 0;
  opening minutes are the most volatile)
- episode: 2025-10-15 to 2025-11-21 dummy (data-identified; expect > 0)
"Staleness" from the original plan is dropped: matching is within one minute
by construction.

**Specs.**
(1) token FE, SE clustered by date. (2) = (1) without half_spread.
(3) token + date FE (episode absorbed), SE clustered by date.
(4) = (3) with two-way clustering by date and token (only 14 token clusters:
    treat as a rough check). (5) = (3) with log(abs_gap).
Main spec = (3): date FE remove common shocks (crypto moves, market news), so
coefficients come from differences across tokens on the same day.
Inference: 5% two-sided. No other specs to be added to the main table.

## 2026-11-23: Weekend test follow-ups (step 5)

Written before running `weekend_robust.R`. The main weekend results
(`output/weekend_regressions.txt`) HAVE been seen, so these checks are chosen
knowing the main slope (0.48) and the loss of significance with controls.
Nothing below changes the main specification.

1. Same-sample comparison: regression 1 (no controls) rerun on exactly the rows
   used by regression 2 (BTC and SPYx available, SPY/QQQ rows dropped).
2. The four robustness checks promised on 2026-11-18, with drift_K as main
   regressor: (a) K staleness 30 min instead of 120; (b) K = Monday 04:00 ET
   (i.e. 04:00 on the next-open day), staleness 120 min, no-control spec only
   (no BTC price at that time); (c) drop windows with d_prev before 2025-11-01;
   (d) r_tok (token close-to-K move) as regressor instead of drift_K.
3. Spread-noise check: token price at K = average of the last buy and last sell
   trade at or before K, each at most 120 min old (bid/ask bounce in a single
   trade biases the slope toward zero).
4. Forecast benchmark, main sample: mean squared error of the stock gap under
   (i) naive forecast 0 (stock opens at Friday's close), (ii) drift_K taken
   one-for-one, (iii) slope*drift_K with the slope estimated leaving out that
   window (out-of-sample). Test MSE differences with a regression of the
   per-row squared-error difference on a constant, SE clustered by window.
5. Leave-one-window-out: re-estimate regression 1 dropping each window in turn;
   report the slope range and the most influential window.
Each spec: slope, SE clustered by window, p (slope = 0), t (slope = 1), N, clusters.

## 2026-11-23: Housekeeping (no rule changes)

Scripts renamed and numbered in run order (see README.md); entries above use
the old names: weekend_panel.R -> 20_weekend_panel.R, btc_checkpoints.R ->
21_btc_checkpoints.R, weekend_tracker.R -> 22_weekend_tracker.R,
basis_mid.R -> 10_basis_mid.R, determinants.R -> 11_determinants.R,
weekend_robust.R -> 23_weekend_robust.R. Untitled.R was split into
01_clean_stock.R, 02_clean_kraken.R, 03_explore_coverage.R (code unchanged;
original kept in archive/). basis_minute.R and fig_basis_slide.R moved to archive/
(superseded by the midpoint measure).
Databento stock bars were obtained with free-trial credits (user, 2026-11-23).

## 2026-11-23: archive/ removed

The superseded scripts (basis_minute.R, fig_basis_slide.R, and the original
Untitled.R) were deleted. They remain in the git history (first commit, 51e5cc4).
