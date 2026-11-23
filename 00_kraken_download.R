# 00_kraken_download.R
# Downloads every trade for the 14 xStock tokens from Kraken's public Trades
# API (no key needed). LONG NETWORK JOB: already completed on 2026-10-10 (all 14
# tokens have a .done file). Rerunning skips finished tokens, so it is safe,
# but there is no need to run it again.
#
# Output per token in kraken_data/: <PAIR>.csv (raw trades, never edit by hand),
# <PAIR>.cursor (resume point), <PAIR>.done (finished flag).

library(jsonlite)

# ---- CONFIG: values used for the real run (test run used 3 calls, OUT_DIR "kraken_test") ----
MAX_CALLS_PER_TOKEN <- Inf
OUT_DIR <- "kraken_data"
# REAL RUN: use these instead (MAX_CALLS_PER_TOKEN <- Inf, OUT_DIR <- "kraken_data")
TOKENS <- c("COINxUSD","INTCxUSD","MSTRxUSD","CRCLxUSD","AMZNxUSD","GOOGLxUSD","MRVLxUSD",
             "NVDAxUSD","HOODxUSD","TSLAxUSD","SPYxUSD","AAPLxUSD","METAxUSD","QQQxUSD")
# ------------------------------------------------------------------------------

dir.create(OUT_DIR, showWarnings = FALSE)

fetch <- function(pair, since) {
  u <- paste0("https://api.kraken.com/0/public/Trades?pair=", pair,
              "&asset_class=tokenized_asset&count=1000&since=", since)
  for (attempt in 1:6) {
    Sys.sleep(1.2)
    r <- tryCatch(fromJSON(u), error = function(e) NULL)
    if (!is.null(r) && length(r$error) == 0) return(r)
    cat("  retry", attempt, if (!is.null(r)) paste(r$error, collapse = "; ") else "network error", "\n")
    Sys.sleep(15 * attempt)
  }
  stop("Gave up after repeated errors. Rerun the script: it resumes where it stopped.")
}

for (pair in TOKENS) {
  csv   <- file.path(OUT_DIR, paste0(pair, ".csv"))
  state <- file.path(OUT_DIR, paste0(pair, ".cursor"))
  done  <- file.path(OUT_DIR, paste0(pair, ".done"))
  if (file.exists(done)) { cat(pair, "already complete\n"); next }
  since <- if (file.exists(state)) readLines(state, n = 1) else "0"
  n <- 0
  cat("Starting", pair, "from cursor", since, "\n")
  repeat {
    r <- fetch(pair, since)
    m <- r$result[[setdiff(names(r$result), "last")[1]]]
    if (is.null(m) || NROW(m) == 0) { file.create(done); break }
    df <- data.frame(price = m[, 1], vol = m[, 2], time = m[, 3], side = m[, 4],
                     type = m[, 5], id = if (ncol(m) >= 7) m[, 7] else NA,
                     stringsAsFactors = FALSE)
    write.table(df, csv, append = file.exists(csv), sep = ",", row.names = FALSE,
                col.names = !file.exists(csv), quote = FALSE)
    new_since <- as.character(r$result$last)
    if (length(new_since) != 1 || is.na(new_since)) stop("No paging cursor in response; paste me str(r$result$last)")
    writeLines(new_since, state)
    n <- n + 1
    if (n %% 20 == 0 || n <= 3)
      cat(pair, "| call", n, "| reached",
          format(as.POSIXct(as.numeric(tail(m[, 3], 1)), origin = "1970-01-01", tz = "UTC")), "\n")
    if (NROW(m) < 1000 || new_since == since) { file.create(done); break }
    since <- new_since
    if (n >= MAX_CALLS_PER_TOKEN) break
  }
}
cat("Finished.\n")