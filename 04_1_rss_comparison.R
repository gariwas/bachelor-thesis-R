library(tensorTS)
library(vars)

# ── PATHS ─────────────────────────────────────────────────────────────────────
rdspath_tensor  <- "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds"
rdspath_mar1    <- "C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds"
rdspath_mar2    <- "C:/Users/Igor/Desktop/bachelor-thesis-R/est_lse_p2.rds"

# ── LOAD ──────────────────────────────────────────────────────────────────────
xxdiff <- readRDS(rdspath_tensor)
est1   <- readRDS(rdspath_mar1)   # MAR(1) LSE fit
est2   <- readRDS(rdspath_mar2)   # MAR(2) LSE fit

Tval    <- dim(xxdiff)[1]
m       <- 5   # states
n       <- 5   # sectors
states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")

# ── MAR(1) RSS ────────────────────────────────────────────────────────────────
rssmar1 <- sum(est1$res^2)
cat("MAR(1) RSS:", round(rssmar1, 1), "\n")
cat("MAR(1) parameters:", m^2 + n^2 - 1, "\n")   # 50

# ── MAR(2) RSS ────────────────────────────────────────────────────────────────
rssmar2 <- sum(est2$res^2)
cat("MAR(2) RSS:", round(rssmar2, 1), "\n")
cat("MAR(2) parameters:", 2 * (m^2 + n^2 - 1), "\n")   # 100

# ── INDIVIDUAL AR(1) and AR(2) RSS ───────────────────────────────────────────
rssiar1 <- 0
rssiar2 <- 0
for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    y    <- xxdiff[, i, j]
    fit1 <- ar(y, order.max = 1, AIC = FALSE, method = "ols")
    fit2 <- ar(y, order.max = 2, AIC = FALSE, method = "ols")
    rssiar1 <- rssiar1 + sum(fit1$resid^2, na.rm = TRUE)
    rssiar2 <- rssiar2 + sum(fit2$resid^2, na.rm = TRUE)
  }
}
cat("Individual AR(1) total RSS:", round(rssiar1, 1), "\n")
cat("Individual AR(2) total RSS:", round(rssiar2, 1), "\n")
cat("iAR(1) parameters:", m * n * 1, "\n")   # 25
cat("iAR(2) parameters:", m * n * 2, "\n")   # 50

# ── STACKED VAR(1) and VAR(2) RSS ────────────────────────────────────────────
Ymat <- matrix(xxdiff, nrow = Tval, ncol = m * n)
colnames(Ymat) <- paste0(rep(states, each = n), "_", rep(sectors, m))

varfit1 <- VAR(Ymat, p = 1, type = "none")
rssvar1 <- sum(sapply(varfit1$varresult, function(eq) sum(resid(eq)^2)))
cat("Stacked VAR(1) RSS:", round(rssvar1, 1), "\n")
cat("VAR(1) parameters:", (m * n)^2 * 1, "\n")   # 625

varfit2 <- VAR(Ymat, p = 2, type = "none")
rssvar2 <- sum(sapply(varfit2$varresult, function(eq) sum(resid(eq)^2)))
cat("Stacked VAR(2) RSS:", round(rssvar2, 1), "\n")
cat("VAR(2) parameters:", (m * n)^2 * 2, "\n")   # 1250

# ── SUMMARY TABLE ─────────────────────────────────────────────────────────────
results <- data.frame(
  Model      = c("MAR(1)", "MAR(2)", "iAR(1)", "iAR(2)", "VAR(1)", "VAR(2)"),
  Parameters = c(m^2 + n^2 - 1,
                 2 * (m^2 + n^2 - 1),
                 m * n * 1,
                 m * n * 2,
                 (m * n)^2 * 1,
                 (m * n)^2 * 2),
  RSS        = round(c(rssmar1, rssmar2, rssiar1, rssiar2, rssvar1, rssvar2), 1)
)
print(results)