library(tensorTS)
library(vars)

# ── LOAD ──────────────────────────────────────────────────────────────────────
xx_diff <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")
est     <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds")

T_val   <- dim(xx_diff)[1]
m       <- 5   # states
n       <- 5   # sectors
states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")

# ── MAR(1) RSS ────────────────────────────────────────────────────────────────
rss_mar <- sum(est$res^2)
cat("MAR(1) RSS:", round(rss_mar, 1), "\n")
cat("MAR(1) parameters: m^2 + n^2 - 1 =", m^2 + n^2 - 1, "\n")

# ── INDIVIDUAL AR(1) and AR(2) ────────────────────────────────────────────────
rss_iar1 <- 0
rss_iar2 <- 0

for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    y <- xx_diff[, i, j]
    
    fit1 <- ar(y, order.max = 1, AIC = FALSE, method = "ols")
    rss_iar1 <- rss_iar1 + sum(fit1$resid^2, na.rm = TRUE)
    
    fit2 <- ar(y, order.max = 2, AIC = FALSE, method = "ols")
    rss_iar2 <- rss_iar2 + sum(fit2$resid^2, na.rm = TRUE)
  }
}

cat("Individual AR(1) total RSS:", round(rss_iar1, 1), "\n")
cat("Individual AR(2) total RSS:", round(rss_iar2, 1), "\n")
cat("iAR(1) parameters: m*n x 1 =", m * n * 1, "\n")
cat("iAR(2) parameters: m*n x 2 =", m * n * 2, "\n")

# ── STACKED VAR(1) ────────────────────────────────────────────────────────────
Y_mat <- matrix(xx_diff, nrow = T_val, ncol = m * n)
colnames(Y_mat) <- paste0(rep(states, each = n), "_", rep(sectors, m))

var_fit <- VAR(Y_mat, p = 1, type = "none")
rss_var <- sum(sapply(var_fit$varresult, function(eq) sum(resid(eq)^2)))

cat("Stacked VAR(1) RSS:", round(rss_var, 1), "\n")
cat("VAR(1) parameters: (m*n)^2 =", (m * n)^2, "\n")

# ── SUMMARY TABLE ─────────────────────────────────────────────────────────────
results <- data.frame(
  Model      = c("MAR(1)", "iAR(1)", "iAR(2)", "VAR(1)"),
  Parameters = c(m^2 + n^2 - 1, m * n, 2 * m * n, (m * n)^2),
  RSS        = round(c(rss_mar, rss_iar1, rss_iar2, rss_var), 1)
)
print(results)