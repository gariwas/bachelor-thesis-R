library(tensorTS)
library(vars)
# ── LOAD ──────────────────────────────────────────────────────────────────────
xx_diff <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")
est     <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds")
# If you haven't saved est yet, add this to 03_modelling.R:
# saveRDS(est, "C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds")

T_val  <- dim(xx_diff)[1]
states  <- c("IL", "OH", "MI")
sectors <- c("MFG", "CONS", "SMS")

# ── MAR(1) RSS ────────────────────────────────────────────────────────────────
rss_mar <- sum(est$res^2)
cat("MAR(1) RSS:", round(rss_mar, 1), "\n")
cat("MAR(1) parameters: m^2 + n^2 - 1 =", 3^2 + 3^2 - 1, "\n")

# ── INDIVIDUAL AR(1) and AR(2) ────────────────────────────────────────────────
rss_iar1 <- 0
rss_iar2 <- 0

for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    y <- xx_diff[, i, j]
    
    # AR(1)
    fit1 <- ar(y, order.max = 1, AIC = FALSE, method = "ols")
    rss_iar1 <- rss_iar1 + sum(fit1$resid^2, na.rm = TRUE)
    
    # AR(2)
    fit2 <- ar(y, order.max = 2, AIC = FALSE, method = "ols")
    rss_iar2 <- rss_iar2 + sum(fit2$resid^2, na.rm = TRUE)
  }
}

cat("Individual AR(1) total RSS:", round(rss_iar1, 1), "\n")
cat("Individual AR(2) total RSS:", round(rss_iar2, 1), "\n")
cat("iAR(1) parameters: 9 x 1 =", 9 * 1, "\n")
cat("iAR(2) parameters: 9 x 2 =", 9 * 2, "\n")

# ── STACKED VAR(1) ────────────────────────────────────────────────────────────
# Vectorize the tensor: each row is a 9-dim observation
Y_mat <- matrix(xx_diff, nrow = T_val, ncol = 9)   # [T x 9]
colnames(Y_mat) <- paste0(rep(states, each = 3), rep(sectors, 3))

var_fit <- VAR(Y_mat, p = 1, type = "none")
rss_var <- sum(sapply(var_fit$varresult, function(eq) sum(resid(eq)^2)))

cat("Stacked VAR(1) RSS:", round(rss_var, 1), "\n")
cat("VAR(1) parameters: (mn)^2 =", 9^2, "\n")  # 81 vs MAR's 17

# ── SUMMARY TABLE ─────────────────────────────────────────────────────────────
results <- data.frame(
  Model      = c("MAR(1)", "iAR(1)", "iAR(2)", "VAR(1)"),
  Parameters = c(3^2 + 3^2 - 1, 9, 18, 9^2),
  RSS        = round(c(rss_mar, rss_iar1, rss_iar2, rss_var), 1)
)
print(results)