# 07_lag_order_selection.R

library(tensorTS)

xx_diff <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")
T <- dim(xx_diff)[1]; m <- dim(xx_diff)[2]; n <- dim(xx_diff)[3]

cat("P | BIC (from package)\n")
cat("------------------------\n")

for (P in 1:3) {
  fit <- tenAR.est(xx_diff, R = 1, P = P, method = "LSE")
  cat("P =", P, "| BIC =", round(fit$BIC, 4), "\n")
}