# =============================================================================
# 04_backtest_mar_vs_var.R
# Rolling-window walk-forward backtest: MAR(1) vs VAR(1, vectorized 25-dim)
# Forecast horizon: h = 1
# Metric: Frobenius norm MSFE, per-series RMSE, Diebold-Mariano test
# =============================================================================

library(tensorTS)
library(vars)
library(forecast)   # dm.test

# ---------------------------------------------------------------------------
# PATHS & CONFIG
# ---------------------------------------------------------------------------
tensor_path <- "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds"
results_dir <- "C:/Users/Igor/Desktop/bachelor-thesis-R/backtest/"
dir.create(results_dir, showWarnings = FALSE)

states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")
N_states   <- length(states)   # 5
N_sectors  <- length(sectors)  # 5
N_series   <- N_states * N_sectors  # 25

# ---------------------------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------------------------
xx <- readRDS(tensor_path)
T_total <- dim(xx)[1]
cat("Tensor dimensions:", dim(xx), "\n")
cat("Total time points:", T_total, "\n")

# ---------------------------------------------------------------------------
# ROLLING WINDOW SETUP
# ---------------------------------------------------------------------------
train_size <- floor(0.70 * T_total)
test_start <- train_size + 1
test_end   <- T_total - 1  # need t+1 for h=1 target

n_windows  <- test_end - test_start + 1
cat("Training window size:", train_size, "\n")
cat("Number of OOS windows:", n_windows, "\n")

# ---------------------------------------------------------------------------
# STORAGE
# ---------------------------------------------------------------------------
frob_mar  <- numeric(n_windows)
frob_var  <- numeric(n_windows)
e_mar     <- matrix(NA, n_windows, N_series)
e_var     <- matrix(NA, n_windows, N_series)

# ---------------------------------------------------------------------------
# ROLLING WALK-FORWARD LOOP
# ---------------------------------------------------------------------------
cat("\nStarting rolling backtest...\n")

for (w in seq_len(n_windows)) {
  t_train_end <- train_size + w - 1
  t_target    <- t_train_end + 1
  
  xx_train <- xx[w:t_train_end, , ]
  true_mat <- matrix(as.numeric(xx[t_target, , ]), nrow = N_states)
  true_vec <- as.vector(true_mat)
  
  # ==== MAR(1) ====
  mar_fit <- tryCatch(tenAR.est(xx_train, R = 1, P = 1, method = "LSE"),
                      error = function(e) NULL)
  
  if (!is.null(mar_fit)) {
    pred_arr <- predict(mar_fit, n.ahead = 1)
    pred_mar <- matrix(as.numeric(pred_arr[1, , ]), nrow = N_states)
  } else {
    pred_mar <- matrix(0, N_states, N_sectors)
  }
  
  err_mar     <- true_mat - pred_mar
  frob_mar[w] <- norm(err_mar, type = "F")^2
  e_mar[w, ]  <- as.vector(err_mar)
  
  # ==== VAR(1) — vectorized 25-dim ====
  Y_train <- t(apply(xx_train, 1, function(sl) as.vector(sl)))
  
  var_fit <- tryCatch(VAR(Y_train, p = 1, type = "none"),
                      error = function(e) NULL)
  
  if (!is.null(var_fit)) {
    pred_var_vec <- sapply(predict(var_fit, n.ahead = 1)$fcst,
                           function(x) x[1, "fcst"])
  } else {
    pred_var_vec <- rep(0, N_series)
  }
  
  err_var_vec  <- true_vec - pred_var_vec
  frob_var[w]  <- sum(err_var_vec^2)
  e_var[w, ]   <- err_var_vec
  
  if (w %% 10 == 0) cat("  Window", w, "/", n_windows, "\n")
}

cat("\nBacktest complete.\n")

# ---------------------------------------------------------------------------
# AGGREGATE METRICS
# ---------------------------------------------------------------------------
msfe_mar <- mean(frob_mar)
msfe_var <- mean(frob_var)

rmse_mar <- sqrt(colMeans(e_mar^2))
rmse_var <- sqrt(colMeans(e_var^2))

series_labels <- paste0(rep(states, each = N_sectors), "_",
                        rep(sectors, times = N_states))

results_df <- data.frame(
  Series     = series_labels,
  RMSE_MAR   = round(rmse_mar, 4),
  RMSE_VAR   = round(rmse_var, 4),
  RMSE_ratio = round(rmse_mar / rmse_var, 4)  # <1 = MAR wins
)

cat("\n=== AGGREGATE FROBENIUS MSFE ===\n")
cat(sprintf("MAR(1): %.4f\n", msfe_mar))
cat(sprintf("VAR(1): %.4f\n", msfe_var))
cat(sprintf("MSFE ratio (MAR/VAR): %.4f  [<1 = MAR wins]\n", msfe_mar / msfe_var))

cat("\n=== PER-SERIES RMSE ===\n")
print(results_df)

# ---------------------------------------------------------------------------
# DIEBOLD-MARIANO TEST
# ---------------------------------------------------------------------------
dm_result <- dm.test(e1 = frob_mar - frob_var,
                     e2 = rep(0, n_windows),
                     alternative = "less",   # H1: MAR loss < VAR loss
                     h = 1, power = 2)
cat("\n=== DIEBOLD-MARIANO TEST (H1: MAR < VAR) ===\n")
print(dm_result)

# ---------------------------------------------------------------------------
# SAVE RESULTS
# ---------------------------------------------------------------------------
write.csv(results_df,
          paste0(results_dir, "per_series_rmse.csv"), row.names = FALSE)

write.csv(data.frame(Window = seq_len(n_windows),
                     Loss_MAR = frob_mar, Loss_VAR = frob_var),
          paste0(results_dir, "rolling_losses.csv"), row.names = FALSE)

saveRDS(list(msfe_mar = msfe_mar, msfe_var = msfe_var,
             frob_mar = frob_mar, frob_var = frob_var,
             e_mar = e_mar, e_var = e_var,
             results_df = results_df, dm_result = dm_result),
        paste0(results_dir, "backtest_results.rds"))

cat("\nAll results saved to:", results_dir, "\n")

# ---------------------------------------------------------------------------
# PLOTS
# ---------------------------------------------------------------------------
# 1. Rolling loss over time
png(paste0(results_dir, "rolling_loss_comparison.png"),
    width = 10, height = 5, units = "in", res = 300)
par(mar = c(4, 4, 3, 2))
plot(frob_mar, type = "l", col = "steelblue", lwd = 1.5,
     ylim = range(c(frob_mar, frob_var)),
     xlab = "OOS Window", ylab = "Squared Frobenius Error",
     main = "Rolling OOS Loss: MAR(1) vs VAR(1)")
lines(frob_var, col = "firebrick", lwd = 1.5)
legend("topright", legend = c("MAR(1)", "VAR(1)"),
       col = c("steelblue", "firebrick"), lwd = 2, bty = "n")
dev.off()

# 2. Per-series RMSE ratio bar chart
png(paste0(results_dir, "rmse_ratio_by_series.png"),
    width = 10, height = 6, units = "in", res = 300)
par(mar = c(6, 4, 3, 2))
colors <- ifelse(results_df$RMSE_ratio < 1, "steelblue", "firebrick")
barplot(results_df$RMSE_ratio,
        names.arg = results_df$Series, las = 2, cex.names = 0.75,
        col = colors,
        ylab = "RMSE Ratio (MAR / VAR)",
        main = "Per-Series RMSE Ratio: MAR(1) / VAR(1)\n[Blue <1 = MAR wins, Red >1 = VAR wins]")
abline(h = 1, lty = 2, col = "gray40")
dev.off()

cat("\nPlots saved.\n")

