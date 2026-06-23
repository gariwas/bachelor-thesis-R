# =============================================================================
# 05_backtest_mar1_mar2_vs_var1.R
# Rolling-window walk-forward backtest: MAR(1), MAR(2) vs VAR(1)
# Forecast horizon: h = 1
# Metric: Frobenius norm MSFE, per-series RMSE, Diebold-Mariano test
# =============================================================================

library(tensorTS)
library(vars)
library(forecast)

# ── PATHS ─────────────────────────────────────────────────────────────────────
tensor_path <- "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds"
results_dir <- "C:/Users/Igor/Desktop/bachelor-thesis-R/backtest/"
dir.create(results_dir, showWarnings = FALSE)

states    <- c("IL", "OH", "MI", "IN", "KY")
sectors   <- c("MFG", "CONS", "RET", "GOVT", "FIRE")
N_states  <- length(states)
N_sectors <- length(sectors)
N_series  <- N_states * N_sectors

# ── LOAD DATA ─────────────────────────────────────────────────────────────────
xx      <- readRDS(tensor_path)
T_total <- dim(xx)[1]
cat("Tensor dimensions:", dim(xx), "\n")
cat("Total time points:", T_total, "\n")

# ── ROLLING WINDOW SETUP ──────────────────────────────────────────────────────
train_size <- floor(0.70 * T_total)
test_start <- train_size + 1
test_end   <- T_total - 1

n_windows <- test_end - test_start + 1
cat("Training window size:", train_size, "\n")
cat("Number of OOS windows:", n_windows, "\n")

# ── STORAGE ───────────────────────────────────────────────────────────────────
frob_mar1 <- numeric(n_windows)
frob_mar2 <- numeric(n_windows)
frob_var  <- numeric(n_windows)
e_mar1    <- matrix(NA, n_windows, N_series)
e_mar2    <- matrix(NA, n_windows, N_series)
e_var     <- matrix(NA, n_windows, N_series)
mar1_failures <- 0
mar2_failures <- 0
var_failures  <- 0

# ── ROLLING WALK-FORWARD LOOP ─────────────────────────────────────────────────
cat("\nStarting rolling backtest...\n")

for (w in seq_len(n_windows)) {
  t_train_end <- train_size + w - 1
  t_target    <- t_train_end + 1
  
  xx_train <- xx[w:t_train_end, , ]
  true_mat <- matrix(as.numeric(xx[t_target, , ]), nrow = N_states)
  true_vec <- as.vector(true_mat)
  
  # ==== MAR(1) ====
  mar1_fit <- tryCatch(
    tenAR.est(xx_train, R = 1, P = 1, method = "LSE"),
    error = function(e) NULL
  )
  
  if (!is.null(mar1_fit)) {
    pred_arr1 <- predict(mar1_fit, n.ahead = 1)
    pred_mar1 <- matrix(as.numeric(pred_arr1[1, , ]), nrow = N_states)
  } else {
    pred_mar1 <- matrix(0, N_states, N_sectors)
    mar1_failures <- mar1_failures + 1
  }
  
  err_mar1      <- true_mat - pred_mar1
  frob_mar1[w]  <- norm(err_mar1, type = "F")^2
  e_mar1[w, ]   <- as.vector(err_mar1)
  
  # ==== MAR(2) ====
  mar2_fit <- tryCatch(
    tenAR.est(xx_train, R = 1, P = 2, method = "LSE"),
    error = function(e) NULL
  )
  
  if (!is.null(mar2_fit)) {
    pred_arr2 <- predict(mar2_fit, n.ahead = 1)
    pred_mar2 <- matrix(as.numeric(pred_arr2[1, , ]), nrow = N_states)
  } else {
    pred_mar2 <- matrix(0, N_states, N_sectors)
    mar2_failures <- mar2_failures + 1
  }
  
  err_mar2      <- true_mat - pred_mar2
  frob_mar2[w]  <- norm(err_mar2, type = "F")^2
  e_mar2[w, ]   <- as.vector(err_mar2)
  
  # ==== VAR(1) — vectorized 25-dim ====
  Y_train <- t(apply(xx_train, 1, function(sl) as.vector(sl)))
  
  var_fit <- tryCatch(
    VAR(Y_train, p = 1, type = "none"),
    error = function(e) NULL
  )
  
  if (!is.null(var_fit)) {
    pred_var_vec <- sapply(predict(var_fit, n.ahead = 1)$fcst,
                           function(x) x[1, "fcst"])
  } else {
    pred_var_vec <- rep(0, N_series)
    var_failures <- var_failures + 1
  }
  
  err_var_vec  <- true_vec - pred_var_vec
  frob_var[w]  <- sum(err_var_vec^2)
  e_var[w, ]   <- err_var_vec
  
  if (w %% 10 == 0) cat(" Window", w, "/", n_windows, "\n")
}

cat("\nBacktest complete.\n")
cat("MAR(1) estimation failures:", mar1_failures, "/", n_windows, "\n")
cat("MAR(2) estimation failures:", mar2_failures, "/", n_windows, "\n")
cat("VAR(1) estimation failures:", var_failures,  "/", n_windows, "\n")

# ── AGGREGATE METRICS ─────────────────────────────────────────────────────────
msfe_mar1 <- mean(frob_mar1)
msfe_mar2 <- mean(frob_mar2)
msfe_var  <- mean(frob_var)

rmse_mar1 <- sqrt(colMeans(e_mar1^2))
rmse_mar2 <- sqrt(colMeans(e_mar2^2))
rmse_var  <- sqrt(colMeans(e_var^2))

series_labels <- paste0(rep(states, times = N_sectors), "_",
                        rep(sectors, each = N_states))

results_df <- data.frame(
  Series      = series_labels,
  RMSE_MAR1   = round(rmse_mar1, 4),
  RMSE_MAR2   = round(rmse_mar2, 4),
  RMSE_VAR1   = round(rmse_var,  4),
  Ratio_MAR1  = round(rmse_mar1 / rmse_var, 4),
  Ratio_MAR2  = round(rmse_mar2 / rmse_var, 4)
)

cat("\n=== AGGREGATE FROBENIUS MSFE ===\n")
cat(sprintf("MAR(1): %.4f\n", msfe_mar1))
cat(sprintf("MAR(2): %.4f\n", msfe_mar2))
cat(sprintf("VAR(1): %.4f\n", msfe_var))
cat(sprintf("MSFE ratio (MAR1/VAR1): %.4f [<1 = MAR wins]\n", msfe_mar1 / msfe_var))
cat(sprintf("MSFE ratio (MAR2/VAR1): %.4f [<1 = MAR wins]\n", msfe_mar2 / msfe_var))

cat("\n=== PER-SERIES RMSE ===\n")
print(results_df)

# ── DIEBOLD-MARIANO TESTS ─────────────────────────────────────────────────────
dm_mar1 <- dm.test(e1 = sqrt(frob_mar1),
                   e2 = sqrt(frob_var),
                   alternative = "less",
                   h = 1, power = 2)
cat("\n=== DIEBOLD-MARIANO TEST (H1: MAR(1) < VAR(1)) ===\n")
print(dm_mar1)

dm_mar2 <- dm.test(e1 = sqrt(frob_mar2),
                   e2 = sqrt(frob_var),
                   alternative = "less",
                   h = 1, power = 2)
cat("\n=== DIEBOLD-MARIANO TEST (H1: MAR(2) < VAR(1)) ===\n")
print(dm_mar2)

# ── SAVE RESULTS ──────────────────────────────────────────────────────────────
write.csv(results_df,
          paste0(results_dir, "per_series_rmse_three_model.csv"),
          row.names = FALSE)

write.csv(data.frame(Window    = seq_len(n_windows),
                     Loss_MAR1 = frob_mar1,
                     Loss_MAR2 = frob_mar2,
                     Loss_VAR1 = frob_var),
          paste0(results_dir, "rolling_losses_three_model.csv"),
          row.names = FALSE)

saveRDS(list(msfe_mar1    = msfe_mar1,
             msfe_mar2    = msfe_mar2,
             msfe_var     = msfe_var,
             frob_mar1    = frob_mar1,
             frob_mar2    = frob_mar2,
             frob_var     = frob_var,
             e_mar1       = e_mar1,
             e_mar2       = e_mar2,
             e_var        = e_var,
             results_df   = results_df,
             dm_mar1      = dm_mar1,
             dm_mar2      = dm_mar2,
             mar1_failures = mar1_failures,
             mar2_failures = mar2_failures,
             var_failures  = var_failures),
        paste0(results_dir, "backtest_results_three_model.rds"))

cat("\nAll results saved to:", results_dir, "\n")

# ── PLOTS ─────────────────────────────────────────────────────────────────────
png(paste0(results_dir, "rolling_loss_three_model.png"),
    width = 10, height = 5, units = "in", res = 300)
par(mar = c(4, 4, 3, 2))
plot(frob_mar1, type = "l", col = "steelblue", lwd = 1.5,
     ylim = range(c(frob_mar1, frob_mar2, frob_var)),
     xlab = "OOS Window", ylab = "Squared Frobenius Error",
     main = "Rolling OOS Loss: MAR(1), MAR(2) vs VAR(1)")
lines(frob_mar2, col = "darkorange",  lwd = 1.5)
lines(frob_var,  col = "firebrick",   lwd = 1.5)
legend("topright", legend = c("MAR(1)", "MAR(2)", "VAR(1)"),
       col = c("steelblue", "darkorange", "firebrick"), lwd = 2, bty = "n")
dev.off()

png(paste0(results_dir, "rmse_ratio_three_model.png"),
    width = 12, height = 6, units = "in", res = 300)
par(mar = c(6, 4, 3, 2))
x     <- barplot(rbind(results_df$Ratio_MAR1, results_df$Ratio_MAR2),
                 beside = TRUE,
                 names.arg = rep("", nrow(results_df)),
                 col = c("steelblue", "darkorange"),
                 ylab = "RMSE Ratio (Model / VAR(1))",
                 main = "Per-Series RMSE Ratio vs VAR(1)\n[<1 = model wins]")
abline(h = 1, lty = 2, col = "gray40")
text(x = colMeans(x), y = -0.05,
     labels = results_df$Series, srt = 45, adj = 1,
     xpd = TRUE, cex = 0.7)
legend("topright", legend = c("MAR(1)", "MAR(2)"),
       fill = c("steelblue", "darkorange"), bty = "n")
dev.off()

cat("Plots saved.\n")