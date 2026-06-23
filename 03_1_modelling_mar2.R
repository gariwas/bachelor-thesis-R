# ══════════════════════════════════════════════════════════════════════════════
# 03_tenAR_estimation.R  —  TenAR(2) Estimation: LSE & MLE
# ══════════════════════════════════════════════════════════════════════════════

library(tensorTS)

# ── CONFIG ────────────────────────────────────────────────────────────────────
rds_path     <- "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds"
out_dir      <- "C:/Users/Igor/Desktop/bachelor-thesis-R/"

states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")

R_rank <- 1   # Kronecker rank — increase if BIC suggests higher rank
P      <- 2   # Lag order

# ── LOAD DIFFERENCED TENSOR ───────────────────────────────────────────────────
xx_diff <- readRDS(rds_path)
cat("Loaded tensor dimensions:", dim(xx_diff), "\n")
# Expected: [T-1, 5, 5]


# ══════════════════════════════════════════════════════════════════════════════
# OPTIONAL: BIC-based rank selection (uncomment to run before fixing R_rank)
# ══════════════════════════════════════════════════════════════════════════════
# bic_result <- tenAR.bic(xx_diff, P = P, method = "LSE")
# cat("BIC-optimal rank:", bic_result$R, "\n")
# R_rank <- bic_result$R


# ══════════════════════════════════════════════════════════════════════════════
# STEP 1: FIT TenAR(2) — LSE
# ══════════════════════════════════════════════════════════════════════════════
cat("\n--- Fitting TenAR(2) via LSE ---\n")
est_lse <- tenAR.est(xx_diff, R = R_rank, P = P, method = "LSE")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 2: FIT TenAR(2) — MLE
# ══════════════════════════════════════════════════════════════════════════════
cat("--- Fitting TenAR(2) via MLE ---\n")
est_mle <- tenAR.est(xx_diff, R = R_rank, P = P, method = "MLE")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 3: EXTRACT AND PRINT COEFFICIENT MATRICES
# ══════════════════════════════════════════════════════════════════════════════
# A[[lag]][[r]][[k]]:
#   lag = 1 or 2  (lag order)
#   r   = 1..R    (Kronecker term; R=1 gives one term)
#   k   = 1 (states, 5x5) or 2 (sectors, 5x5)

print_coef <- function(est, label) {
  for (lag in 1:P) {
    for (k in 1:2) {
      mode_name <- if (k == 1) "States" else "Sectors"
      cat(sprintf("\n=== %s | Lag %d | %s mode ===\n", label, lag, mode_name))
      mat <- est$A[[lag]][[1]][[k]]
      rownames(mat) <- if (k == 1) states else sectors
      colnames(mat) <- if (k == 1) states else sectors
      print(round(mat, 4))
    }
  }
}

print_coef(est_lse, "LSE")
print_coef(est_mle, "MLE")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 4: MLE — SEPARABLE ERROR COVARIANCE (Sigma_1, Sigma_2)
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== MLE | Sigma_1 — States covariance (5x5) ===\n")
sig1 <- est_mle$SIGMA[[1]]
rownames(sig1) <- colnames(sig1) <- states
print(round(sig1, 4))

cat("\n=== MLE | Sigma_2 — Sectors covariance (5x5) ===\n")
sig2 <- est_mle$SIGMA[[2]]
rownames(sig2) <- colnames(sig2) <- sectors
print(round(sig2, 4))


# ══════════════════════════════════════════════════════════════════════════════
# STEP 5: BIC COMPARISON
# ══════════════════════════════════════════════════════════════════════════════
cat("\n--- BIC ---\n")
cat("LSE BIC:", est_lse$BIC, "\n")
cat("MLE BIC:", est_mle$BIC, "\n")
cat("Lower BIC = better fit.\n")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 6: SAVE RESULTS
# ══════════════════════════════════════════════════════════════════════════════
saveRDS(est_lse, paste0(out_dir, "est_lse_p2.rds"))
saveRDS(est_mle, paste0(out_dir, "est_mle_p2.rds"))
cat("\nSaved: est_lse_p2.rds and est_mle_p2.rds\n")