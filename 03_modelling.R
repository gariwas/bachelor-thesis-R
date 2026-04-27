xx_diff <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 6: FIT TenAR MODEL
# ══════════════════════════════════════════════════════════════════════════════

set.seed(123)
est <- tenAR.est(xx_diff, R = 2, P = 1, method = "MLE")
saveRDS(est, "C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds")

A <- est$A[[1]][[1]][[1]]   # states × states (row interactions)
B <- est$A[[1]][[1]][[2]]   # sectors × sectors (column interactions)
rownames(A) <- colnames(A) <- states
rownames(B) <- colnames(B) <- sectors
print(round(A, 3))
print(round(B, 3))


# Model summary
cat("BIC:", est$BIC, "\n")

# Residuals — same shape as xx_diff
res <- est$res
cat("Residual dimensions:", dim(res), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 7: EIGENVALUE STABILITY CHECK
# ══════════════════════════════════════════════════════════════════════════════

A1 <- est$A[[1]][[1]][[1]]   # lag 1, term 1, mode 1 (states dimension)
eigs   <- eigen(A1)$values
moduli <- Mod(eigs)

cat("\nEigenvalue moduli:\n")
print(round(moduli, 4))
cat("All inside unit circle (stable):", all(moduli < 1), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 8: RESIDUAL DIAGNOSTICS
# ══════════════════════════════════════════════════════════════════════════════

# ── STEP 8b: SAVE ACF OF RESIDUALS ────────────────────────────────────────────
png(filename = "C:/Users/Igor/Desktop/bachelor-thesis-R/acf_residuals.png",
    width = 10, height = 8, units = "in", res = 300)
mplot.acf(res)
dev.off()

# Goal: no significant spikes remaining → model captured the dynamics