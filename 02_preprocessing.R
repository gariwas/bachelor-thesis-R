# Preprocessing - stability check (eigenvalues and so on)

# ══════════════════════════════════════════════════════════════════════════════
# 02_analysis.R  —  Preprocessing & Modelling
# ══════════════════════════════════════════════════════════════════════════════

library(tensorTS)
library(tseries)

states  <- c("IL", "OH", "MI")
sectors <- c("MFG", "CONS", "SMS")

# ── LOAD TENSOR ───────────────────────────────────────────────────────────────
xx <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/xx_tensor.rds")
cat("Loaded tensor dimensions:", dim(xx), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 1: ADF UNIT ROOT TESTS ON RAW DATA
# ══════════════════════════════════════════════════════════════════════════════
# H0: unit root present (non-stationary)
# p < 0.05 → stationary, no differencing needed
# p >= 0.05 → unit root → must difference

cat("\n=== ADF Tests: Raw Data ===\n")
for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    result <- adf.test(xx[, i, j])
    cat(states[i], sectors[j], "| p =", round(result$p.value, 4),
        ifelse(result$p.value < 0.05, "-> stationary", "-> UNIT ROOT"), "\n")
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 2: FIRST DIFFERENCING
# ══════════════════════════════════════════════════════════════════════════════

xx_diff <- xx[-1, , ] - xx[-dim(xx)[1], , ]
cat("\nDimensions after differencing:", dim(xx_diff), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 3: ADF TESTS ON DIFFERENCED DATA
# ══════════════════════════════════════════════════════════════════════════════
# All series should now be stationary (p < 0.05)

cat("\n=== ADF Tests: Differenced Data ===\n")
for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    result <- adf.test(xx_diff[, i, j])
    cat(states[i], sectors[j], "| p =", round(result$p.value, 4),
        ifelse(result$p.value < 0.05, "-> stationary", "-> STILL HAS UNIT ROOT"), "\n")
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 4: VISUAL CHECK OF DIFFERENCED DATA
# ══════════════════════════════════════════════════════════════════════════════

png(filename = "C:/Users/Igor/Desktop/bachelor-thesis-R/employment_differenced.png",
    width = 10, height = 8, units = "in", res = 300)

par(mfrow = c(length(states), length(sectors)), mar = c(2, 2, 2, 1), oma = c(1, 1, 3, 1))
for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    plot(xx_diff[, i, j], type = "l", col = "darkorange",
         xlab = "", ylab = "",
         main = paste0(states[i], sectors[j]), cex.main = 0.95)
    abline(h = 0, col = "gray50", lty = 2)
  }
}
mtext("First-Differenced Employment (Month-on-Month Change)",
      outer = TRUE, side = 3, line = 1, cex = 1.1, font = 2)
par(mfrow = c(1, 1))
dev.off()


# ══════════════════════════════════════════════════════════════════════════════
# STEP 5: ACF CHECK
# ══════════════════════════════════════════════════════════════════════════════
# Significant spike at lag 12 → seasonal pattern → may need seasonal differencing

# ── STEP 5b: SAVE ACF OF DIFFERENCED DATA (original, before model fitting) ────
png(filename = "C:/Users/Igor/Desktop/bachelor-thesis-R/acf_original.png",
    width = 10, height = 8, units = "in", res = 300)
mplot.acf(xx_diff)
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# PREPROCESSING DONE — xx_diff is ready for tenAR.est
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(xx_diff, "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")
