# Preprocessing - stability check 

# ══════════════════════════════════════════════════════════════════════════════
# 02_analysis.R  —  Preprocessing & Modelling
# ══════════════════════════════════════════════════════════════════════════════

library(tensorTS)
library(tseries)

states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")

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

png(filename = "C:/Users/Igor/Desktop/bachelor-thesis-R/visualisations/employment_differenced.png",
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
# STEP 5b: SAVE ACF OF DIFFERENCED DATA — ggplot2 version
# ══════════════════════════════════════════════════════════════════════════════

library(ggplot2)
library(patchwork)   # install.packages("patchwork") if needed

T_len <- dim(xx_diff)[1]
ci    <- qnorm(0.975) / sqrt(T_len)   # 95% CI band

plot_list <- list()

for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    
    acf_obj <- acf(xx_diff[, i, j], lag.max = 24, plot = FALSE)
    df      <- data.frame(lag = as.numeric(acf_obj$lag),
                          acf = as.numeric(acf_obj$acf))
    df      <- df[df$lag > 0, ]   # drop lag 0 (always 1)
    
    p <- ggplot(df, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0,   color = "grey40", linewidth = 0.4) +
      geom_hline(yintercept =  ci, color = "#2166ac", linetype = "dashed", linewidth = 0.5) +
      geom_hline(yintercept = -ci, color = "#2166ac", linetype = "dashed", linewidth = 0.5) +
      geom_segment(aes(xend = lag, yend = 0),
                   color = "darkorange", linewidth = 0.8) +
      geom_point(color = "darkorange", size = 1.2) +
      scale_x_continuous(breaks = c(6, 12, 18, 24)) +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = paste0(states[i], " · ", sectors[j]), x = NULL, y = NULL) +
      theme_minimal(base_size = 8) +
      theme(
        plot.title   = element_text(face = "bold", size = 7, hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey92"),
        axis.text    = element_text(size = 6),
        plot.margin  = margin(3, 4, 3, 4)
      )
    
    plot_list[[paste0(states[i], sectors[j])]] <- p
  }
}

# Assemble 5×5 grid with patchwork
combined <- wrap_plots(plot_list, nrow = length(states), ncol = length(sectors)) +
  plot_annotation(
    title    = "ACF of First-Differenced Employment (Lags 1–24)",
    theme    = theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5, margin = margin(b = 6))
    )
  )

ggsave(
  filename = "C:/Users/Igor/Desktop/bachelor-thesis-R/visualisations/acf_original.png",
  plot     = combined,
  width    = 14, height = 10, dpi = 300
)

# ══════════════════════════════════════════════════════════════════════════════
# PREPROCESSING DONE — xx_diff is ready for tenAR.est
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(xx_diff, "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")
