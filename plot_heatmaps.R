# ── PATHS ─────────────────────────────────────────────────────────────────────
est_path  <- "C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds"
out_dir   <- "C:/Users/Igor/Desktop/bachelor-thesis-R/"

states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")

# ── LOAD ──────────────────────────────────────────────────────────────────────
library(ggplot2)
library(reshape2)

est <- readRDS(est_path)

A <- est$A[[1]][[1]][[1]]
B <- est$A[[1]][[1]][[2]]
rownames(A) <- colnames(A) <- states
rownames(B) <- colnames(B) <- sectors

# ── HELPER ────────────────────────────────────────────────────────────────────
plot_heatmap <- function(mat, title) {
  df <- melt(round(mat, 3))
  colnames(df) <- c("Row", "Col", "value")
  
  ggplot(df, aes(Col, Row, fill = value)) +
    geom_tile(color = "#cccccc", linewidth = 1.0) +
    geom_text(aes(label = sprintf("%.3f", value)), size = 4.2, fontface = "bold",
              color = ifelse(abs(df$value) > 0.5, "white", "black")) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4",
                         midpoint = 0, name = "Coeff.") +
    scale_y_discrete(limits = rev(rownames(mat))) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text        = element_text(face = "bold", size = 12),
      panel.grid       = element_blank(),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
      legend.position  = "right",
      plot.background  = element_rect(fill = "lightgrey", color = NA),
      panel.background = element_rect(fill = "lightgrey", color = NA)
    )
}

# ── EXPORT ────────────────────────────────────────────────────────────────────
ggsave(paste0(out_dir, "heatmap_A.png"),
       plot = plot_heatmap(A, "Matrix A — State Dynamics"),
       width = 5.5, height = 4.5, dpi = 200)

ggsave(paste0(out_dir, "heatmap_B.png"),
       plot = plot_heatmap(B, "Matrix B — Sector Dynamics"),
       width = 5.5, height = 4.5, dpi = 200)

cat("Saved heatmap_A.png and heatmap_B.png to", out_dir, "\n")