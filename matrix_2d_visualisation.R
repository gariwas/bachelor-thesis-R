library(grid)

states  <- c("IL", "OH", "MI", "IN", "WI")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")

NS <- length(states)   # 5
NJ <- length(sectors)  # 5

sector_cols <- list(
  c("#1a3a6b", "#2b5ba8", "#4f8ef7", "#88b8ff", "#c4dbff"),  # MFG  — blues
  c("#6b2a1a", "#a84030", "#f7794f", "#ffaa88", "#ffd4c0"),  # CONS — reds
  c("#1a5c2a", "#2e8a40", "#4fcc7a", "#88e8a8", "#c2f5d4"),  # RET  — greens
  c("#4a1a5c", "#7a2e99", "#cc4fc8", "#e888e4", "#f7c4f5"),  # GOVT — purples
  c("#5c4a00", "#a88200", "#e8d44d", "#f5e888", "#faf4c0")   # FIRE — yellows
)

# ── CONFIG ────────────────────────────────────────────────────────────────────
out_dir <- "C:/Users/Igor/Desktop/bachelor-thesis-R/visualisations"

# ── Layout constants (px) ─────────────────────────────────────────────────────
cell_px  <- 110
gap_px   <- 16
margin_l <- 120
margin_b <- 80
margin_r <- 50
margin_t <- 50

W <- margin_l + NS * cell_px + (NS - 1) * gap_px + margin_r
H <- margin_b + NJ * cell_px + (NJ - 1) * gap_px + margin_t

nx <- function(p) p / W
ny <- function(p) p / H

label_col  <- "#333333"
label_font <- 2

draw_cells <- function() {
  for (xi in seq_len(NS)) {
    for (zi in seq_len(NJ)) {
      fill     <- sector_cols[[zi]][xi]
      x_left   <- margin_l + (xi - 1) * (cell_px + gap_px)
      y_bottom <- margin_b + (zi - 1) * (cell_px + gap_px)
      grid.rect(
        x      = unit(nx(x_left   + cell_px / 2), "npc"),
        y      = unit(ny(y_bottom + cell_px / 2), "npc"),
        width  = unit(nx(cell_px), "npc"),
        height = unit(ny(cell_px), "npc"),
        gp     = gpar(fill = fill, col = "white", lwd = 2.5)
      )
    }
  }
}

# ═════════════════════════════════════════════════════════════════════════════
# PLOT 1  —  axis labels, no in-cell text
# ═════════════════════════════════════════════════════════════════════════════
png(file.path(out_dir, "matrix_2d_labels.png"), width = W, height = H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))
draw_cells()

for (xi in seq_len(NS)) {
  xc <- margin_l + (xi - 1) * (cell_px + gap_px) + cell_px / 2
  grid.text(states[xi],
            x  = unit(nx(xc), "npc"),
            y  = unit(ny(margin_b - 30), "npc"),
            gp = gpar(col = label_col, fontsize = 18, fontface = label_font))
}

for (zi in seq_len(NJ)) {
  yc <- margin_b + (zi - 1) * (cell_px + gap_px) + cell_px / 2
  grid.text(sectors[zi],
            x    = unit(nx(margin_l - 14), "npc"),
            y    = unit(ny(yc), "npc"),
            just = "right",
            gp   = gpar(col = sector_cols[[zi]][1], fontsize = 18, fontface = label_font))
}

dev.off()
message("Saved: matrix_2d_labels.png")

# ═════════════════════════════════════════════════════════════════════════════
# PLOT 2  —  no axis labels, in-cell x subscripts (column-major)
# ═════════════════════════════════════════════════════════════════════════════
png(file.path(out_dir, "matrix_2d_subscripts.png"), width = W, height = H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))
draw_cells()

for (xi in seq_len(NS)) {
  for (zi in seq_len(NJ)) {
    x_left   <- margin_l + (xi - 1) * (cell_px + gap_px)
    y_bottom <- margin_b + (NJ - zi) * (cell_px + gap_px)
    xc       <- x_left   + cell_px / 2
    yc       <- y_bottom + cell_px / 2
    
    idx     <- (xi - 1) * NJ + zi
    txt_col <- ifelse(xi <= 2, "white", label_col)
    
    grid.text("x  ",
              x  = unit(nx(xc - 10), "npc"),
              y  = unit(ny(yc + 8), "npc"),
              gp = gpar(col = txt_col, fontsize = 40, fontface = 2))
    
    grid.text(paste0(idx, "t"),
              x  = unit(nx(xc + 12), "npc"),
              y  = unit(ny(yc - 6), "npc"),
              gp = gpar(col = txt_col, fontsize = 30, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_2d_subscripts.png")


# PLOT 2b — same matrix, but t-1 subscripts
png(file.path(out_dir, "matrix_2d_subscripts_tminus1.png"), width = W, height = H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))
draw_cells()

for (xi in seq_len(NS)) {
  for (zi in seq_len(NJ)) {
    x_left   <- margin_l + (xi - 1) * (cell_px + gap_px)
    y_bottom <- margin_b + (NJ - zi) * (cell_px + gap_px)
    xc       <- x_left   + cell_px / 2
    yc       <- y_bottom + cell_px / 2
    
    idx     <- (xi - 1) * NJ + zi
    txt_col <- ifelse(xi <= 2, "white", label_col)
    
    grid.text("x    ",
              x  = unit(nx(xc - 10), "npc"),
              y  = unit(ny(yc + 8), "npc"),
              gp = gpar(col = txt_col, fontsize = 40, fontface = 2))
    
    grid.text(paste0(idx, "t-1"),
              x  = unit(nx(xc + 12), "npc"),
              y  = unit(ny(yc - 6), "npc"),
              gp = gpar(col = txt_col, fontsize = 30, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_2d_subscripts_tminus1.png")


# COLORLESS 5x5 X_{t-1} MATRIX
png(file.path(out_dir, "matrix_2d_subscripts_tminus1_nocolor.png"), width = W, height = H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))
draw_cells()

for (xi in seq_len(NS)) {
  for (zi in seq_len(NJ)) {
    x_left   <- margin_l + (xi - 1) * (cell_px + gap_px)
    y_bottom <- margin_b + (NJ - zi) * (cell_px + gap_px)
    xc       <- x_left   + cell_px / 2
    yc       <- y_bottom + cell_px / 2
    
    idx <- (xi - 1) * NJ + zi
    
    grid.rect(
      x      = unit(nx(xc), "npc"),
      y      = unit(ny(yc), "npc"),
      width  = unit(nx(cell_px), "npc"),
      height = unit(ny(cell_px), "npc"),
      gp     = gpar(fill = "white", col = "black", lwd = 2)
    )
    
    txt_col <- "black"
    
    grid.text("x    ",
              x  = unit(nx(xc - 10), "npc"),
              y  = unit(ny(yc + 8), "npc"),
              gp = gpar(col = txt_col, fontsize = 40, fontface = 2))
    
    grid.text(paste0(idx, "t-1"),
              x  = unit(nx(xc + 12), "npc"),
              y  = unit(ny(yc - 6), "npc"),
              gp = gpar(col = txt_col, fontsize = 30, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_2d_subscripts_tminus1_nocolor.png")

# ═════════════════════════════════════════════════════════════════════════════
# PLOT 3  —  25×1 vertical vector, column-major order, colours + labels
#            k=1 at TOP, k=25 at BOTTOM
# ═════════════════════════════════════════════════════════════════════════════
n_cells  <- NS * NJ   # 25
vec_cell <- cell_px
vec_gap  <- gap_px

V_margin_l <- 80
V_margin_r <- 80
V_margin_b <- 50
V_margin_t <- 50

V_W <- V_margin_l + vec_cell + V_margin_r
V_H <- V_margin_t + n_cells * vec_cell + (n_cells - 1) * vec_gap + V_margin_b

vx <- function(p) p / V_W
vy <- function(p) p / V_H

png(file.path(out_dir, "matrix_2d_vector.png"), width = V_W, height = V_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (k in seq_len(n_cells)) {
  xi   <- ceiling(k / NJ)
  zi   <- ((k - 1) %% NJ) + 1
  fill <- sector_cols[[zi]][xi]
  
  xc     <- V_margin_l + vec_cell / 2
  y_bottom <- V_margin_b + (n_cells - k) * (vec_cell + vec_gap)
  yc       <- y_bottom + vec_cell / 2
  
  grid.rect(
    x      = unit(vx(xc), "npc"),
    y      = unit(vy(yc), "npc"),
    width  = unit(vx(vec_cell), "npc"),
    height = unit(vy(vec_cell), "npc"),
    gp     = gpar(fill = fill, col = "white", lwd = 2.5)
  )
  
  txt_col <- ifelse(xi <= 2, "white", label_col)
  
  grid.text("x ",
            x  = unit(vx(xc - 10), "npc"),
            y  = unit(vy(yc + 8), "npc"),
            gp = gpar(col = txt_col, fontsize = 40, fontface = 2))
  
  grid.text(paste0(k, "t"),
            x  = unit(vx(xc + 12), "npc"),
            y  = unit(vy(yc - 6), "npc"),
            gp = gpar(col = txt_col, fontsize = 30, fontface = 2))
}

dev.off()
message("Saved: matrix_2d_vector.png")


# ═════════════════════════════════════════════════════════════════════════════
# PLOT: colorful 25×1 vector of x_{t-1}
# ═════════════════════════════════════════════════════════════════════════════
png(file.path(out_dir, "vector_xtminus1_color.png"), width = V_W, height = V_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (k in seq_len(n_cells)) {
  xi   <- ceiling(k / NJ)
  zi   <- ((k - 1) %% NJ) + 1
  fill <- sector_cols[[zi]][xi]
  
  xc       <- V_margin_l + vec_cell / 2
  y_bottom <- V_margin_b + (n_cells - k) * (vec_cell + vec_gap)
  yc       <- y_bottom + vec_cell / 2
  
  grid.rect(
    x      = unit(vx(xc), "npc"),
    y      = unit(vy(yc), "npc"),
    width  = unit(vx(vec_cell), "npc"),
    height = unit(vy(vec_cell), "npc"),
    gp     = gpar(fill = fill, col = "white", lwd = 2.5)
  )
  
  txt_col <- ifelse(xi <= 2, "white", label_col)
  
  grid.text("x    ",
            x  = unit(vx(xc - 10), "npc"),
            y  = unit(vy(yc + 8), "npc"),
            gp = gpar(col = txt_col, fontsize = 40, fontface = 2))
  
  grid.text(paste0(k, "t-1"),
            x  = unit(vx(xc + 12), "npc"),
            y  = unit(vy(yc - 6), "npc"),
            gp = gpar(col = txt_col, fontsize = 30, fontface = 2))
}

dev.off()
message("Saved: vector_xtminus1_color.png")


# 25x1 vector x_{t-1}, no colors
png(file.path(out_dir, "vector_xtminus1.png"), width = V_W, height = V_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (k in seq_len(n_cells)) {
  xc <- V_margin_l + vec_cell / 2
  y_bottom <- V_margin_b + (n_cells - k) * (vec_cell + vec_gap)
  yc <- y_bottom + vec_cell / 2
  
  grid.rect(
    x      = unit(vx(xc), "npc"),
    y      = unit(vy(yc), "npc"),
    width  = unit(vx(vec_cell), "npc"),
    height = unit(vy(vec_cell), "npc"),
    gp     = gpar(fill = "white", col = "black", lwd = 2.5)
  )
  
  grid.text("x    ",
            x  = unit(vx(xc - 10), "npc"),
            y  = unit(vy(yc + 8), "npc"),
            gp = gpar(col = "black", fontsize = 40, fontface = 2))
  
  grid.text(paste0(k, "t-1"),
            x  = unit(vx(xc + 12), "npc"),
            y  = unit(vy(yc - 6), "npc"),
            gp = gpar(col = "black", fontsize = 30, fontface = 2))
}

dev.off()
message("Saved: vector_xtminus1.png")


# 25x1 vector alpha (a1,...,a25), grey
png(file.path(out_dir, "vector_alpha.png"), width = V_W, height = V_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (k in seq_len(n_cells)) {
  xc <- V_margin_l + vec_cell / 2
  y_bottom <- V_margin_b + (n_cells - k) * (vec_cell + vec_gap)
  yc <- y_bottom + vec_cell / 2
  
  grid.rect(
    x      = unit(vx(xc), "npc"),
    y      = unit(vy(yc), "npc"),
    width  = unit(vx(vec_cell), "npc"),
    height = unit(vy(vec_cell), "npc"),
    gp     = gpar(fill = "grey80", col = "black", lwd = 2.5)
  )
  
  grid.text(bquote(alpha~" "),
            x  = unit(vx(xc - 10), "npc"),
            y  = unit(vy(yc + 8), "npc"),
            gp = gpar(col = "black", fontsize = 40, fontface = 2))
  
  grid.text(bquote(.(k)),
            x  = unit(vx(xc + 12), "npc"),
            y  = unit(vy(yc - 6), "npc"),
            gp = gpar(col = "black", fontsize = 30, fontface = 2))
}

dev.off()
message("Saved: vector_alpha.png")


# 25x1 vector epsilon_t (epsilon1t,...,epsilon25t), grey
png(file.path(out_dir, "vector_epsilon_t.png"), width = V_W, height = V_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (k in seq_len(n_cells)) {
  xc <- V_margin_l + vec_cell / 2
  y_bottom <- V_margin_b + (n_cells - k) * (vec_cell + vec_gap)
  yc <- y_bottom + vec_cell / 2
  
  grid.rect(
    x      = unit(vx(xc), "npc"),
    y      = unit(vy(yc), "npc"),
    width  = unit(vx(vec_cell), "npc"),
    height = unit(vy(vec_cell), "npc"),
    gp     = gpar(fill = "grey80", col = "black", lwd = 2.5)
  )
  
  grid.text(bquote(epsilon~" "),
            x  = unit(vx(xc - 10), "npc"),
            y  = unit(vy(yc + 8), "npc"),
            gp = gpar(col = "black", fontsize = 40, fontface = 2))
  
  grid.text(bquote(.(k)*t),
            x  = unit(vx(xc + 12), "npc"),
            y  = unit(vy(yc - 6), "npc"),
            gp = gpar(col = "black", fontsize = 30, fontface = 2))
}

dev.off()
message("Saved: vector_epsilon_t.png")


# 25x25 matrix B with column-based colors from sector/state mapping
B_margin_l <- 80
B_margin_r <- 80
B_margin_b <- 80
B_margin_t <- 80

B_cell <- cell_px
B_gap  <- gap_px

B_n    <- n_cells  # 25

B_W <- B_margin_l + B_n * B_cell + (B_n - 1) * B_gap + B_margin_r
B_H <- B_margin_t + B_n * B_cell + (B_n - 1) * B_gap + B_margin_b

bx <- function(p) p / B_W
by <- function(p) p / B_H

png(file.path(out_dir, "matrix_B_25x25.png"), width = B_W, height = B_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (i in seq_len(B_n)) {
  for (j in seq_len(B_n)) {
    xi_j <- ceiling(j / NJ)
    zi_j <- ((j - 1) %% NJ) + 1
    fill_j <- sector_cols[[zi_j]][xi_j]
    
    x_left   <- B_margin_l + (j - 1) * (B_cell + B_gap)
    y_bottom <- B_margin_b + (B_n - i) * (B_cell + B_gap)
    xc       <- x_left   + B_cell / 2
    yc       <- y_bottom + B_cell / 2
    
    grid.rect(
      x      = unit(bx(xc), "npc"),
      y      = unit(by(yc), "npc"),
      width  = unit(bx(B_cell), "npc"),
      height = unit(by(B_cell), "npc"),
      gp     = gpar(fill = fill_j, col = "white", lwd = 1.5)
    )
    
    grid.text(bquote(b~"     "),
              x  = unit(bx(xc - 4), "npc"),
              y  = unit(by(yc + 3), "npc"),
              gp = gpar(col = "black", fontsize = 40, fontface = 2))
    
    grid.text(bquote(.(i)*","*.(j)),
              x  = unit(bx(xc + 5), "npc"),
              y  = unit(by(yc - 2), "npc"),
              gp = gpar(col = "black", fontsize = 23, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_B_25x25.png")


# ═════════════════════════════════════════════════════════════════════════════
# PLOT 4  —  MAR matrices
# ═════════════════════════════════════════════════════════════════════════════

# 5x5 matrix A (states x states), gradient only
A_margin_l <- 80
A_margin_r <- 80
A_margin_b <- 80
A_margin_t <- 80

A_cell <- cell_px
A_gap  <- gap_px

A_n <- NS  # 5

A_W <- A_margin_l + A_n * A_cell + (A_n - 1) * A_gap + A_margin_r
A_H <- A_margin_t + A_n * A_cell + (A_n - 1) * A_gap + A_margin_b

ax <- function(p) p / A_W
ay <- function(p) p / A_H

png(file.path(out_dir, "matrix_A_5x5.png"), width = A_W, height = A_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (i in seq_len(A_n)) {
  for (j in seq_len(A_n)) {
    grad_vals <- colorRampPalette(c("#eeeeff", "black"))(A_n)
    fill_ij <- grad_vals[i]
    
    x_left   <- A_margin_l + (j - 1) * (A_cell + A_gap)
    y_bottom <- A_margin_b + (A_n - i) * (A_cell + A_gap)
    xc       <- x_left   + A_cell / 2
    yc       <- y_bottom + A_cell / 2
    
    grid.rect(
      x      = unit(ax(xc), "npc"),
      y      = unit(ay(yc), "npc"),
      width  = unit(ax(A_cell), "npc"),
      height = unit(ay(A_cell), "npc"),
      gp     = gpar(fill = fill_ij, col = "white", lwd = 1.5)
    )
    
    text_col <- ifelse(i <= 2, "black", "white")
    
    grid.text(expression(bold(italic(a*" "))),
              x  = unit(ax(xc - 8), "npc"),
              y  = unit(ay(yc + 3), "npc"),
              gp = gpar(col = text_col, fontsize = 42, fontface = 2))
    
    grid.text(bquote(bold(.(i)*","*.(j))),
              x  = unit(ax(xc + 10), "npc"),
              y  = unit(ay(yc - 4), "npc"),
              gp = gpar(col = text_col, fontsize = 20, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_A_5x5.png")


# 5x5 matrix B (sectors x sectors), colours only
B_margin_l <- 80
B_margin_r <- 80
B_margin_b <- 80
B_margin_t <- 80

B_cell <- cell_px
B_gap  <- gap_px

B_n <- NJ  # 5

B_W <- B_margin_l + B_n * B_cell + (B_n - 1) * B_gap + B_margin_r
B_H <- B_margin_t + B_n * B_cell + (B_n - 1) * B_gap + B_margin_b

bx <- function(p) p / B_W
by <- function(p) p / B_H

png(file.path(out_dir, "matrix_B_5x5.png"), width = B_W, height = B_H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (i in seq_len(B_n)) {
  for (j in seq_len(B_n)) {
    base_col <- sector_cols[[j]][1]
    
    x_left   <- B_margin_l + (j - 1) * (B_cell + B_gap)
    y_bottom <- B_margin_b + (B_n - i) * (B_cell + B_gap)
    xc       <- x_left   + B_cell / 2
    yc       <- y_bottom + B_cell / 2
    
    grid.rect(
      x      = unit(bx(xc), "npc"),
      y      = unit(by(yc), "npc"),
      width  = unit(bx(B_cell), "npc"),
      height = unit(by(B_cell), "npc"),
      gp     = gpar(fill = base_col, col = "white", lwd = 1.5)
    )
    
    grid.text(expression(bold(italic(b*" "))),
              x  = unit(bx(xc - 8), "npc"),
              y  = unit(by(yc + 3), "npc"),
              gp = gpar(col = "white", fontsize = 42, fontface = 2))
    
    grid.text(bquote(bold(.(i)*","*.(j))),
              x  = unit(bx(xc + 10), "npc"),
              y  = unit(by(yc - 4), "npc"),
              gp = gpar(col = "white", fontsize = 20, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_B_5x5.png")


# 5x5 ERROR MATRIX E_t (epsilon_ij,t), grey, no colour/gradient
png(file.path(out_dir, "matrix_Et_5x5.png"), width = W, height = H, bg = "#d3d3d3")
grid.newpage()
grid.rect(gp = gpar(fill = "#d3d3d3", col = NA))

for (xi in seq_len(NS)) {  # rows: i = 1,...,5 (states)
  for (zi in seq_len(NJ)) {  # cols: j = 1,...,5 (sectors)
    x_left   <- margin_l + (zi - 1) * (cell_px + gap_px)
    y_bottom <- margin_b + (NS - xi) * (cell_px + gap_px)
    xc       <- x_left   + cell_px / 2
    yc       <- y_bottom + cell_px / 2
    
    grid.rect(
      x      = unit(nx(xc), "npc"),
      y      = unit(ny(yc), "npc"),
      width  = unit(nx(cell_px), "npc"),
      height = unit(ny(cell_px), "npc"),
      gp     = gpar(fill = "grey80", col = "black", lwd = 2)
    )
    
    grid.text(expression(bold(italic(epsilon*" "))),
              x  = unit(nx(xc - 8), "npc"),
              y  = unit(ny(yc + 3), "npc"),
              gp = gpar(col = "black", fontsize = 42, fontface = 2))
    
    grid.text(bquote(bold(.(xi)*","*.(zi))),
              x  = unit(nx(xc + 10), "npc"),
              y  = unit(ny(yc - 4), "npc"),
              gp = gpar(col = "black", fontsize = 20, fontface = 2))
  }
}

dev.off()
message("Saved: matrix_Et_5x5.png")