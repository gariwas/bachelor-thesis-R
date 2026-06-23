#install.packages("rgl")
library(rgl)

states  <- c("IL", "OH", "MI", "IN", "KY")
sectors <- c("MFG", "CONS", "RET", "GOVT", "FIRE")
T_steps <- 16

NS <- length(states)
NJ <- length(sectors)
NT <- T_steps

cell <- c(0.8, 0.8, 0.8)
gap  <- c(0.15, 0.15, 0.15)

sector_cols <- list(
  c("#1a3a6b", "#2b5ba8", "#4f8ef7", "#88b8ff", "#c4dbff"),  # MFG  — blues
  c("#6b2a1a", "#a84030", "#f7794f", "#ffaa88", "#ffd4c0"),  # CONS — reds
  c("#1a5c2a", "#2e8a40", "#4fcc7a", "#88e8a8", "#c2f5d4"),  # RET  — greens
  c("#4a1a5c", "#7a2e99", "#cc4fc8", "#e888e4", "#f7c4f5"),  # GOVT — purples
  c("#5c4a00", "#a88200", "#e8d44d", "#f5e888", "#faf4c0")   # FIRE — yellows
)
# Then index directly: sector_cols[[zi]][xi]



draw_cube <- function(x0, y0, z0, col) {
  x1 <- x0 + cell[1]
  y1 <- y0 + cell[2]
  z1 <- z0 + cell[3]
  
  faces <- list(
    rbind(c(x0,y1,z0), c(x1,y1,z0), c(x1,y1,z1), c(x0,y1,z1)), # top
    rbind(c(x0,y0,z0), c(x1,y0,z0), c(x1,y1,z0), c(x0,y1,z0)), # back
    rbind(c(x0,y0,z1), c(x1,y0,z1), c(x1,y1,z1), c(x0,y1,z1)), # front
    rbind(c(x0,y0,z0), c(x0,y0,z1), c(x0,y1,z1), c(x0,y1,z0)), # left
    rbind(c(x1,y0,z0), c(x1,y0,z1), c(x1,y1,z1), c(x1,y1,z0)), # right
    rbind(c(x0,y0,z0), c(x1,y0,z0), c(x1,y0,z1), c(x0,y0,z1))  # bottom
  )
  
  # AFTER — soft matte shading, no specular/metallic feel:
  bright <- c(0.95, 0.75, 1.00, 0.80, 0.85, 0.70)
  #           top   back  front left  right bottom
  
  for (f in seq_along(faces)) {
    shade <- adjustcolor(col, red.f = bright[f], green.f = bright[f], blue.f = bright[f])
    quads3d(faces[[f]], col = shade, alpha = 1)
  }
}

open3d(windowRect = c(50, 50, 900, 700))
bg3d(color = "#d3d3d3")

# ADD THIS — disables OpenGL lighting entirely:
material3d(lit = FALSE)

for (xi in seq_len(NS)) {
  for (yi in seq_len(NT)) {
    for (zi in seq_len(NJ)) {
      x0 <- (xi - 1) * (cell[1] + gap[1])
      y0 <- (yi - 1) * (cell[2] + gap[2])
      z0 <- (zi - 1) * (cell[3] + gap[3])
      draw_cube(x0, y0, z0, sector_cols[[zi]][xi])
    }
  }
}

label_col <- "#333333"

# ── STATE labels ──────────────────────────────────────────────────────────
for (xi in seq_len(NS)) {
  x <- (xi - 1) * (cell[1] + gap[1]) + cell[1] / 2
  text3d(x, -0.5, -0.5,
         texts = states[xi], col = label_col, cex = 0.9, font = 2)
}

# ── SECTOR labels ─────────────────────────────────────────────────────────
for (zi in seq_len(NJ)) {
  z <- (zi - 1) * (cell[3] + gap[3]) + cell[3] / 2
  text3d(-1.0, -0.5, z,
         texts = sectors[zi], col = sector_cols[[zi]][1], cex = 0.9, font = 2)
}

# ── TIME labels ───────────────────────────────────────────────────────────
show_lags <- c(0, 1, 2, 3, 4, 5, 10, 15)

for (lag in show_lags) {
  yi    <- lag + 1
  y     <- (yi - 1) * (cell[2] + gap[2]) + cell[2] / 2
  label <- if (lag == 0) "t" else paste0("t-", lag)
  text3d((NS + 0.5) * (cell[1] + gap[1]),
         y, 0.0,
         texts = label, col = "#333333", cex = 0.7, font = 2)
}

view3d(theta = 30, phi = 20, zoom = 0.7)