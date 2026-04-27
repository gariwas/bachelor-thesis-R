# Working with data

# ── CONFIG ────────────────────────────────────────────────────────────────────
data_dir <- "C:/Users/Igor/Desktop/bachelor-thesis-R/Data/"

states  <- c("IL", "OH", "MI")
sectors <- c("MFG", "CONS", "SMS")

# ── READ ALL 9 SERIES ─────────────────────────────────────────────────────────
series_list <- list()

for (s in states) {
  for (sec in sectors) {
    key  <- paste0(s, sec)
    path <- paste0(data_dir, key, ".csv")
    df   <- read.csv(path)
    colnames(df) <- c("date", "value")
    df$date <- as.Date(df$date)
    df <- df[order(df$date), ]
    series_list[[key]] <- df
  }
}

# ── DIAGNOSE DATE RANGES ──────────────────────────────────────────────────────
for (key in names(series_list)) {
  df <- series_list[[key]]
  cat(key, ": ", format(min(df$date)), "to", format(max(df$date)),
      "| N =", nrow(df), "\n")
}

# ── FIND COMMON DATE RANGE ────────────────────────────────────────────────────
all_dates    <- lapply(series_list, function(df) df$date)
common_dates <- Reduce(intersect, lapply(all_dates, as.integer))
common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
T            <- length(common_dates)
cat("\nCommon range:", format(min(common_dates)), "to", format(max(common_dates)), "| T =", T, "\n")

# ── BUILD TENSOR ARRAY [T, states, sectors] ───────────────────────────────────
xx <- array(NA_real_, dim = c(T, length(states), length(sectors)))

for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    key        <- paste0(states[i], sectors[j])
    df         <- series_list[[key]]
    df         <- df[df$date %in% common_dates, ]
    df         <- df[order(df$date), ]
    stopifnot(nrow(df) == T)
    xx[, i, j] <- df$value
  }
}

# ── SANITY CHECKS ─────────────────────────────────────────────────────────────
cat("Array dimensions:", dim(xx), "\n")
cat("Total NAs:", sum(is.na(xx)), "\n")

# ── VISUALISE ─────────────────────────────────────────────────────────────────
library(tensorTS)

states  <- c("IL", "OH", "MI")
sectors <- c("MFG", "CONS", "SMS")

# ── SAVE PLOT ─────────────────────────────────────────────────────────────────
out_path <- "C:/Users/Igor/Desktop/bachelor-thesis-R/sectoral_employment.png"

png(filename = out_path,
    width    = 10,    # inches
    height   = 8,
    units    = "in",
    res      = 300)   # 300 dpi — print quality

# ── PLOT GRID WITH TITLES ─────────────────────────────────────────────────────
par(mfrow = c(length(states), length(sectors)),
    mar   = c(2, 2, 2, 1),
    oma   = c(1, 1, 3, 1))

n_plot <- 100

for (i in seq_along(states)) {
  for (j in seq_along(sectors)) {
    plot(xx[1:n_plot, i, j],
         type = "l",
         col  = "steelblue",
         xlab = "",
         ylab = "",
         main = paste0(states[i], sectors[j]),
         cex.main = 0.95)
  }
}

mtext("Sectoral Employment — First 100 Months",
      outer = TRUE, side = 3, line = 1, cex = 1.1, font = 2)

par(mfrow = c(1, 1))
dev.off()  # ← closes device and writes the file


# end of 01_data_setup.R
saveRDS(xx, "C:/Users/Igor/Desktop/bachelor-thesis-R/xx_tensor.rds")









