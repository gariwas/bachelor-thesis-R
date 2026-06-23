xx_diff <- readRDS("C:/Users/Igor/Desktop/bachelor-thesis-R/xx_diff_tensor.rds")

T_len <- dim(xx_diff)[1]
d1    <- dim(xx_diff)[2]
d2    <- dim(xx_diff)[3]


# ══════════════════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════════════════

get_mode_matrices <- function(est) {
  node <- est$A[[1]][[1]]
  if (length(node) == 1 && is.list(node[[1]])) node <- node[[1]]
  A <- matrix(node[[1]], nrow = sqrt(length(node[[1]])))
  B <- matrix(node[[2]], nrow = sqrt(length(node[[2]])))
  list(A = A, B = B)
}

compute_residuals <- function(xx, A, B) {
  T_len <- dim(xx)[1]
  d1    <- dim(xx)[2]
  d2    <- dim(xx)[3]
  res   <- array(0, dim = c(T_len - 1, d1, d2))
  for (t in 2:T_len) {
    res[t - 1, , ] <- xx[t, , ] - A %*% xx[t - 1, , ] %*% t(B)
  }
  res
}

compute_bic <- function(res, n_params) {
  n_obs  <- length(res)
  sigma2 <- sum(res^2) / n_obs
  n_obs * log(sigma2) + n_params * log(n_obs)
}

run_method <- function(xx, method, states, sectors, vis_dir) {
  cat("\n══════════════════════════════════════════════════\n")
  cat("METHOD:", method, "\n")
  cat("══════════════════════════════════════════════════\n")
  
  set.seed(123)
  est <- tenAR.est(xx, R = 1, P = 1, method = method)
  saveRDS(est, paste0("C:/Users/Igor/Desktop/bachelor-thesis-R/est_", tolower(method), ".rds"))
  
  modes <- get_mode_matrices(est)
  A <- modes$A
  B <- modes$B
  rownames(A) <- colnames(A) <- states
  rownames(B) <- colnames(B) <- sectors
  
  cat("\nA (states × states):\n");   print(round(A, 3))
  cat("\nB (sectors × sectors):\n"); print(round(B, 3))
  
  # Residuals
  if (!is.null(est$res) && length(dim(est$res)) == 3) {
    res <- est$res
  } else {
    res <- compute_residuals(xx, A, B)
  }
  cat("Residual dimensions:", dim(res), "\n")
  
  # BIC — always use manual formula for comparability across methods
  bic_val <- compute_bic(res, d1^2 + d2^2)
  cat("BIC (manual):", round(bic_val, 2), "\n")
  
  # Eigenvalue stability
  eig_A    <- Mod(eigen(A)$values)
  eig_B    <- Mod(eigen(B)$values)
  max_kron <- max(outer(eig_A, eig_B))
  
  cat("\nEigenvalue moduli of A:\n"); print(round(eig_A, 4))
  cat("Eigenvalue moduli of B:\n");   print(round(eig_B, 4))
  cat("Max |λ_i(A)·λ_j(B)|:", round(max_kron, 4), "\n")
  cat("Stability condition satisfied:", max_kron < 1, "\n")
  
  # ACF plot — res is already T × d1 × d2
  png(file.path(vis_dir, paste0("acf_residuals_", tolower(method), ".png")),
      width = 10, height = 8, units = "in", res = 300)
  mplot.acf(res)
  dev.off()
  
  invisible(list(est = est, A = A, B = B, res = res))
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 6–8: FIT ALL THREE METHODS
# ══════════════════════════════════════════════════════════════════════════════

vis_dir <- "C:/Users/Igor/Desktop/bachelor-thesis-R/visualisations"

results       <- list()
results$PROJ  <- run_method(xx_diff, "PROJ", states, sectors, vis_dir)
results$LSE   <- run_method(xx_diff, "LSE",  states, sectors, vis_dir)
results$MLE   <- run_method(xx_diff, "MLE",  states, sectors, vis_dir)

saveRDS(results$LSE, "C:/Users/Igor/Desktop/bachelor-thesis-R/est_mar.rds")