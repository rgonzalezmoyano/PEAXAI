### pruebas simulacions

# ===
# parameters
# ===

DGP <- "cobb_douglas_XnY1"
N <- 25
noise <- c(0, 0.02, 0.05)
nX <- 1

# ===
# generate simulations table
# ===

techniques_compare <- c("DEA", "BDEA", "cross-efficiency", "super-efficiency")
label_type <- c("additive", "BDEA")
cor_type <- c("spearman", "kendall")


repl <- 100

simulaciones <- data.frame (
  
  # ======= #
  # general #
  # ======= #
  
  id = rep(NA, repl),
  DGP = rep(NA, repl),
  scenario = rep(NA, repl),
  N = rep(NA, repl),
  noise = rep(NA, repl)
  
)
