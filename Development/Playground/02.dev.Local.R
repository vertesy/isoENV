# 02.Local.R
# source('~/GitHub/Packages/isoENV/Development/Playground/02.Local.R')

# devtools::document("~/GitHub/Packages/isoENV")
# devtools::load_all("~/GitHub/Packages/isoENV");


y <- 2 * x
res <- funFFF(y)

cat("Result is:", res, fill = T)

z <- 33
vNA <- NA

funGGG <- function(x) x^3

# defines: y, z, res
# returns to .GlobalEnv:
#   full env as `.env.02.Local.R`
#     if sourceClean(assignEnv = TRUE)`
#   variables defined in `sourceClean(output.variables)`


