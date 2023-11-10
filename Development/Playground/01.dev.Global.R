# 01.Global.R
# source('~/GitHub/Packages/isoENV/Development/Playground//01.Global.R')
devtools::document("~/GitHub/Packages/isoENV")
devtools::load_all("~/GitHub/Packages/isoENV");

rm(list = ls(all.names = TRUE))
require(checkmate)

(ProjectDir <- rstudioapi::getActiveProject())

my <- NULL
x <- 4
fff <- function(x) x^3

my.script = paste0(ProjectDir, '/Development/Playground/02.dev.Local.R')
devtools::document(ProjectDir);
devtools::load_all(ProjectDir);

sourceClean(path = my.script
             , input.variables = c('x', 'my', 'notmine')
             , output.variables = c('res','z', 'ys')
             , passAllFunctions = F
             , input.functions = "fff"
              )

res
y
z






