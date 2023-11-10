# 01.Global.R
# source('~/GitHub/Packages/isoENV/Development/Playground//01.Global.R')
devtools::document("~/GitHub/Packages/isoENV")
devtools::load_all("~/GitHub/Packages/isoENV");

rm(list = ls(all.names = TRUE))
require(checkmate)


my <- NULL
x <- 4
fff <- function(x) x^3

my.script = '~/GitHub/Packages/isoENV/Examples/Environment.02.Local.R'
devtools::document("~/GitHub/Packages/isoENV");

devtools::load_all("~/GitHub/Packages/isoENV");
sourceClean(path = my.script
             , input.variables = c('x', 'my', 'notmine')
             , output.variables = c('res','z', 'ys')
             , passAllFunctions = F
             , input.functions = "fff"
              )

res
y
z






