# 01.Global.R
devtools::load_all("~/GitHub/Packages/isoENV");

# Define some stuff
my <- NULL
x <- 4
fff <- function(x) x^3

isoENV::sourceClean(path = './02.Local.R'
                    , input.variables = c('x', 'my', 'notmine')
                    , output.variables = c('res','z', 'ys')
                    , passAllFunctions = F
                    , input.functions = "fff")

# Check
res
y # not found
z

