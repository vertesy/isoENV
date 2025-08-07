# 01.Global.R
# source('~/GitHub/Packages/isoENV/Development/Playground//01.Global.R')
devtools::document("~/GitHub/Packages/isoENV")
devtools::load_all("~/GitHub/Packages/isoENV");

rm(list = ls(all.names = TRUE))
# require(checkmate)



(ProjectDir <- rstudioapi::getActiveProject())
my.script = paste0(ProjectDir, '/Development/Playground/02.dev.Local.R')



vNULL <- NULL
x <- 4
funFFF <- function(x) x^3

devtools::document(ProjectDir);
devtools::load_all(ProjectDir);

sourceClean(path = my.script
            , input.variables = c('x')
            , output.variables = c('res','z')
            , passAllFunctions = T, input.functions = c("funFFF")
)


sourceClean(path = my.script
            , input.variables = c('x', 'vNULL', 'vNotDefined', 'funFFF')
            , passAllFunctions = F, input.functions = c("funFFF", "funGGG", 'x', 'vNotDefined' )
            , output.variables = c('res','z', 'vMissing', 'vNA', 'funGGG')
)



sourceClean(path = my.script
            , input.variables = c('x', 'vNULL', 'vNotDefined')
            , output.variables = c('res','z', 'vMissing', 'vNA', 'funGGG')
            , passAllFunctions = F, input.functions = c("funFFF", "funGGG")
)




res
y
z





write an R function that takes a char vec of object names, and checks if they correspond to variables or functions?
  gives warning() with the name of functions and returns the list of variable.names()
use camelcase. make roxygen

x <- 4
fff <- function(x) x^3

...
function(names <- c('x', 'fff'))
  ...

?variable.names(myEnv)

variables <- c("x", "my", "vNotDefined", "funGGG")

variables[sapply(variables, exists)]

variables[sapply(variables, FUN = is.function(get(envir = .GlobalEnv)))]

sapply(variables, function(x) is.function(get(x, envir = .GlobalEnv)))

