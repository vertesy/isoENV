print('Test 2 started')
rm(list = ls(all.names = TRUE))
library(testthat); library(checkmate); print('')

# removeAllExceptFunctions -------------------------------------------------------------------------
library(testthat)
library(devtools)

# Assume 'ProjectDir' is defined and points to the directory of your R package
devtools::document(ProjectDir)
devtools::load_all(ProjectDir)

# Define the environment and variables for testing
vNULL <- NULL
x <- 4
funFFF <- function(x) x^3

# Define a path to the script
my.script <- file.path("~/my.script.R")

# Write the content to 'my.script'
writeLines(
  "y <- 2 * x
   res <- funFFF(y)
   cat('Result is:', res, fill = T)
   z <- 33
   vNA <- NA
   funGGG <- function(x) x^3",
  con = my.script
)

# Test 1: Handle various input variables and functions, including undefined and NULL
test_that("sourceClean handles various input variables and functions", {
  expect_warning(sourceClean(path = my.script
                             , input.variables = c('x', 'vNULL', 'vNotDefined', 'funFFF')
                             , passAllFunctions = FALSE
                             , input.functions = c("funFFF", "funGGG", 'x', 'vNotDefined')
                             , output.variables = c('res','z', 'vMissing', 'vNA', 'funGGG')
  ))
})

# Test 2: Handle minimal input and output, expecting no warnings or errors
test_that("sourceClean handles minimal input and output without errors", {
  expect_no_warning(sourceClean(path = my.script
                            , input.variables = c('x')
                            , output.variables = c('res','z')
                            , passAllFunctions = FALSE
                            , input.functions = c("funFFF")
  ))
})

# Test 3: Handle a mix of defined and undefined inputs and outputs
test_that("sourceClean handles a mix of defined and undefined inputs and outputs", {
  expect_warning(sourceClean(path = my.script
                             , input.variables = c('x', 'vNULL', 'vNotDefined')
                             , output.variables = c('res','z', 'vMissing', 'vNA', 'funGGG')
                             , passAllFunctions = FALSE
                             , input.functions = c("funFFF", "funGGG")
  ))
})
