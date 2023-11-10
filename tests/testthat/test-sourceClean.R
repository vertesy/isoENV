print('test-sourceClean started')
rm(list = ls(all.names = TRUE))
library(testthat); library(checkmate); print('')


# sourceClean -------------------------------------------------------------------------

# Assume we have a script that squares an input variable 'x' and stores it in 'res'.
script_path <- tempfile()
writeLines("res <- x^2", con = script_path)

# Unit Test 1: Test sourcing with input and output variables
print('sourceClean 1')
test_that("sourceClean sources correctly with input and output variables", {
  x <- 4  # Define a global variable
  sourceClean(path = script_path,
               input.variables = c("x"),
               output.variables = c("res"),
               passAllFunctions = TRUE,
               returnEnv = FALSE)

  expect_equal(res, 16)
  # Clean up
  rm(res, envir = .GlobalEnv)
}); print('')

# Unit Test 2: Test sourcing without passing all functions
print('sourceClean 2')
test_that("sourceClean doesn't pass all functions when specified", {
  # Define a dummy function in the global environment
  dummy_func <- function() TRUE
  sourceClean(path = script_path,
               input.variables = c("x"),
               output.variables = c("res"),
               passAllFunctions = FALSE,
               input.functions = 'a',  # Intentionally not passing dummy_func
               returnEnv = TRUE)

  # Test the new environment does not contain dummy_func

  result_env_name <- paste0(".env.", basename(script_path))
  expect_false(exists("dummy_func", envir = get(result_env_name)))
  # Clean up
  # rm(list = c("dummy_func", result_env_name))
}); print('')


# -------------------------------------------------------------------------
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
