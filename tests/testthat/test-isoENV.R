print('Test started')
rm(list = ls(all.names = TRUE))
library(testthat); library(checkmate); print('')

# removeAllExceptFunctions -------------------------------------------------------------------------

# Test that functions are not removed
# print('removeAllExceptFunctions 1')
# test_that("Functions are not removed", {
#   f <- function() return(TRUE)
#   g <- function() return(FALSE)
#   x <- 42
#
#   isoENV::removeAllExceptFunctions(envir = environment())
#   # print(ls())
#   expect_true(exists("f", envir = environment() ))
#   expect_true(exists("g", envir = environment() ))
#   expect_false(exists("x", envir = environment() ))
#
#   # Clean up
#   rm(list = c("f", "g"), envir = environment())
# }); print('')


# Test that it works on a specified environment
print(''); print('removeAllExceptFunctions 2')
test_that("Variables are removed from specified environment", {
  test_env <- new.env()
  assign("a", 1, envir = test_env)
  assign("b", 2, envir = test_env)
  assign("fx", function() { TRUE }, envir = test_env)
  isoENV::removeAllExceptFunctions(env = test_env)

  expect_false(exists("a", envir = test_env))
  expect_false(exists("b", envir = test_env))
  expect_true(exists("fx", envir = test_env))

  # Clean up
  rm(test_env)
}); print('')


# isoENV::checkVars ----------------------------------------------------------------------------------------


print('checkVars 1')
test_that("checkVars does not issue a message for defined variables", {

  {
    test_env <- new.env()
    test_env$'var_defined' <- 1
    test_env$'var_null' <- NULL
    test_env$'var_na' <- NA
    test_env$'var_nan' <- as.numeric(NaN)
    test_env$'var_inf' <- Inf
    test_env$'var_empty' <- numeric(0)
  }

  output_variables <- c("var_defined")

  # Capture the message
  # expect_message(isoENV::checkVars(output_variables, test_env), "var_defined is defined and not empty")
  expect_null(isoENV::checkVars(output_variables, test_env), "var_defined is defined and not empty - thus NULL is returned")

  # Clean up
  rm(test_env)
}); print('')

cat('checkVars 2')
test_that("checkVars issues warnings appropriately", {

  {
    test_env <- new.env()
    test_env$'var_defined' <- 1
    test_env$'var_null' <- NULL
    test_env$'var_na' <- NA
    test_env$'var_nan' <- as.numeric(NaN)
    test_env$'var_inf' <- Inf
    test_env$'var_empty' <- numeric(0)
  }

  output_variables <- c("var_defined", "var_null", "var_na",
                        "var_nan", "var_inf", "var_empty", "var_missing")

  # Capture the warnings
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_null is NULL")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_na is NA")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_nan is NaN")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_inf is Inf")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_empty is empty")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_missing is missing")
}); print('')



# rm('test_env')

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

# More tests can be added to cover all branches and possible edge cases.



# xxx -------------------------------------------------------------------------



# xxx -------------------------------------------------------------------------



# xxx -------------------------------------------------------------------------

