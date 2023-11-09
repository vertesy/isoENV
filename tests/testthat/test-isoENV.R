rm(list = ls(all.names = TRUE))
library(testthat); print('')

# removeAllExceptFunctions -------------------------------------------------------------------------

# Test that functions are not removed
print('removeAllExceptFunctions 1')
test_that("Functions are not removed", {
  f <- function() return(TRUE)
  g <- function() return(FALSE)
  x <- 42

  isoENV::removeAllExceptFunctions(envir = environment())
  # print(ls())
  expect_true(exists("f", envir = environment() ))
  expect_true(exists("g", envir = environment() ))
  expect_false(exists("x", envir = environment() ))

  # Clean up
  rm(list = c("f", "g"), envir = environment())
}); print('')


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
  expect_message(isoENV::checkVars(output_variables, test_env), "var_defined is defined and not empty")

  # Clean up
  rm(test_env)
}); print('')

# cat('checkVars 2')
# test_that("checkVars issues warnings appropriately", {
#
#   {
#     test_env <- new.env()
#     test_env$'var_defined' <- 1
#     test_env$'var_null' <- NULL
#     test_env$'var_na' <- NA
#     test_env$'var_nan' <- as.numeric(NaN)
#     test_env$'var_inf' <- Inf
#     test_env$'var_empty' <- numeric(0)
#   }
#
#   output_variables <- c("var_defined", "var_null", "var_na",
#                         "var_nan", "var_inf", "var_empty", "var_missing")
#
#   # Capture the warnings
#   expect_warning(isoENV::checkVars(output_variables, test_env), "var_null is NULL")
#   expect_warning(isoENV::checkVars(output_variables, test_env), "var_na is NA")
#   expect_warning(isoENV::checkVars(output_variables, test_env), "var_nan is NaN")
#   expect_warning(isoENV::checkVars(output_variables, test_env), "var_inf is Inf")
#   expect_warning(isoENV::checkVars(output_variables, test_env), "var_empty is empty")
#   expect_warning(isoENV::checkVars(output_variables, test_env), "var_missing is missing")
# }); print('')



# rm('test_env')

# xxx -------------------------------------------------------------------------



# xxx -------------------------------------------------------------------------



# xxx -------------------------------------------------------------------------



# xxx -------------------------------------------------------------------------

