print("test-removeAllExceptFunctions started")
rm(list = ls(all.names = TRUE))
library(testthat)
library(checkmate)
print("")

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
print("")
print("removeAllExceptFunctions 2")
test_that("Variables are removed from specified environment", {
  test_env <- new.env()
  assign("a", 1, envir = test_env)
  assign("b", 2, envir = test_env)
  assign("fx", function() {
    TRUE
  }, envir = test_env)
  isoENV::removeAllExceptFunctions(env = test_env)

  expect_false(exists("a", envir = test_env))
  expect_false(exists("b", envir = test_env))
  expect_true(exists("fx", envir = test_env))

  # Clean up
  rm(test_env)
})
print("")
