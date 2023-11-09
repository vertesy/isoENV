library(testthat)



# Test that functions are not removed
test_that("Functions are not removed", {
  f <- function() return(TRUE)
  g <- function() return(FALSE)
  x <- 42

  isoENV::removeAllExceptFunctions(envir = environment())

  print(ls())
  expect_true(exists("f", envir = environment()))
  expect_true(exists("g", envir = environment()))
  expect_false(exists("x", envir = environment()))

  # Clean up
  rm(f, g)
})

# Test that it works on a specified environment
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
})

