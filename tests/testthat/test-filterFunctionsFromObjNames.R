print("test-.filterFunctionsFromObjNames started")
rm(list = ls(all.names = TRUE))
library(testthat)
library(checkmate)
print("")


# .filterFunctionsFromObjNames -------------------------------------------------------------------------

# Set up an environment for testing
testEnv <- new.env()
testEnv$x <- 4
testEnv$fff <- function(x) x^2
testEnv$missing_var <- NULL

# Begin tests
print(".filterFunctionsFromObjNames 1")
test_that("Function identifies functions correctly", {
  expect_warning(.filterFunctionsFromObjNames(names = c("x", "fff", "missing_var"), envir = testEnv))
  expect_equal(.filterFunctionsFromObjNames(names = c("x", "fff"), envir = testEnv), "x")
  expect_warning(.filterFunctionsFromObjNames(names = c("missing_var"), envir = testEnv), "missing_var does not exist in the environment")
})
print("")





# xxx -------------------------------------------------------------------------
