# test-removeBigObjsFromEnv.R
print("test-removeBigObjsFromEnv.R started")
rm(list = ls(all.names = TRUE))
library(testthat)
library(checkmate)
print("")

library(testthat)


# ----------------------------------------------------------------------------------------
env <- new.env()
env$large_obj <- rnorm(1e7)
env$small_obj <- 1

# Get the names and sizes of the objects in env
obj_names <- ls(envir = env)
(obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env))))


# ----------------------------------------------------------------------------------------
print("test 1\n")
test_that("objects larger than max.size are removed", {
  expect_warning(env <- .removeBigObjsFromEnv(env, max.size = 1e6))
  expect_false("large_obj" %in% ls(env))
  expect_true("small_obj" %in% ls(env))
})
print("")

# ----------------------------------------------------------------------------------------
print("test 2\n")
test_that("invalid env argument causes an error", {
  expect_error(.removeBigObjsFromEnv(env = "not_an_env", max.size = 1e6))
})
print("")

# ----------------------------------------------------------------------------------------
print("test 3\n")
test_that("invalid max.size argument causes an error", {
  env <- new.env()
  expect_error(.removeBigObjsFromEnv(env, max.size = -1))
  expect_error(.removeBigObjsFromEnv(env, max.size = "not_numeric"))
})
print("")

# ----------------------------------------------------------------------------------------
print("test 4\n")
test_that("the returned object is an environment", {
  env <- new.env()
  result <- .removeBigObjsFromEnv(env, max.size = 1e6)
  expect_true(is.environment(result))
})
print("")
