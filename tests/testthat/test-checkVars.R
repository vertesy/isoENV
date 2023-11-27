print("test-checkVars started")
rm(list = ls(all.names = TRUE))
library(testthat)
library(checkmate)
print("")


# isoENV::checkVars ----------------------------------------------------------------------------------------


print("checkVars 1")
test_that("checkVars does not issue a message for defined variables", {
  {
    test_env <- new.env()
    test_env$"var_defined" <- 1
    test_env$"var_null" <- NULL
    test_env$"var_na" <- NA
    test_env$"var_nan" <- as.numeric(NaN)
    test_env$"var_inf" <- Inf
    test_env$"var_empty" <- numeric(0)
  }

  output_variables <- c("var_defined")

  # Capture the message
  # expect_message(isoENV::checkVars(output_variables, test_env), "var_defined is defined and not empty")
  expect_null(isoENV::checkVars(output_variables, test_env), "var_defined is defined and not empty - thus NULL is returned")

  # Clean up
  rm(test_env)
})
print("")

cat("checkVars 2")
test_that("checkVars issues warnings appropriately", {
  {
    test_env <- new.env()
    test_env$"var_defined" <- 1
    test_env$"var_null" <- NULL
    test_env$"var_na" <- NA
    test_env$"var_nan" <- as.numeric(NaN)
    test_env$"var_inf" <- Inf
    test_env$"var_empty" <- numeric(0)
  }

  output_variables <- c(
    "var_defined", "var_null", "var_na",
    "var_nan", "var_inf", "var_empty", "var_missing"
  )

  # Capture the warnings
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_null is NULL")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_na is NA")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_nan is NaN")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_inf is Inf")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_empty is empty")
  expect_warning(isoENV::checkVars(output_variables, test_env), "var_missing is missing")
})
print("")



# rm('test_env')
