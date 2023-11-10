# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(isoENV)

test_check("isoENV")

devtools::document("~/GitHub/Packages/isoENV")
devtools::load_all("~/GitHub/Packages/isoENV");

# Run the tests


'OKAY'
test_file(file = "~/GitHub/Packages/isoENV/tests/testthat/test-removeBigObjsFromEnv.R")
test_file(file = "~/GitHub/Packages/isoENV/tests/testthat/test-removeAllExceptFunctions.R")

'FAIL'
test_file(file = "~/GitHub/Packages/isoENV/tests/testthat/test-sourceClean.R")
test_file(file = "~/GitHub/Packages/isoENV/tests/testthat/test-checkVars.R")
test_file(file = "~/GitHub/Packages/isoENV/tests/testthat/test-filterFunctionsFromObjNames.R")



