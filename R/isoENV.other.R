# ____________________________________________________________________
# isoENV.other.R  contains funcitons not used for now  ----
# ____________________________________________________________________
# devtools::load_all("~/GitHub/Packages/isoENV"); devtools::document("~/GitHub/Packages/isoENV")
# try(source("~/GitHub/Packages/isoENV/R/isoENV.other.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/isoENV/main/isoENV.R"), silent = TRUE)


# ______________________________________________________________________________________________----
# Section 1  ----
# ____________________________________________________________________


# ______________________________________________________________________________________________----
# 2. Debugging  ----
# ____________________________________________________________________

# ______________________________________________________________________________________________----
#' Remove Non-function Objects
#'
#' Removes all objects that are not functions from the specified environment.
#'
#' @param env environment from which to remove the variables, defaulting to the global environment.
#' @return Nothing, but modifies the environment in place.
#' @examples
#' # Define some variables and functions in the global environment
#' x <- 1
#' y <- "a"
#' z <- list(1, 2, 3)
#' f <- function(x) x + 1
#' g <- function(y) y * 2
#'
#' # Check the names of the objects in the global environment
#' ls()
#' # [1] "f" "g" "x" "y" "z"
#'
#' # Remove all objects except functions from the global environment
#' removeAllExceptFunctions()
#'
#' # Check the names of the objects in the global environment again
#' ls()
#' # [1] "f" "g"
#' @export
removeAllExceptFunctions <- function(envir = .GlobalEnv) {
  to_remove <- setdiff(ls(envir = envir), lsf.str(envir = envir))
  rm(list = to_remove, envir = envir)
}





# ______________________________________________________________________________________________----
# 3. Helpers  ----
# ____________________________________________________________________
