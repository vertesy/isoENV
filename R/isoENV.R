# ____________________________________________________________________
# isoENV helps you to work with isolated environments.  ----
# ____________________________________________________________________
# devtools::load_all("~/GitHub/Packages/isoENV"); devtools::document("~/GitHub/Packages/isoENV")
# try(source("~/GitHub/Packages/isoENV/R/isoENV.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/isoENV/main/isoENV.R"), silent = TRUE)


# ______________________________________________________________________________________________----
# Section 1  ----
# ____________________________________________________________________




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
#' [1] "f" "g" "x" "y" "z"
#'
#' # Remove all objects except functions from the global environment
#' removeAllExceptFunctions()
#'
#' # Check the names of the objects in the global environment again
#' ls()
#' [1] "f" "g"
#' @export
removeAllExceptFunctions <- function(envir = .GlobalEnv) {
  to_remove <- setdiff(ls(envir = envir), lsf.str(envir = envir))
  rm(list = to_remove, envir = envir)

}

# ________________________________________________________________________________________________
#' Source Script in a New Environment and Pass Functions
#'
#' This function creates a new environment with the base environment as its parent,
#' sources an R script into this new environment, and then copies all functions
#' from the global environment to the new environment.
#'
#' @param scriptPath The path to the R script that should be sourced.
#' @return A new environment containing the sourced script and copied functions.
#' @export
#' @examples
#' \dontrun{
#'   myEnv <- sourceScriptAndPassFunctions('~/path/to/script.R')
#' }
sourceScriptAndPassFunctions <- function(scriptPath) {
  myEnv <- new.env(parent = baseenv())
  source(file = scriptPath, local = myEnv)

  # Identify and copy functions from the global environment to myEnv
  globalFunctions <- lsf.str(envir = .GlobalEnv)
  list2env(mget(globalFunctions, .GlobalEnv), envir = myEnv)

  return(myEnv)
}


# ______________________________________________________________________________________________----
# Section 2  ----
# ____________________________________________________________________





# ______________________________________________________________________________________________----
# Section 3  ----
# ____________________________________________________________________

