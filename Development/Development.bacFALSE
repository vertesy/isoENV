# ____________________________________________________________________
# isoENV helps you to work with isolated environments.  ----
# ____________________________________________________________________
# devtools::load_all("~/GitHub/Packages/isoENV"); devtools::document("~/GitHub/Packages/isoENV")
# try(source("~/GitHub/Packages/isoENV/R/isoENV.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/isoENV/main/isoENV.R"), silent = TRUE)


# ______________________________________________________________________________________________----
# Section 1  ----
# ____________________________________________________________________


# ________________________________________________________________________________________________
#' Source a script with strict environment control
#'
#' This function sources a script file into a new environment. It can selectively import variables
#' and functions from the global environment and return specified variables back to the global environment.
#'
#' @param path The file path of the R script to be sourced.
#' @param input.variables A character vector of global variable names to be passed on.
#' @param output.variables A character vector of variable names from the sourced environment to be returned to the global environment.
#' @param passAllFunctions Logical; if TRUE, all global functions are passed on, otherwise only those in input.functions.
#' @param input.functions A character vector of global function names to be passed on if passAllFunctions is FALSE.
#' @param assignEnv Logical; if TRUE, assigns the new environment to the global environment.
#' @return No return value, the function returns variables into the .GlobalEnv.
#' @export
#' @examples
#' \dontrun{
#'   sourceClean(path = "path/to/your/script.R",
#'                input.variables = c("x"),
#'                output.variables = c("res"),
#'                passAllFunctions = TRUE,
#'                input.functions = NULL,
#'                assignEnv = TRUE)
#' }
sourceClean <- function(path, input.variables, output.variables
                         , passAllFunctions = TRUE, input.functions = NULL
                         , assignEnv = TRUE) {

  # Argument assertions
  stopifnot(
    is.character(path),
    is.character(input.variables),
    is.character(output.variables),
    "Either passAllFunctions OR give a character of fun names" =
      passAllFunctions || is.character(input.functions)
  )
  script_name <- basename(path)

  checkVars(input.variables, envir =  globalenv())

  # Create new environment that does not see .GlobalEnv (not it's parent)/
  myEnv <- new.env(parent = baseenv())

  # Copy specified global variables to myEnv
  vars <- mget(input.variables, .GlobalEnv, ifnotfound = NA)
  vars <- Filter(Negate(is.na), vars)
  list2env(vars, envir = myEnv)

  # Depending on the flag, either pass all functions or only specified ones
  if (passAllFunctions) {
    globalFunctions <- lsf.str(envir = .GlobalEnv)
  } else {
    if (is.null(input.functions) || length(input.functions) == 0) {
      stop("input.functions must be provided if passAllFunctions is FALSE")
    }
    globalFunctions <- input.functions
  }

  # Copy functions from the global environment to myEnv
  functionsToPass <- mget(globalFunctions, .GlobalEnv, ifnotfound = NA)
  # functionsToPass <- Filter(Negate(is.na), functionsToPass)
  list2env(functionsToPass, envir = myEnv)

  # Source the script in myEnv
  source(file = path, local = myEnv)

  cat(11)
  # After sourcing, check if all output variables can be found in myEnv
  if (all(output.variables %in% ls(envir = myEnv))) {
    cat(setdiff(output.variables, ls(envir = myEnv)), "variables are missing from the output environment of the script", fill = T)
  }
  # checkmate::assertSubset(x = output.variables, choices = ls(envir = myEnv),
  #                         add = "Not all of the output.variables were found to be returned.")

  # Warn if any of the output variables are NULL, NA, or empty
  if (any(sapply(output.variables, function(x) checkmate::anyMissing(mget(x, envir = myEnv))))) {
    warning("Some of the output.variables are NULL, NA, or empty.")
  }

  checkVars(output.variables, envir =  myEnv)
  cat(">> Returning:", output.variables, 'from', script_name, '\n')

  # Copy specified myEnv variables back to .GlobalEnv
  varsOut <- mget(output.variables, envir = myEnv, ifnotfound = NA)
  varsOut <- Filter(Negate(is.na), varsOut)
  list2env(varsOut, envir = .GlobalEnv)

  if (assignEnv) {
    env.name <- paste0(".env.", script_name)
    assign(x = env.name, value = myEnv, envir = .GlobalEnv)
    cat("Script local environment is returned as:", env.name, '\n')
  }
  # return(myEnv)
}




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





# ______________________________________________________________________________________________----
# 3. Helpers  ----
# ____________________________________________________________________

#' Check Variables in an Environment
#'
#' This function iterates over a list of variable names and checks their
#' existence and value in a given environment. It issues warnings for variables
#' that are missing, NULL, NA, NaN, infinite, or empty, and sends a message for
#' variables that are defined and not empty.
#'
#' @param variables A character vector of variable names to check.
#' @param envir The environment in which to look for the variables.
#' @param verbose Report on good (passing) variables.
#' @param suffix Suffix to append to printed summary statment.
#' @return No return value, called for side effects.
#' @examples
#' myEnv <- new.env()
#' myEnv$aaa <- 111
#' myEnv$xxx <- NULL
#' myEnv$yyy <- list()
#' myEnv$zzz <- numeric()
#' output.variables <- c('aaa','xxx', 'zzz', 'yyy', 'bbb')
#' checkVars(output.variables, envir = myEnv)
#' @export
checkVars <- function(variables, envir, verbose = F, suffix = NULL) {

  stopifnot(is.character(variables), is.environment(envir))

  cat(length(variables), substitute(variables), "are checked for content:", variables, suffix, "\n")

  for (var in variables) {
    value <- envir[[var]]
    if (!exists(var, envir = envir)) {
      warning(var, " is missing")
    } else if (is.null(value)) {
      warning(var, " is NULL", value)
    } else if (identical(value, NA)) {
      warning(var, " is NA: ", value)
    } else if (length(value) == 0) {
      warning(var, " is empty")
    } else if (is.nan(value)) {
      warning(var, " is NaN: ", value)
    } else if (is.infinite(value)) {
      warning(var, " is Inf: ", value)
    } else if (verbose) {
      message(var, " is defined and not empty")
    }
  }
}


# ______________________________________________________________________________________________----
# 3. Private Helper Functions  ----
# ____________________________________________________________________
