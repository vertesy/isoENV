# ____________________________________________________________________
# isoENV helps you to work with isolated environments.  ----
# ____________________________________________________________________
# devtools::load_all("~/GitHub/Packages/isoENV");
# devtools::document("~/GitHub/Packages/isoENV"); devtools::load_all("~/GitHub/Packages/isoENV");
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
#' @param returnEnv Logical; if TRUE, assigns the script environment to the global environment.
#' @param removeBigObjs Logical; if TRUE, cleans the script environment from big objects, and return the remaing env to the global environment.
#' @param max.size a numeric value specifying the maximum size of an objects to keep in the env, in bytes. Default =1e6 (1MB).
#' @param ... Arguments to pass on to source()
#' @return No return value, the function returns variables into the .GlobalEnv.
#' @export
#' @examples
#' \dontrun{
#'   sourceClean(path = "path/to/your/script.R",
#'                input.variables = c("x"),
#'                output.variables = c("res"),
#'                passAllFunctions = TRUE,
#'                input.functions = NULL,
#'                returnEnv = TRUE)
#' }
sourceClean <- function(path, input.variables, output.variables
                        , passAllFunctions = TRUE, input.functions = NULL
                        , returnEnv = TRUE, removeBigObjs = TRUE, max.size = 1e6
                        , ...) {

  # Argument assertions
  stopifnot(
    is.character(path),
    is.character(input.variables),
    is.character(output.variables),
    "Either passAllFunctions OR give a character of fun names" =
      passAllFunctions || is.character(input.functions)
  )
  script_name <- basename(path)

  # ________________________________________________________________________________________________
  # Input Variables ----

  objects.existing <- checkVars(input.variables, envir =  globalenv(), prefix = "Problematic INPUT!\n")
  obj.is.function <- sapply(objects.existing, function(x) is.function(get(x, envir = .GlobalEnv)))
  if(any(obj.is.function )) {
    xm <- cat("FUNCTIONS passed to input.variables:", objects.existing[obj.is.function]
              , '\nSkipped.\n')
  }

  # Create new environment that does not see .GlobalEnv (not it's parent)/
  myEnv <- new.env(parent = baseenv())

  # Copy specified global variables to myEnv
  vars <- mget(input.variables, .GlobalEnv, ifnotfound = NA)
  vars <- Filter(Negate(is.na), vars)
  list2env(vars, envir = myEnv)

  # ________________________________________________________________________________________________
  # Input Functions ----

  # Depending on the flag, either pass all functions or only specified ones
  if (isFALSE(passAllFunctions)) {
    if (length(input.functions) == 0) {
      stop("input.functions must be provided if passAllFunctions is FALSE")
    } else {
      if(checkmate::anyMissing(input.functions)) warning("Missing function!\n", immediate. = T)
      objects.existing <- checkVars(input.functions, envir =  globalenv(), prefix = "Missing FUNCTIONS!\n")
      obj.is.function <- sapply(objects.existing, function(x) is.function(get(x, envir = .GlobalEnv)))

      if(any(!obj.is.function )) {
        xm <- cat("Non-FUNCTIONS passed to input.functions:", objects.existing[!obj.is.function]
                  , '\nSkipped.\n')
      }
      input.functions <- objects.existing[obj.is.function]
    }
  }

  functions2pass <-
    if (passAllFunctions) {
      lsf.str(envir = .GlobalEnv)
    } else {
      input.functions
    }

  # Copy functions from the global environment to myEnv
  functionsToPass <- mget(functions2pass, .GlobalEnv, ifnotfound = NA)
  # functionsToPass <- Filter(Negate(is.na), functionsToPass)
  list2env(functionsToPass, envir = myEnv)

  # ________________________________________________________________________________________________
  # Source the script in myEnv
  source(file = path, local = myEnv, ...)


  # ________________________________________________________________________________________________
  # Output Variables ----
  output.variables.existing <- checkVars(output.variables, envir =  myEnv, prefix = "Problematic OUTPUT!\n")

  # ________________________________________________________________________________________________
  # Output Functions ----
  "Output Functions are not checked atm."

  # Copy specified myEnv variables back to .GlobalEnv
  varsOut <- mget(output.variables.existing, envir = myEnv, ifnotfound = NA)
  varsOut <- Filter(Negate(is.na), varsOut)
  cat(">> Returning:", output.variables.existing, 'from', script_name, '\n')
  list2env(varsOut, envir = .GlobalEnv)

  if (returnEnv) {
    env.name <- paste0(".env.", script_name)
    if (removeBigObjs) myEnv <- isoENV::.removeBigObjsFromEnv(myEnv, max.size = max.size)

    assign(x = env.name, value = myEnv, envir = .GlobalEnv)
    cat(">> Script local environment is returned as:", env.name, '\n')
  }
  # return(myEnv)
}



# ______________________________________________________________________________________________----
# 2. Debugging  ----
# ____________________________________________________________________

# See: source("~/GitHub/Packages/isoENV/R/isoENV.other.RR")


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
checkVars <- function(variables, envir, verbose = F
                      , prefix = 'Problematic variables!\n', suffix = NULL) {

  stopifnot(is.character(variables), is.environment(envir))

  # filter out functions that should be returned!
  # variables <- isoENV:::.filterFunctionsFromObjNames(variables, envir = envir)

  cat("\n--------------------------------------------------------------------------------\n"
      , length(variables), "variables are checked for content in", substitute(variables), variables, suffix, "\n")

  wasProblem <- FALSE
  for (var in variables) {
    # cat("Checking variable:", var, "\n")
    if (!exists(var, envir = envir)) {
      warning(var, " is missing", immediate. = T); wasProblem <- TRUE
    } else {
      value <- get(var, envir = envir)
      if (is.function(value)) {
        # cat(var, "is a function, skipping...\n")
        next
      } else if (is.null(value)) {
        warning(var, " is NULL", immediate. = T); wasProblem <- TRUE
      } else if (identical(value, NA)) {
        warning(var, " is NA", immediate. = T); wasProblem <- TRUE
      } else if (length(value) == 0) {
        warning(var, " is empty", immediate. = T); wasProblem <- TRUE
      } else if (is.numeric(value) && any(is.nan(value))) {
        warning(var, " contains NaN values", immediate. = T); wasProblem <- TRUE
      } else if (is.numeric(value) && any(is.infinite(value))) {
        warning(var, " contains Inf values", immediate. = T); wasProblem <- TRUE
      } else if (verbose) {
        message(var, " is defined and not empty", immediate. = T); wasProblem <- TRUE
      }
    }
  } # for

  if(!is.null(prefix) && wasProblem ) cat(as.character(prefix), fill=T)

  variables.existing <- variables[sapply(variables, exists)]
  return(variables.existing)
}

# ______________________________________________________________________________________________----
# 3. Private Helper Functions  ----
# ____________________________________________________________________

#' Check Names for Variable or Function Type
#'
#' This function takes a character vector of object names and checks whether
#' they correspond to variables or functions within the provided environment.
#' It issues warnings for function names and for missing objects, and returns a list of variable names.
#'
#' @param names A character vector containing names to check.
#' @param envir The environment in which to check for names.
#' @return A character vector of variable names.
#' @export
#' @examples
#' myEnv <- new.env()
#' myEnv$x <- 4
#' myEnv$fff <- function(x) x^3
#' .filterFunctionsFromObjNames(c('x', 'fff'), envir = myEnv)

.filterFunctionsFromObjNames <- function(names, envir) {
  # Argument assertions
  stopifnot(is.character(names), is.environment(envir))

  idxFuns <- sapply(names, function(name) {
    # Check if the object exists in the environment
    if (!exists(name, envir = envir)) {
      warning(name, " does not exist in the environment.\n")
      return(FALSE)
    }
    is.function(get(name, envir = envir))
  })

  FunNames <- names(idxFuns)[idxFuns]
  VarNames <- names(idxFuns)[!idxFuns]

  if (length(FunNames) > 0) {
    warning(paste(paste(FunNames, "is a function.\n"), collapse = " "), immediate. = TRUE
            # , "Only variables are returned: ", paste(VarNames, collapse = ", ")
            )
  }

  return(VarNames)
}



# ____________________________________________________________________
#' Remove large objects from an environment
#'
#' This function removes objects from the specified environment that exceed a certain size.
#' @param env an environment from which large objects should be removed.
#' @param max.size a numeric value specifying the maximum size of an objects to keep in the env, in bytes.
#' @return The modified environment with large objects removed.
#' @examples
#' env <- new.env()
#' env$a <- rnorm(1e7)
#' env$b <- 1
#' # Get the names and sizes of the objects in env
#' obj_names <- ls(envir = env)
#' obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env)))
#' env <- removeBigObjsFromEnv(env, max.size = 1e6)
#' ls(env) # should not include 'a'
#' @export

.removeBigObjsFromEnv <- function(env, max.size = 1e6) {

  # Assertions for input arguments
  stopifnot(is.environment(env))
  stopifnot(is.numeric(max.size) && max.size > 0)

  # Get the names and sizes of the objects in env
  obj_names <- ls(envir = env)
  obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env)))

  # Filter the names of the objects that are bigger than max.size
  big_objs <- obj_names[obj_sizes > max.size]

  # Remove the big objects from env
  rm(list = big_objs, envir = env)

  # Warn the user about the names of the objects that were removed
  if (length(big_objs) > 0) {
    warning(paste("Objects were bigger than and",
                  format(max.size, scientific = FALSE, big.mark = ","), "bytes are removed from",
                  substitute(env), "\n",
                  paste(big_objs, collapse = ", ")))
  }

  # Return the modified environment
  return(env)

  # Output assertion
  stopifnot(is.environment(env))
  return(env)
}


# ____________________________________________________________________


# ____________________________________________________________________


# ______________________________________________________________________________________________----
# 5. Code checks (non-core functionality)  ----
# ____________________________________________________________________


#' @title Enforce Strict Evaluation in a Function
#'
#' @description Modifies an existing function (`f1`) to enforce strict evaluation of its arguments.
#' This is achieved by redefining the function with its arguments evaluated in a specified environment,
#' which can help in debugging and ensuring that all variables are defined before function execution.
#' @source From: moodymudskipper at https://stackoverflow.com/questions/6216968/r-force-local-scope/
#'
#' @param f1 The function for which strict evaluation is to be enforced.
#' @param ... Additional arguments to be passed to the modified function.
#'
#' @return The result of executing the modified function with strict evaluation.
#' @export
#'
#' @examples
#' # Define a sample function
#' Z <- 2
#' funOK <- function(x, y) { x + y }
#' funBAD <- function(x, y) { x + Z }
#' funOKwoParenthesis <- function(x, y) x + 1
#'
#' # Check strict evaluation
#' strict(funOK, 1, 2)
#' strict(funBAD, 1, 2)
#' strict(funOKwoParenthesis, 1, 2)
strict <- function(f1, ...){
  function_text <- deparse(f1)
  function_text <- paste(function_text[1], function_text[2]
                         , paste(function_text[c(-1, -2, -length(function_text))], collapse =";")
                         , "}", collapse = "")
  strict0 <- function(f1, pos=2) eval(substitute(f1), as.environment(pos))
  f1 <- eval(parse(text = paste0("strict0(", function_text, ")")))
  do.call(f1,list(...))
}



# ____________________________________________________________________
#' @title Check for Use of Global Variables in a Function
#'
#' @description This function checks whether the specified function (`f`) uses any global variables.
#' It returns `TRUE` if no global variables are used, and `FALSE` otherwise. If global variables are found
#' and `silent` is `FALSE`, a warning is issued listing the global variables.
#'
#' @param f The function to be checked for global variable usage.
#' @param silent Logical parameter with a default value of `FALSE`.
#' If `TRUE`, the function suppresses warnings about global variable usage.
#' @importFrom codetools findGlobals
#' @return Returns `TRUE` if no global variables are used in the function `f`, `FALSE` otherwise.
#' If `silent` is `FALSE` and global variables are found, a warning is issued.
#' @export
#'
#' @examples
#' testFunction <- function(x, y) { z <- x + y; return(z) }
#' checkGlobalVars(testFunction)
#' checkGlobalVars(testFunction, silent = TRUE)
checkGlobalVars <- function(f, silent = FALSE) {

  stopifnot(is.function(f), is.logical(silent))

   if (!requireNamespace("codetools", quietly = TRUE)) {
    stop("Please install codetools, using install.packages('codetools')")
  }
  vars <- codetools::findGlobals(f)
  found <- !vapply(vars, exists, logical(1), envir=as.environment(2))
  if (!silent && any(found)) {
    warning("global variables used: ", paste(names(found)[found], collapse=', '))
    return(invisible(FALSE))
  }
  !any(found)
}



# ____________________________________________________________________




# ____________________________________________________________________

