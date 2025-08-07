# junkyard.R

sourceClean <- function(path, input.variables, output.variables
                        , passAllFunctions = TRUE, input.functions = NULL
                        , returnEnv = TRUE) {

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

  # Create new environment that does not see .GlobalEnv (not its parent)/
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


  # After sourcing, check if all output variables can be found in myEnv
  if (!all(output.variables %in% ls(envir = myEnv))) {
    cat('Missing output', fill=T)
    # cat(setdiff(output.variables, ls(envir = myEnv)), "variables are missing from the output environment of the script", fill = T)
    warning("variable(s) missing: " , paste(setdiff(output.variables, ls(envir = myEnv))
                                            , "from the output environment of the script"), immediate. = T)
    output.variables <- setdiff(ls(envir = myEnv), output.variables)

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

  if (returnEnv) {
    env.name <- paste0(".env.", script_name)
    assign(x = env.name, value = myEnv, envir = .GlobalEnv)
    cat("Script local environment is returned as:", env.name, '\n')
  }
  # return(myEnv)
}

