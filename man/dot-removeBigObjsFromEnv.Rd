% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isoENV.R
\name{.removeBigObjsFromEnv}
\alias{.removeBigObjsFromEnv}
\title{Remove large objects from an environment}
\usage{
.removeBigObjsFromEnv(env, max.size = 1e+06)
}
\arguments{
\item{env}{an environment from which large objects should be removed.}

\item{max.size}{a numeric value specifying the maximum size of an objects to keep in the env, in bytes.}
}
\value{
The modified environment with large objects removed.
}
\description{
This function removes objects from the specified environment that exceed a certain size.
}
\examples{
env <- new.env()
env$a <- rnorm(1e7)
env$b <- 1
# Get the names and sizes of the objects in env
obj_names <- ls(envir = env)
obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env)))
env <- removeBigObjsFromEnv(env, max.size = 1e6)
ls(env) # should not include 'a'
}
