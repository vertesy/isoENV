######################################################################################################
# Create_the_isoENV_Package.R
######################################################################################################
# source("~/GitHub/Packages/isoENV/Development/Create_the_isoENV_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
require("devtools")
require("roxygen2")
require("stringr")


# Setup ------------------------
PackageName <- "isoENV"
package.version <- "0.0.1"
setwd("~/GitHub/Packages/")

(RepositoryDir <- paste0("~/GitHub/Packages/", PackageName, "/"))
(fname <- paste0(PackageName, ".R"))
(Package_FnP <- paste0(RepositoryDir, "R/", fname))

(BackupDir <- paste0("~/GitHub/Packages/", PackageName, "/Development/"))
dir.create(BackupDir)


DESCRIPTION <- list("Title" = "isoENV helps you to work with isolated environments"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "isoENV is a set of R functions that help you to work with isolated environments"
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = package.version
    , "Packaged" =  Sys.time()
    # , "Repository" =  "CRAN"
    , "Depends" =  "Stringendo"
    , "Imports" = "sessioninfo, stats"
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/isoENV/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE", "isoENV.Rproj")))
    create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)


# replace output files ------------------------------------------------
BackupOldFile <- paste0(BackupDir, "Development", ".bac", print = FALSE)
AnnotatedFile <- paste0(BackupDir, "Development", ".annot.R", print = FALSE)
file.copy(from <- Package_FnP, to = BackupOldFile, overwrite = TRUE)


# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
document()
warnings()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T, "^version: v.+", paste0("version: v", package.version))
}

# Install your package ------------------------------------------------
# setwd(RepositoryDir)
install(RepositoryDir, upgrade = F)

# require("isoENV")
# # remove.packages("isoENV")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()

# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/isoENV")

# require("isoENV")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("isoENV")

check(RepositoryDir, cran = TRUE)


# Check package dependencies ------------------------------------------------
{
  depFile <- paste0(RepositoryDir, 'Development/Dependencies.R')

  (f.deps <- NCmisc::list.functions.in.file(filename = Package_FnP))
  # clipr::write_clip(f.deps)

  sink(file <- depFile); print(f.deps); sink()
  p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
  write(x = p.deps, file = depFile, append = T)
  p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
  p.dep.new <- sort(union( p.deps, p.dep.declared))
  # clipr::write_clip(p.dep.new)
}

if (F) {
  "run only once when initializing the repo"
  usethis::use_testthat(edition = 3)
  use_test(name = fname)
  # use_test(name = NULL, open = rlang::is_interactive())

}


