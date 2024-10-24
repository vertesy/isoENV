# Configuration for the Package
# file.edit("~/GitHub/Packages/XXXXXXXXXX/Development/config.R")

DESCRIPTION <- list(
  package.name = "isoENV",
  version = "0.2.0",
  title = "Tools to work with isolated environments for in-memory pipelines in R.",
  description = "isoENV is a set of R functions to invoke scripts in isolated, and controlled environments for
    in-memory pipelines in R. Useful for single-session pipelines and exploratory data analysis.",

  depends = "Stringendo, checkmate",
  imports = "sessioninfo, stats",
  suggests = "",

  author.given = "Abel",
  author.family = "Vertesy",
  author.email = "av@imba.oeaw.ac.at",
  github.user = "vertesy",
  license = "GPL-3 + file LICENSE"
)

