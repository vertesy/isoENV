# AGENTS

Welcome to **isoENV**, an R package offering tools to run scripts in controlled, isolated environments. This file guides contributors and AI agents working in this repository.

## Repository structure
- `R/` – Core package functions (e.g., `sourceClean`, `removeAllExceptFunctions`).
- `man/` – Generated documentation (roxygen2 output).
- `tests/` – `testthat` unit tests.
- `Examples/` – Small scripts demonstrating typical usage.
- `Development/` – Experimental or development helper scripts.
- Root files like `DESCRIPTION`, `NAMESPACE`, and `README.md` define package metadata and usage.

## Working with the codebase
- Code is written in R and documented with **roxygen2** comments. After editing functions in `R/`, run:
  ```bash
  R -q -e 'devtools::document()'
  ```
  to update documentation.
- Run the test suite before committing:
  ```bash
  R -q -e 'devtools::test()'
  ```
- For a full package check (optional but recommended for releases):
  ```bash
  R CMD check .
  ```

## Tips for newcomers
1. Read `README.md` to understand package goals and basic usage.
2. Explore the `Examples/` scripts to see isolated environments in action.
3. Browse `tests/testthat` to learn expected behaviors.
4. The `man/` directory hosts function reference documentation generated from roxygen comments.

## What to learn next
- Familiarize yourself with **roxygen2** and **testthat** conventions.
- Review R package development practices (e.g., dependency management, `DESCRIPTION` fields).
- Look at open issues on GitHub for potential contributions or feature ideas.

Happy hacking!
