# Reverse dependency check for a single package, used by .github/workflows/revdep.yaml.
#
#   Rscript revdep_check.R check <tarball> <result.rds>
#   Rscript revdep_check.R compare <old.rds> <new.rds>
#
# `check` runs R CMD check on a reverse dependency against whichever poorman is installed, as CRAN would: tests
# which call `skip_on_cran()` are skipped and missing Suggests are allowed. `compare` fails when the check against the
# development poorman finds a problem which the check against the CRAN poorman did not, i.e. a "change to worse".

args <- commandArgs(trailingOnly = TRUE)

run_check <- function(tarball, out) {
  cat("Checking", basename(tarball), "with poorman", format(utils::packageVersion("poorman")), "\n")
  res <- rcmdcheck::rcmdcheck(
    tarball,
    args = "--no-manual",
    check_dir = sub("\\.rds$", "", out),
    env = c(NOT_CRAN = "false", `_R_CHECK_FORCE_SUGGESTS_` = "false"),
    error_on = "never"
  )
  saveRDS(res, out)
}

# A problem is identified by its first line, e.g. "checking tests ... ERROR", which names the check and its result
# but not the run time or output, so the same problem matches across the two runs.
problem_keys <- function(res) {
  problems <- c(res$errors, res$warnings, res$notes)
  unique(trimws(sub("\n.*", "", problems)))
}

compare_checks <- function(old_file, new_file) {
  old <- readRDS(old_file)
  new <- readRDS(new_file)
  print(rcmdcheck::compare_checks(old, new))
  worse <- setdiff(problem_keys(new), problem_keys(old))
  if (length(worse) == 0L) {
    cat("\nNo changes to worse.\n")
    return(invisible())
  }
  cat("\nChanges to worse with the development version of poorman:\n\n")
  new_problems <- c(new$errors, new$warnings, new$notes)
  cat(new_problems[trimws(sub("\n.*", "", new_problems)) %in% worse], sep = "\n\n")
  quit(status = 1L)
}

switch(
  args[1L],
  check = run_check(args[2L], args[3L]),
  compare = compare_checks(args[2L], args[3L]),
  stop("Unknown command: ", args[1L])
)
