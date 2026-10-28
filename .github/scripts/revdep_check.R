# Reverse dependency check, used by .github/workflows/revdep.yaml.
#
#   Rscript revdep_check.R <source dir> <dev lib> <check dir>
#
# Checks every package tarball in <source dir> twice, as CRAN would (tests which call `skip_on_cran()` are skipped and
# missing Suggests are allowed): once against the poorman installed in the default library, i.e. the CRAN version,
# and once with <dev lib>, which holds the development version, first on the library path. The checks run in
# parallel. The script fails when the check against the development version finds a problem which the check against
# the CRAN version did not, i.e. a "change to worse".

args <- commandArgs(trailingOnly = TRUE)
src_dir <- args[1L]
dev_lib <- normalizePath(args[2L])
check_dir <- args[3L]

cran_version <- utils::packageVersion("poorman")
dev_version <- utils::packageVersion("poorman", lib.loc = dev_lib)
cat("CRAN poorman:", format(cran_version), "\nDevelopment poorman:", format(dev_version), "\n")
if (cran_version == dev_version) stop("The CRAN and development versions of poorman are the same version.")

tarballs <- list.files(src_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
packages <- sub("_.*", "", basename(tarballs))

# One check per package and poorman version.
checks <- expand.grid(tarball = tarballs, version = c("cran", "dev"), stringsAsFactors = FALSE)
checks$libpath <- lapply(checks$version, function(v) if (v == "dev") c(dev_lib, .libPaths()) else .libPaths())
checks$out <- file.path(check_dir, checks$version)
for (dir in unique(checks$out)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

run_check <- function(i, checks) {
  try(rcmdcheck::rcmdcheck(
    checks$tarball[i],
    args = "--no-manual",
    libpath = checks$libpath[[i]],
    check_dir = checks$out[i],
    env = c(NOT_CRAN = "false", `_R_CHECK_FORCE_SUGGESTS_` = "false"),
    error_on = "never",
    quiet = TRUE
  ))
}

# Socket workers rather than forks, as rcmdcheck runs R CMD check through processx, which does not support forking.
cores <- max(1L, parallel::detectCores())
cat("Running", nrow(checks), "checks on", cores, "cores\n")
cl <- parallel::makePSOCKcluster(cores)
results <- parallel::parLapplyLB(cl, seq_len(nrow(checks)), run_check, checks = checks)
parallel::stopCluster(cl)

# A problem is identified by its first line, e.g. "checking tests ... ERROR", which names the check and its result
# but not the run time or output, so the same problem matches across the two runs.
first_lines <- function(problems) trimws(sub("\n.*", "", problems))

outcome <- data.frame(package = packages, status = "OK", stringsAsFactors = FALSE)
for (p in seq_along(packages)) {
  old <- results[[which(checks$tarball == tarballs[p] & checks$version == "cran")]]
  new <- results[[which(checks$tarball == tarballs[p] & checks$version == "dev")]]
  cat("::group::", packages[p], "\n", sep = "")
  if (inherits(old, "try-error") || inherits(new, "try-error")) {
    outcome$status[p] <- "CHECK FAILED TO RUN"
    for (res in list(old, new)) if (inherits(res, "try-error")) cat(res)
    cat("::endgroup::\n")
    next
  }
  print(rcmdcheck::compare_checks(old, new))
  new_problems <- c(new$errors, new$warnings, new$notes)
  worse <- setdiff(first_lines(new_problems), first_lines(c(old$errors, old$warnings, old$notes)))
  cat("::endgroup::\n")
  if (length(worse) > 0L) {
    outcome$status[p] <- "WORSE"
    cat("\nChanges to worse in ", packages[p], " with the development version of poorman:\n\n", sep = "")
    cat(new_problems[first_lines(new_problems) %in% worse], sep = "\n\n")
  }
}

cat("\n")
print(outcome, row.names = FALSE)
if (any(outcome$status != "OK")) quit(status = 1L)
