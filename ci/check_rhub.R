message("Running rhub::check()")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Incorrect number of args, needs 1: platform (string)")
}

platform <- args[[1L]]
if (!is.element(platform, c("cran", rhub::platforms()[[1L]]))) {
  stop(paste(platform, "not in rhub::platforms()[[1L]] nor cran"))
}

install.packages("knitr")
install.packages("rmarkdown")
if (platform == "cran") {
  system("apt-get update && apt-get -y install libxml2-dev")
  install.packages("xml2")
  cr <- rhub::check_for_cran(
    show_status = TRUE,
    env_vars = c(
      `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false"
    )
  )
} else {
  cr <- rhub::check(
    platform = platform,
    show_status = TRUE
  )
}
statuses <- cr[[".__enclos_env__"]][["private"]][["status_"]]

res <- do.call(rbind, lapply(statuses, function(this_status) {
  data.frame(
    plaform  = this_status[["platform"]][["name"]],
    errors   = length(this_status[["result"]][["errors"]]),
    warnings = length(this_status[["result"]][["warnings"]]),
    notes    = length(this_status[["result"]][["notes"]]),
    stringsAsFactors = FALSE
  )
}))

message("\n\nTrying to print check results")
print(res)


message("\n\nTrying to print detailed test results")
message("Determining Candidate URLs and download destinations")
test_candidates <- do.call(rbind, lapply(
  cr$urls()$artifacts,
  function(this_url) {
    test_file <- tools::file_path_sans_ext(dir("./tests/"))
    data.frame(
      url = file.path(
        this_url,
        paste0(unique(sapply(statuses, `[[`, "package"))[[1L]], ".Rcheck"),
        "tests",
        c(paste0(test_file, ".Rout"), paste0(test_file, ".Rout.fail"))
      ),
      fileName = paste(
        basename(this_url),
        c(paste0(test_file, ".Rout"), paste0(test_file, ".Rout.fail")),
        sep = "-"
      ),
      stringsAsFactors = FALSE
    )
  }
))

message("Printing Candidate URLs and download destinations: \n")
print(test_candidates)

message("Attempting to download test logs from rhub: \n")
test_logs <- apply(
  test_candidates,
  1L,
  function(x) {
    try(
      utils::download.file(x["url"], x["fileName"], quiet = TRUE),
      silent = TRUE
    )
  }
)
names(test_logs) <- test_candidates[["fileName"]]

message("Attempting to cat test logs from rhub: \n\n")
invisible(lapply(
  names(test_logs[test_logs == 0L]),
  function(x) {
    message("\n", x)
    cat(readLines(x), sep = "\n")
  }
))

if (any(colSums(res[2L:4L]) > 0)) {
  stop("Some checks with errors, warnings or notes.")
}
