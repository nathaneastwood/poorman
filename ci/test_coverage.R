message("\n\nInstalling dependencies")
system("apt-get update && apt-get -y install procps")

message("\n\nRunning package_coverage()")
coverage <- covr::package_coverage(path = ".", quiet = FALSE)

message("\nPrinting package_coverage")
print(coverage)

message("\nPublishing package_coverage")
covr::codecov(
  coverage = coverage,
  token = Sys.getenv("POORMAN_TOKEN_CODECOV")
)

message("\nDone.\n")
