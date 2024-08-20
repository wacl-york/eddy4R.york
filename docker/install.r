devtools::install_github("wacl-york/eddy4R.york", auth_token = readLines("/run/secrets/GITHUB_AUTH", n = 1), force = T)

# install local copy of eddy4R.turb - we have overriden some of the files when we built the docker image
devtools::install("/home/eddy/eddy4R/pack/eddy4R.turb/")

install.packages("duckdb",repos = "https://p3m.dev/cran/__linux__/jammy/2024-06-14")

options(digits.secs=3)