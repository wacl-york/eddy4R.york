devtools::install_github("wacl-york/eddy4R.york", auth_token = readLines("/run/secrets/GITHUB_AUTH", n = 1), force = T)

install.packages("duckdb",repos = "https://p3m.dev/cran/__linux__/jammy/2024-06-14")

options(digits.secs=3)