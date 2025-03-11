devtools::install("/home/york/eddy4r.york")
writeLines("_R_CHECK_SYSTEM_CLOCK_=0", "/home/rstudio/.Renviron")
install.packages("duckdb",repos = "https://p3m.dev/cran/__linux__/jammy/2024-06-14")

options(digits.secs=3)