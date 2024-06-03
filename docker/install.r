at = commandArgs(trailing = TRUE)

devtools::install_github("wacl-york/eddy4R.york", auth_token = at, force = T)

# install local copy of eddy4R.turb - we have overriden some of the files when we built the docker image
devtools::install("/home/eddy/eddy4R/pack/eddy4R.turb/")

install.packages("progress")