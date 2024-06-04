# currently pulling from the latest public deve image
# probably change to annual stable release when flux sclr changes are done
FROM quay.io/battelleecology/eddy4r:deve 

# Need to auth while {eddy4R.york} is private
ARG GITHUB_AUTH

# code for installing {eddy4R.york}, reinstalls the local copy of {eddy4R.turb} and installs {progress}
# can remove the local install of eddy4R.turb once flux sclr changes are done
COPY docker/install.r .
# Change the default prefs to dark mode
# --chown=rstudio:rstudio stops RStudio showing an error when you connect
COPY --chown=rstudio:rstudio docker/rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
# once the flux sclr changes are done this can be removed
COPY docker/eddy4R.turb /home/eddy/eddy4R/pack/eddy4R.turb/

RUN Rscript install.r $GITHUB_AUTH

LABEL org.opencontainers.image.source=https://github.com/wacl-york/eddy4R.york

ENV DISABLE_AUTH=true
EXPOSE 8787