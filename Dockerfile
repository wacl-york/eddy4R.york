FROM quay.io/battelleecology/eddy4r:deve 

ARG GITHUB_AUTH

COPY docker/install.r .
COPY docker/rstudio-prefs.json /home/rstudio/.config/rstudio/
COPY docker/eddy4R.turb /home/eddy/eddy4R/pack/eddy4R.turb/

RUN Rscript install.r $GITHUB_AUTH

LABEL org.opencontainers.image.source=https://github.com/wacl-york/eddy4R.york

EXPOSE 8787