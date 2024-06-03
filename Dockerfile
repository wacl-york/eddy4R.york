FROM quay.io/battelleecology/eddy4r-deve:deve 

ARG GITHUB_AUTH

COPY install.r .
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/
COPY eddy4R.turb /home/eddy/NEON-FIU-algorithm/ext/eddy4R/pack/eddy4R.turb/

RUN Rscript install.r $GITHUB_AUTH

EXPOSE 8787