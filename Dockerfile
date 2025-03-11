# currently pulling from the latest public deve image
# probably change to annual stable release when flux sclr changes are done
FROM quay.io/battelleecology/eddy4r:maps 

WORKDIR /home/york/
COPY docker/install.r .
COPY docker/runtests.r .
COPY eddy4r.york eddy4r.york

# Change the default prefs to dark mode
# --chown=rstudio:rstudio stops RStudio showing an error when you connect
COPY --chown=rstudio:rstudio docker/rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json

RUN Rscript install.r

# Update Labels
LABEL org.opencontainers.image.authors="Will Drysdale"
LABEL org.opencontainers.image.source="https://github.com/wacl-york/eddy4R.york"
LABEL org.opencontainers.image.description="Docker image for WACL's eddy4R workflow. Based on eddy4R:maps"
LABEL org.opencontainers.image.base.name="quay.io/battelleecology/eddy4r:maps"
LABEL org.opencontainers.image.licenses="GPL-3.0-or-later"
LABEL org.opencontainers.image.revision=""
LABEL org.opencontainers.image.title="eddy4R.york"
LABEL org.opencontainers.image.version="0.1.1"
LABEL org.opencontainers.image.vendor=""

ENV DISABLE_AUTH=true

EXPOSE 8787