FROM rocker/shiny
MAINTAINER A N Vu, avu@coh.org

RUN apt-get update && apt-get install -y \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  && apt-get clean \ 
  && rm -rf /var/lib/apt/lists/ \ 
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install R packages
# 'ggplot2', 'plotly', 'data.table', 'shinythemes', 'Hmisc', 'igraph', 'visNetwork', 'reshape2', 'shinycssloaders', 'shinyjqui', 'shinyWidgets', 'r2d3', 'jsonlite', 'optmatch', 'DT'
RUN R -e "install.packages(c('devtools'), repos='http://cran.rstudio.com/')" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN R -e "devtools::install_github('avucoh/DIVE')"

# Copy config
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy the app to the image
COPY app /srv/shiny-server/app

# select port
EXPOSE 8080

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]
