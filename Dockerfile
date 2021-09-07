FROM rocker/shiny:3.6.3

RUN apt-get --allow-releaseinfo-change update && \
    apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    # for jpeg and latticeExtra:
    libjpeg-dev \ 
  # for Bioconductor pkgs:
    libbz2-dev \
    liblzma-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/
    
RUN R -e "install.packages(c('BiocManager'), dependencies=c('Depends', 'Imports'), repos='http://cran.rstudio.com/')"

# cpp11 and latticeExtra are dependencies
RUN R -e "install.packages(c('cpp11', 'latticeExtra', 'shinythemes', 'data.table', 'dplyr', 'ggplot2', 'scales', 'plotly', 'Hmisc', 'igraph', 'visNetwork', 'shinyWidgets', 'optmatch', 'DT', 'assertthat', 'dendextend', 'fastcluster', 'gridExtra', 'rlang', 'purrr', 'DBI', 'duckdb'), dependencies=c('Depends', 'Imports'), repos='http://cran.rstudio.com/')" 
RUN R -e "BiocManager::install(c('mygene', 'Biobase', 'SummarizedExperiment', 'GEOquery'))" 

# install package from current github source
RUN R -e "install.packages('.', repos = NULL)"

ENTRYPOINT ["bin/bash"] 

