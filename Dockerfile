# Extracted from https://shinyproxy.io/documentation/deploying-apps/

# Indicates that the image starts from a pre-built image, available here
# https://hub.docker.com/r/openanalytics/r-base/
FROM openanalytics/r-base

MAINTAINER Boris Delange "boris.delange@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN R -e "install.packages(c('DBI', 'devtools', 'dplyr', 'DT', 'magrittr', 'readr', 'RPostgres', 'RSQLite', 'rlang', 'shiny', 'shiny.router',
'shinyAce', 'shinybusy', 'shinyjs', 'golem', 'rlist', 'shinyFeedback', 'rmarkdown', 'knitr', 'dygraphs', 'vistime', 'ggplot2'), 
repos='https://cloud.r-project.org/')"

# Install R dependencies not available on CRAN
RUN R -e "devtools::install_github('Appsilon/shiny.react')"
RUN R -e "devtools::install_github('Appsilon/shiny.fluent')"

# Copy the app to the image
RUN mkdir /root/cdwtools
COPY cdwtools /root/cwdtools

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "cdwtools::cdwtools()"]
