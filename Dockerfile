# Extracted from https://shinyproxy.io/documentation/deploying-apps/

<<<<<<< HEAD
# Indicates that the image starts from a pre-built image, available here
# https://hub.docker.com/r/openanalytics/r-base/
# FROM openanalytics/r-base
FROM r-base
||||||| 37cb8a1
# Indicates that the image starts from a pre-built image, available here
# https://hub.docker.com/r/openanalytics/r-base/
FROM openanalytics/r-base
=======
FROM r-base
>>>>>>> 3b571fa546b520a95c5b059277896a86057d2e16

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
	libgit2-dev \
	libxml2-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN R -e "install.packages(c('gert', 'usethis', 'roxygen2', 'rversions'), repos='http://cran.rstudio.com')"
RUN R -e "install.packages(c('devtools'), repos='http://cran.rstudio.com')"
RUN R -e "install.packages(c('DBI', 'dplyr', 'DT', 'magrittr', 'readr', 'RPostgres', 'RSQLite', 'rlang', 'shiny', 'shiny.router','shinyAce', 'shinybusy', 'shinyjs', 'golem', 'rlist', 'shinyFeedback', 'rmarkdown', 'knitr', 'dygraphs', 'vistime', 'ggplot2'), repos='http://cran.rstudio.com')"

# Install R dependencies not available on CRAN
RUN R -e "devtools::install_github('Appsilon/shiny.react')"
RUN R -e "devtools::install_github('Appsilon/shiny.fluent')"
RUN mkdir /root/cdwtools
RUN git clone https://github.com/BorisDelange/cdwtools.git /root/cdwtools
EXPOSE 3838

CMD ["R", "-e", "cdwtools::cdwtools()"]
