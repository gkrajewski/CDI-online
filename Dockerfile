FROM rocker/shiny:4.1.1

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    default-jre \
    default-jdk \
    r-cran-rjava \
    r-base-dev \
    libxml2-dev \
    libmariadbclient-dev
# curl, openssl, rstudioapi, yaml, packrat, rsconnect. 
RUN R CMD javareconf
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fresh', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/emayili/emayili_0.4.10.tar.gz', repos=NULL, type='source')"
RUN R -e "install.packages('httr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydisconnect', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('V8', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydisconnect', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RMariaDB', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('logging', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mirtCAT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('sendgridr', repos='http://cran.rstudio.com/')"


# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN mkdir /srv/shiny-server/cdi
RUN mkdir /srv/shiny-server/cdi/answers
RUN mkdir /srv/shiny-server/cdi/designs
RUN mkdir /srv/shiny-server/cdi/subjects
RUN mkdir /srv/shiny-server/cdi/usersProgress
RUN mkdir /srv/shiny-server/cdi/logs

# COPY . /srv/shiny-server/
COPY . /srv/shiny-server/cdi
RUN chown -R shiny: /srv/shiny-server/
COPY shiny-customized.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3839


