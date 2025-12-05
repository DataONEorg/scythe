FROM rocker/r-ver:4

RUN groupadd -r scythe && useradd -r -g scythe scythe
RUN mkdir -p /var/data/scythe && chown scythe.scythe /var/data/scythe
RUN mkdir -p /apps/scythe && chown scythe.scythe /apps/scythe


RUN apt-get update && apt-get install -y \
    librdf-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libssl-dev \
    libcurl4-gnutls-dev \
    libsodium-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('remotes', 'tidyr', 'redland', 'jsonlite', 'lubridate', 'optparse'))"
RUN Rscript -e "remotes::install_github('dataoneorg/rdataone')"
RUN Rscript -e "remotes::install_github('dataoneorg/scythe@v1.1.0')"

USER scythe:scythe

WORKDIR /var/data/scythe

COPY ./scripts/search.R /apps/scythe/