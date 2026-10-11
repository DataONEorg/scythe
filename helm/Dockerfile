FROM rocker/r-ver:4

# The scythe branch, tag or commit to install, for example
# docker buildx build --build-arg SCYTHE_REF=feature-47-helm-full-search ...
ARG SCYTHE_REF=develop

RUN groupadd -r scythe && useradd -r -g scythe scythe
RUN mkdir -p /var/data/scythe && chown scythe.scythe /var/data/scythe
RUN mkdir -p /apps/scythe && chown scythe.scythe /apps/scythe


RUN apt-get update && apt-get install -y \
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

# search.R needs only scythe (which brings in its own dependencies, such as
# callr and jsonlite) and optparse. DataONE identifiers are collected with
# scythe's own Solr queries, so the dataone package (and redland, which it
# needed) isn't installed.
RUN Rscript -e "install.packages(c('remotes', 'optparse'))"
RUN Rscript -e "remotes::install_github('dataoneorg/scythe@${SCYTHE_REF}')"

USER scythe:scythe

WORKDIR /var/data/scythe

COPY ./scripts/search.R /apps/scythe/