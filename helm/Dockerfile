FROM rocker/r-ver:4

RUN groupadd -r scythe && useradd -r -g scythe scythe
RUN mkdir -p /var/data/scythe && chown scythe.scythe /var/data/scythe

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

RUN Rscript -e "install.packages(c('devtools', 'dataone', 'tidyr', 'redland', 'jsonlite', 'lubridate'))"
RUN Rscript -e "devtools::install_github('dataoneorg/scythe@v1.0.0', dependencies = TRUE)"

USER scythe:scythe

WORKDIR /var/data/scythe

COPY ./scripts/search.R .

CMD ["Rscript", "--vanilla", "search.R", "urn:node:ARCTIC"]