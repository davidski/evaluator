FROM rocker/tidyverse:4.0.3 as builder

ARG EVALUATOR_VERSION
ENV BUILD_DATE=2020-11-27
ARG ADD=shiny

LABEL org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/davidski/evaluator" \
      org.opencontainers.image.documentation = "https://evaluator.tidyrisk.org" \
      maintainer="David F. Severski <davidski@deadheaven.com>" \
      org.openctainers.image.authors ="David F. Severski <davidski@deadheaven.com>"

RUN /rocker_scripts/install_shiny_server.sh

COPY . /src/
WORKDIR /src

RUN apt-get update \
    && apt-get install -y zlib1g-dev libproj-dev libpng-dev libxml2-dev \
    && install2.r --deps=TRUE remotes \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY /scripts/create_templates.R /usr/local/bin/create_templates
COPY /scripts/run_analysis.R /usr/local/bin/run_analysis

VOLUME /data

EXPOSE 8787
EXPOSE 3838
