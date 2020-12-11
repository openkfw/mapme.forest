# from https://www.joelnitta.com/post/docker-and-packrat/

FROM rocker/geospatial

RUN sudo apt-get update \
  && apt-get install -y --no-install-recommends \
    gdal-bin \
    libglpk40 \ # for igraph to work
    libgeos-dev \
    libproj-dev \
    libgdal-dev\
    subversion \
    grass\
    grass-dev 

COPY ./init_grass.sh /tmp/init_grass.sh
ENV GISDBASE=/tmp/grass LOCATION=install
RUN mkdir -p $GISDBASE
RUN bash /tmp/init_grass.sh

# install additional r dependencies
RUN install2.r --error \
    knitr \
    rmarkdown \
    ggplot2 \
    magrittr \ 
    plotly \
    R.utils 


# install sen2tools from repository
COPY ./ /usr/local/src/forestIndicators/
WORKDIR /usr/local/src/forestIndicators/
RUN R -e "devtools::install('.')"

# remove temporary files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds /usr/local/src/forestIndicators/ \
    && rm -rf /var/lib/apt/lists/*
