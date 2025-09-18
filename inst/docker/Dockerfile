##

# Dockerfile for iModMix container compilation

#



# Install base R image

FROM r-base:4.3.2



# Install required system libraries

RUN apt-get -y update && apt-get -y install libfontconfig1-dev libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev libfreetype-dev libfreetype6 libfreetype6-dev pkg-config libtiff5-dev bzip2 libharfbuzz-dev libfribidi-dev git



# Create the Shiny app directory in the container

RUN mkdir /iModMix



# Set permissions for the app directory

RUN chown -R 1001:1001 /iModMix



# Install spatialMET dependencies

RUN Rscript -e "install.packages(c('remotes', 'devtools'))"

RUN Rscript -e "install.packages(c('shiny', 'pkgload', 'usethis', 'pkgdown', 'rcmdcheck', 'roxygen2', 'rversions', 'urlchecker', 'BiocManager', 'DT', 'RColorBrewer', 'corrplot', 'cowplot', 'ggplot2', 'purrr', 'stringr', 'tidyr', 'visNetwork', 'shinyBS', 'httr', 'dplyr', 'glassoFast', 'enrichR', 'readxl'))"

RUN Rscript -e "BiocManager::install(c('ComplexHeatmap', 'dynamicTreeCut', 'WGCNA', 'impute'))"

RUN Rscript -e "remotes::install_version('config', version='0.3.2')"

RUN Rscript -e "remotes::install_version('golem', version='0.4.1')"

RUN Rscript -e "devtools::install_github('biodatalab/iModMixData')"



# Pull iModMix repository

RUN git clone https://github.com/biodatalab/iModMix.git



# Set up as working directory

WORKDIR /iModMix



# Expose the port that the app will be running on

EXPOSE 3838



# Run the app on container start

CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)"]





