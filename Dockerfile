FROM rocker/rstudio:4.0.2
RUN apt-get update && apt-get install -y build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libgit2-dev libxt6
RUN Rscript -e 'install.packages(c("devtools", "caret", "readxl", "generalhoslem", "SpecsVerification"), dependencies=TRUE)' -e 'devtools::install_github("adefazio/classifierplots")'
COPY . /miPredict
WORKDIR /miPredict
RUN Rscript -e 'devtools::install_local(dependencies=TRUE, upgrade="never")'
