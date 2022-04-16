# Get the rocker image, latest shiny from rocker in this case:
# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Install packages rsconnect [to allow deploying shiny apps]:
# Install package remotes [to allow installGithub.r for github packages]:
RUN install2.r rsconnect remotes tidyverse devtools \
    bslib DT ggrepel plyr R6 scales shinymanager \
    shinyWidgets waiter

# Install github packages, including one under development if app relies on it:
RUN -e installGithub.r W-Mohammed/ShinyPSA@dev

# Create a working directory in the image:
WORKDIR /home/shinyusr

# Copy the app and all supporting files:
COPY inst/shiny_examples/ShinyPSA/app.R app.R

# Copy the deployment script to the same folder:
COPY deploy.R deploy.R

# Set the command to run once the container runs:
CMD Rscript deploy.R

## To build this file with the name "test1":
# 1. make sure this file is on project root
# 2. navigate to R's local terminal and execute:
# docker build -t test1 .
# 2. if the build exists, this can be overriden:
# docker build --no-cache -t test1 .

## To run the image from the same terminal:
# 1. if the imaage require environment vars and stored in ".Renviron"
# docker run --env-file .Renviron test1
