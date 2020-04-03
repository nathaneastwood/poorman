FROM rocker/r-base:latest

RUN R -e "install.packages('poorman', repos = 'https://cloud.r-project.org/')"
