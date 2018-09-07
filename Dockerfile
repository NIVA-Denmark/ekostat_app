FROM quantumobject/docker-shiny

RUN apt-get update && apt-get install p7zip-full

WORKDIR /srv/shiny-server/ekostat
ADD install_packages.R /srv/shiny-server/ekostat
RUN R -f install_packages.R

ADD . /srv/shiny-server/ekostat

# extract zipped content
WORKDIR /srv/shiny-server/ekostat/data
RUN 7za e ekostat.7z -y