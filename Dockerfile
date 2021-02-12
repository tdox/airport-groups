FROM ubuntu
COPY target /app
# ADD target /app
# RUN apt-get update
# RUN apt-get install --yes libgmp-dev
# EXPOSE 80
RUN apt-get update && apt-get install -y locales && rm -rf /var/lib/apt/lists/* \
    && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG en_US.utf8
WORKDIR /app/airport-groups-webapp
CMD ./bin/airport-groups-service
