FROM ubuntu
ADD target /app
RUN apt-get update
RUN apt-get install --yes libgmp-dev
EXPOSE 80
WORKDIR /app/airport-groups-webapp/bin
CMD ./airport-group-service-exe

