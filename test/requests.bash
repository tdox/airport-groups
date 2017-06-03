#!/bin/bash

echo prog1
curl -X POST -d '{"src":"s1={FAA:SFO}; print(s1);"}'  -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/airport-group
echo

