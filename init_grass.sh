#!/bin/bash

mkdir -p $GISDBASE
# Initialise a GRASS Location so there is a default region
grass -text -c EPSG:2193 -e $GISDBASE/$LOCATION

# Define an alias to start GRASS with a particular Location and the PERMANENT Mapset
GRASSSTART="grass -text $GISDBASE/$LOCATION/PERMANENT --exec"

# Load extensions
# https://grass.osgeo.org/grass76/manuals/addons/r.area.html
$GRASSSTART g.extension extension=r.area
