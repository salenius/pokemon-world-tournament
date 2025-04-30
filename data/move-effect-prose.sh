#!/usr/bin/env bash

# This script is used to extract
# data on moves effect which are
# then further parsed down the line

if [ $1 = "long" ]; then
    csvtool col 4 csv/move_effect_prose.csv 
else
    if [ $1 = "short" ]; then
	csvtool col 3 csv/move_effect_prose.csv 
    else
	echo "choose eihter 'long' or 'short' for move effect prose description.";
    fi;
fi;
