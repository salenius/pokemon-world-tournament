#!/usr/bin/env bash

read_veekun () {
curl "https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/$1.csv" > "$1.csv"
}

for x in $(cat scrapes); do
    read_veekun $x
done

