#!usr/bin/env bash

cat veekun/move_effect_prose.csv |
    sed "s/,9,/,/" |
    awk '{sub(/\.,.*/, ""); print}' |
    grep "^[0-9]" |
    grep "," |
    # 1) Parse individual effects from compound effects
    awk -f separate_individual_effects.awk |
    # ...
    # final step) sort remaining effects and filter duplicates out
    sort -n | uniq
