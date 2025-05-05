#!/usr/bin/env bash

# (C) Tommi Salenius (2025)

# The script collects all the files in the underlying 'csv' folder
# and aggregates the data there into three separate files:

# [X] pokemon.json
# [X] move.json
# [] item.json
#
# All these three files implement for each Pokemon, Move and Item
# their latest in-game data which is relevant in battling, and
# also they expose all the previous data too


source ./pokemon-data.sh > pokemon.json
source ./move-data.sh > move.json

# --- Items ---------

itemtmp1=$(mktemp)
itemtmp2=$(mktemp)
berrytmp=$(mktemp)

# Item flag information
xan join --left item_flag_id csv/item_flag_map.csv flag_id_d <(xan rename flag_id_d,flag_identifier csv/item_flags.csv) |
    xan drop flag_id_d |
    xan implode item_flag_id,flag_identifier > $itemtmp1

# Berry
    xan join --right firm_id <(xan rename firm_id,firmness_description -s id,identifier csv/berry_firmness.csv) firmness_id <(xan join --left natural_gift_type_id csv/berries.csv type_id_d <(xan rename type_id_d -s id csv/types.csv) |
    xan drop damage_class_id,generation_id,natural_gift_type_id,type_id_d |
    xan rename natural_gift_type -s identifier) |
	xan select 2,3,4,1,5,6,11,7:10 |
	xan rename berry_id,berry_item_id -s id,item_id > $berrytmp

# Everything else including category, fling effect, normal effect, basic item data
# This is a mess, clean up at some point
xan join --right item_category_id <(xan join --left id <(xan rename category_identifier -s identifier csv/item_categories.csv) item_category_id csv/item_category_prose.csv) category_id <(xan join --right item_id csv/item_prose.csv id  <(xan join --left fling_effect_id csv/items.csv fling_effect_id_d <(xan join --left fling_effect_id_d <(xan rename fling_effect_id_d,fling_effect_identifier csv/item_fling_effects.csv) item_fling_effect_id csv/item_fling_effect_prose.csv))) |
    xan drop id |
    xan rename item_effect,fling_effect -s 8,19 |
    xan rename lang1,lang2,lang3 -s 3,6,18 > $itemtmp2

xan join --left id <(xan join --left id $itemtmp2 berry_item_id $berrytmp) item_id $itemtmp1 | xan to json |
    jq '[ .[] | {
       id: .id,
       name: .identifier,
       category: {name: .category_identifier,
       		 id: .item_category_id,
		 name_prose: .name},
       effect: {
       	       short: .short_effect,
       	       long: .item_effect,
	       fling: {short: .fling_effect_identifier,
	       	      long: .fling_effect}
		      },
       fling_power: .fling_power,
       flags: [(.item_flag_id), (.flag_identifier)] | map(split("|")) | transpose | map({id: .[0], name: .[1]}),
       berry: {id: .berry_id, firmness: {id: .firmness_id, description: .firmness_description},
       	      natural_gift: {power: .natural_gift_power, type: .natural_gift_type},
	      size, max_harvest, growth_time, soil_dryness, smoothness
	      }
       } 
       ]' > item.json

#### ------ Changelogs ------------

versiongroup=$(mktemp)

itemtbl=$(mktemp)

xan rename name -s identifier csv/items.csv | xan select id,name  > $itemtbl

xan join --left generation_id csv/version_groups.csv id csv/generations.csv |
    xan rename version_gen_id,version_name,generation -s '0,1,6' |
    xan select 0,1,6 > $versiongroup

