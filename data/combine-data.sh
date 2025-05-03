#!/usr/bin/env bash

# (C) Tommi Salenius (2025)

# The script collects all the files in the underlying 'csv' folder
# and aggregates the data there into three separate files:

# [X] pokemon.json
# [] move.json
# [] item.json
#
# All these three files implement for each Pokemon, Move and Item
# their latest in-game data which is relevant in battling, and
# also they expose all the previous data too

mvtemp1=$(mktemp)
mvtemp2=$(mktemp)
mvtemp3=$(mktemp)
mvtemp4=$(mktemp)
mvtemp5=$(mktemp)
mvtemp0=$(mktemp)

metatemp1=$(mktemp)
metatemp2=$(mktemp)
flagtemp1=$(mktemp)
flagtemp2=$(mktemp)

pkmntemp0=$(mktemp)
pkmntemp1=$(mktemp)
pkmntemp2=$(mktemp)
pkmntemp3=$(mktemp)
pkmntemp4=$(mktemp)
pkmntemp5=$(mktemp)
pkmntemp6=$(mktemp)
pkmntemp7=$(mktemp)
pkmntemp8=$(mktemp)
pkmntemp9=$(mktemp)
pkmntemp10=$(mktemp)
pkmntemp11=$(mktemp)


# ----- Moves -------

# Effects
xan join --left effect_id csv/moves.csv move_effect_id csv/move_effect_prose.csv |
    xan select 0:11,17,18 > $mvtemp1

# Generation
xan join --left generation_id $mvtemp1 id csv/generations.csv |
    xan select 0:1,16,3:13 |
    xan rename generation -s '2' > $mvtemp2

# Types
xan join --left type_id $mvtemp2 id csv/types.csv |
    xan select 0:2,15,4:13 |
    xan rename type -s '3' > $mvtemp3

# Target range
xan join --left target_id $mvtemp3 id csv/move_targets.csv |
    xan select 0:7,15,9:13 |
    xan rename target -s '8' > $mvtemp4

# Damage class / category
xan join --left damage_class_id $mvtemp4 id csv/move_damage_classes.csv |
    xan select 0:8,15,10:13 |
    xan rename category -s '9' > $mvtemp5

# Meta => effect type
xan join --left meta_category_id csv/move_meta.csv id csv/move_meta_categories.csv |
    xan select 0,14,2:12 |
    xan rename effect_type -s '1' > $metatemp1

# Meta => ailment type
xan join --left meta_ailment_id $metatemp1 id csv/move_meta_ailments.csv |
    xan select 0:1,14,3:12 |
    xan rename ailment_type -s '2' > $metatemp2

# Flags => join move-flag mapping with a short description of the mapping
xan join --left move_flag_id csv/move_flag_map.csv id csv/move_flags.csv |
    xan select 0,1,3 |
    xan rename flag -s '2' > $flagtemp1

xan join --left move_flag_id $flagtemp1 move_flag_id csv/move_flag_prose.csv |
    xan filter 'local_language_id == 9' |
    xan rename flag_name -s '5' |
    xan rename flag_description -s '6' |
    xan select 0,1,2,5,6 |
    xan implode move_flag_id,flag,flag_name,flag_description > $flagtemp2

xan join --left id $mvtemp5 move_id $metatemp2 | xan select 0:13,15:25 > $mvtemp0

xan join id $mvtemp0 move_id $flagtemp2 |
    xan to json |
    jq '
    [ .[] | . * {flag: .flag | split("|"),
      flag_name: .flag_name | split("|"),
      flag_description: .flag_description | split("|"),
      move_flag_id: .move_flag_id | split("|") } |
    . * {flags: [(.flag), (.flag_name), (.flag_description), (.move_flag_id)]
      | transpose
      | map({flag: (.[0]), name: (.[1]), description: (.[2]), id: (.[3])})} |
    del(.flag, .flag_name, .flag_description, .move_id, .move_flag_id)
    ]
    ' > move.json

# ------- Pokemon --------

# Species
xan join --left species_id csv/pokemon.csv id csv/pokemon_species.csv |
    xan select 0:7,9:27 |
    xan rename species -s '8' > $pkmntemp0

# Stats
xan join --left stat_id csv/pokemon_stats.csv id csv/stats.csv |
    xan select 0,6,2,3 |
    xan rename stat -s '1' |
    xan implode stat,base_stat,effort > $pkmntemp1

# Types
xan join --left type_id csv/pokemon_types.csv id csv/types.csv |
    xan select 0,4,2 |
    xan rename type -s '1' |
    xan implode type,slot |
    xan rename type_slot -s slot >  $pkmntemp2

# Abilities
abilitytemp0=$(mktemp)
abilitytemp=$(mktemp)
xan filter 'local_language_id == 9' csv/ability_prose.csv > $abilitytemp0
xan join --left id csv/abilities.csv ability_id $abilitytemp0 > $abilitytemp
xan join --left ability_id csv/pokemon_abilities.csv id $abilitytemp  |
    xan select 0,5,1,2,3,10,11  |
    xan rename ability -s '1' |
    xan implode --sep '<<@>>' ability,ability_id,is_hidden,slot,short_effect,effect |
    xan rename ability_slot,ability_short_effect,ability_effect -s slot,short_effect,effect > $pkmntemp3

# Form
xan join --left id,identifier $pkmntemp0 pokemon_id,identifier csv/pokemon_forms.csv  |
    xan select 0:5,8:24,26,29,31:36 > $pkmntemp4

# Egg groups
xan join --left egg_group_id csv/pokemon_egg_groups.csv id csv/egg_groups.csv |
    xan select 0,3 |
    xan implode identifier |
    xan rename egg_group -s '1' > $pkmntemp5

# Generation
xan join --left generation_id $pkmntemp4 id csv/generations.csv |
    xan drop 31 |
    xan rename generation -s '32' > $pkmntemp6

# Join abilities, stats and types
xan join --left id $pkmntemp6 pokemon_id $pkmntemp1 > $pkmntemp7
xan join --left id $pkmntemp7 pokemon_id $pkmntemp2 > $pkmntemp8
xan join --left id $pkmntemp8 pokemon_id $pkmntemp3 > $pkmntemp9

xan join --left species_id $pkmntemp9 species_id $pkmntemp5 > $pkmntemp10

# Here we create Pokemon for all the attributes except for the
# changelog data (incl. abilities' change log)
cat $pkmntemp10 | xan to json | jq '
		  [ .[] | . * {stat: [(.stat | split("|")),
			    	   (.base_stat | split("|") | map(tonumber)),
				   (.effort | split("|") | map(tonumber))]
				   | transpose
				   | map({(.[0]): { "basestat": (.[1]), "effort": (.[2]) }}),
			    	   abilities: [(.ability), (.ability_slot), (.is_hidden), (.ability_short_effect), (.ability_effect), (.ability_id)]
				   | map(split("<<@>>"))
				   | transpose
				   | map({ability: {id: .[5] | tonumber, name: .[0], short: .[3], long: .[4]},
				   slot: .[1] | tonumber, "is_hidden": .[2] | tonumber})
                    } ]
		  ' | jq '
		  [.[] | reduce .stat[] as $stat (.; . * $stat) |
		  del(.pokemon_id, .stat, .base_stat, .effort,
		  .ability, .is_hidden, .ability_slot, .ability_short_effect, .ability_effect, .ability_id
		  ) |
		  . * {egg_group: .egg_group | split("|")} ]
		  ' | jq '
		  [ .[] | . * {types: .type | split("|")} | del(.type, .type_slot)] |
		  [ .[] | . * {species: {
		  name: .species,
		  id: .species_id,
		  gender_rate: .gender_rate,
		  is_baby: .is_baby,
		  is_legendary: .is_legendary,
		  is_mythical: .is_mythical,
		  egg_group: .egg_group,
		  hatch_counter: .hatch_counter,
		  has_gender_differences: .has_gender_differences,
		  capture_rate: .capture_rate,
		  base_happiness: .base_happiness,
		  forms_switchable: .forms_switchable,
		  growth_rate_id: .growth_rate_id,
		  color_id: .color_id,
		  shape_id: .shape_id,
		  habitat_id: .habitat_id,
		  evolves_from_species_id: .evolves_from_species_id, 
		  evolution_chain_id: .evolution_chain_id
		  }} | del(
		  .species_id,
		  .gender_rate,
		  .is_baby,
		  .is_legendary,
		  .is_mythical,
		  .egg_group,
		  .hatch_counter,
		  .has_gender_differences,
		  .capture_rate,
		  .base_happiness,
		  .growth_rate_id,
		  .forms_switchable,
		  .color_id,
		  .habitat_id,
		  .shape_id,
		  .evolves_from_species_id,
		  .evolution_chain_id,
		  .generation_id,
		  .main_region_id
		  )
		  ] |
		  [ .[] | . * {form: {
		  identifier: .form_identifier,
		  introduced_in_version_group_id: .introduced_in_version_group_id,
		  is_default: .is_default,
		  is_battle_only: .is_battle_only,
		  is_mega: .is_mega,
		  order: .form_order
		  } } | del(
		  .form_identifier,
		  .introduced_in_version_group_id,
		  .is_default,
		  .is_battle_only,
		  .is_mega,
		  .form_order,
		  .conquest_order
		  ) ]
		  
		  '  > $pkmntemp11

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

pkmntbl=$(mktemp)
mvtbl=$(mktemp)
itemtbl=$(mktemp)

xan select id,identifier,ability_id,is_hidden,slot <(xan join --left id csv/pokemon.csv pokemon_id csv/pokemon_abilities.csv) > $pkmntbl
xan select id,identifier csv/moves.csv > $mvtbl
xan rename name -s identifier csv/items.csv | xan select id,name  > $itemtbl

xan join --left generation_id csv/version_groups.csv id csv/generations.csv |
    xan rename version_gen_id,version_name,generation -s '0,1,6' |
    xan select 0,1,6 > $versiongroup

## Ability
changelogabil=$(mktemp)
xan join --left id csv/ability_changelog.csv ability_changelog_id csv/ability_changelog_prose.csv |
    xan filter 'local_language_id == 9' |
    xan drop local_language_id,ability_changelog_id > $changelogabil

changelogabil2=$(mktemp)
xan join --left changed_in_version_group_id $changelogabil version_gen_id $versiongroup |
    xan drop id,changed_in_version_group_id |
    xan implode --sep "<%>" effect,version_gen_id,version_name,generation > $changelogabil2

## Add changelog to Pokemon's current abilities
abilitylog=$(mktemp)
xan join --left ability_id $pkmntbl ability_id $changelogabil2 | xan drop 5 |
    xan implode --sep "<@>" ability_id,is_hidden,slot,effect,version_gen_id,version_name,generation |
    xan to json | jq '[ .[] | {id, identifier,
    abilities_changelog: [.ability_id, .slot, .is_hidden, .effect, .version_name, .version_gen_id, .generation] |
    map(split("<@>")) |
    transpose |
    map({ability: {id: .[0] | tonumber, 
    versions: [.[4], .[5], .[6], .[3]] | map(if . == null then "" else . end) | map(split("<%>")) | transpose | map({game: .[0], gen_id: .[1] | tonumber, generation: .[2], effect: .[3]})
    }, slot: .[1] | tonumber, is_hidden: .[2] | tonumber})
    }]' > $abilitylog

# Insert ability change log data for each ability in a given Pokemon
# Here the (.[0] // {}) is something no idea why it works => why is there abilities which are presumably null?

pkmntemp12=$(mktemp)
jq -s '[.[0], .[1]] | transpose | map(.[0] * .[1] | . * {abilities: [.abilities, .abilities_changelog] | transpose | map((.[0] // {}) * .[1])} | del(.abilities_changelog)) ' <(cat $pkmntemp11) $abilitylog > $pkmntemp12

#  ---- Past abilities for Pokemon
changelogabil3=$(mktemp)
xan join --left id csv/pokemon.csv pokemon_id <(xan join --left generation_id csv/pokemon_abilities_past.csv id csv/generations.csv |
						    xan drop id,main_region_id |
						    xan rename generation -s identifier) |
    xan select id,identifier,generation,ability_id,is_hidden,slot > $changelogabil3

changelogabil4=$(mktemp)
xan join --left ability_id <(xan join --left ability_id $changelogabil3 ability_id $changelogabil2 |
				 xan drop 6 |
				 xan rename generation_with_ability,version_effect,version_generation -s 2,6,9) ability_id_d <(
    xan select id,identifier,short_effect,effect $abilitytemp | xan rename ability_id_d,ability,long_effect -s 0,1,effect
) |
    xan fill -v -66 |
    #xan filter 'ability_id != -66' |
    xan drop ability_id_d |
    xan implode --sep "<%>" 6:9 | # Different versions of the same ability
    xan implode --sep "<@>" 3:12 | # Multiple abilities for a Pokemon in a given generation
    xan implode --sep "<#>" 2:12 | # Different set of abilities across generations, if they exist
    xan to json | #jq '[. [] | select(.id == 16 or .id == 94 or .id == 244 or .id == 609)]' |
    jq '[ .[] | {id: .id, identifier: .identifier, past_abilities: [
    .generation_with_ability, .ability_id, .ability, .slot, .is_hidden, .short_effect, .long_effect, .version_effect, .version_gen_id, .version_name, .version_generation
    ] | map(split("<#>")) | transpose | map({generation: .[0],

      abilities: [.[1], .[2], .[5], .[6], .[3], .[4], .[9], .[8], .[10], .[7]] | map(split("<@>")) | transpose | map({ability: { id: .[0] | tonumber, name: .[1], short: .[2], long: .[3]} | (if .id < 0 then {} else . end), slot: .[4] | tonumber, is_hidden: .[5] | tonumber, is_missing: .[0] | tonumber | (if 0 > . then 1 else 0 end), versions: [.[6], .[7], .[8], .[9]] | map(split("<%>")) | transpose | map({game: .[0], gen_id: .[1], generation: .[2], effect: .[3]}) | map(select(.game != "-66"))}) ,

    })
    } | if .past_abilities[0].generation == "-66" then {id,identifier,past_abilities: []} else . end]' > $changelogabil4

#cat $changelogabil4 | jq '.[] | select(.identifier == "bulbasaur")'

#jq -s '[.[0], .[1]] | transpose | map(.[0] * .[1])' <(cat $pkmntemp12) $changelogabil4 > pokemon.json

# --- Past types for Pokemon
echo "---"

typetmp2=$(mktemp)
xan join --left id csv/pokemon.csv pokemon_id <(xan join --left generation_id <(xan join --left type_id csv/pokemon_types_past.csv id csv/types.csv |
										    xan select pokemon_id,generation_id,slot,identifier) id csv/generations.csv |
						    xan drop id,main_region_id,generation_id) |
    xan drop species_id,height,weight,base_experience,order,is_default |
    xan rename type,generation -s '4,5' |
    xan implode --sep "|" slot,type | 
    xan implode --sep "<@>" slot,type,generation | 
    xan drop pokemon_id |
    xan to json |
    jq '[ .[] | {id, identifier, past_types: . | (if .slot == "" then [] else ([.generation, .type] | map(split("<@>")) | transpose | map({generation: .[0], types: .[1] | split("|")})) end)}]' > $typetmp2


jq -s '[.[0], .[1], .[2]] | transpose | map(.[0] * .[1] * .[2])' <(cat $pkmntemp12) $changelogabil4 $typetmp2 > pokemon.json

cat pokemon.json | head -5000

#### ------ Change log for moves ###


