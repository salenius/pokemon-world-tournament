#!/usr/bin/env bash

# (C) Tommi Salenius (2025)

# Rewritten version of how pokemon.json file is produced in combine-data.sh script

stat_divider="<<--stat-->>"
abil_divider="<<--abil-->>"
abil_past_divider="<<--abil-past-->>"
type_divider="<<--type-->>"
slot_divider="<<--slot-->>"

pkmn_bare=$(mktemp)

xan select id,identifier csv/pokemon.csv |
    xan rename id,name > $pkmn_bare

## --- Stats
pkmn_stats=$(mktemp)

xan join --left stat_id csv/pokemon_stats.csv id csv/stats.csv |
    xan drop is_battle_only,game_index,damage_class_id,id,stat_id |
    xan rename stat_name -s identifier |
    xan implode base_stat,effort,stat_name | 
    xan join --left id $pkmn_bare pokemon_id - |
    xan drop pokemon_id |
    xan to json |
    jq '[. [] | {id,name,stats: [.stat_name, .base_stat, .effort]
    | map(split("|"))
    | transpose
    | map({(.[0]): {base: .[1] | tonumber, effort: .[2] | tonumber}})
    | (reduce .[] as $stat_obj ({}; . * $stat_obj))}]' > $pkmn_stats

## --- Ability
pkmn_abilities=$(mktemp)
ability_csv=$(mktemp)
pkmn_past_abilities=$(mktemp)

xan filter "local_language_id == 9" csv/ability_prose.csv |
    xan join --left id csv/abilities.csv ability_id - |
    xan rename long_effect -s effect |
    xan drop ability_id,local_language_id,is_main_series,generation_id |
    xan join --left id - ability_id csv/ability_changelog.csv |
    xan rename ability_name,change_id -s 1,4 |
    xan join --left change_id - ability_changelog_id <(xan filter "local_language_id == 9" csv/ability_changelog_prose.csv) |
    xan join --left changed_in_version_group_id - id csv/version_groups.csv |
    xan drop changed_in_version_group_id,local_language_id,order,ability_id,change_id |
    xan rename ability_id,game,long_effect_delta -s 0,identifier,effect |
    xan drop id |
    xan join --left generation_id - id csv/generations.csv |
    xan drop generation_id,id,main_region_id |
    xan rename generation -s identifier |
    xan implode --sep $abil_divider ability_changelog_id,long_effect_delta,game,generation > $ability_csv

xan join --left id $pkmn_bare pokemon_id csv/pokemon_abilities.csv |
    xan join --left ability_id - ability_id $ability_csv |
    xan drop ability_id,pokemon_id | 
    xan implode --sep $slot_divider 2:11 |
    xan to json |
    jq --arg slotdiv $slot_divider --arg abildiv $abil_divider '[.[] | {
    id,name,abilities: [.slot, .is_hidden,
    		       .ability_id, .ability_name, .short_effect, .long_effect,
    		       .ability_changelog_id, .long_effect_delta, .game, .generation
    		       ]
		       | map(split($slotdiv))
		       | transpose
		       | map({slot: .[0] | tonumber, is_hidden: .[1] | tonumber | (. > 0),
		       ability: {id: .[2] | tonumber, name: .[3], effect: {short: .[4], long: .[5]}},
		       versions: .[6:10] | map((. // "") | split($abildiv))
		       		 | transpose
				 | map({changelog_id: .[0] | tonumber, game: .[2], generation: .[3], effect: .[1]})
		       		       })
		       }] ' > $pkmn_abilities


# Notice, this one doesn't take into account if multiple of a single Pokemon's abilities
# change within a single generation. Should be fixed at some point! Currently no Pokemon
# have experienced this so the code still works though.
xan join --left id $pkmn_bare pokemon_id csv/pokemon_abilities_past.csv |
    xan join --left ability_id - ability_id $ability_csv |
    xan drop ability_id |
    xan join --left generation_id - id csv/generations.csv | 
    xan drop 14,15 |
    xan rename game_changelog,generation_changelog,generation_past_ability -s 12,13,14 |
    xan map 'pokemon_id ne null and ability_id eq null' ability_missing |
    xan drop pokemon_id,generation_id |
    xan implode --sep $abil_past_divider 2:13 |
    xan to json |
    jq --arg pastdiv $abil_past_divider --arg abildiv $abil_divider '
    [.[] | {id, name, past_abilities: (if .slot == "" then [] else [
    	 .generation_past_ability, .ability_missing, .slot,
	 .is_hidden, .ability_id, .ability_name, .short_effect, .long_effect,
    	 .ability_changelog_id, .generation_changelog, .game_changelog, .long_effect_delta
	 ]
	 | map(split($pastdiv))
	 | transpose
	 | map({generation: .[0], change: (if .[1] == "true" then
    	   {slot: .[2] | tonumber, is_hidden: .[3] | tonumber | (. > 0), missing: true} else {
    	   slot: .[2] | tonumber,
	   is_hidden: .[3] | tonumber | (. > 0), missing: false,
	   ability: {id: .[4] | tonumber, name: .[5], effect: {short: .[6], long: .[7]}},
    	   versions: .[8:12] | map((. // "") | split($abildiv))
		       		 | transpose
				 | map({id: .[0] | tonumber, game: .[2], generation: .[1], effect: .[3]})
    } end)})
    end)}]' > $pkmn_past_abilities

### --- Type
pkmn_types=$(mktemp)

xan join --left id $pkmn_bare pokemon_id csv/pokemon_types.csv |
    xan join --left type_id - id csv/types.csv |
    xan drop 2,5,7,8 |
    xan rename type -s identifier |
    xan implode type_id,slot,type |

    xan join --left id - pokemon_id csv/pokemon_types_past.csv |
    xan rename past_type_id,past_type_slot -s 7,8 |
    xan join --left past_type_id - id csv/types.csv |
    xan rename past_type_id2,past_type -s 9,10 |
    xan drop past_type_id2 |
    xan join --left generation_id - id csv/generations.csv |
    xan rename generation -s 14 |
    xan drop 10:13 |
    xan implode --sep $type_divider generation_id,past_type_id,past_type_slot,past_type,generation |
    xan implode past_type_id,past_type_slot,past_type |
    xan to json |
    jq --arg typediv $type_divider '[.[] | {id,name, types: [.type_id, .slot, .type]
       | map(split("|"))
       | transpose
       | map({slot: .[1] | tonumber, id: .[0] | tonumber, name: .[2]}),
       past_types: [.generation, .past_type_slot, .past_type_id, .past_type]
       | map(split($typediv))
       | transpose
       | map(. | map(split("|")) | transpose | map({generation: .[0], slot: .[1] | tonumber, id: .[2] | tonumber, name: .[3]}))
       }]' > $pkmn_types

### -- Species and basic info
# Here we also normalize height and weight back to normal levels (they had previously been multiplied by 10)
# --> check if this normalization should be done later on
pkmn_species=$(mktemp)

xan join --left species_id csv/pokemon.csv id csv/pokemon_species.csv |
    xan rename species_id2,species_name -s 8,9 |
    xan drop 6,8,26 |
    xan join --left species_id - evolves_from_species_id <(xan select id,evolves_from_species_id csv/pokemon_species.csv) |
    xan rename next_evol,x -s -2,-1 |
    xan drop x |
    xan join --left color_id - id csv/pokemon_colors.csv |
    xan rename color -s -1 |
    xan rename color_id2 -s -2 |
    xan drop color_id2,conquest_order,evolution_chain_id,color_id |
    xan join --left shape_id - id csv/pokemon_shapes.csv |
    xan rename shape,shape_id2 -s -1,-2 |
    xan drop shape_id2,shape_id,generation_id |
    xan join --left growth_rate_id - id csv/growth_rates.csv |
    xan rename name,growth_rate,growth_rate_formula -s 1,-2,-1 |
    xan drop growth_rate_id,habitat_id |
    xan implode next_evol |
    xan join --left species_id - pokemon_species_id <(xan filter "local_language_id == 9" csv/pokemon_species_prose.csv) |
    xan to json |
    jq '[.[] | {id, name, species: {
    species_id,
    name: .species_name,
    form_description,
    gender: {
    female_probability: .gender_rate | (. / 8 ) | (if . < 0 then 0 else . end),
    genderless: .gender_rate | (. < 0),
    has_gender_differences: .has_gender_differences | (. > 0)
    },
    capture_rate,
    base_happiness,
    hatch_counter,
    is_baby: .is_baby | (. > 0),
    is_legendary: .is_legendary | (. > 0),
    is_mythical: .is_mythical | (. > 0),
    next_evolutions: .next_evol | split("|") | map(tonumber),
    color,
    shape,
    growth_rate,
    growth_rate_formula	    
	    
    },
    physiology: {weight: .weight | (. / 10), height: .height | (. / 10)},
    base_experience
    }]' > $pkmn_species


### --- Species
## Final output
jq -s '[.[]] | transpose | map(reduce .[] as $item ({}; . * $item))' <(cat $pkmn_stats) $pkmn_species $pkmn_abilities $pkmn_past_abilities $pkmn_types

