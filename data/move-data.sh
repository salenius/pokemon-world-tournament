#!/usr/bin/env bash

# (C) Tommi Salenius (2025)

# This is the rewriting of the move data
# transformations from CSV into JSON
# described in the combine-data.sh file.

effdiv="<<<@>>>"
flagdiv="<<<%>>>"
mvchdiv="<<<#>>>"

move_data=$(mktemp)
eff_data=$(mktemp)
move_base_data=$(mktemp)
move_change_data=$(mktemp)

# Join all the current effect data and its changelog into the general move data
#
# Also more generally replace damage_class_id,target_id etc with their respective values from other CSV files.
#
# As a general notice, as of May 4th 2025, many new moves are still lacking the description of the effect in
# the move_effect_prose.csv file, which explains why the joins end up with many null values in the effect
# columns. This seems to be related to the fact that the Veekun database, which the PokeAPI draws vast majority
# of the underlying data from, the move effect description data only lasts until Generation VII
# --
xan join --left effect_id csv/moves.csv move_effect_id <(xan filter "local_language_id == 9" csv/move_effect_prose.csv) |
    xan drop contest_type_id,contest_effect_id,super_contest_effect_id,move_effect_id,local_language_id |
    xan rename name -s identifier |
    xan join --left target_id - id csv/move_targets.csv |
    xan rename target_id2,target -s -2,-1 |
    xan drop target_id2 |
    xan join --left damage_class_id - id csv/move_damage_classes.csv |
    xan rename status_id,damage_class -s -2,-1 |
    xan drop status_id |
    xan drop target_id,damage_class_id |
    xan join --left type_id - id csv/types.csv |
    xan rename type_id,type -s -4,-3 |
    xan drop type_id,damage_class_id,generation_id |
    xan drop generation_id |
    xan join --left effect_id - effect_id csv/move_effect_changelog.csv |
    xan rename effect_change_id -s -3 |
    xan join --left effect_change_id - move_effect_changelog_id <(xan filter "local_language_id == 9" csv/move_effect_changelog_prose.csv) |
    xan drop local_language_id,effect_change_id |
    xan rename effect_delta_id,version_group_changed_id,effect_changelog_id,effect_delta -s -4,-3,-2,-1 |
    xan join --left version_group_changed_id - id csv/version_groups.csv |
    xan rename version_group_id,version_group -s -4,-3 |
    xan drop order |
    xan join --left generation_id - id csv/generations.csv |
    xan rename gen_id,main_reg_id,generation -s -3,-2,-1 |
    xan drop gen_id,main_reg_id,generation_id,version_group_id,version_group_changed_id |
    xan implode --sep $effdiv effect_delta_id,effect_changelog_id,effect_delta,version_group,generation > $move_data

# Save the effect data to be reused later when the move changelogs are used
xan select effect_id,short_effect,effect,effect_delta_id,effect_changelog_id,effect_delta,version_group,generation $move_data | xan dedup > $eff_data

# Flags
xan join --left id $move_data move_id csv/move_flag_map.csv |
    xan join --left move_flag_id - id csv/move_flags.csv |
    xan join --left move_flag_id - move_flag_id <(xan filter "local_language_id == 9" csv/move_flag_prose.csv) |
    xan rename move_id2,move_flag_id,id2,flag_name,move_flag_id2,local_lang,flag_short_description,flag_long_description -s 19:26 |
    xan drop 19,21,23:24 |
    xan implode --sep $flagdiv move_flag_id,flag_name,flag_short_description,flag_long_description |
    xan to json |
    jq --arg effdiv $effdiv --arg flagdiv $flagdiv '
    [.[] | {id, name, basics: {pp, accuracy, priority, damage_class, target},
    type: {id: .type_id, name: .type},
    power, effect: {effect_chance: .effect_chance, short: .short_effect, long: .effect,
    versions: [.effect_delta_id, .effect_chagelog_id, .effect_delta, .version_group, .generation]
    | map((. // "")
    | split($effdiv))
    | transpose
    | map({log_id: .[1] , delta_id: .[0] | tonumber, effect_delta: .[2], version_group: .[3], generation: .[4]})
    },
    flags: [.move_flag_id, .flag_name, .flag_short_description, .flag_long_description]
    | map(split($flagdiv)) | transpose | map({id: .[0] | tonumber, name: .[1], short: .[2], long: .[3]})
    }]
    ' > $move_base_data

xan join --left effect_id csv/move_changelog.csv effect_id $eff_data |
    xan join --left target_id - id csv/move_targets.csv |
    xan rename target_id2,target,version_group_changed_id -s -2,-1,1 |
    xan drop target_id2,effect_id |
    xan drop target_id |
    xan join --left type_id - id csv/types.csv |
    xan rename type_id2,type_name -s -4,-3 |
    xan drop type_id2,generation_id,damage_class_id |
    xan join --left version_group_changed_id - id csv/version_groups.csv |
    xan rename version_group_id,version_group -s -4,-3 |
    xan drop order |
    xan join --left generation_id - id csv/generations.csv |
    xan drop generation_id,id,main_region_id,version_group_changed_id |
    xan rename generation -s identifier |
    xan rename effect_version_group,effect_generation -s 13,14 |
    xan implode --sep $mvchdiv 1:19 |
    #xan filter "move_id == 16 or move_id == 26 or move_id == 71" |
    xan join --left id <(cat csv/moves.csv | xan select id) move_id - |
    xan to json |
    jq --arg mvdiv $mvchdiv '[.[] | {
    id: .id,
    changelog: [
    .version_group_id,
    .version_group,
    .generation,
    .type_id,
    .type_name,
    .power,
    .pp,
    .accuracy,
    .priority,
    .target,
    .effect_chance,
    .effect_id,
    .short_effect,
    .effect,
    .effect_delta_id,
    .effect_changelog_id,
    .effect_delta,
    .effect_version_group,
    .effect_generation
    ] | map(split($mvdiv) | map(. as $var | try (. | tonumber) catch $var)) | transpose | map([., [
    "version_group_id",
    "version_group",
    "generation",
    "type_id",
    "type_name",
    "power",
    "pp",
    "accuracy",
    "priority",
    "target",
    "effect_chance",
    "effect_id",
    "short_effect",
    "effect",
    "effect_delta_id",
    "effect_changelog_id",
    "effect_delta",
    "effect_version_group",
    "effect_generation"
    ]] | transpose | map(select(.[0] != "" and .[0] != null)) | map( . as [$val, $key] |
    (if (["generation","version_group_id","version_group"] | contains([$key]))
    	then {($key): $val}
	else {change: {($key): $val}} end))
    | (reduce .[] as $item ({}; . * $item)) )
    }]' > $move_change_data

jq -s '[.[0], .[1]] | transpose | map(reduce .[] as $idx ({}; . * $idx))' <(cat $move_base_data) $move_change_data



 
