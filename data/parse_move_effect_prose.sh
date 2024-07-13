#!/usr/bin/env sh
# This shell script is used to parse veekun/move_effect_prose.csv file
# where move's each single atomic effect is in its separate line
##################################
### Author: Tommi Salenius (C) ###
### Date: 13.7.2024 ##############
##################################

stats_regex="(stats|Speed|accuracy|evasion|(Special )?(Attack|Defense))"
screen_regex="\\\[.*\\\]\\\{move:(light-screen|reflect|safeguard)\\\}"

replaceAndTerms(){
    awk -F\@ -v VAR="$1" -f move_effect_split_and.awk 
}

cat veekun/move_effect_prose.csv |
 sed 's/,9/,/' |
 awk '{sub(/\.,.*/, ""); print}' |
 grep '^[0-9]' |
 grep "," |
 sed 's/,,/,/' |
 sed 's/",".*/"/' |
 sed 's/"//g' |
 sed 's/\.,.*//' |
 # 352 case
 sed 's/1\.5/3\/2/' |
 awk -F, '{sub(",","@"); print}' |
 awk -F\@ -f move_effect_it_rename.awk |
 awk -F\@ '{split($0,result,".");print result[1]; print $1"@"result[2];}' |
 awk -F\@ '($2 != "") {print $0}' |
 awk -F\@ -v stats="$stats_regex" '$0 ~ stats", "stats", and "stats {print $0 FS "3 stats";} $0 !~ stats", "stats", and "stats {print $0 FS "no stats";}' |
 awk -F\@ '{slicable = "no slicable and";} $0 ~ /, and/ && $3 != "3 stats" {slicable = "has slicable and"} {print $0 FS slicable} '  |
 awk -F\@ '$2 ~ /Has a( .*effect_chance|n increased chance).*and a .*effect_chance/ {$4 = "has slicable and"} {OFS=FS}{print }' |
 awk -F\@ -v STATS="$stats_regex" -f move_effect_split_three_stats.awk |
 awk -F\@ '{sub(/at the end of the next turn/, "after one turn"); print}' |
 awk -F\@ -v STATS="$screen_regex" -f move_effect_split_three_stats.awk |
 awk -F\@ -v VAR="$stats_regex" -f move_effect_split_and.awk |
 awk -F\@ -v VAR="[$]effect_chance[%]" '{gsub(VAR, "<effect_chance>"); print}' |
 replaceAndTerms "(moves|effects|immunities)" |
 replaceAndTerms "(Reflect|Light Screen)" |
 replaceAndTerms "(Power|type)" |
 replaceAndTerms "(doubled power|the weather's type)" |
 # Handle all the ", and" cases
 replaceAndTerms "(Has double power against,|can hit,)" |
 awk -F\@ '/falls in love/ {sub(/, and has/, ", then target")} {print}' |
 # BEGIN {OFS = FS} ensures that the field separator is not changed when modifying a column
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ /^For five turns,.*, and/ {sub(/, and/, ", and For five turns,")} {print}' |
 awk -F\@ -v STATS="(Frees the user from binding moves|removes Leech Seed|blows away Spikes)" -f move_effect_split_three_stats.awk |
 awk -F\@ '$0 !~ /, and / {print} /, and / {split($2, dict, ", and"); for(item in dict){$2 = dict[item]; print $1 FS $2 FS $3 FS $4}}' |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ /^User and target swap/ {sub(/and/,"with",$2)} {print}' |
 awk -F\@ '{contAnd = "-"} $2 ~ / and / {contAnd = "contains and"} {print $0 FS contAnd}' |
 replaceAndTerms "[A-Z][a-z]+" |
 replaceAndTerms "(major status ailments|confusion)" |
 replaceAndTerms "(Ignores|destroys)" |
 replaceAndTerms "(HP fully restored|any major status effect removed)" |
 replaceAndTerms "(damaging target for 1/4 target's max HP|preventing the move)" |
 sed 's/Randomly selects and uses/Randomly uses/' |
 # ---
 awk -F\@ '$2 ~ /^Has a(n increased)? / {sub(/ and a/, " and Has a")} {print}' |

 awk -F\@ '{hasFor="-"; hasIf="-";} $2 ~ /^If.*,/ {kopio=$2;split(kopio,res1,",");hasIf=res1[1]} $2 ~ /^For.*,/ {kopio1=$2;split(kopio1,res2,",");hasFor=res2[1]} {print $0 FS hasIf FS hasFor}' |
 awk -F\@ -v VAR="'s" 'BEGIN {OFS = FS} $5 ~ "contains and" && $2 ~ /(all )?Pokémon on the ground/ {sub(/ their /, " Pokémon"VAR " on the ground ")} {print}' |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ /^Discards.*stat changes/ {sub(/$/," stat changes",$2)} {print}' |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ / [a-z]+ and [a-z]+s .*for.*turns/ {kopio=$2; split(kopio, dict, "for"); sub(/.*/,"for"dict[2],$7)} {print}' |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ /between [0-9]+% and [0-9]+%/ {sub(/between/,"from", $2); sub(/and/, "to",$2)} {print}' |
 awk -F\@ -v VAR="'s" 'BEGIN {OFS = FS} $2 ~ /^Sets.*HP.*average/ {sub(/their/, "user"VAR" \\& target"VAR)} {print}' |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ / both / {sub(/both/, ""); sub(/faint/,"faints")} {print}' |
 # Last few cases
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ /^Raises.*by.*stage(s)? and user.*by.*stages/ {sub(/and user/, "and Raises user",$2)} {print}' |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ /^Blocks/ || $2 ~ /^Grants the user protection for the rest/ {sub(/.*/, "for the rest of the turn", $7)} {print}' |
 replaceAndTerms "(User|target)" |
 replaceAndTerms "(the user's|target(s)?'s)" |
 replaceAndTerms "\\[\\]\\{type:[a-z]+\\}" |
 # Split from any and
 awk -F\@ -f move_handle_352_and_369.awk |
 awk -F\@ 'BEGIN {OFS = FS} $2 ~ / and / {c1 = $0; c2 = $0; s2 = $2; split(s2,dict," and "); sub(s2, dict[1],c1); sub(s2, dict[2],c2); print c1; print c2} $2 !~ / and / {print}' |
 awk -F\@ 'BEGIN {OFS = FS; print "0" FS "effect" FS "required condition" FS "how long"}{sub(/^ /, "",$2);print $1 FS $2 FS $6 FS $7}' |
 sort -k 1 -n -t "@" > move_effect_prose_parsed.csv
