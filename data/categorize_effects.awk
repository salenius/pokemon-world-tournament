BEGIN {OFS = FS}

{

    stat = "(stats|accuracy|evasion|Speed|((Special )?(Attack|Defense)))";

    pokemonStr = "Pokémon";
    xxxNewEff = "^XXX";


    # Book keeping

    ifCondCol = 3;
    forTurnCols = 4;

    # Initializations


    hasProbability = "has probability";
    isRequirement = "is requirement not effect";
    isCapability = "is capability";
    # Tag those effects which require knowledge on user AND target both
    # These are moves where you can't just replace the user or target or ally with
    # a generic <pokemon> term
    isInterPokemon = "is inter-pokemon";
    isUserSpecific = "is user specific";
    isTargetSpecific = "is target specific";
    pokemonAffected = "pokemon affected";
    damageIsModified = "has damage modification"
    effectExecutionOrder = "effect execution order"

    new_columns[hasProbability] = 0;
    new_columns[isCapability] = 0;
    new_columns[isRequirement] = 0;
    new_columns[isInterPokemon] = 0;
    new_columns[isUserSpecific] = 0;
    new_columns[isTargetSpecific] = 0;
    new_columns[pokemonAffected] = "-";
    new_columns[damageIsModified] = 0;
    # This will be modified when there are
    # so called conditional effects, where
    # another effect must stricly follow another
    # one. These are characterized by the phrase ", then"
    # but there could be others too.
    new_columns[effectExecutionOrder] = 1;


    # Do these globally
    
    gsub(/ (the|a|an) /, " "); # Remove articles as redudant
    sub(/^ /, ""); # Remove trailing white space from the descriptions beginning
    gsub(/  /, " "); # Remove double spacing
    gsub(/½/,"1/2");
    gsub(/⅔/,"2/3");

}

function unpack_new_columns(string1){
    string = string1;
    for(item in new_columns){
	# ---------------------- DELETE THIS AFTER DEBUGGING
	if (NR == 1) {
	    string = string FS item;
	} else {
	    string = string FS new_columns[item];
	}
    }
    return string;
}


function take_match(string_to_match, pattern){
    # Take a STRING_TO_MATCH, and return the first PATTERN that matches that string.
    match(string_to_match, pattern, arr);
    counter = 0;
    start = 0;
    length1 = 0;
    text = "";
    for (item in arr){
	counter++;
	if (counter == 1){
	    start = arr[item]; 
	} else if (counter == 2){
	    length1 = arr[item];
	} else {
	    text = arr[item];
	}
    }
    return substr(string_to_match, start, length1);
}

function replace_column(col_num, text){
    # Replace column number COL_NUM with TEXT
    for(i = 1; i<=NF; i++){
	if (i == col_num) {
	    $i = text;
	}
    }
}

function fold_str(fs, arr){
    total = "";
    for(item in arr){
	total = total fs arr[item];
    }
    # Remove the extra fs in the front
    sub(fs,"",total);
    return total;
}

function genetive_next_word_after_match(str, pattern){
    kopio = str;
    m = take_match(kopio, pattern);
    sub(m,"",kopio);
    split(kopio, arr, " ");
    new = "";
    for (item in arr){
	if (item == 1){
	    arr[item] = arr[item]"s";
	}
	new = new" "arr[item];
    }
    sub(" ","",new);
    return m" "new;
}


$2 ~ /^Has <effect_chance> chance to/ {
    new_columns[hasProbability] = 1;
    $2 = genetive_next_word_after_match($2, "^Has .* chance to");
}

$2 ~ /^(Only works|Can only) / {new_columns[isRequirement] = 1;}

$2 ~ /[dD]estroys/ {sub("[dD]estroys", "Removes target's");}

#### DAMAGE MODIFICATION
$2 ~ /(Has double power|Inflicts (double |twice |more )?damage|Damages target for)/ {
    new_columns[damageIsModified] = 1;
}

$2 ~ /^Power (is (higher|doubled|[0-9]+ times)|doubles every turn|varies randomly|increases|depend|can range)/ {new_columns[damageIsModified] = 1;}

$2 ~ /^type depend/ {new_columns[damageIsModified] = 1;}
$2 ~ /hits back .*damage/ {new_columns[damageIsModified] = 1;}
$2 ~ /Hits [0-9]-[0-9] times/ {new_columns[damageIsModified] = 1;}
$2 ~ /Inflicts.*points of damage/ {new_columns[damageIsModified] = 1;}
$2 ~ /Inflicts regular damage/ {new_columns[damageIsModified] = 1;}
$2 ~ /Super-effective against/ {new_columns[damageIsModified] = 1;}
$2 ~ /this move has/ {new_columns[damageIsModified] = 1;}
$2 ~ /always score critical hit/ {new_columns[damageIsModified] = 1;}
$2 ~ /Damages opponents, but/ {new_columns[damageIsModified] = 1;}

##### INTER-POKEMON
$2 ~ /([Cc]op(y|ies) |[Uu]ser with target.* swap |target.*user also|Exchanges user's.*with target's)/ {
    new_columns[isInterPokemon] = 1;
}
$2 ~ /user.*more than.*target/ {new_columns[isInterPokemon] = 1;}
$2 ~ /^Gives user's.*to target/ {new_columns[isInterPokemon] = 1;}
$2 ~ /^User swaps [A-Z].* changes with target/ {new_columns[isInterPokemon] = 1;}
$2 ~ /User becomes target's type/ {new_columns[isInterPokemon] = 1;}


### CAPABILITIES
$2 ~ /^(Never misses|[Cc]an (^only))/ {new_columns[isCapability] = 1;}
$2 ~ /Never misses/ {new_columns[isCapability] = 1;}
$2 ~ /^Deals.*type.*damage/ {new_columns[isCapability] = 1;}
$2 ~ /^Hits through/ {new_columns[isCapability] = 1;}
$2 ~ /can hit/ {new_columns[isCapability] = 1;}
$2 ~ /^hits even/ {new_columns[isCapability] = 1;}
$2 ~ /Can only/ {new_columns[isCapability] = 1;}
$2 ~ /Has increased chance .* critical hit/ {new_columns[isCapability] = 1;}

# Eg. 125
$2 ~ " for .* turns$" && $forTurnCols == "-" {
    y = take_match($2,"for .* turns");
    replace_column(forTurnCols, y);
    
}

$2 ~ /[Uu]ser's replacement/ && $ifCondCol == "-" {
    $ifCondCol = "user leaves battle field";
}

$2 == "uses berry" {sub("uses berry", "uses target's berry", $2);}

$2 ~ /^Pokémon cannot/ {sub(/Pokémon/, "all Pokémon",$2);}

$2 ~ /removes Leech Seed/ {sub(/removes/, "removes user's",$2);}

$2 ~ /Lets frozen/ {
    sub(/Lets frozen Pokémon thaw themselves/, "Cures user of freeze",$2);
    $ifCondCol = "if user is frozen";
}

#### CURSE
$2 ~ /^(Ghosts|Others)/ {
    if ($2 ~ /Ghost/){
	$ifCondCol = "user is Ghost type";
	new = "inflicts target 1/4 target's max HP in damage";
	sub(/hurt target/, new, $2);
    } else {
	$ifCondCol = "user is not Ghost type";
    }
    sub(/(Ghosts|Others)/, "User", $2);
}

# Sketch and Mimic => handle these so handlying "copying"
# won't become an issue later on
$2 ~ /(Permanently becomes|[Cc]opies target's last)/ {
    if ($2 ~ /Permanently/){
	$2 = $2 " permanently";
    } else {
	$2 = $2 " temporarily";
    }

    gsub(/(Permanently becomes|[Cc]opies)/, "move is replaced by");
}

# Excluding "even" so that 
($2 ~ " if .*$")  && ($2 !~ " even ") {
    y = take_match($2, " if .*");
    gsub(/,.*/,"",y);
    replace_column(ifCondCol, y);
}

# Only targets can be really flinched. Stealing implies
# target only effect too.
# Target's priority modification dynamically is also target specific
$2 ~ /(flinch|[sS]teal)/ {new_columns[isTargetSpecific] = 1;}
$2 ~ /^Takes target's/ {new_columns[isTargetSpecific] = 1;}
$2 ~ /^Makes target act/ {new_columns[isTargetSpecific] = 1;}
$2 ~ /^Causes .* KO/ {new_columns[isTargetSpecific] = 1;}

# Recoil and draining is user specific, same as going semi-invulnerable
# Sky Drop is a case which needs to be handled delicadedly here.
$2 ~ /[uU]ser (takes|receives) .* in recoil/ {new_columns[isUserSpecific] = 1;}
$2 ~ /^Drains/ {new_columns[isUserSpecific] = 1;}
$2 ~ /^User charges/ {new_columns[isUserSpecific] = 1;}
$2 ~ /^Requires turn to charge/ {new_columns[isUserSpecific] = 1;}
$2 ~ /recharge(s)?/ {new_columns[isUserSpecific] = 1;}
$2 ~ /dodging all attacks/ {new_columns[isUserSpecific] = 1;}

#############3
### VERY IMPORTANT
#################
(($2 ~ /(entire party|(friendly|opposing) Pokémon|selected ally|Ally|[uU]ser|[Tt]arget|[aA]ll .*Pokémon( on (the )?ground| with {ability:[a-z]+} or {ability:[a-z]})?)/) &&
 (new_columns[isTargetSpecific] == 0) &&
 (new_columns[isUserSpecific] == 0) &&
 (new_columns[isCapability] == 0) &&
 (new_columns[isRequirement] == 0) &&
 (new_columns[damageIsModified] == 0) &&
 (new_columns[isInterPokemon] == 0)) {

    new_columns[pokemonAffected] = take_match($2, "(entire party|(opposing|friendly) Pokémon|selected ally|Ally|[uU]ser|[tT]arget|[aA]ll .*Pokémon( on (the )?ground|with {ability:[a-z]+} or {ability:[a-z]})?)"); 
} 

{new1 = unpack_new_columns($0);print new1}

# NR > 1 {print $0 FS hasProbability }
