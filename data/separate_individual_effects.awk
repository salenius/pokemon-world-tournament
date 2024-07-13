###########################################
### Veekun Pokedex effect prose decoder ###
### Step 1: Separate individual effects ###
###########################################
#####---------------------------------- ###
## The purpose of this AWK script is to
## take the move_effect_prose.csv file as
## an input and parse all the individual
## effects from compound effects (meaning
## effects which do A and (then) B). When
## this file has been ran, then we proceed
## to the next step where effects are to
## be more abstracted out and further
## structured into record like rows.
###########################################
### Author: Tommi Salenius (C)
### Date: 11.7.2024
###########################################


BEGIN {
    count1 = 0;
}

{
    statPattern = "(stats|Speed|accuracy|evasion|(Special )?(Attack|Defense))";
    shellSmashMagicNumber = 309; # Use this to handle a special case. Ugly hack, yes.
}

function getMatch(x){
    return substr(x,RSTART,RLENGTH);
}


/^[0-9]+,"/{
    gsub(/",".*/,"");
    gsub(/"/,"");
    gsub(/\.,.*/,"");
    gsub(/,".*/,"");
}

{
    number = $1;
    sub(/,.*/,"",number);
    gsub(/^[0-9]+,/,""); 

    # Initiate variable which tracks whether there is a reference to user or target
    # when using the term "it"
    userOrTargetVar = "";

    for(i = 1; i <= NF; i++) {
	if ($i ~ /^[uU]ser('s)?$/ || $i  ~ /^[tT]arget('s)?$/){
	    userOrTargetVar = $i;
	};
	if ($i ~ /^it(s)?$/ && userOrTargetVar != ""){
	    sub("it", userOrTargetVar, $i);
	    sub("sers","ser's",$i);
	    sub("argets","arget's",$i);
	}

    };

    
}

function smartprint(x){
    if (x !~ / and /){
	y = x;
	gsub(/\.  .*/,"",y); # This is buggy when dealing with Shell Smash (# 309)
	print number " " y;
    }

}



function splitListed(x, pattern, patterncopy, dotPattern){
    linecopy = x;
    # patterncopy = $0;
    sp = pattern;
    gsub(sp, "<pattern>", linecopy);
    gsub(dotPattern, "", linecopy); # if a comma and then something, ignore it.
    # gsub("(^"sp")", "", patterncopy);
    # patterncopy = andthese;
    # print ">>!!!>> "patterncopy;
    gsub(",", "", patterncopy); # Remove colons
    gsub("Special ", "Special-", patterncopy); # Easier iteration next
    gsub(" and ", " ", patterncopy); # Remove and
    
    split(patterncopy, patterns, " ");
    for(item in patterns){
	tempvar = linecopy;
	sub("-", " ", patterns[item]); # Special-Attack -> Special Attack again
	sub("<pattern>", patterns[item], tempvar);
	smartprint(tempvar);
    }

 
}

### Averages

/^Averages.*with/ {
    # x = $0;
    split($0, avg, "and");
    s1 = avg[1];
    s2 = avg[2];
    sub(statPattern, "", s1);
    sub(statPattern, "", s2);
    smartprint(avg[1] s2);
    smartprint("Averages"avg[2]);
    $0 ="";
}

/^Sets.*average/ {
    gsub("and","<->");
}

/^Removes.*Safeguard/ {
    gsub(/.*/, "Removes Safeguard");
}

/both faint/{
    ptrn = "User and target both faint";
    x = $0;
    y = $0;
    sub(ptrn, "User faints", x);
    sub(ptrn, "Target faints", y);
    smartprint(x);
    smartprint(y);
    $0 = "";
}

/Target falls in love./ {
    gsub(", and has", ", then has");
}

/User faints,/ {
    gsub(", and", ", then");
}

/Frees the user.*removes Leech.*away Spikes/ {
    split($0, frees, ",");
    for(item=1; item<=3;item++){
	sub(/(  |and )/, "",frees[item]);
	smartprint(frees[item]);
    }
    $0 = "";
}

/^Has double power against,.*switch.*/ {
    kopio = $0;
    sub(", and can hit,", "", $0);
    sub("^Has.*can hit,","Can hit",kopio);
    smartprint($0);
    smartprint(kopio);
    $0 = "";

}

/between [0-9]+( %|%) and [0-9]+( %|%)/ {
    sub("and","to");
    sub("between","in range from");
}

/^Ghost.*Others/ {
    gsub("Ghosts","If user is Ghost-type,");
    gsub("Others","If user is not Ghost-type,");
}

/^User and target swap/ {
    sub(/User and target swap/, "User swaps");
    $0 = $0 " with target";

}

/^User swaps.*and.*with (the )?target/ {
    kopio = $0;
    sub(" "statPattern" and", "", kopio);
    sub("and "statPattern " ", "", $0);
    smartprint($0);
    smartprint(kopio);
    $0 = "";
}

/^(User swaps.*and|All Pok.*mon.*are swapped)/ {
    sub(" and "," <-> ");
}

/dodging all attacks.*, and (drops|hits)/ {
    sub(", and (drops|hits)", ", then hits");
}


{
    stats = ""
}

match($0,statPattern", "statPattern", and "statPattern) {
    stats = substr($0,RSTART,RLENGTH);
}

$0 ~ statPattern", "statPattern", and "statPattern {
    gsub("each(.)?$",""); # Remove trailing each referring to each stat change separately

    splitListed($0,statPattern", "statPattern", and "statPattern,stats,"\\.  .*");

    ## For Shell Smash handle this specifically
    if (number != shellSmashMagicNumber){$0 = "";} else {sub(/.*\.  /, "", $0)}
    
    }

match($0,/(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+/) {
    andthese = substr($0,RSTART,RLENGTH);
}




function capitalize(x){
    return toupper( substr(x, 1, 1 ) ) substr(x, 2 );
}


function andNewLine(x, keyword, result){
    # Use this to get "---- and xxxxx" -> "-----" & "xxxxx" on different lines
    split(x, result, " and ");
    smartprint(result[1]);
    sub(keyword,capitalize(keyword),result1[2]);
    smartprint(result[2]);
    return result[1];
}

function replaceTwoTermsWithOne(x, pattern){
    # Use this to get "----- X and Y >>>>>>" -> "------ X >>>>>" and "------ Y >>>>>>"
    split(pattern, results, "and");
    sub(pattern,"<mark>",x);
    copy1 = x;
    copy2 = x;
    sub("<mark>", results[1],copy1);
    sub("<mark>", results[2],copy2);
    smartprint(copy1);
    smartprint(copy2);
    
}

/Raises.*Attack and Speed/ {
    replaceTwoTermsWithOne($0, "Attack and Speed");
    $0 = "";
}

/Raises.*Attack and accuracy/ {
    replaceTwoTermsWithOne($0, "Attack and accuracy");
    $0 = "";
}


/^Power and type/ {
    replaceTwoTermsWithOne($0, "Power and type");
}

/Protect and Detect/ {
    replaceTwoTermsWithOne($0, "Protect and Detect");
}


###################### Testaa toimiiko #############################
statPattern" and "statPattern".*by.*stage" {
    statl[1] = "Special Attack";
    statl[2] = "Special Defense";
    statl[3] = "Attack";
    statl[4] = "Defense";
    statl[5] = "Speed";
    statl[5] = "accuracy";
    statl[5] = "evasion";

    found = 0;
    for(i in statl){
	for(j in statl){
	    term = statl[i] " and " statl[j];
	    if ($0 ~ term){
		replaceTwoTermsWithOne($0, term);
		$0 = "";
		found = 1;
		break;
	    }
	    if (found == 1){
		break;
	    }
	}
    }
}


# /\..*(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+/ {

    # splitListed($0,"(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+",andThese,".*\\.  ");
# }



/\.  [A-Z]/{
    split($0, result, "\\.");
    gsub(/  /, "", result[2]);
    smartprint(result[1]);
    smartprint(result[2]);
}



/Spit Up and Swallow/ {
    # andNewLine($0, "Swallow", result);
    replaceTwoTermsWithOne($0, "Spit Up and Swallow");
    $0 = "";
}

/(Lowers the user's.*after inflicting damage|Lowers the target's.*Attack and Defense.*)/ {
    replaceTwoTermsWithOne($0,"Attack and Defense");
    $0 = "";
}

/Destroys Reflect and Light Screen/ {
    replaceTwoTermsWithOne($0, "Reflect and Light Screen");
    $0 = "";
}

/ and can /{
    split($0, result1, " and ");
    smartprint(result1[1]);
    sub("can","Can",result1[2]);
    smartprint(result1[2]);
    $0 = result1[1];
}

/ and confuses / {
    $0 = andNewLine($0, "confuses", result3);
}

/ and lowers / {
    $0 = andNewLine($0, "lowers", resultLowers);
}

/ and uses the berry/ {
    $0 = andNewLine($0, "uses", resultLowers);
}
/ and cures / {
    $0 = andNewLine($0, "cures", resultLowers);
}
/ and wakes / {
    $0 = andNewLine($0, "wakes", resultLowers);
}
/ and heals / {
    $0 = andNewLine($0, "heals", resultLowers);
}

/moves and (effect|immunities)/ {
    sub("moves and ", "moves/");
}

/^Forces the target.*allows target to be hit by/ {
    sub("and ","");
    split($0,foresight,",");
    gsub(/^ a/, "A", foresight[2]);
    smartprint(foresight[1]);

    if (foresight[2] ~ /Fighting/){
	normal = foresight[2];
	fighting = foresight[2];
	gsub(/and Fighting/,"",normal);
	gsub(/Normal and/,"",fighting);
	smartprint(normal);
	smartprint(fighting);
    }
    else {
	smartprint(foresight[2]);
    }

    $0 = "";

}

/replacement/ {
    gsub(" and ", " then ");
    
}

/Blocks damaging/ {
    gsub(/and damages/, "then damages");
}

/Discards/ {
    gsub(/.*/,"Copies the target's stat changes");
}

function splitByCommaAndThenByAnd(x){
    split(x, weather, ",");
    split(weather[2], effect, "and")
    smartprint(weather[1]", "effect[1]);
    smartprint(weather[1]", "effect[2]);
 
}

/If there be weather/ {
    gsub("and the", "and this move has");
    splitByCommaAndThenByAnd($0)
    $0 = "";
}

/Protects the user's field/ {
    kopio = $0;
    gsub("major status ailments and","", kopio);
    gsub("and confusion","",$0);
    smartprint($0);
    smartprint(kopio);
}

# This is about Terrain changes
/strengthens/ {
    gsub("their", "all Pokemon");

    splitByCommaAndThenByAnd($0);
    $0 = "";

}


/Grants the user/ {
    gsub("Grants the user protection", "Protects the user");
}

/Protects (P.* on the ground|the user for)/ {
    gsub("their","Pokemon");
    these = $0;
    split(these,resultset,"and");
    gsub(/^ /, "",resultset[2]);
    resultset[2] = capitalize(resultset[2]);
    smartprint(resultset[1]);
    smartprint(resultset[2]);
    $0 = "";
}



/^Has a(n)? .*effect_chance/ {
    kopio = $0;
    gsub(".*and a", "Has a", kopio);
    gsub("and a.*", "", $0);
    smartprint($0);
    smartprint(kopio);
}

/and halves/ {
    gsub(".*and halves", "Halves");
    smartprint($0);
}

/selects and uses/ {
    gsub("selects and uses","uses");
}


/Prevents.*(fleeing|leaving).*inflicts.*damage/ {
    gsub(" and inflicts", " while at same time inflicts");
    smartprint($0);
}


$0 !~ /( and |\.  )/{
    if ($0 != ""){
	sub(/\.  .*/,"",$0);
	smartprint($0);
    }
}


/ [a-z]+ and [a-z]+ /{
    count1 += 1;
    print ">" $0;

    
}


$0 ~ /, and/{
}

