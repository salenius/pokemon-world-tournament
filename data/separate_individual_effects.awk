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
}

function getMatch(x){
    return substr(x,RSTART,RLENGTH);
}

function smartprint(x){
    if (x !~ / and /){
	print x;
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

/^[0-9]+,"/{
    gsub(/",".*/,"");
    gsub(/"/,"");
    gsub(/\.,.*/,"");
    gsub(/,".*/,"");
}

{
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

/^Removes.*Safeguard/ {
    gsub(/.*/, "Removes Safeguard");
}

/Target falls in love./ {
    gsub(", and has", ", then has");
}

/User faints,/ {
    gsub(", and", ", then");
}


/^Ghost.*Others/ {
    gsub("Ghosts","If user is Ghost-type,");
    gsub("Others","If user is not Ghost-type,");
}

/^User and target swap/ {
    sub(/User and target swap/, "User swaps");
    $0 = $0 " with target";

}

/^User swaps.*and/ {
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

    # 
    # ptrn = statPattern"(,)?";
    # gsub(" ","-",ptrn); # Special Attack -> Special-Attack
    # linecopy = $0;
    # gsub(" Special "," Special-", linecopy);


    # Extract the pattern from the line, replace the pattern
    # in a line's copy with a general marker.
    # linecopy = $0;
    # sp = statPattern", "statPattern", and "statPattern;
    # gsub(sp, "<stat>", linecopy);
    # gsub(/\.  .*/, "", linecopy); # if a comma and then something, ignore it.
    # patterncopy = stats;
    # gsub(",", "", patterncopy); # Remove colons
    # gsub("Special ", "Special-", patterncopy); # Easier iteration next
    # gsub(" and ", " ", patterncopy); # Remove and
    
    # split(patterncopy, patterns, " ");
    # for(item in patterns){
	# tempvar = linecopy;
	# sub("-", " ", patterns[item]); # Special-Attack -> Special Attack again
	# sub("<stat>", patterns[item], tempvar);
	# smartprint(tempvar);
    # }
    splitListed($0,statPattern", "statPattern", and "statPattern,stats,"\\.  .*");
    
    
    }

match($0,/(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+/) {
    andthese = substr($0,RSTART,RLENGTH);
}



/(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+.*\./{
    # print "@@@@@@@" $0;

    # 
    # ptrn = statPattern"(,)?";
    # gsub(" ","-",ptrn); # Special Attack -> Special-Attack
    # linecopy = $0;
    # gsub(" Special "," Special-", linecopy);


    # Extract the pattern from the line, replace the pattern
    # in a line's copy with a general marker.
    splitListed($0,"(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+",andThese,"\\.  .*");
    
    
    }

/\..*(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+/ {

    splitListed($0,"(Special )?[A-Z][a-z]+ and (Special )?[A-Z][a-z]+",andThese,".*\\.  ");
}

/\.  [A-Z]/{
    split($0, result, "\\.");
    gsub(/  /, "", result[2]);
    smartprint(result[1]);
    smartprint(result[2]);
}

function capitalize(x){
    return toupper( substr(x, 1, 1 ) ) substr(x, 2 );
}


function andNewLine(x, keyword, result){
    split(x, result, " and ");
    smartprint(result[1]);
    sub(keyword,capitalize(keyword),result1[2]);
    smartprint(result[2]);
    return result[1];

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
    gsub("^.*, and a", "A");
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

$0 !~ /( and |\.  )/{
    normal = $0;
    fighting = $0;
    gsub("Dark","Ghost",normal);
    gsub("Dark","Ghost",fighting);
    gsub("Psychic","Normal",normal);
    gsub("Psychic","Fighting",fighting);

    smartprint($0);
    smartprint(normal);
    smartprint(fighting);
}

/Prevents.*(fleeing|leaving).*inflicts.*damage/ {
    gsub(" and inflicts", " while at same time inflicts");
    print;
}

/ [a-z]+ and [a-z]+ /{
    count1 += 1;
    print ">" $0;

    
}


$0 ~ /, and/{
    # print;
}
