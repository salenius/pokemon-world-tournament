function capitalize(x){
    return toupper( substr(x, 1, 1 ) ) substr(x, 2 );
}

/ doll, / {gsub("it","doll"); print}

$0 !~ / doll / {
    userOrTarget = "";
    itEncountered = 0;

    text = $2;
    new2 = "";
    split(text, words, " ");
    for (item in words){
	w = words[item];
	wcopy = capitalize(w);
	sub("'s","",wcopy);
	if (wcopy == "User" || wcopy == "Target"){
	    userOrTarget = w;
	    sub("'s","",userOrTarget);
	}
	if ((wcopy == "It" || wcopy == "Its") && userOrTarget != ""){
	    w = userOrTarget;
	    if (wcopy == "Its") {
		w = w"'s";
	    }
	}
	new2 = new2" "w;
    }
    $2 = new2;
    sub(" ", "", $2);
    print $1 FS $2;
    userOrTarget = "";
    itEncountered = 0;

}

