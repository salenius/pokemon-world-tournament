BEGIN {
    this_and_that = VAR" and "VAR;
}

match($0,this_and_that) {
    stats = substr($0,RSTART,RLENGTH);
}

$2 ~ this_and_that {
    split(stats, dict, "and");
    for(item in dict){
        # # Create a copy so that the whole line isn't modified and loop then would fail
	# # to insert a new stat after the 1st iteration
	copy1 = $0; 
	sub(this_and_that, dict[item], copy1); # Replace 3 stats with each one individually
	print copy1; # Now instead of 1 line with 3 stats, there are 3 lines with 1 stat each
    }
}

$2 !~ this_and_that {
    print;
}

