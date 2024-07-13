BEGIN {
    three_stats = STATS", "STATS", and "STATS;
}

match($0,three_stats) {
    stats = substr($0,RSTART,RLENGTH);
}

$2 ~ three_stats {
    sub("and", "", stats);
    sub(", ",",",stats);
    split(stats, dict, ",");
    for(item in dict){
        # # Create a copy so that the whole line isn't modified and loop then would fail
	# # to insert a new stat after the 1st iteration
	copy1 = $0; 
	sub("  ", "",dict[item]); # Get rid of double space
	sub(three_stats, dict[item], copy1); # Replace 3 stats with each one individually
	sub("@3 stats@","@no stats@",copy1);
	print copy1; # Now instead of 1 line with 3 stats, there are 3 lines with 1 stat each
    }
}

$2 !~ three_stats {
    print;
}

