#
#	Rbit.R
#Fri Aug  7 21:26:19 CEST 2009


bitSize = 32;

# bf: integer vector to hold the bit field
# n: starting bit (0, 1, ...)
# l: length
BFunpack = function(bf, n, l) {
	i0 = n %/% bitSize;
	b0 = (n %% bitSize); 
	# part 0
	p0 = (bf[i0] %/% 2^b0) %% (2^(l - b0));
	# part 1
	p1 = ifelse (l <= b0, 0, bf[i0 + 1] %% 2^(l - b0));
	# result
	r = p0 + p1 * 2^b0;
	r
}

# v: value to pack
BFpack = function(bf, n, l, v) {
	i0 = n %/% bitSize;
	b0 = (n %% bitSize); 
	# one element of bf affected
	if (b0 + l < bitSize) {
		bf[i0] = bf[i0] + ((v - (bf[i0] %/% b0)) %% 2^l) * 2^b0;
	} else {
		bf[i0] = (v %% 2^(l - b0)) * 2^b0 + bf[i0] %% 2^b0;
		bf[i0 + 1] = bf[i0 + 1] + (v %/% 2^(l - b0)) - bf[i0] %% 2^(l - b0);
	}
	bf
}
