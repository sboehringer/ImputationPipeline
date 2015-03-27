#
#	RlabParallel.R
#Tue Aug 14 11:33:41 2012

parallel5 = function(e) {
	stop('not to be called during probe');
}
parallel4 = function(e) {
	l = rep(e, 15);
	Lapply(l, parallel5);
}
parallel3 = function(e) {
	l = rep(e, 10);
	Lapply(l, parallel4);
}
parallel0.1 = function() {
	r = Lapply(1:50, parallel3);
	Log("about to call parallel4");
	Lapply(1:49, parallel4);
	r
}

parallel2 = function(e) { stop('not to be called during probe'); }
parallel1 = function(e) { l = rep(e, 10); Lapply(l, parallel2); }
parallel0 = function() { Lapply(1:50, parallel1); }

parallel9 = function(e) { log(1:e) %*% log(1:e); }
#parallel8 = function(e) { Lapply(1:13, parallel9) }
parallel8 = function(e) { log(1:e) %*% log(1:e); }
parallel7 = function(e) { rep(e, e) %*% 1:e * 1:e; }
#parallel6 = function(e) { l = rep(e, 15); Lapply(l, parallel7); }
parallel6 = function(e) { Lapply(rep(e, 15), parallel7); }
parallel0.2 = function() {
	r = sapply(Lapply(1:50, parallel6), function(e){sum(as.vector(unlist(e)))});
	#r = Lapply(1:50, parallel6);
	r0 = sapply(Lapply(1:49, parallel8), as.vector);
	#r = r + sum(r0);
	r
}
