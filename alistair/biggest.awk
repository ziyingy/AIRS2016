BEGIN {
	stopat = 100000
	ngroups = 40
	#
	rounder = 0.9999999
	if (rho==0) {
		printf "No rho parameter, use -v rho=x.x\n"
		exit
	}
	#
	if (rbpp==0) {
		rbpp = 0.5
	}
	# rbp weights
	w[1] = (1-rbpp)
	for (k=2; k<stopat; k++) {
		w[k] = w[k-1]*rbpp
	}

	# the main game now
	v = 1 + 1/(rho-1.0)
	# printf "v = %.3f\n",  v
	v = int(v)
	printf "v  = %d\n", v
	bv = v
	ev = int(bv*rho+rounder) - 1
	printf "bv = %d, ev = %d\n", bv, ev

	# reciprocal rank calculations
	bg = 1
	bigdiff = 0.0
	for (g=1; g<=ngroups; g++) {
		eg = int(rho*bg+rounder) - 1
		if (eg>=stopat) break
		diff = 1.0/bg
		for (k=bg; k<=eg; k++) {
			diff -= 1.0/k/(eg-bg+1)
		}
		if (diff>bigdiff) {
			bigdiff = diff
			bigdiffg = g
		}
		bg = eg+1
	}
	printf "RR:      g=%d, bigdiff = %.4f\n", bigdiffg, bigdiff

	# RBP rank calculations
	bg = 1
	diff = 0.0
	bigdiff = 0.0
	for (g=1; g<=ngroups; g++) {
		eg = int(rho*bg+rounder) - 1
		if (eg>=stopat) break
		rel = 0
		# what does half-relevant look like, in every group
		for (k=bg; k<=(bg+eg)/2; k++) {
			diff += 1.0*w[k]
			rel += 1
		}
		# now smear that amount out over that same group
		for (k=bg; k<=eg; k++) {
			diff -= 1.0*rel/(eg-bg+1)*w[k]
		}
		if (diff>bigdiff) {
			bigdiff = diff
			bigdiffg = g
		}
		bg = eg+1
	}
	printf "RBP%.2f: g=%d, bigdiff = %.4f\n", rbpp, bigdiffg, bigdiff
}
