BEGIN {
	FS = " "
}
{
	sys = $1
	topic = $2
	systop = sys "-" topic
	rank = $3*1.0
	score = $4*1.0
	# docnum = $3

	# first row for each system and each topic gets free pass
	if (sys != oldsys) {
		first = 1
		nsys ++
	}
	if (systop != oldsystop) {
		nsystop++
	}
	if (topic != oldtopic) {
		first = 1
	}
	if (first) {
		first = 0
		oldsys = sys
		oldsystop = systop
		oldtopic = topic
		oldscore = score
		oldrank = rank
		next
	}

	# by now now we know it isn't the first row for a topic (or system)
	if (oldscore == score) {
		# got a tie on scores
		scoredocties++
		if (systop != lasttiesystop) {
			systopdocties++
			lasttiesystop = systop
		}
		if (sys != lasttiesys) {
			sysdocties++
			lasttiesys = sys
		}
	} else if (oldscore < score) {
		scoreviols++
		print
		if (systop != lastsystopscoviol) {
			systopscoviols++
			lastsystopscoviol = systop
		}
		if (sys != lastsysscoviol) {
			sysscoviols++
			lastsysscoviol = sys
		}
	}
	# now look at the ranks
	if (oldrank == rank) {
		rankdocties++
	} else if (oldrank > rank) {
		rankviols++
	}
	# finally, look for contradictions
	if (oldscore<score && oldrank<rank ||
	    oldscore>score && oldrank>rank) {
		contradictions++
		if (sys != lastsyscon) {
			# print sys", "topic", "rank", "docnum
			syscons++
			lastsyscon = sys
		}
		if (systop != lastsystopcon) {
			systopcons++
			lastsystopcon = systop
		}
	}
	oldscore = score
	oldrank = rank
}
END {
	printf "scoredocties    = %6d = %6.2f%%\n", \
		scoredocties, scoredocties*100.0/NR
	printf "systopdocties   = %6d = %6.2f%%\n", \
		systopdocties, systopdocties*100.0/nsystop
	printf "sysdocties      = %6d = %6.2f%%\n", \
		sysdocties, sysdocties*100.0/nsys
	printf "\n"
	printf "scoreviols      = %6d = %6.2f%%\n", \
		scoreviols,   scoreviols  *100.0/NR
	printf "systopscoviols  = %6d = %6.2f%%\n", \
		systopscoviols, systopscoviols*100.0/nsystop
	printf "sysscoviols     = %6d = %6.2f%%\n", \
		sysscoviols, sysscoviols*100.0/nsys
	printf "\n"
	printf "rankdocties     = %6d = %6.2f%%\n",
		rankdocties, rankdocties*100.0/NR
	printf "rankviols       = %6d = %6.2f%%\n",
		rankviols  , rankviols  *100.0/NR
	printf "\n"
	printf "contradictions  = %6d = %6.2f%%\n",
		contradictions  , contradictions  *100.0/NR
	printf "systopcons      = %6d = %6.2f%%\n",
		systopcons, systopcons*100.0/nsystop
	printf "syscons         = %6d = %6.2f%%\n",
		syscons, syscons*100.0/nsys
		
}

