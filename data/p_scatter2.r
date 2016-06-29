#
# Which systems are involved in the bottom right 
# corner of "../figs/p_value_scatter_sys_pairs.pdf"
# generated from p_scatter
#
# Andrew Turpin
# Fri 24 Jun 2016 18:16:31 AEST
#

d <- read.csv('all_scores_new.csv')

load(file="p_scatter.Rdata")
###    # stats - dim 1: index into systems
###    # stats - dim 2: index into systems
###    # stats - dim 3: index into rhos
###    # stats - dim 4: metric "RR", "RBP05", "RBP085", "AP"
###    # stats - dim 5: pval of t-test

extract <- function(m) {
    dim(m) <- NULL  # flatten
    z <- is.na(m)   # remove NAs
    return(m[!z])
}

#barplot((
#    d$AP[d$system=="uoftimgr" & d$rho == 1.0]-d$AP[d$system=="uoftimgr" & d$rho == 2.0],
#    d$AP[d$system=="uoftimgu" & d$rho == 1.0]-d$AP[d$system=="uoftimgu" & d$rho == 2.0]
#), beside=TRUE)

########################################################################
#
########################################################################
for (i_rho in 11)  # 1:length(rhos)) 
for (i_metric in 1:length(metrics)) {

    print(metrics[i_metric])
    for (i_sys1 in 1:length(systems)) {
        for (i_sys2 in 1:length(systems)) {
            ps1 <- extract(stats[,,1,i_metric,1])

            if (!is.na(stats[i_sys1, i_sys2, 1    ,i_metric,1]) && 
                !is.na(stats[i_sys1, i_sys2, i_rho,i_metric,1]) &&
                stats[i_sys1, i_sys2, 1    ,i_metric,1] <= 0.05 && 
                stats[i_sys1, i_sys2, i_rho,i_metric,1] > 0.05) {
                    z <- d$system == systems[i_sys1] & d$rho == 1
                    sys1_mean <- mean(d[z, 3 + i_metric])

                    z <- d$system == systems[i_sys2] & d$rho == 1
                    sys2_mean <- mean(d[z, 3 + i_metric])

                    z <- d$system == systems[i_sys1] & d$rho == rhos[i_rho]
                    sys1_mean_new <- mean(d[z, 3 + i_metric])

                    z <- d$system == systems[i_sys2] & d$rho == rhos[i_rho]
                    sys2_mean_new <- mean(d[z, 3 + i_metric])

                    print(sprintf("%15s %15s %6.4f %6.4f %+6.4f %5.3f %6.4f %6.4f %+6.4f %5.3f   %+6.4f %+5.3f",
                        systems[i_sys1], systems[i_sys2], 
                        sys1_mean, sys2_mean, 
                        d1 <- sys1_mean - sys2_mean,
                        p1 <- stats[i_sys1, i_sys2, 1    ,i_metric,1],
                        sys1_mean_new, sys2_mean_new, 
                        d2 <- sys1_mean_new - sys2_mean_new,
                        p2 <- stats[i_sys1, i_sys2, i_rho,i_metric,1],
                        d1 - d2,
                        p1 - p2
                    ))
                }
         }
     }
}
