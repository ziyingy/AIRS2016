#
# For each system pair
#   - compute median and IQR range of differences when rho==1 (A)
#   - compute median and IQR range of differences when rho==2 (B)
# Plot B in order of A
#
# Andrew Turpin
# Thu 23 Jun 2016 06:11:13 AEST
#

d <- read.csv('all_scores.csv')

systems <- unique(d$system)
topics <- unique(d$topics)
metrics <- c("RR", "RBP05", "RBP085", "AP")

########################################################################
# Given 50 scores sorted by topic in col1, col2, 
# return the triple on difference (col1 - col2) 
# (median, lo 95%, hi 95%)
########################################################################
calc <- function(col1, col2) {
    delta <- col1 - col2
    #qs <- quantile(delta, probs=c(0.5, 0.025, 0.975))
    qs <- quantile(delta, probs=c(0.5, 0.25, 0.75))
    return(qs)
}

########################################################################
# Extract data
########################################################################
    # stats - dim 3: rho 1 or 2
    # stats - dim 4: metric "RR", "RBP05", "RBP085", "AP"
    # stats - dim 5: 1 = median, 2 = lo 95%, 3 = hi 95%
stats <- array(NA, dim=c(length(systems), length(systems), 2, 4, 3))

for (i_sys1 in 1:length(systems)) {
    z1_1 <- d$system == systems[i_sys1] & d$rho == 1
    z1_2 <- d$system == systems[i_sys1] & d$rho == 2
    for (i_sys2 in 1:length(systems)) {
        if (i_sys2 <= i_sys1)
            next

        z2 <- d$system == systems[i_sys2] & d$rho == 1
        for (i_metric in 1:length(metrics)) {
            col <- 3 + i_metric
            stats[i_sys1, i_sys2, 1, i_metric, ] <- calc(d[z1_1, col], d[z2, col])
        }

        z2 <- d$system == systems[i_sys2] & d$rho == 2
        for (i_metric in 1:length(metrics)) {
            col <- 3 + i_metric
            stats[i_sys1, i_sys2, 2, i_metric, ] <- calc(d[z1_2, col], d[z2, col])
        }
    }
}

########################################################################
# plot
########################################################################
extract <- function(m) {
    dim(m) <- NULL  # flatten
    z <- is.na(m)   # remove NAs
    return(m[!z])
}

shade_col <- grey(0.7)
old_col <- grey(0.6)
new_col <- grey(0.2)

pdf("../figs/diffs_sorted_by_orig.pdf")
options(error=dev.off)
layout(matrix(1:4,2,2))
par(mgp=c(2,1,0))
par(mar=c(3,3,0,0)+0.2)

for (i_metric in 1:length(metrics)) {
    m.o  <- extract(stats[,,1,i_metric,1])
    lo.o <- extract(stats[,,1,i_metric,2])
    hi.o <- extract(stats[,,1,i_metric,3])

    m  <-   extract(stats[,,2,i_metric,1])
    lo <-   extract(stats[,,2,i_metric,2])
    hi <-   extract(stats[,,2,i_metric,3])

    o <- order(m.o)
    plot(m[o], type="n", ylim=c(-1, 1), las=1,
        xlab=ifelse(i_metric == 2 || i_metric == 4, "System Pair", ""),
        ylab=ifelse(i_metric <= 2, expression(paste("Metric difference ",rho==1," less ", rho==2)), "")
    )

    #polygon(c(1:length(lo), length(hi):1), c(lo[o], rev(hi[o])), col=new_col, border=NA)
    #polygon(c(1:length(lo), length(hi):1), c(lo.o[o], rev(hi.o[o])), col=old_col, border=NA)
    segments(1:length(lo.o), lo.o[o], 1:length(hi.o), hi.o[o], col=old_col)

    points(m[o], type="p", pch=19, cex=0.5, col=new_col)

    lines(m.o[o], type="l", pch=19, cex=0.5, col=old_col)
    abline(h=0, lty=3)

    text(0, 0.9, metrics[i_metric], pos=4)
}

dev.off()
options(error=NULL)
