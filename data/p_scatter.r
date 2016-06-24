#
# For each system pair
#   - compute t-test pval for rho == 1 (A)
#   - compute t-test pval for rho==* (B)
# Plot A against B
#
# Andrew Turpin
# Thu 23 Jun 2016 10:53:16 AEST
#

d <- read.csv('all_scores.csv')

systems <- unique(d$system)
rhos <- unique(d$rho)
metrics <- c("RR", "RBP 0.5", "RBP 0.85", "AP")

########################################################################
# Given 50 scores sorted by topic in col1, col2, 
# return the triple on difference (col1 - col2) 
# (median, lo 95%, hi 95%)
########################################################################
calc <- function(col1, col2) {
    return(t.test(col1, col2, pair=TRUE)$p.value)
}

###########################################################################
#### Extract data
###########################################################################
###    # stats - dim 1: index into systems
###    # stats - dim 2: index into systems
###    # stats - dim 3: index into rhos
###    # stats - dim 4: metric "RR", "RBP05", "RBP085", "AP"
###    # stats - dim 5: pval of t-test
###stats <- array(NA, dim=c(length(systems), length(systems), length(rhos), 4, 1))
###
###for (i_rho in 1:length(rhos))
###for (i_sys1 in 1:length(systems)) {
###    z1_1 <- d$system == systems[i_sys1] & d$rho == 1
###    z1_2 <- d$system == systems[i_sys1] & d$rho == rhos[i_rho]
###    for (i_sys2 in 1:length(systems)) {
###        if (i_sys2 <= i_sys1)
###            next
###
###        z2 <- d$system == systems[i_sys2] & d$rho == 1
###        for (i_metric in 1:length(metrics)) {
###            col <- 3 + i_metric
###            stats[i_sys1, i_sys2, 1, i_metric, ] <- calc(d[z1_1, col], d[z2, col])
###        }
###
###        z2 <- d$system == systems[i_sys2] & d$rho == rhos[i_rho]
###        for (i_metric in 1:length(metrics)) {
###            col <- 3 + i_metric
###            stats[i_sys1, i_sys2, i_rho, i_metric, ] <- calc(d[z1_2, col], d[z2, col])
###        }
###    }
###}
###save(stats, metrics, rhos, systems, file="p_scatter.Rdata")
load(file="p_scatter.Rdata")

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

pdf("../figs/p_value_scatter_sys_pairs.pdf")
options(error=dev.off)
layout(matrix(1:4,2,2, byrow=TRUE))
par(mgp=c(2,1,0))
par(mar=c(3,3,0,0)+0.2)

for (i_rho in 1:length(rhos)) 
for (i_metric in 1:length(metrics)) {
    ps1 <- extract(stats[,,1,i_metric,1])
    ps2 <- extract(stats[,,i_rho,i_metric,1])

    plot(ps1, ps2, xlim=c(0,1), ylim=c(0, 1), las=1,
        xlab="p-value of t-test on original",
        ylab=expression(paste("p-value of t-test (",rho==.(rhos[i_rho]))),
        pch=19, col=rgb(1,0,0,0.1)
    )
    abline(v=0.05, h=0.05, lty=3)

    text(0, 0.9, metrics[i_metric], pos=4)

    tl <- sum(ps1<=0.05 & ps2>0.05)
    tr <- sum(ps1 >0.05 & ps2>0.05)
    bl <- sum(ps1<=0.05 & ps2<=0.05)
    br <- sum(ps1 >0.05 & ps2<=0.05)

    text(0.80, 0.2, paste0(round(tl/length(ps1)*100,1), "%"), pos=2)
    text(0.80, 0.1, paste0(round(bl/length(ps1)*100,1), "%"), pos=2)
    text(1.00, 0.2, paste0(round(tr/length(ps1)*100,1), "%"), pos=2)
    text(1.00, 0.1, paste0(round(br/length(ps1)*100,1), "%"), pos=2)
    segments(c(0.6, 0.8, 1.0), rep(0.05, 3), c(0.6, 0.8, 1.0), rep(0.25, 3), lty=c(1,3,1))
    segments(rep(0.6, 3), c(0.05, 0.15, 0.25), rep(1.0, 3), c(0.05, 0.15, 0.25), lty=c(1,3,1))

    print(paste(sprintf("%4.1f %10s %3.0f (%6.2f%%)",rhos[i_rho], metrics[i_metric], x<-sum(ps1<=0.05 & ps2>0.05), x/length(ps1)*100)))
}

    ### plot the four corner percentages varying the cut-off
cut_offs <- seq(0.05/length(ps1), 0.05, length=10)

par(mar=c(3,3,3,4)+0.2)
par(mgp=c(3,1,0))

for (i_metric in 1:length(metrics)) {
    ps1 <- extract(stats[,,1,i_metric,1])
    ps2 <- extract(stats[,,11,i_metric,1])

    tls <- sapply(cut_offs, function(co) sum(ps1<=co & ps2>co)/length(ps1)*100)
    trs <- sapply(cut_offs, function(co) sum(ps1> co & ps2>co)/length(ps1)*100)
    bls <- sapply(cut_offs, function(co) sum(ps1<=co & ps2<=co)/length(ps1)*100)
    brs <- sapply(cut_offs, function(co) sum(ps1> co & ps2<=co)/length(ps1)*100)

    m <- cbind(tls*50, trs, bls, brs*50)
    matplot(cut_offs, m, type="b", las=1, 
        xlab="Cut off p-value", 
        ylab="% in bottom-left and top-right corner",
        main="% agreement of 'significance' using various p-value cut-offs
    (2 and 3 left scale, 1 and 4 right scale)"
    )
    axis(4, label=(1:8)*10/50, at=1:8*10, las=1)
    mtext("% in top-left and bottom-right corner", 4, 3)

    segments(c(4, 4.5, 5)/100, rep(70, 3), c(4, 4.5, 5)/100, rep(80, 3), lty=c(1,3,1))
    segments(rep(4/100, 3), c(70,75,80), rep(5/100, 3), c(70,75,80), lty=c(1,3,1))
    text((4+4.5)/2/100, (75+80)/2, "1", col="black") # tl
    text((4.5+5)/2/100, (75+80)/2, "2", col="red") # tr
    text((4+4.5)/2/100, (70+75)/2, "3", col="green") # bl
    text((4.5+5)/2/100, (70+75)/2, "4", col="blue") # br
}
dev.off()
options(error=NULL)