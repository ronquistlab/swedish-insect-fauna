# This script generates plots showing the accuracy of different
# species estimators using well known groups and species known
# 2002/2003 of poorly known groups. Well known group is defined
# in this context as a group where no new species have been
# found in the SMTP inventory.

# We focus the analysis here on Chao1, Chao2, CNE and Preston
# estimators

# First read in a function for plotting squared error of the log
# estimates against different parameters
source( "sqdevPlotFxn.R" )

# Read data from other scripts
D <- read.table("quantRichnessEstimates-known2003.csv",header=TRUE,sep=";")
E <- read.table("quantRichnessEstimatesKnownGroups.csv",header=TRUE,sep=";")

# Add the squared deviation on the absolute and log scales.
# We use known spp 2002/2003 as ground truth for the poorly
# known groups, where we only look at this subset of species.
# For the well known groups, we use the expert guess 2017 as
# ground truth.

D$chao1_sqdev <- (D$chao1 - D$numSp2003)^2
D$chao2_sqdev <- (D$chao2 - D$numSp2003)^2
D$cne_sqdev <- (D$cne - D$numSp2003)^2
D$preston_sqdev <- (D$preston - D$numSp2003)^2
D$chao1p_sqdev <- (D$chao1p - D$numSp2003)^2
D$chao2p_sqdev <- (D$chao2p - D$numSp2003)^2
D$jack1p_sqdev <- (D$jack1p - D$numSp2003)^2
D$jack2p_sqdev <- (D$jack2p - D$numSp2003)^2

D$chao1_sqdev_log <- (log(D$chao1) - log(D$numSp2003))^2
D$chao2_sqdev_log <- (log(D$chao2) - log(D$numSp2003))^2
D$cne_sqdev_log <- (log(D$cne) - log(D$numSp2003))^2
D$preston_sqdev_log <- (log(D$preston) - log(D$numSp2003))^2
D$chao1p_sqdev_log <- (log(D$chao1p) - log(D$numSp2003))^2
D$chao2p_sqdev_log <- (log(D$chao2p) - log(D$numSp2003))^2
D$jack1p_sqdev_log <- (log(D$jack1p) - log(D$numSp2003))^2
D$jack2p_sqdev_log <- (log(D$jack2p) - log(D$numSp2003))^2

E$chao1_sqdev <- (E$chao1 - E$expert2017)^2
E$chao2_sqdev <- (E$chao2 - E$expert2017)^2
E$cne_sqdev <- (E$cne - E$expert2017)^2
E$preston_sqdev <- (E$preston - E$expert2017)^2
E$chao1p_sqdev <- (E$chao1p - E$expert2017)^2
E$chao2p_sqdev <- (E$chao2p - E$expert2017)^2
E$jack1p_sqdev <- (E$jack1p - E$expert2017)^2
E$jack2p_sqdev <- (E$jack2p - E$expert2017)^2

E$chao1_sqdev_log <- (log(E$chao1) - log(E$expert2017))^2
E$chao2_sqdev_log <- (log(E$chao2) - log(E$expert2017))^2
E$cne_sqdev_log <- (log(E$cne) - log(E$expert2017))^2
E$preston_sqdev_log <- (log(E$preston) - log(E$expert2017))^2
E$chao1p_sqdev_log <- (log(E$chao1p) - log(E$expert2017))^2
E$chao2p_sqdev_log <- (log(E$chao2p) - log(E$expert2017))^2
E$jack1p_sqdev_log <- (log(E$jack1p) - log(E$expert2017))^2
E$jack2p_sqdev_log <- (log(E$jack2p) - log(E$expert2017))^2

# Add the absolute deviation on absolute and log scales.
D$chao1_dev <- (D$chao1 - D$numSp2003)
D$chao2_dev <- (D$chao2 - D$numSp2003)
D$cne_dev <- (D$cne - D$numSp2003)
D$preston_dev <- (D$preston - D$numSp2003)
D$chao1p_dev <- (D$chao1p - D$numSp2003)
D$chao2p_dev <- (D$chao2p - D$numSp2003)
D$jack1p_dev <- (D$jack1p - D$numSp2003)
D$jack2p_dev <- (D$jack2p - D$numSp2003)

D$chao1_dev_log <- (log(D$chao1) - log(D$numSp2003))
D$chao2_dev_log <- (log(D$chao2) - log(D$numSp2003))
D$cne_dev_log <- (log(D$cne) - log(D$numSp2003))
D$preston_dev_log <- (log(D$preston) - log(D$numSp2003))
D$chao1p_dev_log <- (log(D$chao1p) - log(D$numSp2003))
D$chao2p_dev_log <- (log(D$chao2p) - log(D$numSp2003))
D$jack1p_dev_log <- (log(D$jack1p) - log(D$numSp2003))
D$jack2p_dev_log <- (log(D$jack2p) - log(D$numSp2003))

E$chao1_dev <- (E$chao1 - E$expert2017)
E$chao2_dev <- (E$chao2 - E$expert2017)
E$cne_dev <- (E$cne - E$expert2017)
E$preston_dev <- (E$preston - E$expert2017)
E$chao1p_dev <- (E$chao1p - E$expert2017)
E$chao2p_dev <- (E$chao2p - E$expert2017)
E$jack1p_dev <- (E$jack1p - E$expert2017)
E$jack2p_dev <- (E$jack2p - E$expert2017)

E$chao1_dev_log <- (log(E$chao1) - log(E$expert2017))
E$chao2_dev_log <- (log(E$chao2) - log(E$expert2017))
E$cne_dev_log <- (log(E$cne) - log(E$expert2017))
E$preston_dev_log <- (log(E$preston) - log(E$expert2017))
E$chao1p_dev_log <- (log(E$chao1p) - log(E$expert2017))
E$chao2p_dev_log <- (log(E$chao2p) - log(E$expert2017))
E$jack1p_dev_log <- (log(E$jack1p) - log(E$expert2017))
E$jack2p_dev_log <- (log(E$jack2p) - log(E$expert2017))

D100.5 <- D[ D$numSpec > 100 & D$numTraps > 5, ]
E100.5 <- E[ E$numSpec > 100 & E$numTraps > 5, ]

pdf("Figure_3.pdf", useDingbats=FALSE)
par( mfrow=c(2,2) )

legendTitle <- "Estimator"
estimators <- c("Chao1", "Chao2", "Preston", "CNE")
plotcols <- c("blue", "green", "red", "black" )
plotsymbols <- c(0, 1, 2, 3)
yrange <- c(0.0,7.0)
legendSize <- 0.6

x <- c(log(D100.5$numSpec), log(E100.5$numSpec))
xrange <- range(x)
sqdevPlot( D100.5, E100.5, log(D100.5$numSpec), log(E100.5$numSpec), xlab="Number of specimens (log)", ylab="Squared error", xlim=xrange, ylim=yrange)
legend( 0.8*(xrange[2]-xrange[1])+xrange[1], yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(A)", side=1, line=3, adj=0, cex=1.2)

x <- c(D100.5$numTraps, E100.5$numTraps)
xrange <- range(x)
sqdevPlot( D100.5, E100.5, D100.5$numTraps, E100.5$numTraps, xlab="Number of sites", ylab="Squared error", xlim=xrange, ylim=yrange)
legend( 0.8*(xrange[2]-xrange[1])+xrange[1], yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(B)", side=1, line=3, adj=0, cex=1.2)

x <- c(D100.5$numSpec/D100.5$numTraps, E100.5$numSpec/E100.5$numTraps)
xrange <- range(x)
sqdevPlot( D100.5, E100.5, D100.5$numSpec/D100.5$numTraps, E100.5$numSpec/E100.5$numTraps, xlab="Specimens per site", ylab="Squared error", xlim=xrange, ylim=yrange)
legend( 0.8*(xrange[2]-xrange[1])+xrange[1], yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(C)", side=1, line=3, adj=0, cex=1.2)

x <- c(D100.5$numSpSmtp/D100.5$numSp2003, E100.5$numSpSmtp/E100.5$expert2017)
xrange <- range(x)
sqdevPlot( D100.5, E100.5, D100.5$numSpSmtp/D100.5$numSp2003, E100.5$numSpSmtp/E100.5$expert2017, xlab="Proportion of species sampled", ylab="Squared error", xlim=xrange, ylim=yrange)
legend( 0.8*(xrange[2]-xrange[1])+xrange[1], yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
text( 0.81, 1.1, "Adelognathinae", cex=0.8, pos=3 )
mtext( "(D)", side=1, line=3, adj=0, cex=1.2)

dev.off()

pdf("Figure_4.pdf", useDingbats=FALSE)
par( mfrow=c(2,2) )

y <- c(D100.5$chao1_dev_log, E100.5$chao1_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$chao1_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Chao1")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$chao1_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(A)", side=1, line=3, adj=0, cex=1.2)

y <- c(D100.5$chao2_dev_log, E100.5$chao2_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$chao2_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Chao2")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$chao2_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(B)", side=1, line=3, adj=0, cex=1.2)

D100.5x <- D100.5[ D100.5$group != "Milichiidae excl Phyllomyza" & !is.na(D100.5$preston), ]
E100.5x <- E100.5[ !is.na(E100.5$preston), ]
y <- c(D100.5x$preston_dev_log, E100.5x$preston_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5x$numSpSmtp/D100.5x$numSp2003, D100.5x$preston_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Preston")
points( E100.5x$numSpSmtp/E100.5x$expert2017, E100.5x$preston_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(C)", side=1, line=3, adj=0, cex=1.2)

y <- c(D100.5$cne_dev_log, E100.5$cne_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$cne_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="CNE")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$cne_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(D)", side=1, line=3, adj=0, cex=1.2)

dev.off()

pdf("S1_Figure.pdf", useDingbats=FALSE)
par( mfrow=c(2,2) )

yrange <- c(0.0,7.0)

x <- c(D100.5$S1/D100.5$numSpSmtp, E100.5$S1/E100.5$numSpSmtp)
xrange <- rev( range(x) )
sqdevPlot( D100.5, E100.5, D100.5$S1/D100.5$numSpSmtp, E100.5$S1/E100.5$numSpSmtp, xlab="Proportion of singletons (S1)", ylab="Squared error", xlim=xrange, ylim=yrange)
legend( xrange[1] - 0.8*(xrange[1]-xrange[2]), yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(A)", side=1, line=3, adj=0, cex=1.2)

x <- c(D100.5$Q1/D100.5$numSpSmtp, E100.5$Q1/E100.5$numSpSmtp)
xrange <- rev( range(x) )
sqdevPlot( D100.5, E100.5, D100.5$Q1/D100.5$numSpSmtp, E100.5$Q1/E100.5$numSpSmtp, xlab="Proportion of uniques (Q1)", ylab="Squared error", xlim=xrange, ylim=yrange)
legend( xrange[1] - 0.8*(xrange[1]-xrange[2]), yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(B)", side=1, line=3, adj=0, cex=1.2)

x <- c(D100.5$numSpec/D100.5$numSpSmtp, E100.5$numSpec/E100.5$numSpSmtp)
xrange <- range(x)
sqdevPlot( D100.5, E100.5, D100.5$numSpec/D100.5$numSpSmtp, E100.5$numSpec/E100.5$numSpSmtp, xlab="Specimens per species", ylab="Squared error", xlim=xrange, ylim=yrange, log="x")
legend( 0.4*(xrange[2]-xrange[1]) + xrange[1], yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(C)", side=1, line=3, adj=0, cex=1.2)

x <- c(D100.5$numSiteObs/D100.5$numSpSmtp, E100.5$numSiteObs/E100.5$numSpSmtp)
xrange <- range(x)
sqdevPlot( D100.5, E100.5, D100.5$numSiteObs/D100.5$numSpSmtp, E100.5$numSiteObs/E100.5$numSpSmtp, xlab="Site observations per species", ylab="Squared error", xlim=xrange, ylim=yrange, log="x")
legend( 0.48*(xrange[2]-xrange[1]) + xrange[1], yrange[2], estimators, cex=legendSize, col=plotcols, pch=plotsymbols, title=legendTitle )
mtext( "(D)", side=1, line=3, adj=0, cex=1.2)

dev.off()

pdf("S2_Figure.pdf", useDingbats=FALSE)
par( mfrow=c(2,2) )

y <- c(D100.5$chao1p_dev_log, E100.5$chao1p_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$chao1p_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Chao1P")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$chao1p_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(A)", side=1, line=3, adj=0, cex=1.2)

y <- c(D100.5$chao2p_dev_log, E100.5$chao2p_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$chao2p_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Chao2P")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$chao2p_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(B)", side=1, line=3, adj=0, cex=1.2)

y <- c(D100.5$jack1p_dev_log, E100.5$jack1p_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$jack1p_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Jack1P")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$jack1p_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(C)", side=1, line=3, adj=0, cex=1.2)

y <- c(D100.5$jack2p_dev_log, E100.5$jack2p_dev_log)
yrange <- range(y)
yrange <- c(-2.5,1.0)
xrange <- c(0.0,1.0)
plot( D100.5$numSpSmtp/D100.5$numSp2003, D100.5$jack2p_dev_log, xlab="Proportion of species sampled", ylab="Bias", xlim=xrange, ylim=yrange, pch=19, main="Jack2P")
points( E100.5$numSpSmtp/E100.5$expert2017, E100.5$jack2p_dev_log, pch=19 )
abline( a=0.0, b=0.0 )
mtext( "(D)", side=1, line=3, adj=0, cex=1.2)

dev.off()


