# This script generate trend plots for SMTP data

# Read in the trend data
load("SmtpStats.RData")

# Get rid of the alpine data
SmtpBiogeoStats <- SmtpBiogeoStats[ SmtpBiogeoStats$Region != "alpine" & SmtpBiogeoStats$Region!="", ]
SmtpBiogeoStats$Region <- factor( SmtpBiogeoStats$Region )

# Preparatory steps
barNames <- c( "Nemoral", "Boreonem", "Boreal" )
regionOrder <- c( "nemoral", "boreonemoral", "boreal" )
indexVec <- match( SmtpBiogeoStats$Region, regionOrder ) 

# Generate figure for compositional trends
pdf( "Figure_9.pdf" )
par( mfrow=c(2,2) )
nameSize <- 0.8

# Generate plots for taxon composition

ypos <- 0.95
ylim <- c(0.0,0.65)
barplot( SmtpBiogeoStats$PropDiptera[indexVec], ylim=ylim, col="blue", cex.names=nameSize, names.arg=barNames, main="Diptera", ylab="Fraction" )
text( 0.2, ypos*ylim[2], "p=0.0295", pos=4 )
mtext( "(A)", side=1, line=3, adj=0, cex=1.2 )

ylim <- c(0.0,0.43)
barplot( SmtpBiogeoStats$PropHymenoptera[indexVec], ylim=ylim, col="blue", cex.names=nameSize, names.arg=barNames, main="Hymenoptera", ylab="Fraction" )
text( 2.5, ypos*ylim[2], "p=0.1858", pos=4 )
mtext( "(B)", side=1, line=3, adj=0, cex=1.2 )

# Generate plots for niche composition

ylim <- c(0.0,0.55)
barplot( SmtpBiogeoStats$PropSaprophagous[indexVec], ylim=ylim, col="blue", cex.names=nameSize, names.arg=barNames, main="Decomposers", ylab="Fraction" )
text( 0.2, ypos*ylim[2], "p=0.0469", pos=4 )
mtext( "(C)", side=1, line=3, adj=0, cex=1.2 )

ylim <- c(0.0,0.31)
barplot( SmtpBiogeoStats$PropParasitoids[indexVec], ylim=ylim, col="blue", cex.names=nameSize, names.arg=barNames, main="Parasitoids", ylab="Fraction" )
text( 2.5, ypos*ylim[2], "p=0.7471", pos=4 )
mtext( "(D)", side=1, line=3, adj=0, cex=1.2 )

dev.off()

