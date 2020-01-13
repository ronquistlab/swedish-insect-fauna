# This script generates bar charts showing various aspects
# of the composition of the Swedish insect fauna

# Read table with compositional data
D <- read.table( "../misc_data/swedish_insect_fauna.tsv" )

# Add a factor that groups all orders except the five largest
bigFive <- c("Hymenoptera", "Diptera", "Coleoptera", "Lepidoptera", "Hemiptera")
x <- ""
for ( i in 1:length(D$Order) )
{
    if ( D$Order[i] %in% bigFive )
        x[i] <- as.character( D$Order[i] )
    else
        x[i] <- "Other"
}
D$OrderGroup <- factor(x)

# Simplify niche groupings and divide into primary and secondary
x <- ""
y <- ""
z <- ""
for ( i in 1:length(D$Main.feeding.niche) )
{
    if ( is.na( D$Main.feeding.niche[i] ) )
    {
        x[i] <- y[i] <- z[i] <- NA
        next
    }

    if ( D$Main.feeding.niche[i] == "Phytophage-parasitoid" || D$Main.feeding.niche[i] == "Phytophagous" )
        x[i] <- "Phytophagous"
    else if ( D$Main.feeding.niche[i] == "Saprophage-parasitoid" || D$Main.feeding.niche[i] == "Saprophagous" )
        x[i] <- "Saprophagous"
    else if ( D$Main.feeding.niche[i] == "Predator-parasitoid" || D$Main.feeding.niche[i] == "Predator" )
        x[i] <- "Predator"
    else
        x[i] <- "Other"

    if ( D$Main.feeding.niche[i] == "Phytophage-parasitoid" || D$Main.feeding.niche[i] == "Saprophage-parasitoid" || D$Main.feeding.niche[i] == "Predator-parasitoid"
        || D$Main.feeding.niche[i] == "General parasitoid" )
        y[i] <- "Secondary"
    else
        y[i] <- "Primary"

    if ( x[i] == "Other" )
        z[i] <- "Other"
    else
        z[i] <- as.character( D$Main.feeding.niche[i] )
}
D$NichePrimary <- factor(x)
D$NicheSecondary <- factor(y)
D$Niche <- factor(z)

# Simplify habitat groupings
x <- ""
for ( i in 1:length(D$Main.feeding.habitat) )
{
    if ( is.na( D$Main.feeding.habitat[i] ) )
    {
        x[i] <- NA
        next
    }

    if ( D$Main.feeding.habitat[i] == "Homeothermic animals" || D$Main.feeding.habitat[i] == "" )
        x[i] <- "Other"
    else if ( D$Main.feeding.habitat[i] == "Temporary habitats" )
        x[i] <- "Temporary"
    else
        x[i] <- as.character( D$Main.feeding.habitat[i] )
}
D$Habitat <- factor(x)

# Normalizing functions so that we can compare the compositional graphs more easily
f0 <- function(x) { sum(x) / sum(D$Corrected.values.2003) }
f1 <- function(x) { sum(x) / sum(D$Sweden.estimated.total) }
f2 <- function(x) { sum(x) / sum(D$Estimate.minus.known.2003) }

# Get numbers for orders. We only focus on the five large ones, grouping all others in one category.
orderPrevious <- aggregate( D$Corrected.values.2003,by=list(D$OrderGroup), FUN=f0)
orderEstimate <- aggregate(D$Sweden.estimated.total,by=list(D$OrderGroup), FUN=f1)
orderAdded <- aggregate(D$Estimate.minus.known.2003,by=list(D$OrderGroup), FUN=f2)

# Get numbers for niche
nichePrevious <- aggregate( D$Corrected.values.2003,by=list(D$Niche), FUN=f0)
nicheEstimate <- aggregate(D$Sweden.estimated.total,by=list(D$Niche), FUN=f1)
nicheAdded <- aggregate(D$Estimate.minus.known.2003,by=list(D$Niche), FUN=f2)

# Get numbers for habitat
habitatPrevious <- aggregate( D$Corrected.values.2003,by=list(D$Habitat), FUN=f0)
habitatEstimate <- aggregate(D$Sweden.estimated.total,by=list(D$Habitat), FUN=f1)
habitatAdded <- aggregate(D$Estimate.minus.known.2003,by=list(D$Habitat), FUN=f2)


# Now plot Figure 7
pdf("Figure_7.pdf")
par(mfrow=c(3,2))

nameSize <- 0.6
axisSize <- 0.8

# Plot taxonomic composition, ensuring the order is decreasing with respect to previously known fauna
orderEstimate <- orderEstimate[with(orderEstimate,order(-x)), ]
barNames <- c("Hym","Dip","Col","Lep","Hem", "Oth" )
orderAdded <- orderAdded[match(orderEstimate$Group.1,orderAdded$Group.1), ]
barplot( height=orderEstimate$x, main="Taxonomic composition, true fauna", ylab="Fraction", names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,0.45))
barplot( height=orderAdded$x, main="Taxonomic composition, new species", ylab="Fraction", names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,0.45))

# Plot niche composition in stacked bar chart
nicheNames <- c("Phytophagous", "Saprophagous", "Predator", "Other")
hyperNicheNames <- c("Phytophage-parasitoid", "Saprophage-parasitoid", "Predator-parasitoid")
x <- nicheEstimate$x[ match( nicheNames, nicheEstimate$Group.1 ) ]
y <- nicheEstimate$x[ match( hyperNicheNames, nicheEstimate$Group.1 ) ]
y[ 4 ] <- 0
nicheEstimateHeights <- matrix( c(x, y), nrow=2, ncol=4, byrow=TRUE )
x <- nicheAdded$x[ match( nicheNames, nicheAdded$Group.1 ) ]
y <- nicheAdded$x[ match( hyperNicheNames, nicheAdded$Group.1 ) ]
y[ 4 ] <- 0
nicheAddedHeights <- matrix( c(x, y), nrow=2, ncol=4, byrow=TRUE )
barplot( height=nicheEstimateHeights, main="Niche composition, true fauna", ylab="Fraction", names.arg=nicheNames, cex.names=nameSize, cex.axis=axisSize, col=c("blue","red"), beside=TRUE, ylim=c(0.0,0.35), legend.text=c("Primary member", "Parasitoid"), args.legend=list(bty="n", cex=0.9) )
barplot( height=nicheAddedHeights, main="Niche composition, new species", ylab="Fraction", names.arg=nicheNames, cex.names=nameSize, cex.axis=axisSize, col=c("blue","red"), beside=TRUE, ylim=c(0.0,0.35), legend.text=c("Primary member", "Parasitoid"), args.legend=list(bty="n", cex=0.9) )

# Plot habitat composition, ensuring the order is decreasing with respect to previously known fauna
habitatEstimate <- habitatEstimate[with(habitatEstimate,order(-x)), ]
habitatAdded <- habitatAdded[match(habitatEstimate$Group.1,habitatAdded$Group.1), ]
barplot( height=habitatEstimate$x, main="Habitat composition, true fauna", ylab="Fraction", names.arg=habitatEstimate$Group.1, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,0.6))
barplot( height=habitatAdded$x, main="Habitat composition, new species", ylab="Fraction", names.arg=habitatAdded$Group.1, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,0.6))


# Now plot Figure 8, showing historical trends in absolute numbers
pdf("Figure_8.pdf")
par( mfrow=c(3,4)  )

nameSize <- 0.5
axisSize <- 0.7

# Normalizing functions
f1 <- function(x) { sum(x) / sum(D$Linné..1761.) }
f2 <- function(x) { sum(x) / sum(D$Wahlgren...Tullgren..1922.) }
f3 <- function(x) { sum(x) / sum(D$Corrected.values.2003) }
f4 <- function(x) { sum(x) / sum(D$Sweden.estimated.total) }

# Get numbers for taxonomix composition
taxon1761 <- aggregate(D$Linné..1761.,by=list(D$OrderGroup), FUN=f1)
taxon1922 <- aggregate(D$Wahlgren...Tullgren..1922.,by=list(D$OrderGroup), FUN=f2)
taxon2003 <- aggregate(D$Corrected.values.2003,by=list(D$OrderGroup), FUN=f3)
taxonEstimate <- aggregate(D$Sweden.estimated.total,by=list(D$OrderGroup), FUN=f4)
maxTaxon <- max( taxon1761$x, taxon1922$x, taxon2003$x, taxonEstimate$x )

# Get numbers for niche
niche1761 <- aggregate(D$Linné..1761.,by=list(D$Niche), FUN=f1)
niche1922 <- aggregate(D$Wahlgren...Tullgren..1922.,by=list(D$Niche), FUN=f2)
niche2003 <- aggregate(D$Corrected.values.2003,by=list(D$Niche), FUN=f3)
nicheEstimate <- aggregate(D$Sweden.estimated.total,by=list(D$Niche), FUN=f4)
maxNiche <- max( niche1761$x, niche1922$x, niche2003$x, nicheEstimate$x )

# Get numbers for habitat
habitat1761 <- aggregate(D$Linné..1761.,by=list(D$Habitat), FUN=f1)
habitat1922 <- aggregate(D$Wahlgren...Tullgren..1922.,by=list(D$Habitat), FUN=f2)
habitat2003 <- aggregate(D$Corrected.values.2003,by=list(D$Habitat), FUN=f3)
habitatEstimate <- aggregate(D$Sweden.estimated.total,by=list(D$Habitat), FUN=f4)
maxHabitat <- max( habitat1761$x, habitat1922$x, habitat2003$x, habitatEstimate$x )

# Plot taxonomic composition, ensuring the order is decreasing with respect to true fauna
taxonEstimate <- taxonEstimate[with(taxonEstimate,order(-x)), ]
taxon1761 <- taxon1761[match(taxonEstimate$Group.1,taxon1761$Group.1), ]
taxon1922 <- taxon1922[match(taxonEstimate$Group.1,taxon1922$Group.1), ]
taxon2003 <- taxon2003[match(taxonEstimate$Group.1,taxon2003$Group.1), ]
barNames <- c("Hy","Di","Co","Le","He", "Oth" )
barplot( height=taxon1761$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", main="Taxon composition", ylim=c(0.0,maxTaxon) )
legend( "topright", legend="1761", bty="n" )
barplot( height=taxon1922$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxTaxon) )
legend( "topright", legend="1922", bty="n" )
barplot( height=taxon2003$x,  names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxTaxon) )
legend( "topright", legend="2003", bty="n" )
barplot( height=taxonEstimate$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxTaxon) )
legend( "topright", legend="Estimate", bty="n" )

# Plot niche composition in stacked bar chart
nicheNames <- c("Phytophagous", "Saprophagous", "Predator", "Other")
hyperNicheNames <- c("Phytophage-parasitoid", "Saprophage-parasitoid", "Predator-parasitoid")
x <- niche1761$x[ match( nicheNames, niche1761$Group.1 ) ]
y <- niche1761$x[ match( hyperNicheNames, niche1761$Group.1 ) ]
y[ 4 ] <- 0
niche1761Heights <- matrix( c(x, y), nrow=2, ncol=4, byrow=TRUE )
x <- niche1922$x[ match( nicheNames, niche1922$Group.1 ) ]
y <- niche1922$x[ match( hyperNicheNames, niche1922$Group.1 ) ]
y[ 4 ] <- 0
niche1922Heights <- matrix( c(x, y), nrow=2, ncol=4, byrow=TRUE )
x <- niche2003$x[ match( nicheNames, niche2003$Group.1 ) ]
y <- niche2003$x[ match( hyperNicheNames, niche2003$Group.1 ) ]
y[ 4 ] <- 0
niche2003Heights <- matrix( c(x, y), nrow=2, ncol=4, byrow=TRUE )
x <- nicheEstimate$x[ match( nicheNames, nicheEstimate$Group.1 ) ]
y <- nicheEstimate$x[ match( hyperNicheNames, nicheEstimate$Group.1 ) ]
y[ 4 ] <- 0
nicheEstimateHeights <- matrix( c(x, y), nrow=2, ncol=4, byrow=TRUE )
barNames <- c( "PlF", "Dec", "Pred", "Oth" )
barplot( height=niche1761Heights, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col=c("blue","red"), beside=TRUE, ylim=c(0.0,maxNiche), main="Niche composition" )
legend( "topright", legend="1761", bty="n" )
barplot( height=niche1922Heights, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col=c("blue","red"), beside=TRUE, ylim=c(0.0,maxNiche) )
legend( "topright", legend="1922", bty="n" )
barplot( height=niche2003Heights, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col=c("blue","red"), beside=TRUE, ylim=c(0.0,maxNiche) )
legend( "topright", legend="2003", bty="n" )
barplot( height=nicheEstimateHeights, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col=c("blue","red"), beside=TRUE, ylim=c(0.0,maxNiche) )
legend( "topright", legend="Estimate", bty="n" )

# Plot habitat composition, ensuring the order is decreasing with respect to true fauna
habitatEstimate <- habitatEstimate[with(habitatEstimate,order(-x)), ]
habitat1761 <- habitat1761[match(habitatEstimate$Group.1,habitat1761$Group.1), ]
habitat1922 <- habitat1922[match(habitatEstimate$Group.1,habitat1922$Group.1), ]
habitat2003 <- habitat2003[match(habitatEstimate$Group.1,habitat2003$Group.1), ]
barNames <- c( "Pl","So", "Te", "Wa", "Fu", "Wo", "Ot" )
barplot( height=habitat1761$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxHabitat), main="Habitat composition" )
legend( "topright", legend="1761", bty="n" )
barplot( height=habitat1922$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxHabitat) )
legend( "topright", legend="1922", bty="n" )
barplot( height=habitat2003$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxHabitat) )
legend( "topright", legend="2003", bty="n" )
barplot( height=habitatEstimate$x, names.arg=barNames, cex.names=nameSize, cex.axis=axisSize, col="blue", ylim=c(0.0,maxHabitat) )
legend( "topright", legend="Estimate", bty="n" )

dev.off()

