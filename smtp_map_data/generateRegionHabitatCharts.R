# This script generates the data for the map
# and for Figure 2

# Using colorRamps and colorspace palettes
library( colorspace )
library( colorRamps )

# Name the groups we want to analyze
Groups <- c(
      "Figitidae excl Charipinae",
      "Heleomyzidae+Odiniidae",
      "Ichneumoninae excl Phaeogenini",
      "Lonchaeidae",
      "Meteorini",
      "Mycetophilidae+Keroplatidae",
      "Phoridae",
      "Platygastridae (s str)",
      "Porricondylinae (s lat)",
      "Symphyta"
     )

barOrder <- c( 1, 3, 5, 8, 10, 2, 4, 6, 7, 9 )
barNames <- c( "Fi", "Ic", "Me", "Pl", "Sy", "He", "Lo", "My", "Ph", "Po" )

# Read the trap data (biogeographic region and habitat)
Traps <- read.table( "../misc_data/trap_data.tsv" )

# Read the collecting event data (number of trap days)
CollEvents <- read.table( "../misc_data/coll_event_data.tsv" )

# Read in the check list of known species
known2003 <- read.table( "../misc_data/checklist2003.tsv" )

# Only keep the species that actually were known in 2003
known2003 <- known2003[ known2003$Status.2003 == "present", ]

# Function for transforming the data by extending it with various
# types of information we will need later
transformData <- function( X )
{
    # Extend data with biogeographic region and habitat class (based on TrapID)
    X <- merge( X, Traps[ , c( "TrapID", "Habitat.class", "Biogeographic.region" ) ], by="TrapID" )

    # Extend data with number of trapping days (based on EventID)
    X <- merge( X, CollEvents[ , c( "EventID", "Sampling.period..days." ) ], by="EventID" )

    # Get rid of generic determinations and extra trap data
    X <- X[ X$Species != "sp.", ]
    X <- X[ X$TrapID < 3000, ]

    # Restrict to groups of interest
    X <- X[ X$AnalysisTaxon %in% Groups, ]
    X$AnalysisTaxon <- factor(X$AnalysisTaxon)

    # Add a column with status
    y <- rep( "New to Sweden", times=length( X$SciName ) )
    j <- X$SciName %in% known2003$Species
    y[j] <- "Known 2003"
    j <- grepl("200[3456789]", X$Author)
    y[j] <- "New to science"
    j <- grepl("201[0123456789]", X$Author)
    y[j] <- "New to science"
    j <- X$Author  == "n. sp."
    y[j] <- "New to science"

    X["Status"] <- factor(y)

    colsNeeded <- c( "AnalysisTaxon", "SciName", "Status", "Habitat.class", "Biogeographic.region", "EventID", "Sampling.period..days." )
    X <- X[ , colsNeeded ]

    X
}

# Read the data
D <- read.table( "../quantitative_data/quant-combined-data.tsv" )
E <- read.table( "../non-quantitative_data/non-quant-combined-data.tsv" )

# Transform data
D <- transformData( D )
E <- transformData( E )

# Merge data
F <- rbind( D, E )

# Get analysis groups in alphabetical order
F$AnalysisTaxon <- as.character( F$AnalysisTaxon )
F$AnalysisTaxon <- factor( F$AnalysisTaxon )

# Collecting effort by region
G <- F[ ,c( "AnalysisTaxon", "Biogeographic.region", "EventID", "Sampling.period..days." ) ]
G <- unique( G )
effort1 <- aggregate( G$Sampling.period..days., by=list( G$AnalysisTaxon, G$Biogeographic.region ), FUN=sum )
names( effort1 ) <- c( "AnalysisTaxon", "BiogeographicRegion", "SamplingEffort" )

# Collecting effort by habitat
G <- F[ ,c( "AnalysisTaxon", "Habitat.class", "EventID", "Sampling.period..days." ) ]
G <- unique( G )
effort2 <- aggregate( G$Sampling.period..days., by=list( G$AnalysisTaxon, G$Habitat.class ), FUN=sum )
names( effort2 ) <- c( "AnalysisTaxon", "HabitatClass", "SamplingEffort" )

# Diversity by taxon
G <- F[ , c("SciName", "AnalysisTaxon" ) ]
G <- unique( G )

X <- aggregate( G$SciName, by=list( G$AnalysisTaxon ), FUN=length )
names( X ) <- c( "AnalysisTaxon", "Diversity" )

# Diversity by region
G <- F[ , c("SciName", "Biogeographic.region", "AnalysisTaxon", "Status" ) ]
G <- unique( G )
byRegion <- aggregate( G$SciName, by=list( G$Biogeographic.region, G$AnalysisTaxon, G$Status ), FUN=length )
names( byRegion ) <- c( "BiogeographicRegion", "AnalysisTaxon", "Status", "SpeciesRichness" )
byRegion <- merge( byRegion, X, by="AnalysisTaxon" )
byRegion$SpeciesFraction <- byRegion$SpeciesRichness / byRegion$Diversity

# Diversity by habitat
G <- F[ , c("SciName", "Habitat.class", "AnalysisTaxon", "Status" ) ]
G <- unique( G )
byHabitat <- aggregate( G$SciName, by=list( G$Habitat.class, G$AnalysisTaxon, G$Status ), FUN=length )
names( byHabitat ) <- c( "HabitatClass", "AnalysisTaxon", "Status", "SpeciesRichness" )
byHabitat <- merge( byHabitat, X, by="AnalysisTaxon" )
byHabitat$SpeciesFraction <- byHabitat$SpeciesRichness / byHabitat$Diversity

# Create charts for Figure 1

pdf("Figure_1_charts.pdf")
par(mfrow=c(3,2))

# A useful function for creating chart data.
# It makes sure we get zeros for the classes
# where there are no instances in the data.
matchOrZero <- function( vec, names1, names2 )
{
    x <- rep( 0.0, length=length( names1 ) )

    matches <- match( names1, names2 )

    for ( i in 1:length(names1) )
        if ( !is.na( matches[i] ) )
            x[i] <- vec[ matches[i] ]

    x
}

# Legend for the charts
legend_text = c( "Previously known", "New to Sweden", "New to science" )

# A function for computing the heights we need for the bar charts
heights <- function( X )
{   
    T1 <- X[ X$Status == "Known 2003", ]
    T2 <- X[ X$Status == "New to Sweden", ]
    T3 <- X[ X$Status == "New to science", ]
    
    x1 <- matchOrZero( T1$SpeciesFraction, Groups, T1$AnalysisTaxon )
    x2 <- matchOrZero( T2$SpeciesFraction, Groups, T2$AnalysisTaxon )
    x3 <- matchOrZero( T3$SpeciesFraction, Groups, T3$AnalysisTaxon )

    matrix( c(x1, x2, x3), nrow=3, ncol=10, byrow=TRUE )
}

# Compute the regional charts of species of different types
for ( regionName in levels( byRegion$BiogeographicRegion ) )
{
    G <- byRegion[ byRegion$BiogeographicRegion == regionName, ]
    H <- heights( G )
    H <- H[ , barOrder ]
    if ( regionName == "alpine" )
        barplot( height=H, names.arg=barNames, ylim=c(0.0,0.85), cex.names=0.7, beside=FALSE, main=regionName, legend.text=legend_text, args.legend=list(bty="n") )
    else
        barplot( height=H, names.arg=barNames, ylim=c(0.0,0.85), cex.names=0.7, beside=FALSE, main=regionName )
}

# Total diversity
G <- F[ , c("SciName", "AnalysisTaxon", "Status" ) ]
G <- unique( G )
total <- aggregate( G$SciName, by=list( G$AnalysisTaxon, G$Status ), FUN=length )
names( total ) <- c( "AnalysisTaxon", "Status", "SpeciesRichness" )
total <- merge( total, X, by="AnalysisTaxon" )
total$SpeciesFraction <- total$SpeciesRichness / total$Diversity

H <- heights( total )
H <- H[ , barOrder ]
barplot( height=H, names.arg=barNames, ylim=c(0.0,1.0), cex.names=0.7, beside=FALSE, main="Total diversity" )

# Effort by region
x1 <- xtabs( effort1$SamplingEffort~effort1$BiogeographicRegion+effort1$AnalysisTaxon )
rowOrder <- c( 4, 3, 2, 1 )
x1 <- x1[ rowOrder, barOrder ]
barplot( height=x1, names.arg=barNames, cex.names=0.7, beside=FALSE, main="Effort by region", legend=TRUE, args.legend=list(x="topright", bty="n") )

dev.off()

# Create Figure 2 
pdf( "Figure_2.pdf", height=9, width=7 )
par(mfrow=c(4,2))

plotOrder <- c( 5, 3, 6, 1, 4, 2 )
for (  i in plotOrder )
{
    habitatName <- levels( byHabitat$HabitatClass )[i]
    G <- byHabitat[ byHabitat$HabitatClass == habitatName, ]
    H <- heights( G )
    H <- H[ , barOrder ]
    if ( i == 5 )
        barplot( height=H, names.arg=barNames, ylim=c(0.0,0.75), cex.names=0.7, beside=FALSE, main=habitatName, legend.text=legend_text, args.legend=list(x="topright", bty="n") )
    else
        barplot( height=H, names.arg=barNames, ylim=c(0.0,0.75), cex.names=0.7, beside=FALSE, main=habitatName )
}

x2 <- xtabs( effort2$SamplingEffort~effort2$HabitatClass+effort2$AnalysisTaxon )
rowOrder <- c( 2, 1, 3, 4, 5, 6 )
x2 <- x2[ rowOrder, barOrder ]
cols <- topo.colors(6)
barplot( height=x2, names.arg=barNames, cex.names=0.7, col=cols, beside=FALSE, main="Effort per habitat", legend=FALSE )

plot.new()
barplot( height=x2, names.arg=barNames, cex.names=0.7, plot=FALSE, col=topo.colors(6), beside=FALSE, main="Effort per habitat")
legend( x="left", bty="n", fill=cols, legend=dimnames(x2)$`effort2$HabitatClass`, plot=TRUE )

dev.off()

