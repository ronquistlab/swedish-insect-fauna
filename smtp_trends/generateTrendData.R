# This script will generate data for trend plots for SMTP data

# Read in cneFxn
source( "../richness_estimates/cneFxn.R" )

# Read in trap data
T <- read.table( "../misc_data/trap_data.tsv" )

# Get rid of empty columns and rows
# Note that we do not get rid of the extra empty factor ("") of
# factor variables (in general). This should cause no problems
# in the following, as there are no observations (rows) of these
# factors. If it is a concern, recreate the factors here.
T <- T[ 1:73,1:22 ]
T$Biogeographic.region <- factor( T$Biogeographic.region )

# Read in SMTP abundance data
D <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Get rid of generic determinations and extraneous traps
D <- D[ D$Species != "sp.", ]
D <- D[ D$TrapID < 3000, ]

# Read in biological and taxonomic information for analysis taxa
B <- read.table( "../misc_data/overview_analysis_taxa.tsv", dec="," )

# Aggregate D into X with respect to TrapID, AnalysisTaxon and Species
# This should simplify and speed up subsequent operations
X <- aggregate( D$Total, by=list(D$TrapID, D$AnalysisTaxon, D$SciName), FUN=sum )
colnames(X) <- c( "TrapID", "AnalysisTaxon", "SciName", "numSpecimens" )

# Extend X with information on taxon and biology
indexVec <- match( X$AnalysisTaxon, B$Analysis.Taxon )
X$Order <- B$Order[ indexVec ]
X$Niche <- B$Main.feeding.niche[ indexVec ]
X$Habitat <- B$Main.feeding.habitat[ indexVec ]

# Extend with number of specimens of select categories
X$HymenopteraSpecimens <- X$numSpecimens * (X$Order == "Hymenoptera")
X$DipteraSpecimens <- X$numSpecimens * (X$Order == "Diptera")
X$SaprophagousSpecimens <- X$numSpecimens * (X$Niche == "Saprophagous" | X$Niche=="Saprophage.parasitoid")
X$ParasitoidSpecimens <- X$numSpecimens * (X$Niche=="Saprophage.parasitoid" | X$Niche=="Phytophage.parasitoid" | X$Niche=="Predator.parasitoid" | X$Niche=="General.parasitoid")
X$TemporarySpecimens <- X$numSpecimens * (X$Habitat == "Temporary.habitats")
X$FungiSpecimens <- X$numSpecimens * (X$Habitat == "Fungi" )

# Aggregate into SmtpTrapStats beginning with number of species in each trap
print( "Aggregating trap data" )
foo <- function(x) { sum( x > 0 ) }
SmtpTrapStats <- aggregate( X$numSpecimens, by=list(X$TrapID), FUN=foo )
colnames(SmtpTrapStats) <- c( "TrapID", "NumSpecies" )

# Add number of specimens
x <- aggregate( X$numSpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumSpecimens <- x$x

# Add taxon proportions of interest
print( "Adding taxon proportions" )
foo <- function(x) { sum( x == "Diptera" ) }
x <- aggregate( X$Order, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumDiptera <- x$x
SmtpTrapStats$PropDiptera <- x$x / SmtpTrapStats$NumSpecies
foo <- function(x) { sum( x == "Hymenoptera" ) }
x <- aggregate( X$Order, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumHymenoptera <- x$x
SmtpTrapStats$PropHymenoptera <- x$x / SmtpTrapStats$NumSpecies
x <- aggregate( X$DipteraSpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumDipteraSpecimens <- x$x
SmtpTrapStats$PropDipteraSpecimens <- x$x / SmtpTrapStats$NumSpecimens
x <- aggregate( X$HymenopteraSpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumHymenopteraSpecimens <- x$x
SmtpTrapStats$PropHymenopteraSpecimens <- x$x / SmtpTrapStats$NumSpecimens

# Add niche proportions of interest
print( "Adding niche proportions" )
foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Saprophagous", na.rm=TRUE ) }
x <- aggregate( X$Niche, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumSaprophagous <- x$x
SmtpTrapStats$PropSaprophagous <- x$x / SmtpTrapStats$NumSpecies
foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Predator.parasitoid" | x == "Phytophage.parasitoid" | x == "General.parasitoid", na.rm=TRUE ) }
x <- aggregate( X$Niche, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumParasitoids <- x$x
SmtpTrapStats$PropParasitoids <- x$x / SmtpTrapStats$NumSpecies
foosum <- function(x) { sum(x, na.rm=TRUE) }
x <- aggregate( X$SaprophagousSpecimens, by=list(X$TrapID), FUN=foosum )
SmtpTrapStats$NumSaprophagousSpecimens <- x$x
SmtpTrapStats$PropSaprophagousSpecimens <- x$x / SmtpTrapStats$NumSpecimens
x <- aggregate( X$ParasitoidSpecimens, by=list(X$TrapID), FUN=foosum )
SmtpTrapStats$NumParasitoidSpecimens <- x$x
SmtpTrapStats$PropParasitoidSpecimens <- x$x / SmtpTrapStats$NumSpecimens

# Add habitat proportions of interest
print( "Adding habitat proportions" )
foo <- function(x) { sum( x == "Temporary.habitats", na.rm=TRUE ) }
x <- aggregate( X$Habitat, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumTemporary <- x$x
SmtpTrapStats$PropTemporary <- x$x / SmtpTrapStats$NumSpecies
foo <- function(x) { sum( x == "Fungi", na.rm=TRUE ) }
x <- aggregate( X$Habitat, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumFungi <- x$x
SmtpTrapStats$PropFungi <- x$x / SmtpTrapStats$NumSpecies
x <- aggregate( X$TemporarySpecimens, by=list(X$TrapID), FUN=foosum )
SmtpTrapStats$NumTemporarySpecimens <- x$x
SmtpTrapStats$PropTemporarySpecimens <- x$x / SmtpTrapStats$NumSpecimens
x <- aggregate( X$FungiSpecimens, by=list(X$TrapID), FUN=foosum )
SmtpTrapStats$NumFungiSpecimens <- x$x
SmtpTrapStats$PropFungiSpecimens <- x$x / SmtpTrapStats$NumSpecimens

# Now add latitude and other trap data
j <- match( SmtpTrapStats$TrapID, T$TrapID )
SmtpTrapStats$Latitude <- T$Latitude[ j ]
SmtpTrapStats$BioRegion <- T$Biogeographic.region[ j ]
SmtpTrapStats$Habitat <- T$Habitat.class[ j ]

# Define a function that aggregates information by biogeographic region.
# This is done in the matrix Y, which is used to build smtpBiogeoStats, which is
# returned by the function. The data frame T is used to match traps to biogeographic
# regions
print( "Defining functions to aggregate biogeographic region data" )
aggregateBiogeoStats <- function( D, T, B )
{
    # Create Y, aggregating info in D by biogeographic region, analysis taxon and species
    biogeo <- T$Biogeographic.region[ match( D$TrapID, T$TrapID ) ]
    Y <- aggregate( D$Total, by=list(biogeo, D$AnalysisTaxon, D$SciName), FUN=sum )
    colnames(Y) <- c( "Biogeo", "AnalysisTaxon", "SciName", "numSpecimens" )

    # Extend Y with information on taxon and biology
    indexVec <- match( Y$AnalysisTaxon, B$Analysis.Taxon )
    Y$Order <- B$Order[ indexVec ]
    Y$Niche <- B$Main.feeding.niche[ indexVec ]
    Y$Habitat <- B$Main.feeding.habitat[ indexVec ]

    # Extend with number of specimens of select categories
    Y$HymenopteraSpecimens <- Y$numSpecimens * (Y$Order == "Hymenoptera")
    Y$DipteraSpecimens <- Y$numSpecimens * (Y$Order == "Diptera")
    Y$SaprophagousSpecimens <- Y$numSpecimens * (Y$Niche == "Saprophagous" | Y$Niche=="Saprophage.parasitoid")
    Y$ParasitoidSpecimens <- Y$numSpecimens * (Y$Niche=="Saprophage.parasitoid" | Y$Niche=="Phytophage.parasitoid" | Y$Niche=="Predator.parasitoid" | Y$Niche=="General.parasitoid")
    Y$TemporarySpecimens <- Y$numSpecimens * (Y$Habitat == "Temporary.habitats")
    Y$FungiSpecimens <- Y$numSpecimens * (Y$Habitat == "Fungi" )

    # Now start building biogeoStats by beginning with number of species in each region
    foo <- function(x) { length(unique(x)) }
    biogeoStats <- aggregate( Y$SciName, by=list(Y$Biogeo), FUN=foo )
    colnames(biogeoStats) <- c( "Region", "NumSpecies" )

    # Add number of specimens
    x <- aggregate( Y$numSpecimens, by=list(Y$Biogeo), FUN=sum )
    biogeoStats$NumSpecimens <- x$x

    # Add taxon proportions of interest
    foo <- function(x) { sum( x == "Diptera" ) }
    x <- aggregate( Y$Order, by=list(Y$Biogeo), FUN=foo )
    biogeoStats$NumDiptera <- x$x
    biogeoStats$PropDiptera <- x$x / biogeoStats$NumSpecies
    foo <- function(x) { sum( x == "Hymenoptera" ) }
    x <- aggregate( Y$Order, by=list(Y$Biogeo), FUN=foo )
    biogeoStats$NumHymenoptera <- x$x
    biogeoStats$PropHymenoptera <- x$x / biogeoStats$NumSpecies
    x <- aggregate( Y$DipteraSpecimens, by=list(Y$Biogeo), FUN=sum )
    biogeoStats$NumDipteraSpecimens <- x$x
    biogeoStats$PropDipteraSpecimens <- x$x / biogeoStats$NumSpecimens
    x <- aggregate( Y$HymenopteraSpecimens, by=list(Y$Biogeo), FUN=sum )
    biogeoStats$NumHymenopteraSpecimens <- x$x
    biogeoStats$PropHymenopteraSpecimens <- x$x / biogeoStats$NumSpecimens

    # Add niche proportions of interest
    foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Saprophagous", na.rm=TRUE ) }
    x <- aggregate( Y$Niche, by=list(Y$Biogeo), FUN=foo )
    biogeoStats$NumSaprophagous <- x$x
    biogeoStats$PropSaprophagous <- x$x / biogeoStats$NumSpecies
    foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Predator.parasitoid" | x == "Phytophage.parasitoid" | x == "General.parasitoid", na.rm=TRUE ) }
    x <- aggregate( Y$Niche, by=list(Y$Biogeo), FUN=foo )
    biogeoStats$NumParasitoids <- x$x
    biogeoStats$PropParasitoids <- x$x / biogeoStats$NumSpecies
    foosum <- function(x) { sum(x, na.rm=TRUE) }
    x <- aggregate( Y$SaprophagousSpecimens, by=list(Y$Biogeo), FUN=foosum )
    biogeoStats$NumSaprophagousSpecimens <- x$x
    biogeoStats$PropSaprophagousSpecimens <- x$x / biogeoStats$NumSpecimens
    x <- aggregate( Y$ParasitoidSpecimens, by=list(Y$Biogeo), FUN=foosum )
    biogeoStats$NumParasitoidSpecimens <- x$x
    biogeoStats$PropParasitoidSpecimens <- x$x / biogeoStats$NumSpecimens

    # Add habitat proportions of interest
    foo <- function(x) { sum( x == "Temporary.habitats", na.rm=TRUE ) }
    x <- aggregate( Y$Habitat, by=list(Y$Biogeo), FUN=foo )
    biogeoStats$NumTemporary <- x$x
    biogeoStats$PropTemporary <- x$x / biogeoStats$NumSpecies
    foo <- function(x) { sum( x == "Fungi", na.rm=TRUE ) }
    x <- aggregate( Y$Habitat, by=list(Y$Biogeo), FUN=foo )
    biogeoStats$NumFungi <- x$x
    biogeoStats$PropFungi <- x$x / biogeoStats$NumSpecies
    x <- aggregate( Y$TemporarySpecimens, by=list(Y$Biogeo), FUN=foosum )
    biogeoStats$NumTemporarySpecimens <- x$x
    biogeoStats$PropTemporarySpecimens <- x$x / biogeoStats$NumSpecimens
    x <- aggregate( Y$FungiSpecimens, by=list(Y$Biogeo), FUN=foosum )
    biogeoStats$NumFungiSpecimens <- x$x
    biogeoStats$PropFungiSpecimens <- x$x / biogeoStats$NumSpecimens

    return( biogeoStats )
}

# Compute observed proportions
print( "Computing observed biogeographic region data" )
SmtpBiogeoStats <- aggregateBiogeoStats( D, T, B )

# Compute randomized (permuted) observations
print( "Computing randomized biogeographic region data" )
numReps <- 10000
x <- data.frame( boreal=numeric(numReps), boreonemoral=numeric(numReps), nemoral=numeric(numReps) )
propDipteraRnd      <- x
propHymenopteraRnd  <- x
propSaprophagousRnd <- x
propParasitoidsRnd  <- x

T3 <- T[ T$Biogeographic.region != "alpine", ]
T3$Biogeographic.region <- factor( T3$Biogeographic.region )

for ( i in 1:numReps )
{
    if ( i %% 100 == 0 )
        cat( "Sample", i, "\n" )

    T.rnd <- T3
    T.rnd$Biogeographic.region <- sample( T.rnd$Biogeographic.region )

    biogeoStats <- aggregateBiogeoStats( D, T.rnd, B )

    propDipteraRnd[i, ]      <- biogeoStats$PropDiptera
    propHymenopteraRnd[i, ]  <- biogeoStats$PropHymenoptera
    propSaprophagousRnd[i, ] <- biogeoStats$PropSaprophagous
    propParasitoidsRnd[i, ]  <- biogeoStats$PropParasitoids
}


# Define a function to calculate p values
# of observations from the distribution estimate
# obtained from randomized (permuted) data
# Columns are assumed to represent categories and
# rows different random draws
pvalue <- function( obs, rnd )
{
    # Get size
    ncols <- ncol( rnd )
    nrows <- nrow( rnd )

    # Get means for classes
    m <- lapply( rnd, mean )

    # Get squared deviation of random (permuted) values
    # Note that 'vector' initializes elements to 0.0 in R
    sqDevRnd <- vector( mode="numeric", nrows )
    for ( i in 1:ncols )
        sqDevRnd <- sqDevRnd + ( rnd[ , i] - m[[i]] ) * ( rnd[ , i] - m[[i]] ) / m[[i]]

    # Get squared deviation of observed values
    sqDevObs <- 0
    for ( i in 1:ncols )
        sqDevObs <- sqDevObs + (obs[i] - m[[i]]) * (obs[i] - m[[i]] ) / m[[i]]

    sum( sqDevRnd >= sqDevObs ) / nrows
}


save( SmtpTrapStats, SmtpBiogeoStats,
      propDipteraRnd, propHymenopteraRnd, propSaprophagousRnd, propParasitoidsRnd,
      file="SmtpBiogeoStats.RData" )

cat( "Prop Diptera: p =", pvalue( SmtpBiogeoStats$PropDiptera[-1], propDipteraRnd ), "\n" )
cat( "Prop Hymenoptera: p =", pvalue( SmtpBiogeoStats$PropHymenoptera[-1], propHymenopteraRnd ), "\n" )
cat( "Prop saprophagous: p =", pvalue( SmtpBiogeoStats$PropSaprophagous[-1], propSaprophagousRnd ), "\n" )
cat( "Prop parasitoids: p =", pvalue( SmtpBiogeoStats$PropParasitoids[-1], propParasitoidsRnd ), "\n" )

