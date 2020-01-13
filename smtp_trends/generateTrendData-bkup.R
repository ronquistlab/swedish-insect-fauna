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

# Define a function to trim a data frame X down to the groups that should be
# included. We use the criterion that there should be at least 100 specimens
# and at least five traps in each of the biogeographic regions except the
# alpine region for a  group to be included. T is used to link X$TrapID to
# biogeographic region.
trimCNE <- function( X, T )
{
    includedTaxa <- character()
    biogeo <- T$Biogeographic.region[ match( X$TrapID, T$TrapID ) ]
    Y <- X[ biogeo != "alpine", ]
    Y$AnalysisTaxon <- factor( Y$AnalysisTaxon )
    biogeo <- T$Biogeographic.region[ match( Y$TrapID, T$TrapID ) ]
    biogeo <- factor( biogeo )  # get rid of empty region(s) if any

    for ( i in levels( Y$AnalysisTaxon ) )
    {
        if ( i == "" ) next

        isValid <- TRUE

        for ( j in levels( biogeo ) )
        {
            if ( j == "" ) next
            
            Z <- Y[ Y$AnalysisTaxon == i & biogeo == j, ]

            if ( sum(Z$Specimens < 100 ) || length( unique(Z$TrapID) ) < 5 )
            {
                isValid <- FALSE
                break
            }
        }

        if ( isValid )
            includedTaxa[ length(includedTaxa)+1 ] <- i
    }

    includeVec <- Y$AnalysisTaxon %in% includedTaxa

    Z <- Y[ includeVec, ]
    Z$AnalysisTaxon <- factor( Z$AnalysisTaxon)

    return( Z )
}


# Define function for adding CNE predictions of number
# of species. We assume that X has been trimmed down first
# to all groups that should be included
addCNEBiogeoStats <- function( X, T, biogeoStats )
{
    # Get the biogeographic data matching X
    biogeo <- T$Biogeographic.region[ match( X$TrapID, T$TrapID ) ]
    biogeo <- factor( biogeo )
    x <- numeric()

    # Prepare biogeoStats for primary additions
    nrows <- nrow( biogeoStats )
    biogeoStats$NumSpeciesCNE <- numeric( nrows )
    biogeoStats$NumDipteraCNE <- numeric( nrows )
    biogeoStats$NumHymenopteraCNE <- numeric( nrows )
    biogeoStats$NumSaprophagousCNE <- numeric( nrows )
    biogeoStats$NumParasitoidsCNE <- numeric( nrows )

    # Cycle over groups and add stats
    for ( i in levels( X$AnalysisTaxon ) )
    {
        if ( i == "" ) next

        # Find out info about the analysis taxon
        index1 <- match( i, X$AnalysisTaxon )
        order  <- X$Order[ index1 ]
        niche  <- X$Niche[ index1 ]

        for( j in levels( biogeo ) )
        {
            if ( j == "" ) next

            # Get the number of species from CNE extrapolation
            Y <- X[ X$AnalysisTaxon == i & biogeo == j, ]
            Y$SciName <- factor( Y$SciName )
            Y$TrapID <- factor( Y$TrapID )

            mytable <- xtabs( Y$numSpecimens~Y$SciName+Y$TrapID )

            numSpecies <- cneFxn( mytable )

            # Add species to the appropriate counters
            index2 <- match( j, biogeoStats$Region )

            biogeoStats$NumSpeciesCNE[index2] <- biogeoStats$NumSpeciesCNE[index2] + numSpecies

            if ( order == "Diptera" )
                biogeoStats$NumDipteraCNE[index2] <- biogeoStats$NumDipteraCNE[index2] + numSpecies
        
            if ( order == "Hymenoptera" )
                biogeoStats$NumHymenopteraCNE[index2] <- biogeoStats$NumHymenopteraCNE[index2] + numSpecies
        
            if ( !is.na(niche) && niche == "Saprophagous" )
                biogeoStats$NumSaprophagousCNE[index2] <- biogeoStats$NumSaprophagousCNE[index2] + numSpecies
        
            if ( !is.na(niche) && ( niche == "Saprophage.parasitoid" || niche == "Phytophage.parasitoid" || niche == "Predator.parasitoid" || niche == "General.parasitoid" ) )
                biogeoStats$NumParasitoidsCNE[index2] <- biogeoStats$NumParasitoidsCNE[index2] + numSpecies
        }
    }

    # Add secondary stats (proportions) (note use of vector operations here; each row computes ratios for all regions)
    biogeoStats$PropDipteraCNE      <- biogeoStats$NumDipteraCNE      / biogeoStats$NumSpeciesCNE
    biogeoStats$PropHymenopteraCNE  <- biogeoStats$NumHymenopteraCNE  / biogeoStats$NumSpeciesCNE
    biogeoStats$PropSaprophagousCNE <- biogeoStats$NumSaprophagousCNE / biogeoStats$NumSpeciesCNE
    biogeoStats$PropParasitoidsCNE  <- biogeoStats$NumParasitoidsCNE  / biogeoStats$NumSpeciesCNE

    return( biogeoStats )
}

# Compute observed proportions
print( "Computing observed biogeographic region data" )
SmtpBiogeoStats <- aggregateBiogeoStats( D, T, B )
X <- trimCNE( X, T )
SmtpBiogeoStats <- addCNEBiogeoStats( X, T, SmtpBiogeoStats )

# Compute randomized (permuted) observations
print( "Computing randomized biogeographic region data" )
numReps <- 10000
x <- data.frame( boreal=numeric(numReps), boreonemoral=numeric(numReps), nemoral=numeric(numReps) )
propDipteraRnd      <- x
propHymenopteraRnd  <- x
propSaprophagousRnd <- x
propParasitoidsRnd  <- x
propDipteraCNERnd      <- x
propHymenopteraCNERnd  <- x
propSaprophagousCNERnd <- x
propParasitoidsCNERnd  <- x

T3 <- T[ T$Biogeographic.region != "alpine", ]
T3$Biogeographic.region <- factor( T3$Biogeographic.region )

for ( i in 1:numReps )
{
    if ( i %% 100 == 0 )
        cat( "Sample", i, "\n" )

    T.rnd <- T3
    T.rnd$Biogeographic.region <- sample( T.rnd$Biogeographic.region )

    biogeoStats <- aggregateBiogeoStats( D, T.rnd, B )
#   biogeoStats <- addCNEBiogeoStats( X, T.rnd, biogeoStats )

    propDipteraRnd[i, ]      <- biogeoStats$PropDiptera
    propHymenopteraRnd[i, ]  <- biogeoStats$PropHymenoptera
    propSaprophagousRnd[i, ] <- biogeoStats$PropSaprophagous
    propParasitoidsRnd[i, ]  <- biogeoStats$PropParasitoids

#    propDipteraCNERnd[i, ]      <- biogeoStats$PropDipteraCNE
#    propHymenopteraCNERnd[i, ]  <- biogeoStats$PropHymenopteraCNE
#    propSaprophagousCNERnd[i, ] <- biogeoStats$PropSaprophagousCNE
#    propParasitoidsCNERnd[i, ]  <- biogeoStats$PropParasitoidsCNE
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
      propDipteraCNERnd, propHymenopteraCNERnd, propSaprophagousCNERnd, propParasitoidsCNERnd,
      file="SmtpBiogeoStats.RData" )

cat( "Prop Diptera: p =", pvalue( SmtpBiogeoStats$PropDiptera[-1], propDipteraRnd ), "\n" )
cat( "Prop Hymenoptera: p =", pvalue( SmtpBiogeoStats$PropHymenoptera[-1], propHymenopteraRnd ), "\n" )
cat( "Prop saprophagous: p =", pvalue( SmtpBiogeoStats$PropSaprophagous[-1], propSaprophagousRnd ), "\n" )
cat( "Prop parasitoids: p =", pvalue( SmtpBiogeoStats$PropParasitoids[-1], propParasitoidsRnd ), "\n" )
# cat( "Prop Diptera CNE: p =", pvalue( SmtpBiogeoStats$PropDipteraCNE[-1], propDipteraCNERnd ), "\n" )
# cat( "Prop Hymenoptera CNE: p =", pvalue( SmtpBiogeoStats$PropHymenopteraCNE[-1], propHymenopteraCNERnd ), "\n" )
# cat( "Prop saprophagous CNE: p =", pvalue( SmtpBiogeoStats$PropSaprophagousCNE[-1], propSaprophagousCNERnd ), "\n" )
# cat( "Prop parasitoids CNE: p =", pvalue( SmtpBiogeoStats$PropParasitoidsCNE[-1], propParasitoidsCNERnd ), "\n" )

