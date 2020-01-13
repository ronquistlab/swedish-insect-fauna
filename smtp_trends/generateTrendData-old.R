# This script will generate data for trend plots for SMTP data

# Read in trap data. We need it for latitude information.
T <- read.table( "trap_data.csv",sep=";",header=TRUE,fileEncoding="latin1",quote="",dec="," )

# Get rid of empty columns and rows
T <- T[ 1:73,1:22 ]

# Read in SMTP abundance data
D <- read.table( "quant-combined-data.tsv" )

# Get rid of generic determinations and extraneous traps
D <- D[ D$Species != "sp.", ]
D <- D[ D$TrapID < 3000, ]

# Read in biological and taxonomic information for analysis taxa
B <- read.table( "overview_analysis_taxa.tsv", dec="," )

# Get rid of empty columns
B <- B[ ,1:17]

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
foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Saprophagous" ) }
x <- aggregate( X$Niche, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumSaprophagous <- x$x
SmtpTrapStats$PropSaprophagous <- x$x / SmtpTrapStats$NumSpecies
foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Predator.parasitoid" | x == "Phytophage.parasitoid" | x == "General.parasitoid" ) }
x <- aggregate( X$Niche, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumParasitoids <- x$x
SmtpTrapStats$PropParasitoids <- x$x / SmtpTrapStats$NumSpecies
x <- aggregate( X$SaprophagousSpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumSaprophagousSpecimens <- x$x
SmtpTrapStats$PropSaprophagousSpecimens <- x$x / SmtpTrapStats$NumSpecimens
x <- aggregate( X$ParasitoidSpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumParasitoidSpecimens <- x$x
SmtpTrapStats$PropParasitoidSpecimens <- x$x / SmtpTrapStats$NumSpecimens

# Add habitat proportions of interest
print( "Adding habitat proportions" )
foo <- function(x) { sum( x == "Temporary.habitats" ) }
x <- aggregate( X$Habitat, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumTemporary <- x$x
SmtpTrapStats$PropTemporary <- x$x / SmtpTrapStats$NumSpecies
foo <- function(x) { sum( x == "Fungi" ) }
x <- aggregate( X$Habitat, by=list(X$TrapID), FUN=foo )
SmtpTrapStats$NumFungi <- x$x
SmtpTrapStats$PropFungi <- x$x / SmtpTrapStats$NumSpecies
x <- aggregate( X$TemporarySpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumTemporarySpecimens <- x$x
SmtpTrapStats$PropTemporarySpecimens <- x$x / SmtpTrapStats$NumSpecimens
x <- aggregate( X$FungiSpecimens, by=list(X$TrapID), FUN=sum )
SmtpTrapStats$NumFungiSpecimens <- x$x
SmtpTrapStats$PropFungiSpecimens <- x$x / SmtpTrapStats$NumSpecimens

# Now add latitude and other trap data
j <- match( SmtpTrapStats$TrapID, T$TrapID )
SmtpTrapStats$Latitude <- T$Latitude[ j ]
SmtpTrapStats$BioRegion <- T$Biogeographic.region[ j ]
SmtpTrapStats$Habitat <- T$Habitat.class[ j ]

# Aggregate into SmtpBiogeoStats, organizing information by biogeographic region.
# The fact that X is aggregated by TrapID is a problem for us here, so we aggregate
# directly into biogeographic regions and species, without worrying about traps.
# This is done in the matrix Y, which is used to build SmtpBiogeoStats. We first
# need to add biogeographic region to D, though, as this is tied to TrapID.
print( "Aggregating biogeographic region data" )
D$Biogeo <- T$Biogeographic.region[ match( D$TrapID, T$TrapID ) ]
Y <- aggregate( D$Total, by=list(D$Biogeo, D$AnalysisTaxon, D$SciName), FUN=sum )
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

# Now start building SmtpBiogeoStats by beginning with number of species in each region
foo <- function(x) { length(unique(x)) }
SmtpBiogeoStats <- aggregate( Y$SciName, by=list(Y$Biogeo), FUN=foo )
colnames(SmtpBiogeoStats) <- c( "Region", "NumSpecies" )

# Add number of specimens
x <- aggregate( Y$numSpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumSpecimens <- x$x

# Add taxon proportions of interest
print( "Adding taxon proportions" )
foo <- function(x) { sum( x == "Diptera" ) }
x <- aggregate( Y$Order, by=list(Y$Biogeo), FUN=foo )
SmtpBiogeoStats$NumDiptera <- x$x
SmtpBiogeoStats$PropDiptera <- x$x / SmtpBiogeoStats$NumSpecies
foo <- function(x) { sum( x == "Hymenoptera" ) }
x <- aggregate( Y$Order, by=list(Y$Biogeo), FUN=foo )
SmtpBiogeoStats$NumHymenoptera <- x$x
SmtpBiogeoStats$PropHymenoptera <- x$x / SmtpBiogeoStats$NumSpecies
x <- aggregate( Y$DipteraSpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumDipteraSpecimens <- x$x
SmtpBiogeoStats$PropDipteraSpecimens <- x$x / SmtpBiogeoStats$NumSpecimens
x <- aggregate( Y$HymenopteraSpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumHymenopteraSpecimens <- x$x
SmtpBiogeoStats$PropHymenopteraSpecimens <- x$x / SmtpBiogeoStats$NumSpecimens

# Add niche proportions of interest
print( "Adding niche proportions" )
foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Saprophagous" ) }
x <- aggregate( Y$Niche, by=list(Y$Biogeo), FUN=foo )
SmtpBiogeoStats$NumSaprophagous <- x$x
SmtpBiogeoStats$PropSaprophagous <- x$x / SmtpBiogeoStats$NumSpecies
foo <- function(x) { sum( x == "Saprophage.parasitoid" | x == "Predator.parasitoid" | x == "Phytophage.parasitoid" | x == "General.parasitoid" ) }
x <- aggregate( Y$Niche, by=list(Y$Biogeo), FUN=foo )
SmtpBiogeoStats$NumParasitoids <- x$x
SmtpBiogeoStats$PropParasitoids <- x$x / SmtpBiogeoStats$NumSpecies
x <- aggregate( Y$SaprophagousSpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumSaprophagousSpecimens <- x$x
SmtpBiogeoStats$PropSaprophagousSpecimens <- x$x / SmtpBiogeoStats$NumSpecimens
x <- aggregate( Y$ParasitoidSpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumParasitoidSpecimens <- x$x
SmtpBiogeoStats$PropParasitoidSpecimens <- x$x / SmtpBiogeoStats$NumSpecimens

# Add habitat proportions of interest
print( "Adding habitat proportions" )
foo <- function(x) { sum( x == "Temporary.habitats" ) }
x <- aggregate( Y$Habitat, by=list(Y$Biogeo), FUN=foo )
SmtpBiogeoStats$NumTemporary <- x$x
SmtpBiogeoStats$PropTemporary <- x$x / SmtpBiogeoStats$NumSpecies
foo <- function(x) { sum( x == "Fungi" ) }
x <- aggregate( Y$Habitat, by=list(Y$Biogeo), FUN=foo )
SmtpBiogeoStats$NumFungi <- x$x
SmtpBiogeoStats$PropFungi <- x$x / SmtpBiogeoStats$NumSpecies
x <- aggregate( Y$TemporarySpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumTemporarySpecimens <- x$x
SmtpBiogeoStats$PropTemporarySpecimens <- x$x / SmtpBiogeoStats$NumSpecimens
x <- aggregate( Y$FungiSpecimens, by=list(Y$Biogeo), FUN=sum )
SmtpBiogeoStats$NumFungiSpecimens <- x$x
SmtpBiogeoStats$PropFungiSpecimens <- x$x / SmtpBiogeoStats$NumSpecimens

save( SmtpTrapStats, SmtpBiogeoStats, file="SmtpBiogeoStats.RData" )

