# Read in taxon data
print("Reading FaEu taxon table")
Taxon <- read.table( "FaEu_Taxa.csv",sep=";",header=TRUE,fileEncoding="latin1",quote="" )

# Read in distribution data. This will be our main table, so call it D
print("Reading FaEu distribution table")
D <- read.table( "FaEu_Distribution.csv",sep=";",header=TRUE,fileEncoding="latin1",quote="" )

# Read in country code data
print("Reading FaEu country codes")
CountryCodes <- read.table("FaEu_CountryCodes.csv",header=TRUE,sep=";",quote="",fileEncoding="latin1")

# Read in country latitude data
# Data from https://developers.google.com/public-data/docs/canonical/countries_csv
# Accessed 2017-06-29
print("Reading country latitutdes")
CountryLats <- read.table("country_latitudes.csv",header=TRUE,sep=";",quote="",fileEncoding="latin1")

# Read in country area data
# Data from http://ec.europa.eu/eurostat/documents/205002/6786255/AreaSize_20161109.xlsx
# complemented by data from https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area
# Both sites accessed on 2017-06-29
print("Reading country areas")
CountryAreas <- read.table("country_areas.csv",header=TRUE,sep=";",quote="",fileEncoding="latin1",dec=",")

# Read in taxonomic and biological data (setting file encoding to latin1 is not good here)
print("Reading taxonomic and biological data")
Bio <- read.table("../misc_data/FaEu_bio_info.tsv")

# Read in subfamily classification
print("Reading FaEu subfamily classification")
Subfam <- read.table("FaEu_subfamily_genera.csv",header=TRUE,sep=";",quote="",fileEncoding="latin1")

# Add family, genus and species names to distribution table, including composed scientific name
print("Adding family, genus and species names")
D$FAMILY_NAME <- Taxon$FAMILY_NAME[ match( D$TAXON_ID, Taxon$FAEU_TAXON_ID_EPITHET ) ]
D$GENUS_NAME <- Taxon$GENUS_NAME[ match( D$TAXON_ID, Taxon$FAEU_TAXON_ID_EPITHET ) ]
D$SPECIES_NAME <- Taxon$SPECIES_NAME[ match( D$TAXON_ID, Taxon$FAEU_TAXON_ID_EPITHET ) ]
D$SciName <- paste( D$GENUS_NAME, D$SPECIES_NAME, sep=" " )

# Compute family-level unit (subfamily for Staphylinidae, Ichneumonidae, Braconidae)
print("Creating family-level unit")
D$FamLevelUnit <- as.character( D$FAMILY_NAME )
for ( i in 1:length(D$FamLevelUnit) )
{
    if ( D$FamLevelUnit[i] == "Staphylinidae" || D$FamLevelUnit[i] == "Ichneumonidae" || D$FamLevelUnit[i] == "Braconidae" )
    {
        j <- match( D$GENUS_NAME[i], Subfam$Genus )
        if ( !is.na( j ) )
            D$FamLevelUnit[i] <- as.character( Subfam$Subfamily[ j ] )
    }
}
D$FamLevelUnit <- factor(D$FamLevelUnit)

# Add order, niche and habitat
print("Adding order, niche and habitat")
D$Order <- rep("",length(D$FamLevelUnit))
D$Niche <- rep("",length(D$FamLevelUnit))
D$Habitat <- rep("",length(D$FamLevelUnit))
j <- match( D$FamLevelUnit, Bio$Family.level.unit..PESI. )
for ( i in 1:length(D$FamLevelUnit) )
{
    if ( !is.na( j[i] ) )
    {
        D$Order[i] <- as.character( Bio$Order[ j[i] ] )
        D$Niche[i] <- as.character( Bio$Main.feeding.niche[ j[i] ] )
        D$Habitat[i] <- as.character( Bio$Main.feeding.habitat[ j[i] ] )
    }
}
D$Order <- factor( D$Order )
D$Niche <- factor( D$Niche )
D$Habitat <- factor( D$Habitat )

# Prepare country data
print("Preparing country data")

# We do not include outside areas: Turkey, Russia or any of the exotic
# islands in the Atlantic (Canaries, Madeira etc). Nor Gibraltar, as it
# is not typical for UK latitudes or other UK characteristics.
excludeCountries <- c (
    "ES.CNY",
    "GB.GI",
    "PT.AZO",
    "PT.MDR",
    "PT.SEL",
    "RU.FJL",
    "RU.KGD",
    "RU.NOZ",
    "RU.RUC",
    "RU.RUE",
    "RU.RUN",
    "RU.RUS",
    "RU.RUW",
    "TR.TUE",
    "AFR",
    "AUS",
    "EPA",
    "NAF",
    "NEA",
    "NEO",
    "NRE",
    "ORR" )
D <- D[ , !(names(D) %in% excludeCountries) ]

# Combine columns into actual countries
combineFxn <- function(x,y)
{
    for ( i in 1:length(x) )
    {
        if ( y[i] == "P" )
            x[i] = "P"
        if ( y[i] == "P?" && x[i] != "P" )
            x[i] = "P?"
    }
    return(x)
}
complexCountries <- c( "PT.POR", "NO.NOR", "NO.SVA", "DK.DEN", "DK.FOR", "ES.BAL", "ES.SPA",
                       "FR.COR", "FR.FRA", "GB.CI", "GB.GRB", "GB.NI", "GR.AEG", "GR.CYC",
                       "GR.DOD", "GR.GRC", "GR.KRI", "IT.ITA", "IT.SAR", "IT.SI" )
D$PT <- D$PT.POR
D$NO <- combineFxn( D$NO.NOR, D$NO.SVA )
D$DK <- combineFxn( D$DK.DEN, D$DK.FOR )
D$ES <- combineFxn( D$ES.BAL, D$ES.SPA )
D$FR <- combineFxn( D$FR.COR, D$FR.FRA )
D$GB <- combineFxn( D$GB.GRB, D$GB.CI )
D$GB <- combineFxn( D$GB, D$GB.NI )
D$GR <- combineFxn( D$GR.GRC, D$GR.AEG )
D$GR <- combineFxn( D$GR, D$GR.CYC )
D$GR <- combineFxn( D$GR, D$GR.DOD )
D$GR <- combineFxn( D$GR, D$GR.KRI )
D$IT <- combineFxn( D$IT.ITA, D$IT.SAR )
D$IT <- combineFxn( D$IT, D$IT.SI )
D <- D[ ,!(names(D) %in% complexCountries) ]

# Finally reorder columns so we get countries towards the end
D <- D[ ,c(1,37:44,2:36,45:52) ]

# Aggregate taxonomic data
print( "Aggregating taxonomic data" )
numPresent <- function(x) sum( x=="P" | x=="P?" )
X1 <- aggregate( D[,10:52], by=list(D$Order), FUN=numPresent )
names(X1)[1] <- "FamLevelUnit"

# Transpose X1 in FaEuTaxon, making sure we do not get row names in X1 as data in FaEuTaxon
FaEuTaxon <- as.data.frame( t(X1[,-1]) )
colnames(FaEuTaxon) <- X1[,1]
FaEuTaxon$Total <- rowSums(FaEuTaxon)
FaEuTaxon$CountryCode <- factor( colnames(X1[,-1]) )

# Add latitude and area
FaEuTaxon$Latitude <- CountryLats$latitude[ match( FaEuTaxon$CountryCode, CountryLats$country ) ]
FaEuTaxon$Area     <- CountryAreas$Area[ match( FaEuTaxon$CountryCode, CountryAreas$CountryCode ) ]

# Add some proportions of interest
FaEuTaxon$PropHymenoptera <- FaEuTaxon$Hymenoptera / FaEuTaxon$Total
FaEuTaxon$PropDiptera <- FaEuTaxon$Diptera / FaEuTaxon$Total
FaEuTaxon$PropHymDip <- (FaEuTaxon$Hymenoptera + FaEuTaxon$Diptera) / FaEuTaxon$Total

# Aggregate niche data
print( "Aggregating niche data" )
X2 <- aggregate( D[,10:52], by=list(D$Niche), FUN=numPresent )
names(X2)[1] <- "Niche"

# Transpose X2 in FaEuNiche, making sure we do not get row names in X2 as data in FaEuNiche
FaEuNiche <- as.data.frame( t(X2[,-1]) )
colnames(FaEuNiche) <- X2[,1]
FaEuNiche$Total <- rowSums(FaEuNiche)
FaEuNiche$CountryCode <- factor( colnames(X2[,-1]) )

# Add latitude and area
FaEuNiche$Latitude <- CountryLats$latitude[ match( FaEuNiche$CountryCode, CountryLats$country ) ]
FaEuNiche$Area     <- CountryAreas$Area[ match( FaEuNiche$CountryCode, CountryAreas$CountryCode ) ]

# Add some proportions of interest
FaEuNiche$PropParasitoids <- (FaEuNiche$Phytophage.parasitoid + FaEuNiche$Saprophage.parasitoid + FaEuNiche$Predator.parasitoid + FaEuNiche$General.parasitoid) / FaEuNiche$Total
FaEuNiche$PropSaprophagous <- (FaEuNiche$Saprophagous + FaEuNiche$Saprophage.parasitoid) / FaEuNiche$Total

# Aggregate habitat data
print( "Aggregating habitat data" )
X3 <- aggregate( D[,10:52], by=list(D$Habitat), FUN=numPresent )
names(X3)[1] <- "Habitat"

# Transpose X3 in FaEuHabitat, making sure we do not get row names in X3 as data in FaEuHabitat
FaEuHabitat <- as.data.frame( t(X3[,-1]) )
colnames(FaEuHabitat) <- X3[,1]
FaEuHabitat$Total <- rowSums(FaEuHabitat)
FaEuHabitat$CountryCode <- factor( colnames(X3[,-1]) )

# Add latitude and area
FaEuHabitat$Latitude <- CountryLats$latitude[ match( FaEuHabitat$CountryCode, CountryLats$country ) ]
FaEuHabitat$Area     <- CountryAreas$Area[ match( FaEuHabitat$CountryCode, CountryAreas$CountryCode ) ]

# Add some proportions of interest
FaEuHabitat$PropTemporary <- FaEuHabitat$Temporary.habitats / FaEuHabitat$Total
FaEuHabitat$PropFungi <- FaEuHabitat$Fungi / FaEuHabitat$Total

# Save the data
print( "Saving data" )
save(FaEuTaxon, FaEuNiche, FaEuHabitat, file="FaEuPlotData.RData")

