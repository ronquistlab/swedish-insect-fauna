# This script combines the abundance data (quantitative data) into a single
# file, polished for further analyses

# Get a list of the files and the data for each file
# from the file "overview_quantitative_data.csv"

overview <- read.table("overview_quantitative_data.csv", header=TRUE, sep=";")

# Get the first file name
filename <- as.character( overview$Filename[1] )
cat(1, " -- ")
print(filename)

# Process the first file
obsFile <- paste( substr( filename,1,nchar(filename)-4 ), "obs.tsv", sep="-" )
obsData <- read.table( obsFile )
obsData$Order <- rep( overview$Order[1], times = length( obsData$Genus ) )
obsData$AnalysisTaxon <- rep( overview$AnalysisTaxon[1], times = length( obsData$Genus ) )

combinedData <- obsData

# Cycle over the rest of the files and add the data we need
for ( i in 2:length(overview$Filename) ) {

    # Get file name
    filename <- as.character( overview$Filename[i] )
    cat(i, " -- ")
    print(filename)

    # Process the file and add it
    obsFile <- paste( substr( filename,1,nchar(filename)-4 ), "obs.tsv", sep="-" )
    obsData <- read.table( obsFile )
    obsData$Order <- rep( overview$Order[i], times = length( obsData$Genus ) )
    obsData$AnalysisTaxon <- rep( overview$AnalysisTaxon[i], times = length( obsData$Genus ) )

    combinedData <- rbind( combinedData, obsData )
}

# Polish the combined file

# We do not need the StartDate column, only the others
combinedData <- combinedData[ , colnames(combinedData) != "StartDate" ]

# We add the computed scientific name for convenience
combinedData$SciName <- paste( combinedData$Genus, combinedData$Species, sep=" " )
 
# Correct a couple of species that ended up in the wrong file and therefore have the
# wrong analysis taxon
for ( i in 1:length(combinedData$Genus) )
{
    if( combinedData$SciName[i] == "Argynnis adippe" )
        combinedData$AnalysisTaxon[i] <- "Macrolepidoptera"

    if ( combinedData$SciName[i] == "Platurocypta punctum" )
        combinedData$AnalysisTaxon[i] <- "Mycetophilidae+Keroplatidae"
}

# Finally write resulting file
write.table( combinedData, "quant-combined-data.tsv", sep="\t" )


