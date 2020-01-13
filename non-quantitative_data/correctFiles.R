# This script corrects and streamlines Author information in
# the observation files.

# Get a list of the files and the data for each file
# from the file "overview_non-quantitative_data.csv"
overview <- read.table("overview_non-quantitative_data.csv", header=TRUE, sep=";")

# Cycle over the rest of the files and add the data we need
for ( i in 1:length( overview$Filename ) )
{
    filename <- as.character( overview$Filename[i] )
    cat(i, " -- ")
    print(filename)

    # Read in obs and taxa files
    obsFile <- paste( substr( filename,1,nchar(filename)-4 ), "obs.tsv", sep="-" )
    obsData <- read.table( obsFile )
    taxaFile <- paste( substr( filename,1,nchar(filename)-4 ), "taxa.tsv", sep="-" )
    taxaData <- read.table( taxaFile )

    # Make sure Author is as.character and no factor
    obsData$Author <- as.character( obsData$Author )

    # Make sure columns in taxon file are as.character
    taxaData$SciName <- paste( taxaData$Genus, taxaData$Species, sep=" " )
    taxaData$Author <- as.character( taxaData$Author )
 
    # Cycle through file and fix Author entries
    for ( j in 1:length( obsData$Author ) )
    {
        author <- obsData$Author[j]
        
        # Try to fix "0" entries
        if ( author == "0" )
        {
            sciName <- paste( obsData$Genus[j], obsData$Species[j], sep=" " )
            k <- match( sciName, taxaData$SciName )
            if ( !is.na( k ) )
                author <- taxaData$Author[ k ]
        }

        # Now streamline the labeling of manuscript names
        if ( author == "n sp" || author == "n. sp. " || author == "n.sp." )
            author <- "inedit."

        # Streamline the labeling of unspecified determinations
        if ( author == "sp indet" )
            author <- "sp. indet."

        obsData$Author[j] <- author
    }

    # Write output file
    write.table( obsData, obsFile, sep="\t" )
}

