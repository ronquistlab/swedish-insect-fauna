# Get a list of files with problems

# Script only used for checking; should not detect any problems (some NA values may be OK)

obsfiles <- dir( pattern="*-obs.tsv" )
taxonfiles <- dir( pattern="*-taxa.tsv")

collfile <- "collevents.tsv"

# Read in collevent list
myCollIDs <- read.table( collfile, sep="\t", header = TRUE )
collevents <- paste( myCollIDs$TrapID, myCollIDs$EventID, sep=" " )

# Cycle over files and record files with problems
for ( i in 1:length(obsfiles) ) {

    print(obsfiles[i])

    # Read observation table
    myData <- read.table( obsfiles[i], sep="\t", header=TRUE )

    # Find if there are rows that are not complete cases
    x <- sum( !complete.cases(myData) )
    if ( x > 0 )
    {
        cat( "   - Rows with incomplete cases (NA values): ")
        print( x )
    }

    # Read taxon table
    myTaxa <- read.table( taxonfiles[i], sep="\t", header=TRUE )

    # Compute scientific name in observation table
    myObsList <- paste(myData$Genus, myData$Species, sep=" " )
    myObsList <- unique( myObsList )
    
    # Compute scientific name from taxon table
    myTaxonList <- paste(myTaxa$Genus, myTaxa$Species, sep=" " )

    # See if there are some nonmatches
    x <- length(myObsList) - sum ( myObsList %in% myTaxonList )

    if ( x > 0 )
    {
        cat( "   - No. taxa not matching taxon list: ")
        print( x )
        cat( "     Missing taxa:")
        a <- myObsList %in% myTaxonList
        for ( i in 1:length(a) )
            if ( !a[i] )
            {
                cat("     ")
                print( myObsList[i] )
            }
    }

    # Compute trap-coll from observation table
    myTrapCollList <- paste(myData$TrapID, myData$EventID, sep=" " )
    myTrapCollList <- unique( myTrapCollList )
    
    # See if there are some nonmatches
    x <- length(myTrapCollList) - sum ( myTrapCollList %in% collevents )

    if ( x > 0 )
    {
        cat( "   - No. non-matching trap-coll combinations: ")
        print( x )
        cat( "     Mismatched trap-coll events:")
        a <- myTrapCollList %in% collevents
        for ( i in 1:length(a) )
            if ( !a[i] )
            {
                cat("     ")
                print( myTrapCollList[i] )
            }
    }
}


