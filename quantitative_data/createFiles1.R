# Get a list of the files we want to work on
# Only keep files ending in ".xls"
filenames <- dir( pattern="*.xls")

# Name the columns we need
columnsNeeded <- c( "Genus", "Species", "Author", "Total", "TrapID", "EventID", "StartDate" )

# Cycle over files and extract the data we need
library(xlsx)
for ( i in 1:1 ) {

    filenames[i] <- "SMTP_Phoridae3.xls"

    cat(i, " -- ")
    print(filenames[i])

    # Read the second sheet (Observation)
    myData <- read.xlsx( filenames[i], 2 )

    # Get rid of empty rows and columns
    myData <- myData[ rowSums( is.na( myData ) ) < ncol( myData ),
                      colSums( is.na( myData ) ) < nrow( myData ) ]
    myData <- myData[ !is.na( myData$Genus ), ]

    # Extract just the columns needed
    myData <- myData[ , columnsNeeded ]

    # Read the fifth sheet (TaxonList)
    myTaxa <- read.xlsx( filenames[i], 5 )

    # Get rid of empty rows and columns
    myTaxa <- myTaxa[ rowSums( is.na( myTaxa ) ) < ncol( myTaxa ),
                      colSums( is.na( myTaxa ) ) < nrow( myTaxa ) ]
    myTaxa <- myTaxa[ !is.na( myTaxa$Genus ), ]

    # Create output file names
    obsFile <- paste( substr( filenames[i],1,nchar(filenames[i])-4 ), "obs.Rdata", sep="-" )
    taxFile <- paste( substr( filenames[i],1,nchar(filenames[i])-4 ), "taxa.Rdata", sep="-" )

    # Write output files
    write.table( myData, obsFile, sep="\t" )
    write.table( myTaxa, taxFile, sep="\t" )
}


