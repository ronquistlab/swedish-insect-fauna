# This script reads sheet 2 (observations) and 5 (taxa) of the
# xls data files in the directory, then writes the required
# data from each file into two new files in tsv format. The
# script assumes that the xls files contain abundance data.

# For unknown reasons, the xlsx library fails on reading some
# cells containing Excel formulas. This means that the Author
# and StartDate fileds of the observation sheet are sometimes
# read as "0" although they do contain correct information
# as displayed by Excel. This is corrected later by the
# script "correctFiles.R" for Author. StartDate is
# dropped later because it is not needed, so the problems
# reading this field are ignored.

# Get a list of the files we want to work on
# Only keep files ending in ".xls"
filenames <- dir( pattern="*.xls")

# Name the columns we need
columnsNeeded <- c( "Genus", "Species", "Author", "Total", "TrapID", "EventID", "StartDate" )

# Cycle over files and extract the data we need
library(xlsx)
for ( i in 1:length(filenames) ) {

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
    obsFile <- paste( substr( filenames[i],1,nchar(filenames[i])-4 ), "obs.tsv", sep="-" )
    taxFile <- paste( substr( filenames[i],1,nchar(filenames[i])-4 ), "taxa.tsv", sep="-" )

    # Write output files
    write.table( myData, obsFile, sep="\t" )
    write.table( myTaxa, taxFile, sep="\t" )
}


