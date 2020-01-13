# This script reads sheet 2 (observations) and 5 (taxa) of the
# xls data files in the directory, then writes the required
# data from each file into two new files in tsv format. The
# script assumes that the xls files contain incidence data.

# For unknown reasons, the xlsx library fails on reading some
# cells containing Excel formulas. This means that the Author
# field of the observation sheet is sometimes read as "0" although
# it does contain correct information as displayed by Excel.
# This is corrected by the script "correctFiles.R".

# Only keep files ending in ".xls"
filenames <- dir( pattern="*.xls")

# Name the columns we need
columnsNeeded <- c( "Genus", "Species", "Author", "TrapID", "EventID" )

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
    obsFile <- paste( substr( filenames[i],1,nchar(filenames[i])-4 ), "obs.Rdata", sep="-" )
    taxFile <- paste( substr( filenames[i],1,nchar(filenames[i])-4 ), "taxa.Rdata", sep="-" )

    # Write output files
    write.table( myData, obsFile, sep="\t" )
    write.table( myTaxa, taxFile, sep="\t" )
}


