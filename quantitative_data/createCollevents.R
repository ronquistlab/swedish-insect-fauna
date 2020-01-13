# Use an arbitrary file
filename <- "SMTP_Dolichopodidae_1.xls"

# Extract the data we need
library(xlsx)
myColls <- read.xlsx( filename, 4 )

# Get rid of empty rows and columns
myColls <- myColls[ rowSums( is.na( myColls ) ) < ncol( myColls ),
                  colSums( is.na( myColls ) ) < nrow( myColls ) ]
myColls <- myColls[ !is.na( myColls$TrapID ), ]

# Write output file
write.table( myColls, "collevents.tsv", sep="\t" )

