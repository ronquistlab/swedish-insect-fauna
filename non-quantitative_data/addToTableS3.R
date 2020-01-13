D <- read.table("overview_non-quantitative_data.csv", sep=";",header=TRUE)

x <- 1
y <- "A"

for ( i in 1:length(D$Filename) )
{
    # Save file name
    y[i] <- as.character( D$Filename[i] )

    print( y[i] )

    # Get observation data file name
    obsFile <- paste( substr( y[i],1,nchar(y[i])-4 ), "obs.tsv", sep="-" )

    E <- read.table(obsFile)

    x[i] <- length( E$Species )
}

Y <- data.frame( file=factor(y), records=x )
write.table( Y, "table_s3_addition.csv", sep=";",quote=FALSE )

