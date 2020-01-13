# Generate species accumulation plots for selected groups

# Select taxa to plot here
G <- c( "Dolichopodidae", "Phoridae", "Drosophilidae",
        "Ichneumoninae excl Phaeogenini", "Heleomyzidae+Odiniidae", "Sepsidae",
        "Platygastridae (s str)", "Symphyta", "Diplazontinae" )
Expert <- c( 360, 1100, 80, 490, 80, 38, 270, 799, 90 )
MR <- c( 370, 1306, 66, 413, 82, 36, 282, 717, 79 )
CNE <- c( 246, 1982, 106, 373, 84, 16, 267, 318, 73 )
Title <- G
Title[4] <- "Ichneumoninae (part)"

# Read data
D <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Prepare figure
pdf( "Figure_5.pdf" )
par( mfrow = c(3,3) )

j <- 1
for ( i in 1:length( G ) )
{
    # Print group we are working on
    cat( "Plotting", G[i], "\n" )

    # Get subset for this group but remove generic determinations
    E <- D[ D$AnalysisTaxon == G[i] & D$Species != "sp.", ]

    # Also remove extraneous traps
    E <- E[ E$TrapID < 3000, ]

    # Make compact representation of factor
    E$SciName <- factor(E$SciName)
    E$TrapID <- factor(E$TrapID)

    # Make a cross tabulation of scientific name and trap ID
    mytable <- xtabs( E$Total~E$SciName+E$TrapID )

    # x is easy
    x <- 1:ncol( mytable )

    # Permute the accumulation order to get y
    y <- numeric( ncol( mytable ) )
    y1 <- y  # use to store y values from one rep
    numReps <- 1000
    for ( n in 1:numReps )
    {
        indexVec <- sample( 1:ncol( mytable ) )

        x1 <- numeric( nrow( mytable ) )  # use to store numspecimens of each species
        numSites <- 0
        for ( j in indexVec )
        {
            x1 <- x1 + mytable[ , j ]
            numSites <- numSites + 1
            y1[numSites] <- sum( x1 > 0 )
        }
        y <- y + y1
    }
    y <- y / numReps

    ymax <- max( Expert[i], MR[i], CNE[i] )
    plot( x, y, ylim=c(min(y),ymax), xlab="Sites", ylab="No. species", type="l", main=Title[i] )
    abline( a=Expert[i], b=0, lty=2 )
    abline( a=MR[i], b=0, lty=3 )
    abline( a=CNE[i], b=0, lty=4 )

    if ( i == 2 )
    {
        text( 5, Expert[i], "Expert", pos=1, offset=0.2, cex=0.8 )
        text( 5, MR[i], "MR", pos=3, offset=0.2, cex=0.8 )
        text( 5, CNE[i], "CNE", pos=1, offset=0.2, cex=0.8 )
    }
}

dev.off()

