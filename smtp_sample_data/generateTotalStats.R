# Script to estimate total number of specimens in the inventory catch.
#
# The input data are 16 samples for which all specimens of each taxon
# have been counted (in Excel file 'StatSamples_2017-09-21' and files
# derived therefrom), and the abundance data for the analysis groups.
#
# We use two methods. First, we simply compute the average number of
# specimens per counted sample and multiply with the total number of
# samples.
#
# We then use groups for which we have a fair amount of abundance data, 
# and that occur in a high proportion of samples. Specifically, we use
# Phoridae, Coleoptera, Trichoptera, Dolichopodidae and Drosophilidae.
#
# We try to predict the total number of specimens in the 16 reference
# samples from the number of specimens of each of these groups. It turns
# out that a loglin model with zero intercept allows the best predictive
# power. An intuitive explanation is that large number of specimens of a
# particular group needs to be taken with some skepticism. It could mean
# that the sample contains a large total number of specimens, but it could
# also partly be due to an unusual abundance of the particular group we
# are analyzing. The loglin model gives a reasonble fit, although there
# is a fair degree of variance, as one might expect.
#
# The results are summarized in the file 'result_summary.xlsx'.

# For an updated estimate of the total size of the catch, see:
#
#    Karlsson et al, Biodiversity Data Journal, "The Swedish Malaise
#       Trap Project:..."


# Read in transformed statistics data for the first-tier groups
Order <- read.table( "StatSamples_Order_2017-09-21.csv", sep=";", header=TRUE )

# Read in transformed statistics data for the second-tier Brachycera groups
Brachycera <- read.table( "StatSamples_Brachycera_2017-09-21.csv", sep=";", header=TRUE )

# Read in the data from the sorted samples
E <- read.csv( "total_smtp_sorting_data_2017-09-21.csv", header=TRUE )

# Read in the quantitative data
F <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Aggregate totals
total <- aggregate( Order$Specimens, by=list(Order$CollID), FUN=sum )

colnames(total) <- c( "CollID", "Total" )
total$CollID <- factor( total$CollID )

# Define stats function
stats <- function( x )
{
    avg <- mean( x )
    se <- sd( x ) / sqrt( length( x ) - 1 )

    a <- c( avg, avg + 2*se, avg + se, avg - se, avg - 2*se )
    return ( a )
}

# Define estimates function
estimates <- function( D, taxon, sampleFraction )
{
    cat( "Estimates for taxon ", taxon, "\n" )

    # Get the determined data
    A <- F[ F$AnalysisTaxon == taxon, ]
    A <- aggregate( A$Total, by=list(A$EventID), FUN=sum )
    colnames( A ) <- c( "CollID", "Specimens" )

    # Get the statistic sample data
    B <- D[ D$Taxon == taxon, ]
    B <- merge( B, total )
    B$Fraction <- B$Specimens / B$Total

    # Print some basic statistics
    cat( "Determined SMTP samples: ", length( A$CollID ), "\n" )
    cat( "Estimated fraction of SMTP samples: ", sampleFraction, "\n" )
    cat( "Summary of SMTP samples\n" )
    print( summary( A$Specimens ) )
    cat( "\n" )

    cat( "Statistics samples: ", sum( B$Specimens > 0 ), "\n" )
    cat( "Fraction of statistics samples: ", sum( B$Specimens > 0 ) / length( B$Specimens ), "\n" )
    cat( "Summary of non-zero statistics samples\n" )
    print( summary( B$Specimens[ B$Specimens > 0 ] ) )
    cat( "\n" )

    # Wilcox test to see if the statistics samples are typical
    cat( "Wilcox test\n" )
    print( wilcox.test( A$Specimens, B$Specimens[ B$Specimens > 0 ] ) )

    # T test to see if the statistics samples are typical
    cat( "T test\n" )
    print( t.test( A$Specimens, B$Specimens[ B$Specimens > 0 ] ) )

    # Plot lin-lin and lin-log prediction of total size
    x <- B$Specimens[ B$Specimens > 0 ]
    y <- B$Total[ B$Specimens > 0 ]
    fileName <- paste( taxon, "linlin.pdf", sep="" )
    pdf( fileName )
    plot( x, y, main=taxon )
    dev.off()
    fileName <- paste( taxon, "loglin.pdf", sep="" )
    pdf( fileName )
    plot( log(x), y, main=taxon )
    dev.off()

    # Fit a linlin model
    fit <- lm( y~x+0 )
    print( summary(fit) )
    k1 <- coefficients( fit ) [1]

    # Fit a loglin model
    fit <- lm( y~log(x) + 0 )
    print( summary(fit) )
    k2 <- coefficients( fit ) [1]

    # Use the group to estimate the total SMTP size
    estimatedCatch <- mean( A$Specimens ) * 1919 * sampleFraction
    x <- estimatedCatch / stats( B$Fraction ) / 1000000
    cat( "Estimated catch using all statistics samples (millions)\n" )
    print( x )
    cat( "\n" )
    cat( "Estimated catch using non-zero statistics samples and estimated SMTP sample fraction (millions)\n" )
    x <- ( mean( A$Specimens ) / stats( B$Fraction[ B$Specimens > 0 ] ) ) * 1919  / 1000000
    print( x )
    cat( "\n" )
    cat( "Estimated catch using linlin model\n" )
    x <- k1 * mean( A$Specimens ) * 1919 / 1000000
    print( x )
    cat( "\n" )
    cat( "Estimated catch using loglin model\n" )
    x <- k2 * mean( log(A$Specimens) ) * 1919 / 1000000
    print( x )
    cat( "\n" )

}

# Use total number specimens for statistics samples to compute total
cat( "Estimate based on total in stats samples\n" )
x <- stats( total$Total ) * 1919 / 1000000
y <- c ( x[1], x[5], x[4], x[3], x[2] )
cat( y, "\n\n" )

# Use Phoridae from Order
estimates( Order, "Phoridae", 0.97 )

# Use Coleoptera from Order
estimates( Order, "Coleoptera", 0.94 )

# Use Trichoptera from Order
estimates( Order, "Trichoptera", 0.62 )

# Use Dolichopodidae from Brachycera
estimates( Brachycera, "Dolichopodidae", 0.77 )

# Use Drosophilidae from Brachycera
estimates( Brachycera, "Drosophilidae", 0.77 )


