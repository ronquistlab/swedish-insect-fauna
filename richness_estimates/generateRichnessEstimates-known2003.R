# Generate richness estimates
# for poorly known groups but 
# only for species known 2003

# Comments on data transformations
# We use the following files
#
# - 'checklist2003.tsv'
# This file contains data on the species known in
# 2003 per group ('AnalysisTaxon') where new species
# have been found in the SMTP
#
# - 'quant-combined-data.tsv'
# Contains the cleaned quantitative data
#
# - 'non-quant-combined-data.tsv'
# Contains the cleaned non-quantitative data
#

library(vegan)
library(BAT)
source("cneFxn.R")

# Read checklist from 2003
T <- read.table("../misc_data/checklist2003.tsv")

# Get rid of absent data
T <- T[ T$Status.2003 == "present", ]

# Get the groups we should analyze
G <- levels(T$AnalysisTaxon)

# Cycle through quantitative data and
# calculate species richness estimates
# using abundance-based and incidence-based
# methods for known species

D <- read.table( "../quantitative_data/quant-combined-data.tsv" )
j <- 1
for ( i in 1:length( G ) )
{
    # Print group we are working on
    print( G[i] )

    # Get list of known taxa 2003
    known2003 <- T$SciName[ T$AnalysisTaxon == G[i] ]

    # Get subset for this group but remove generic determinations
    E <- D[ D$AnalysisTaxon == G[i] & D$Species != "sp.", ]

    # Remove extraneous traps
    E <- E[ E$TrapID < 3000, ]

    # Get subset for this group that was known 2003
    E <- E[ !is.na( match( E$SciName, known2003) ), ]

    # Make compact representation of factor
    E$SciName <- factor(E$SciName)

    # Now get the numbers we need
    if ( length(E$SciName) > 0 )
    {
        numSpec <- sum( E$Total )
        numSiteObs <- length( unique( paste( E$SciName, E$TrapID, sep=" " ) ) )

        numTraps <- length( unique( E$TrapID ) )
        numSamples <- length( unique( E$EventID ) )

        numSp2003 <- length( known2003 )

        smtpSpecies <- unique( E$SciName )

        numSpSmtp <- length( smtpSpecies )

        mytable <- xtabs( E$Total~E$SciName+E$TrapID )

        x <- estimateR( rowSums( mytable ) )
        chao1    <- x[["S.chao1"]]
        chao1.se <- x[["se.chao1"]]
        ace      <- x[["S.ACE"]]
        ace.se   <- x[["se.ACE"]]
        
        x <- specpool( t(mytable) )
        chao2    <- x[["chao"]]
        chao2.se <- x[["chao.se"]]
        jack1    <- x[["jack1"]]
        jack1.se <- x[["jack1.se"]]
        jack2    <- x[["jack2"]]
        boot     <- x[["boot"]]
        boot.se  <- x[["boot.se"]]

        y <- prestonfit( rowSums( mytable ) )
        x <- veiledspec( y )
        preston  <- x[["Extrapolated"]]

        cne <- cneFxn( mytable )

        battable <- t( mytable )
        x1 <- alpha.accum( battable, runs=1 )
        S1       <- x1[dim(x1)[1],"S1"]
        S2       <- x1[dim(x1)[1],"S2"]
        Q1       <- x1[dim(x1)[1],"Q1"]
        Q2       <- x1[dim(x1)[1],"Q2"]
        chao1p   <- x1[dim(x1)[1],"Chao1P"]
        chao2p   <- x1[dim(x1)[1],"Chao2P"]
        jack1p   <- x1[dim(x1)[1],"Jack1inP"]
        jack2p   <- x1[dim(x1)[1],"Jack2inP"]

        # Add a row to the table
        B <- data.frame( 
                         group=G[i],
                         numSpec=numSpec,
                         numSiteObs=numSiteObs,
                         numTraps=numTraps,
                         numSamples=numSamples,
                         numSp2003=numSp2003,
                         numSpSmtp=numSpSmtp,
                         S1=S1,
                         S2=S2,
                         Q1=Q1,
                         Q2=Q2,
                         chao1=chao1, 
                         chao1.se=chao1.se, 
                         ace=ace, 
                         ace.se=ace.se, 
                         chao2=chao2,
                         chao2.se=chao2.se,
                         jack1=jack1,
                         jack1.se=jack1.se,
                         jack2=jack2,
                         boot=boot,
                         boot.se=boot.se,
                         preston=preston,
                         cne=cne,
                         chao1p=chao1p,
                         chao2p=chao2p,
                         jack1p=jack1p,
                         jack2p=jack2p
                       )
        if ( j == 1 )
        {
            A <- B
            j <- 2
        }
        else
            A <- rbind( A, B )
    }
}

# Write table for Excel
write.table( A, "quantRichnessEstimates-known2003.csv", sep=";", row.names=FALSE )

# Now, cycle through non-quantitative data and
# calculate species richness estimates
# using incidence-based methods

D <- read.table( "../non-quantitative_data/non-quant-combined-data.tsv" )
j <- 1
for ( i in 1:length( G ) )
{
    # Print group we are working on
    print( G[i] )

    # Get list of known taxa 2003
    known2003 <- T$SciName[ T$AnalysisTaxon == G[i] ]

    # Get subset for this group but remove generic determinations
    E <- D[ D$AnalysisTaxon == G[i] & D$Species != "sp.", ]

    # Remove extraneous traps
    E <- E[ E$TrapID < 3000, ]

    # Get subset for this group that was known 2003
    E <- E[ !is.na( match( E$SciName, known2003 ) ), ]

    # Make compact representation of factor
    E$SciName <- factor(E$SciName)

    # Now get the numbers we need
    if ( length(E$SciName) > 0 )
    {
        numObs <- length( E$SciName )
        numSiteObs <- length( unique( paste( E$SciName, E$TrapID, paste=" " ) ) )

        numTraps <- length( unique( E$TrapID ) )
        numSamples <- length( unique( E$EventID ) )

        numSp2003 <- length( known2003 )

        smtpSpecies <- unique( E$SciName )

        numSpSmtp <- length( smtpSpecies )

        mytable <- xtabs( ~E$SciName+E$TrapID )

        x <- specpool( t(mytable) )
        chao2    <- x[["chao"]]
        chao2.se <- x[["chao.se"]]
        jack1    <- x[["jack1"]]
        jack1.se <- x[["jack1.se"]]
        jack2    <- x[["jack2"]]
        boot     <- x[["boot"]]
        boot.se  <- x[["boot.se"]]

        battable <- t( mytable )
        x1 <- alpha.accum( battable, runs=1 )
        S1       <- x1[dim(x1)[1],"S1"]
        S2       <- x1[dim(x1)[1],"S2"]
        Q1       <- x1[dim(x1)[1],"Q1"]
        Q2       <- x1[dim(x1)[1],"Q2"]
        chao1p   <- x1[dim(x1)[1],"Chao1P"]
        chao2p   <- x1[dim(x1)[1],"Chao2P"]
        jack1p   <- x1[dim(x1)[1],"Jack1inP"]
        jack2p   <- x1[dim(x1)[1],"Jack2inP"]

        # Add a row to the table
        B <- data.frame( 
                         group=G[i],
                         numObs=numObs,
                         numSiteObs=numSiteObs,
                         numTraps=numTraps,
                         numSamples=numSamples,
                         numSp2003=numSp2003,
                         numSpSmtp=numSpSmtp,
                         Q1=Q1,
                         Q2=Q2,
                         chao2=chao2,
                         chao2.se=chao2.se,
                         jack1=jack1,
                         jack1.se=jack1.se,
                         jack2=jack2,
                         boot=boot,
                         boot.se=boot.se,
                         chao2p=chao2p,
                         jack1p=jack1p,
                         jack2p=jack2p
                       )
        if ( j == 1 )
        {
            A <- B
            j <- 2
        }
        else
            A <- rbind( A, B )

    }
}

# Write table for Excel
write.table( A, "non-quantRichnessEstimates-known2003.csv", sep=";", row.names=FALSE )

