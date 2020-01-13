# Generate overall richness estimates
# Using abundance data for all groups

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

# Read checklist from 2003
T <- read.table("../misc_data/checklist2003.tsv")

# Get the groups we should analyze
# Note that this also includes the taxa with
# no known species from 2003
G <- levels(T$AnalysisTaxon)

# Get rid of absent data
T <- T[ T$Status.2003 == "present", ]

# First, read in quantitative data and
# calculate species richness estimates
# using abundance-based, incidence-based
# and CMR methods

# Read in data
D <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Remove the groups that are not in G
E <- D[ D$AnalysisTaxon %in% G, ]

# Remove generic determinations
E <- E[ E$Species != "sp.", ]

# Remove extraneous traps
E <- E[ E$TrapID < 3000, ]

# Make compact representation of factor
E$SciName <- factor(E$SciName)

# Get list of known taxa 2003
known2003 <- T$SciName[ T$AnalysisTaxon %in% unique( E$AnalysisTaxon) ]

# Compute overall estimates
numSpec <- sum( E$Total )

numTraps <- length( unique( E$TrapID ) )
numSamples <- length( unique( E$EventID ) )

numSp2003 <- length( known2003 )

smtpSpecies <- unique( E$SciName )

numSpSmtp <- length( smtpSpecies )

numNewSmtp <- length( smtpSpecies[ is.na( match( smtpSpecies, known2003 ) ) ] ) 
    
numOldSmtp <- numSpSmtp - numNewSmtp

# Chapman "unbiased" estimator
mr <- ((numSp2003 + 1) * (numSpSmtp + 1)) / (numOldSmtp + 1) - 1

mytable <- xtabs( E$Total~E$SciName+E$TrapID )

x <- estimateR( rowSums( mytable ) )
chao1    <- x[["S.chao1"]]
chao1.se <- x[["se.chao1"]]
ace      <- x[["S.ACE"]]
ace.se   <- x[["se.ACE"]]
        
y <- prestonfit( rowSums( mytable ) )
x <- veiledspec( y )
preston  <- x[["Extrapolated"]]

battable <- t( mytable )
x1 <- alpha.accum( battable, runs=1, prog=FALSE )
S1       <- x1[dim(x1)[1],"S1"]
S2       <- x1[dim(x1)[1],"S2"]
chao1p   <- x1[dim(x1)[1],"Chao1P"]

# Add a row to the table
B <- data.frame( 
                 numSpec=numSpec,
                 numTraps=numTraps,
                 numSamples=numSamples,
                 numSp2003=numSp2003,
                 numSpSmtp=numSpSmtp,
                 numNewSmtp=numNewSmtp,
                 mr=mr, 
                 S1=S1,
                 S2=S2,
                 chao1=chao1, 
                 chao1.se=chao1.se, 
                 ace=ace, 
                 ace.se=ace.se, 
                 preston=preston,
                 chao1p=chao1p
               )

# Write table that opens automatically in Excel
write.table( B, "quantOverallRichnessEstimates.csv", sep=";", row.names=FALSE )

# Second, read in non-quantitative data and
# calculate species richness estimates
# using incidence-based and CMR methods

# Read in data
D <- read.table( "../non-quantitative_data/non-quant-combined-data.tsv" )

# Remove the groups that are not in G
E <- D[ D$AnalysisTaxon %in% G, ]

# Remove generic determinations
E <- E[ E$Species != "sp.", ]

# Remove extraneous traps
E <- E[ E$TrapID < 3000, ]

# Make compact representation of factor
E$SciName <- factor(E$SciName)

# Get list of known taxa 2003
known2003 <- T$SciName[ T$AnalysisTaxon %in% unique( E$AnalysisTaxon) ]

# Compute overall estimates
numObs <- length( E$Species )

numTraps <- length( unique( E$TrapID ) )
numSamples <- length( unique( E$EventID ) )

numSp2003 <- length( known2003 )

smtpSpecies <- unique( E$SciName )

numSpSmtp <- length( smtpSpecies )

numNewSmtp <- length( smtpSpecies[ is.na( match( smtpSpecies, known2003 ) ) ] ) 
    
numOldSmtp <- numSpSmtp - numNewSmtp

# Chapman "unbiased" estimator
mr <- ((numSp2003 + 1) * (numSpSmtp + 1)) / (numOldSmtp + 1) - 1

mytable <- xtabs( ~E$SciName+E$TrapID )

x <- estimateR( rowSums( mytable ) )
chao1    <- x[["S.chao1"]]
chao1.se <- x[["se.chao1"]]
ace      <- x[["S.ACE"]]
ace.se   <- x[["se.ACE"]]
        
y <- prestonfit( rowSums( mytable ) )
x <- veiledspec( y )
preston  <- x[["Extrapolated"]]

battable <- t( mytable )
x1 <- alpha.accum( battable, runs=1, prog=FALSE )
S1       <- x1[dim(x1)[1],"S1"]
S2       <- x1[dim(x1)[1],"S2"]
chao1p   <- x1[dim(x1)[1],"Chao1P"]

# Add a row to the table
B <- data.frame( 
                 numObs=numObs,
                 numTraps=numTraps,
                 numSamples=numSamples,
                 numSp2003=numSp2003,
                 numSpSmtp=numSpSmtp,
                 numNewSmtp=numNewSmtp,
                 mr=mr, 
                 S1=S1,
                 S2=S2,
                 chao1=chao1, 
                 chao1.se=chao1.se, 
                 ace=ace, 
                 ace.se=ace.se, 
                 preston=preston,
                 chao1p=chao1p
               )

# Write table that opens automatically in Excel
write.table( B, "non-quantOverallRichnessEstimates.csv", sep=";", row.names=FALSE )

# Now repeat procedure for quantitative data of known groups.

# Read in data
D <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Remove the groups that are in G
E <- D[ is.na( match( D$AnalysisTaxon, G) ), ]

# Remove two groups only recorded from a single trap
E <- E[ E$AnalysisTaxon != "Syrphidae", ]
E <- E[ E$AnalysisTaxon != "Pachyneuridae", ]

# Remove generic determinations
E <- E[ E$Species != "sp.", ]

# Remove extraneous traps
E <- E[ E$TrapID < 3000, ]

# Make compact representation of factor
E$SciName <- factor(E$SciName)

# Compute overall estimates
numSpec <- sum( E$Total )

numTraps <- length( unique( E$TrapID ) )
numSamples <- length( unique( E$EventID ) )

smtpSpecies <- unique( E$SciName )

numSpSmtp <- length( smtpSpecies )

mytable <- xtabs( E$Total~E$SciName+E$TrapID )

x <- estimateR( rowSums( mytable ) )
chao1    <- x[["S.chao1"]]
chao1.se <- x[["se.chao1"]]
ace      <- x[["S.ACE"]]
ace.se   <- x[["se.ACE"]]
        
y <- prestonfit( rowSums( mytable ) )
x <- veiledspec( y )
preston  <- x[["Extrapolated"]]

battable <- t( mytable )
x1 <- alpha.accum( battable, runs=1, prog=FALSE )
S1       <- x1[dim(x1)[1],"S1"]
S2       <- x1[dim(x1)[1],"S2"]
chao1p   <- x1[dim(x1)[1],"Chao1P"]

# Add a row to the table
B <- data.frame( 
                 numSpec=numSpec,
                 numTraps=numTraps,
                 numSamples=numSamples,
                 numSpSmtp=numSpSmtp,
                 S1=S1,
                 S2=S2,
                 chao1=chao1, 
                 chao1.se=chao1.se, 
                 ace=ace, 
                 ace.se=ace.se, 
                 preston=preston,
                 chao1p=chao1p
               )

# Write table that opens automatically in Excel
write.table( B, "quantOverallRichnessEstimatesKnownGroups.csv", sep=";", row.names=FALSE )


