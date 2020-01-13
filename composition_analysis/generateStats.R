# Generate statistics from DynTaxa

# Read in DynTaxa data for Hexapoda
D <- read.csv( "Hexapoda-2017-02-08.csv", sep=";", fileEncoding="latin1")

# Get rid of "Ej paatraeffad" = levels(D$Svensk.forekomst)[7]
# this assumes that non-ASCII letter \ouml is translated to o in Svensk.forekomst
# We ignore the species with "" in Svensk.forekomst (five species).
D <- D[ D$Svensk.forekomst != levels(D$Svensk.forekomst)[7], ]
D$Svensk.forekomst <- factor( D$Svensk.forekomst )

# Add family for a few records with missing family information
D$Familj <- as.character( D$Familj)
j <- match( "Dimorphopterus spinolae", D$Art )
D$Familj[j] <- "Blissidae"
j <- match( "Ischnodemus sabuleti", D$Art )
D$Familj[j] <- "Blissidae"
j <- match( "Cymus claviculus", D$Art )
D$Familj[j] <- "Cymidae"
j <- match( "Cymus glandicolor", D$Art )
D$Familj[j] <- "Cymidae"
j <- match( "Cymus melanocephalus", D$Art )
D$Familj[j] <- "Cymidae"
D$Familj <- factor(D$Familj)

# Create subset for subfamily statistics
E <- D[ D$Familj == "Ichneumonidae" | D$Familj == "Braconidae" | D$Familj == "Staphylinidae", ]
E$Familj <- factor(E$Familj)
E$Underfamilj <- factor(E$Underfamilj)

# Create subsets for new species
X <- rbind( D[ grepl("200[3456789]", D$Auktor), ], D[ grepl("201[0123456789]", D$Auktor), ] )
Y <- rbind( E[ grepl("200[3456789]", E$Auktor), ], E[ grepl("201[0123456789]", E$Auktor), ] )

NumTotalSpecies <- summary(D$Familj, maxsum=700)
NumNewSpecies <- summary(X$Familj, maxsum=700)

res1 <- data.frame( NumTotalSpecies, NumNewSpecies )

write.csv( res1, "Familystats.txt" )

NumTotalSpecies <- summary(E$Underfamilj, maxsum=200)
NumNewSpecies <- summary(Y$Underfamilj, maxsum=200)

res2 <- data.frame( NumTotalSpecies, NumNewSpecies )

write.csv( res2, "Subfamilystats.txt" )

