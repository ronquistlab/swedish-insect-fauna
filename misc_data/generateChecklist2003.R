# This script generates checklist of taxa present in 2003 for target analysis taxa.
# Data comes from the "Checklist2003_2016-09-16.csv" file, which is a Dyntaxa dump
# with presence and absence of species in 2003 marked for the target taxa. Note that the
# target taxon names have to be changed so that they align with the names used
# in the other analysis files.

oldGroupNames <- c(
    "Acartophtalmidae",
    "Adeliinae",
    "Adelognathinae",
    "Aulacigastridae",
    "Banchini",
    "Brachycyrtinae",
    "Cheloninae",                     
    "Clusiidae",
    "Diacritinae",                    
    "Diplazontinae",
    "Dolichopodidae",                 
    "Drosophilidae",
    "Dryinidae + Embolemidae",        
    "Eupelmidae",
    "Figitidae excl. Charipinae",     
    "Heleomyzidae + Odiniidae",
    "Ichneumoninae excl. Phaeogenini",
    "Lauxaniidae",
    "Lonchaeidae",
    "Meteorini",
    "Milichiidae excl Phyllomyza",
    "Mycetophilidae + Keroplatidae",
    "Mymar",  
    "Netelia",
    "Phoridae",
    "Phrudus group",
    "Pimplinae",                 
    "Piophilidae",
    "Platygastridae (s str)",
    "Porricondylinae s lat",
    "Psocoptera",         
    "Rhyssinae",
    "Rogadinae",                     
    "Sciomyzidae",
    "Sepsidae",                    
    "small mycetophiloid families",
    "Symphyta",   
    "Tephritidae + Ulidiidae",
    "Thysanoptera" )

newGroupNames <- c(
    "Acartophthalmidae",
    "Adeliini",
    "Adelognathinae",
    "Aulacigastridae",
    "Banchini",
    "Brachycyrtinae",
    "Cheloninae",                     
    "Clusiidae",
    "Diacritinae",                    
    "Diplazontinae",
    "Dolichopodidae",                 
    "Drosophilidae",
    "Dryinidae+Embolemidae",        
    "Eupelmidae",
    "Figitidae excl Charipinae",     
    "Heleomyzidae+Odiniidae",
    "Ichneumoninae excl Phaeogenini",
    "Lauxaniidae",
    "Lonchaeidae",
    "Meteorini",
    "Milichiidae excl Phyllomyza",
    "Mycetophilidae+Keroplatidae",
    "Mymar",  
    "Netelia",
    "Phoridae",
    "Phrudus group",
    "Pimplinae",                 
    "Piophilidae",
    "Platygastridae (s str)",
    "Porricondylinae (s lat)",
    "Psocoptera",       
    "Rhyssinae",
    "Rogadinae",                    
    "Sciomyzidae",
    "Sepsidae",                 
    "Small mycetophiloid families",
    "Symphyta",   
    "Tephritidae+Ulidiidae",
    "Thysanoptera" )

# Important to specify that no quotes are used. If we read in as is, we avoid problems
# with replacing factors; we can do simple string replacement instead
D <- read.csv("Checklist2003_2016-09-16.csv", header=TRUE, sep=";", quote="", as.is=TRUE, fileEncoding="latin1")

# Make column names more international
colnames(D) <- c("TaxonId", "Subphylum", "Order", "Family", "Species", "SciName", "Author", "AnalysisTaxon", "Status.2003", "URL", "Occurrence.status")

# Get rid of non-species entries
D <- D[ D$Species != "", ]

# Get rid of taxa outside of target groups
# This actually loses two species that do not have absence or presence
# marked (one in Symphyta and one in Meteorini). They are assumed here
# to be absent.
D <- D[ D$Status.2003 != "", ]

for ( i in 1:length(D$Status.2003) )
{
    if ( D$Status.2003[i] == "ABSENT" )
        D$Status.2003[i] <- "absent"

    j <- match( D$AnalysisTaxon[i], oldGroupNames )

    if ( !is.na( j ) )
        D$AnalysisTaxon[i] <- newGroupNames[j]
}

# Now convert SciName and Species to factors (they should be identical)
D$Species <- factor(D$Species)
D$SciName <- factor(D$SciName)
D$AnalysisTaxon <- factor(D$AnalysisTaxon)

write.table( D, "checklist2003.tsv", sep="\t" )

