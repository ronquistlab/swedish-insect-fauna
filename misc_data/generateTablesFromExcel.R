# Generate R-friendly tables from Excel files
# Usin the xlsx library

# Get the library
library(xlsx)

# Read the Swedish insect fauna data (sheet 1 in Table S1)
# Save it as swedish_insect_fauna.tsv
cat( "Generating tsv table from Table S1\n" )
Swedish_insect_fauna <- read.xlsx( "../S1_Table_Swedish_insect_fauna.xlsx", 1 )
Swedish_insect_fauna <- Swedish_insect_fauna[ 1:663, 1:17 ]
write.table( Swedish_insect_fauna, "swedish_insect_fauna.tsv" )


# Read the trap data (sheet 1 in Table S2)
# Save it as trap_data.tsv
cat( "Generating tsv tables from Table S2\n" )
Traps <- read.xlsx( "../S2_Table_SMTP_Trap_Data.xlsx", 1 )
write.table( Traps, "trap_data.tsv" )

# Read the collecting event data (sheet 2 in Table S2)
# Save it as coll_event_data.tsv
CollEvents <- read.xlsx( "../S2_Table_SMTP_Trap_Data.xlsx", 2 )
write.table( CollEvents, "coll_event_data.tsv" )


# Read the overview of data files (Table S3)
cat( "Generating tsv tables from Table S3\n" )
DataFiles <- read.xlsx( "../S3_Table_SMTP_Data_Files.xlsx", 1 )
DataFiles$Type <- rep( "quantitative", times=length(DataFiles$Order) )
for ( i in 1:length( DataFiles$Type ) )
{
    if ( DataFiles$Data.type[i] == "Incidence" )
        DataFiles$Type[i] <- "non-quantitative"
}
DataFiles <- DataFiles[ , c("Order", "Analysis.Taxon", "File.name", "Type") ] 
colnames(DataFiles) <- c( "Order", "AnalysisTaxon", "Filename", "Type" )

write.table ( DataFiles[ DataFiles$Type == "quantitative", ], "overview_quantitative_data.tsv" )
write.table ( DataFiles[ DataFiles$Type == "non-quantitative", ], "overview_non-quantitative_data.tsv" )


# Read the overview of analysis taxa (sheet 1 in Table S4)
# Save it as overview_analysis_taxa.tsv
cat( "Generating tsv table from Table S4\n" )

# First create vectors with old and new names to correct the group names
oldNames <- c( "Cecidomyiidae: Porricondylinae (s lat)", "Cheloninae excl Adeliini" )
newNames <- c( "Porricondylinae (s lat)", "Cheloninae" )

# Read in original data
D <- read.xlsx( "../S4_Table_Overview_Analysis_Taxa.xlsx", 1 )

# Lose extra columns
D <- D[ , 1:15 ]

# To match strings, D$Analysis.Taxon should be a string
# variable and not a factor variable
D$Analysis.Taxon <- as.character( D$Analysis.Taxon )

for ( i in 1:length(oldNames) )
{
    j <- match( oldNames[i], D$Analysis.Taxon)
    if ( is.na(j) )
        print("Error: Could not find ", oldNames[i] )
    else
        D$Analysis.Taxon[j] <- newNames[i]
}

# Convert niche and habitat classes so that they can be used without
# problems as column names (convert space and hyphen to period)
x <- as.character( D$Main.feeding.niche )
x <- gsub( "-", ".", x )
x <- gsub( " ", ".", x )
D$Main.feeding.niche <- factor(x)
x <- as.character( D$Main.feeding.habitat )
x <- gsub( "-", ".", x )
x <- gsub( " ", ".", x )
D$Main.feeding.habitat <- factor(x)

# Write tsv table for other scripts
write.table( D, "overview_analysis_taxa.tsv" )

# Read the Fauna Europaea taxon biology information (sheet 1 in Table S6)
# Save it as FaEu_taxon_bio_info.tsv
cat( "Generating tsv table from Table S6\n" )
FaEu_bio_info <- read.xlsx( "../S6_Table_Fauna_Europaea_Life_History_Info.xlsx", 1 )

# Convert niche and habitat classes so that they can be used without
# problems as column names (convert space and hyphen to period)
x <- as.character( FaEu_bio_info$Main.feeding.niche )
x <- gsub( "-", ".", x )
x <- gsub( " ", ".", x )
FaEu_bio_info$Main.feeding.niche <- factor(x)
x <- as.character( FaEu_bio_info$Main.feeding.habitat )
x <- gsub( "-", ".", x )
x <- gsub( " ", ".", x )
FaEu_bio_info$Main.feeding.habitat <- factor(x)

write.table( FaEu_bio_info, "FaEu_bio_info.tsv" )

