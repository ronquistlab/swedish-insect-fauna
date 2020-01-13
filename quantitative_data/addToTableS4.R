D <- read.table("quant-combined-data.tsv")

x <- 1
y <- 1

Taxa <- levels(D$AnalysisTaxon)

for ( i in 1:length(Taxa) )
{
    taxon <- Taxa[i]

    E <- D[ D$AnalysisTaxon == taxon, ]    

    x[i] <- sum( E$Total )
    y[i] <- length( unique( E$EventID ) )

}

Y <- data.frame( taxon=Taxa, records=x, samples=y )
write.table( Y, "table_s4_addition.csv", sep=";", quote=FALSE)

