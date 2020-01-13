# Read in the plot data
load( "FaEuPlotData.RData" )

# Now we are ready to plot

# Generate data and figure for species-area relationship
# and latitudinal gradient in species richness
print("Analyzing species area relationship and latitudinal gradient")
pdf("Figure_10.pdf", useDingbats=FALSE)
par( mfrow=c(2,2) )

# Analyze species-area regression and save residuals
y <- log( FaEuTaxon$Total )
x <- log( FaEuTaxon$Area )
fit <- lm( y ~ x )
species.area.res <- resid( fit )
print( summary( fit ) )
plot( x, y, xlab="Area (log)", ylab="Number of species (log)" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
text( -1.2, 10.3, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2, p=p)), pos=4, cex=0.80 )
mtext("(A)", line=3, side=1, adj=0, cex=1.2)

# Analyze residuals
# The plot of residuals shows that there are three extreme
# outliers with very poorly known or otherwise unexpectedly small
# faunas. They are excluded in the following plots. The three
# outliers are Iceland (IS, index 17), San Marino (SM, index 32) and
# Belarus (BY, index 7). We exclude them by using negative
# indexing. We define two include sets in case we want to use
# different includes in the two different figures.
includes1 <- c(-17,-32,-7)
includes2 <- c(-17,-32,-7)

# Analyze and plot all residuals
y <- species.area.res
x <- log( FaEuTaxon$Area )
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab="Area (log)", ylab="Residual")
abline( h=0 )
for ( i in includes2 )
{
    index <- -i
    text( x[index], y[index], rownames(FaEuTaxon)[index], pos=3, cex=0.80 )
}
mtext("(B)", line=3, side=1, adj=0, cex=1.2)

# Analyze latitudinal gradient
y <- species.area.res
x <- FaEuTaxon$Latitude
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab=expression(paste("Latitude (", degree, "N)", sep="")), ylab="Residual" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
text( 34, -2.65, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2, p=p)), pos=4, cex=0.80 )
for ( i in includes2 )
{
    index <- -i
    text( x[index], y[index], rownames(FaEuTaxon)[index], pos=3, cex=0.80 )
}
mtext("(C)", line=3, side=1, adj=0, cex=1.2)

# The last block checks whether there is still a latitudinal gradient
# when the outliers are removed.
y <- species.area.res[ includes2 ]
x <- FaEuTaxon$Latitude[ includes2 ]
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab=expression(paste("Latitude (", degree, "N)", sep="")), ylab="Residual" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
text( 63, 0.695, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2, p=p)), pos=2, cex=0.80 )
mtext("(D)", line=3, side=1, adj=0, cex=1.2)

# The last block checks whether there is still a latitudinal gradient
# when only Iceland is removed.
y <- species.area.res[ -17 ]
x <- FaEuTaxon$Latitude[ -17 ]
fit <- lm( y~x )
print( summary( fit ) )

dev.off()

# Analyze and plot latitudinal trends in composition
print("Analyzing latitudinal trends in composition")
pdf("Figure_11.pdf", useDingbats=FALSE)
par( mfrow=c(2,2) )

# Latitudinal trend in prop. Diptera
y <- FaEuTaxon$PropDiptera[ includes1 ]
x <- FaEuTaxon$Latitude[ includes1 ]
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab=expression(paste("Latitude (", degree, "N)", sep="")), ylab="Fraction", main="Diptera" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
text( 34, 0.298, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2, p=p)), pos=4, cex=0.80 )
mtext("(A)", line=3, side=1, adj=0, cex=1.2)

# Latitudinal trend in prop. Hymenoptera
y <- FaEuTaxon$PropHymenoptera[ includes1 ]
x <- FaEuTaxon$Latitude[ includes1 ]
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab=expression(paste("Latitude (", degree, "N)", sep="")), ylab="Fraction", main="Hymenoptera" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
text( 34, 0.295, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2, p=p)), pos=4, cex=0.80 )
mtext("(B)", line=3, side=1, adj=0, cex=1.2)


# Latitudinal trend in decomposers
y <- FaEuNiche$PropSaprophagous[ includes1 ]
x <- FaEuNiche$Latitude[ includes1 ]
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab=expression(paste("Latitude (", degree, "N)", sep="")), ylab="Fraction", main="Decomposers" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
r2.valstring <- sprintf("%.2f", r2)
text( 63, 0.40, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2.valstring, p=p)), pos=2, cex=0.80 )
mtext("(C)", line=3, side=1, adj=0, cex=1.2)

# Latitudinal trend in parasitoids
y <- FaEuNiche$PropParasitoids[ includes1 ]
x <- FaEuNiche$Latitude[ includes1 ]
fit <- lm( y~x )
print( summary( fit ) )
plot( x, y, xlab=expression(paste("Latitude (", degree, "N)", sep="")), ylab="Fraction", main="Parasitoids" )
abline( fit )
r2 <- round( summary(fit)$r.squared, 2 )
p <- signif( summary(fit)$coefficients[2,4], 2 )
text( 34, 0.265, substitute(paste( r^2, "=", r2, ", p=", p, sep="" ), list(r2=r2, p=p)), pos=4, cex=0.80 )
mtext("(D)", line=3, side=1, adj=0, cex=1.2)

dev.off()

