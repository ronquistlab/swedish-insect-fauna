# Generic function for plotting squared devision of estimates against
# different parameters

sqdevPlot <- function( D, E, x1, x2, ... )
{
    plot( x1, D$chao1_sqdev_log, pch=0, col="blue", ... )
    points(x2,E$chao1_sqdev_log, pch=0, col="blue")
    points(x1,D$chao2_sqdev_log, pch=1, col="green")
    points(x2,E$chao2_sqdev_log, pch=1, col="green")
    points(x1,D$preston_sqdev_log, pch=2, col="red")
    points(x2,E$preston_sqdev_log, pch=2, col="red")
    points(x1,D$cne_sqdev_log, pch=3, col="black")
    points(x2,E$cne_sqdev_log, pch=3, col="black")
}

