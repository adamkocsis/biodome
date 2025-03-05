# Demo script to show the range estimator plans in biodome
# Á.T. Kocsis 2025-03-05
# CC-BY

library(biodome)

########################################
# Example 1. Data from OBIS - nice behavior!
########################################
# 1. Canvas
hex <- hexagrid(deg=5, sf=TRUE)
plot(hex, reset=FALSE, xlim=c(-15, 40), ylim=c(25, 63))

# 2. Records
data(pinna)

# just the coordinates
coordMat <- SimpleCoordinates(pinna, long="decimallongitude", lat="decimallatitude")
points(coordMat)

# 3. calculate and visualize
cent <- ranges_centroid_points(coordMat, plot=TRUE)

# secondary visualization
# points(cent[1], cent[2])

########################################
# grid cell occupancy
occ <- ranges_occupancy(coordMat, icosa=hex, plot=TRUE)

# plot(hex, occ$cells, add=TRUE, col="green")

########################################
# maximum great circle distance cell occupancy
mgcd <- ranges_mgcd(coordMat, plot=TRUE)
# single line visualization
# arcs(coordMat[mgcd$index, ])

########################################
# Centroid distances
centDist <- ranges_centroid_distance(coordMat, plot=TRUE)

# lines(rbind(centDist$centroid,
# 		coordMat[which(centDist$estimate==centDist$distances)[1],]),
# 		col="blue", lwd=3)

########################################
# the latitudinal range
latrange <- ranges_latrange(coordMat, plot=TRUE)

# abline(h=latrange$range, lty=2)

########################################
mst <- ranges_mstlength(coordMat, plot=TRUE)
# lines(mst$show)

########################################
planarChull <- ranges_chullplane(coordMat, gcbound=FALSE, plot=TRUE)
# plot(planarChull$sf, col="#55000033", add=TRUE)

planarChullGC <- ranges_chullplane(coordMat, gcbound=TRUE, plot=TRUE)
# plot(planarChullGC$sf, col="#00550033", add=TRUE)

########################################
sphericalChull <- ranges_chullsphere(coordMat, plot=TRUE)
# plot(sphericalChull$sf, col="#55000033", add=TRUE)



########################################
# Example 2. Procuedural data highlighting some issues
########################################
# coarser grid
hex <- hexagrid(deg=5, sf=TRUE)

# controls only the centroid, but not the actual distribution generation
set.seed(5)
coordMat <- kentPresence(100, kappa=5)

plot(hex, border="gray")
points(coordMat, pch=3, col="blue")


########################################
# What works fine so far:
########################################

# centroids
cent <- ranges_centroid_points(coordMat, plot=TRUE)


# secondary visualization
# points(cent[1], cent[2])

########################################
# grid cell occupancy
occ <- ranges_occupancy(coordMat, icosa=hex, plot=TRUE)

# plot(hex, occ$cells, add=TRUE, col="green")

########################################
# maximum great circle distance cell occupancy
# this can create interesting results
mgcd <- ranges_mgcd(coordMat, plot=TRUE)

# single line visualization
# arcs(coordMat[mgcd$index, ])

########################################
# Centroid distances
centDist <- ranges_centroid_distance(coordMat, plot=TRUE)

# lines(rbind(centDist$centroid,
# 		coordMat[which(centDist$estimate==centDist$distances)[1],]),
# 		col="blue", lwd=3)

#####################################
# the latitudinal range
latrange <- ranges_latrange(coordMat, plot=TRUE)

# abline(h=latrange$range, lty=2)

########################################
mst <- ranges_mstlength(coordMat, plot=TRUE)
arcs(mst$show)

########################################
# What does not work!
########################################
sf::sf_use_s2(FALSE)
planarChull <- ranges_chullplane(coordMat, gcbound=FALSE, plot=TRUE)
# plot(planarChull$sf, col="#55000033", add=TRUE)

# totally fucked up
planarChullGC <- ranges_chullplane(coordMat, gcbound=TRUE, plot=TRUE)
# plot(planarChullGC$sf, col="#00550033", add=TRUE)

########################################
sf::sf_use_s2(TRUE)
sphericalChull <- ranges_chullsphere(coordMat, plot=TRUE)

# plot(sphericalChull$sf, col="#55000033", add=TRUE)
