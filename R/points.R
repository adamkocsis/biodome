#' Generate random presences based on the Kent distribution
#'
#' The function is a wrapper around the \code{rkent} function in the \code{Directional} package.
#'
#' @param Numeric A vector with the number of points to be generated in one distribution
#' @param kappa A vector of kappa parameters of the Kent distribution. kappa = a indicates a uniform distribution, the higher kappa is, the concentrated the presences.
#' @param centers The center points around which the points are to created. If \code{NULL}, then these will be randomized.
#' @param beta The ovalness parameter of the Kent distribution
#' @param drop If there is a single distribution to be created, should this ve a single matrix, rather than a list with one element?
#' @return A list with with as many elements, as the length of n. Each element will be a matrix of point coordinates.
#' @export
kentPresence <- function(n, kappa, centers=NULL, beta=0, drop=TRUE){
	if(!requireNamespace("Directional", quietly=TRUE)) stop("This function requires the 'Directional' package.")


	# the number of pointsets to simulate
	s <- length(n)

	# the number of species
	if(length(kappa)==1) kappa <- rep(kappa, s)

	# if centers not given, do it randomly
	if(is.null(centers)){
		centers <- icosa::rpsphere(s, output="polar")
	}

	# the centers on a unit sphere
	unitCenters <- icosa::PolToCar(centers, radius=1)

	# generate as man
	pointSet <- list()

	for(i in 1:s){

		# points along a great circle
		unitPoints <- Directional::rkent(n=n[i], k=kappa[i], m=as.numeric(unitCenters[i,,drop=FALSE]), b=beta)

		# back to longitude and latitude
		pointSet[[i]]  <- icosa::CarToPol(unitPoints)[,c(1,2)]
	}

	if(s==1 & drop){
		return(pointSet[[1]])
	}else{
		return(pointSet)
	}

}
