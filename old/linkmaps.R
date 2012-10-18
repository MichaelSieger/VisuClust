linkmap <- function(X, D, linetypes=c(1,1,3), linecolors=c(1,1,1), linewidths=c(3,1,1),
						 labels = NULL, cluster = NULL, ...) {

	library(rpanel)

	maps.format <- function(number){
		round(number, 2)
	}

	maps.createLegendString <- function(v1, v2){
		paste(v1, "<= x <", v2)
	}

	maps.updateGUI <- function(panel){
		
	}

	maps.createCircleColors <- function(colors){
		colLen <- length(colors)
		revCol <- rep("black", colLen)
		#for(i in 1:colLen){
		#	revCol[(i+colLen/2)%%colLen] <- colors[i]
		#}
		revCol
	}

	maps.drawLegend <- function(s1, s2, s3){
		t1 <- maps.format(s1)
		t2 <- maps.format(s2)
		t3 <- maps.format(s3)
		legend("topleft",  
				c(maps.createLegendString(0, t1), 
					maps.createLegendString(t1, t2), 
					maps.createLegendString(t2, t3)), 
					lwd=linewidths, lty=linetypes, col=linecolors)
	}

	maps.draw <- function(panel){
		maps.updateGUI(panel)
		dev.hold()
		par(xpd=T, ask=F)
		plot(X, pch=21,col=circleCols[cluster], bg = clusterCols[cluster], ...)
		xRange <- max(X[,1])-min(X[,1])
		yRange <- max(X[,2])-min(X[,2])
		s1 <- panel$s1
		s2 <- panel$s2
		s3 <- panel$s3
		if(length(s1) == 0){
			s1 <- 0
		}
		if(length(s2) == 0){
			s2 <- 0
		}
		if(length(s3) == 0){
			s3 <- 0
		}
		if(s2 < s1){
			s2 <- s1
		}
		if(s3 < s2){
			s3 <- s2
		}
		for(j in 1:n){
			for(i in 1:j){
				if(i != j){
					if(D[i,j] < s3){
						if(D[i,j] >= s2){
							segments(X[i,1],X[i,2],X[j,1],X[j,2], lty=linetypes[3], col=linecolors[3], lwd=linewidths[3])
						}else if(D[i,j] >= s1){
							segments(X[i,1],X[i,2],X[j,1],X[j,2], lty=linetypes[2], col=linecolors[2], lwd=linewidths[2])
						}else{
							segments(X[i,1],X[i,2],X[j,1],X[j,2], lty=linetypes[1], col=linecolors[1], lwd=linewidths[1])
						}
					}
				}
			}
		}
		if(length(labels)!=0)
		{
			relDist = 0.02
			text(X[,1]+xRange*relDist,X[,2]+yRange*relDist,labels)
		}
		maps.drawLegend(s1, s2, s3)
		dev.flush()
		panel
	}

	n <- length(X[,1])
	if(length(cluster) == 0)
	{
		cluster <- rep(1,n)
	}
	nCluster <- length(unique(cluster))
	clusterCols <- rainbow(nCluster)
	circleCols <- maps.createCircleColors(clusterCols)


	min <- min(D[D !=0])*0.9	#below smallest nonzero distance
	max <- max(D)
	panel <- rp.control(title = "Thresholds", slo=0.5, int=1.0, size=c(400, 400))
	rp.slider(panel, var=s1,  from=min, to=max, title="D1", action=maps.draw, pos=c(5, 5, 290, 70), showvalue=TRUE, log=T)
	rp.slider(panel, var=s2, from=min, to=max, title="D2", action=maps.draw, pos=c(5, 70, 290, 90), showvalue=TRUE, log=T)
	rp.slider(panel, var=s3, from=min, to=max, title="D3", action=maps.draw, pos=c(5, 135, 290, 110), showvalue=TRUE, log=T)

}