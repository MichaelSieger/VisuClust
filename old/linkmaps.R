linkmap <- function(X, D, linetypes=c(1,1,3), linecolors=c(1,1,1), linewidths=c(3,1,1),
						 labels = NULL, cluster = NULL, legendDigits = 2, ...) {

	library(aplpack)

	maps.format <- function(number){
		round(number, legendDigits)
	}

	maps.createLegendString <- function(v1, v2){
		paste(v1, "<= x <", v2)
	}

	maps.updateGUI <- function(){
		
		moved1 = (slider(no=1) == maps.s1)
		moved2 = (slider(no=2) == maps.s2)
		moved3 = (slider(no=3) == maps.s2)
		
		if(moved1){
		  if(slider(no=2) < slider(no=1)){
			  slider(set.no.value=c(2, slider(no=1)))
		  }
		  if(slider(no=3) < slider(no=1)){
			  slider(set.no.value=c(3, slider(no=2)))
		  }
		}
		if(moved2)
		{
		  if(slider(no=3) < slider(no=2)){
			  slider(set.no.value=c(3, slider(no=2)))
		  }
		}
		maps.s1 <<- slider(no=1)
		maps.s2 <<- slider(no=2)
		maps.s3 <<- slider(no=3)
	}

	maps.createCircleColors <- function(colors){
		colLen <- length(colors)
		revCol <- rep("black", colLen)
		#for(i in 1:colLen){
		#	revCol[(i+colLen/2)%%colLen] <- colors[i]
		#}
		revCol
	}

	maps.drawLegend <- function(){
		t1 <- maps.format(maps.s1)
		t2 <- maps.format(maps.s2)
		t3 <- maps.format(maps.s3)
		legend("topleft",  
				c(maps.createLegendString(0, t1), 
					maps.createLegendString(t1, t2), 
					maps.createLegendString(t2, t3)), 
					lwd=linewidths, lty=linetypes, col=linecolors)
	}

	maps.draw <- function(){
		maps.updateGUI()
		dev.hold()
		par(xpd=T, ask=F)
		plot(X, pch=21,col=maps.circleCols[cluster], bg = maps.clusterCols[cluster], ...)
		for(j in 1:maps.n){
			for(i in 1:j){
				if(i != j){
					if(D[i,j] < maps.s3){
						if(D[i,j] >= maps.s2){
							segments(X[i,1],X[i,2],X[j,1],X[j,2], lty=linetypes[3], col=linecolors[3], lwd=linewidths[3])
						}else if(D[i,j] >= maps.s1){
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
		maps.drawLegend()
		dev.flush()
	}

	maps.sliderCallback <- function(...){
		maps.draw()
	}

	maps.init <- function()
	{
		maps.n <<- length(X[,1])
		if(length(cluster) == 0)
		{
			cluster <- rep(1,maps.n)
		}
		nCluster <- length(unique(cluster))
		maps.clusterCols <<- rainbow(nCluster)
		maps.circleCols <<- maps.createCircleColors(maps.clusterCols)
		min <- min(D[D !=0])*0.9	#below smallest nonzero distance
		max <- max(D)
		maps.s1 <<- min
		maps.s2 <<- min
		maps.s3 <<- min

		slider(maps.sliderCallback, 
				sl.names=c("D1", "D2", "D3"),
				sl.mins=rep(min,3),
				sl.maxs=rep(max,3),
				sl.deltas=rep(0.1,3), 
				sl.defaults=rep(0,3),
				prompt=T,	
				title="Thresholds")
	}

	maps.init()
}




