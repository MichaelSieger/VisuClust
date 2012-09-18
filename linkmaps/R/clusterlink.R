clusterlink <- function(X, cluster, ...)
	{
		colors = c("red", "green", "blue", "magenta", 
					"yellow",  "purple", "brown")

		dev.hold()
		plot(X, ...)
		nclust = length(unique(cluster))
		n = length(cluster)
		for(i in 1:nclust)
		{
			m = X
			m[cluster != i] = NaN
			l = length(m[,1])
			for(j in 1:l)
			{
				for(k in 1:l)
				{
					if(k < j && !is.na(m[j,1]) && !is.na(m[k, 1]))
					{
						segments(m[j,1], m[j,2], m[k,1], m[k, 2], col=colors[i])
					}
				}
			}
		}
		dev.flush()

	}