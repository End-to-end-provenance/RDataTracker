# Only select taxa for which tolerance values
#   have been computed. 
df1 <- merge(site.species.or, env.data.or, by = "SITE.ID")

mat1 <- as.matrix(df1[, names.match])
			
# First get total abundance
tot.abn <- apply(mat1, 1, sum)		
			
# Use matrix multiplication to compute the sum of all 
# observed tolerance values, and then divide by total 
# abundance to get the mean tolerance value.
mean.tv <- (mat1 %*% WA[names.match])/tot.abn		

plot(df1$temp, mean.tv, xlab = "Temperature", 
     ylab = "Mean weighted average")

