# Compute area under the ROC curve

# Define storage vector for ROC
roc <- rep(NA, times = length(taxa.names))	

for (i in 1:length(taxa.names)) {
	# Compute mean predicted probability of occurrence
  predout <- predict(modlist.glm[[i]], type = "response")

	# Generate logical vector corresponding to presence/absence
  resp <- dfmerge[, taxa.names[i]] > 0

	# Divide predicted probabilities into sites where
	# species is present ("x") and sites where the species is
	# absent ("y").
  x <- predout[resp]
  y <- predout[! resp]

	# Now perform all pairwise comparisons of x vs. y
	# and store results in a matrix
  rocmat <- matrix(NA, nrow = length(x), ncol = length(y))
  for (j in 1:length(x)) {
    rocmat[j,] <- as.numeric(x[j] > y)
  }

	# Summarize all comparisons to compute area under ROC
  roc[i] <- sum(rocmat)/(length(x)*length(y))
}
names(roc) <- taxa.names
print(roc)
