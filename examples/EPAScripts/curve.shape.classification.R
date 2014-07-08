# Perform graphical classification of response shape

unimod.test <- function(mnr, ubnd, lbnd) {

  # Find the maximum and minimum predicted mean probabilities
  lmax <- max(mnr)
  lmin <- min(mnr)

  # Find index locations for these probabilities
  imax <- match(lmax, mnr)
  imin <- match(lmin, mnr)
  
  x.out <- F
  y.out <- F

  # Compare mean predicted probability to the left of maximum point 
  # with upper confidence bound.  Store a T in x.out if
  # any point in the mean response deviates from the 
  # upper confidence limit
  if (imax > 1) {
    x.out <- sum(lmax == pmax(lmax, ubnd[1:(imax-1)])) > 0
  }

  # Store a T in y.out if any point in the mean probability
  # to the right of the maximum point deviates from the upper
  # confidence limit
  if (imax < length(ubnd)) {
    y.out <- sum(lmax == pmax(lmax, 
                   ubnd[(imax+1):length(ubnd)])) > 0
  }

  # Perform same set of tests for lower confidence limit
  a.out <- F
  b.out <- F
  if (imin > 1) {				
    a.out <- sum(lmin == pmin(lmin, lbnd[1:(imin-1)])) > 0
  }
  if (imin < length(lbnd)) {
    b.out <- sum(lmin == pmin(lmin, 
                   lbnd[(imin+1):length(lbnd)])) > 0
  }

  # The information on where the mean curve deviates from the
  # confidence limits tells us its curve shape...
  if (x.out & y.out) {
    return("Unimodal")
  }
  if (a.out & b.out) {
    return("Concave up")
  }
  if (x.out | b.out) {
    return("Increasing")
  }
  if (y.out | a.out) {
    return("Decreasing")
  }
  if (! (x.out | y.out | a.out | b.out)) {
    return(NA)
  }
}

tolcl <- rep("", times = length(taxa.names)) 
for (i in 1:length(taxa.names)) {
  predres <- predict(modlist.gam[[i]], type= "link", se.fit = T)
  
  # Compute upper and lower 90% confidence limits
  up.bound.link <- predres$fit + 1.65*predres$se.fit
  low.bound.link <- predres$fit - 1.65*predres$se.fit
  mean.resp.link <- predres$fit

  # Convert from logit transformed values to probability.
  up.bound <- exp(up.bound.link)/(1+exp(up.bound.link))
  low.bound <- exp(low.bound.link)/(1+exp(low.bound.link))
  mean.resp <- exp(mean.resp.link)/(1+exp(mean.resp.link))

  # unimod.test requires that the responses be sorted by 
  # the value of the environmental variable.
  iord <- order(dfmerge$temp)

  tolcl[i] <- unimod.test(mean.resp[iord], up.bound[iord], 
                          low.bound[iord])
}

names(tolcl) <- taxa.names
print(tolcl)

