# Chi square tests for parametric and non parametric models

# Conduct chi-square tests on nested parametric models
for (i in 1:length(taxa.names)) {

  print(taxa.names[i])
  resp <- dfmerge[,taxa.names[i]] > 0
  
  # Fit a model that is only a constant
  modcmp <- glm(resp ~ 1, family = binomial, data = dfmerge)

  # Compare original model with constant model using
  # a chi-square statistic
  modout <- anova(modlist.glm[[i]], modcmp, test = "Chi")
  print(modout)

  # Select p < 0.05 as statistically significant
  if (modout[2,"P(>|Chi|)"] < 0.05) {
    print("Model significant compared to constant")
  }

  # Fit a model with only a linear explanatory variable
  modcmp <- glm(resp ~ temp, family = binomial, data = dfmerge)

  # Compare original model with constant model using a
  # chi-square statistic
  modout <- anova(modlist.glm[[i]], modcmp, test = "Chi")
  print(modout)
  if (modout[2,"P(>|Chi|)"] < 0.05) {
    print("Model with b2 significant (p < 0.05) improvement over linear model")
  }

}

# Conduct chi-square tests on nested non-parametric models
library(gam)
for (i in 1:length(taxa.names)) {

  print(taxa.names[i])
  resp <- dfmerge[,taxa.names[i]] > 0
  modcmp <- gam(resp ~ 1, family = binomial, data = dfmerge)
  modout <- anova(modlist.gam[[i]], modcmp, test = "Chi")
  print(modout)
  if (modout[2,"P(>|Chi|)"] < 0.05) {
    print("Model significant compared to constant")
  }
  
  # Fit a model with only a linear explanatory variable
  modcmp <- gam(resp ~ temp, family = binomial, data = dfmerge)
  modout <- anova(modlist.glm[[i]], modcmp, test = "Chi")
  print(modout)
  if (modout[2,"P(>|Chi|)"] < 0.05) {
    print("Nonparametric model with two degrees of freedom significant over linear model.")
  }

}
