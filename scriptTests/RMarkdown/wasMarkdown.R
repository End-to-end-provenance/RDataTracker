#Here is the first chunk.

#```{r}
x <- 2+2
y <- x+2
x
y

#```

#Testing ggplot with RDataTracker
#```{r pressure, echo=FALSE}
library(ggplot2)
p<- ggplot(pressure, aes(x=temperature, y=pressure))
p

#```
#Here's another chunk!


#```{r, details = TRUE}
data(iris)
x <- iris[,1]
y <- iris[,2]
summary(lm(y~x))

#```
