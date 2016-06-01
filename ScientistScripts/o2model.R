### Decomp model for the Pitcher Plant system
### MKLau 16Oct2014

## 6:00 sunrise = 360 
## 12:00 noon = 720
## 18:00 sunset = 1080

light <- function(days=3){
    out <- sin(2*pi*(1:720)/1440)
    out[out < 0] <- 0
    out <- c(rep(0,720/2),out,rep(0,720/2))
    rep(out,days)
}

PAR <- function(days=3,rise=6,set=18){
    out <- rep(0,1440)
    out[(rise*60):(set*60)] <- 1
    rep(out,days)
}

BOD <- function(prey,k=1){
   (prey / (prey + k))
}

expDecomp <- function(prey.0=5,beta=0.0005){
    prey.0 * exp(-beta * seq(1,1440))
}

decomp <- function(days=3,prey.0=0,prey.add=5,t.add=720,beta=0.0005){
    if (t.add >=1440){warning('Prey addition outside time bounds')}
    out <- list()
    for (i in 1:days){
        d.0 <- expDecomp(prey.0,beta=beta)
        d.add <- expDecomp(tail(d.0,1)+prey.add,beta=beta)
        out[[i]] <- c(d.0[1:t.add],d.add[1:(1440-t.add)])
        prey.0 <- tail(out[[i]],1)
    }
    return(unlist(out))
}

simO2 <- function(days=10,prey.add=5,beta=0.0005,t.add=720,bod.rescale=TRUE,bod.scalar=10){
    photo <- light(days) * PAR(days=days)
    prey <- decomp(days,prey.add=prey.add,beta=beta,t.add=t.add)
    bod <- BOD(prey)
    if (bod.rescale == TRUE){
        bod <- bod * bod.scalar
    }
    o2 <- photo - bod
    o2[o2 < 0] <- 0
    out <- data.frame(t=rep(1:1440,days),O2=o2,photo=photo,prey=prey,bod=bod)
    return(out)
}
