source("~/project/Changepoints/online_detection/bayesian.r")
## X <- c(rnorm(100,sd=0.5),rnorm(70,mean=5,sd=0.5),rnorm(70,mean = 2,sd=0.5),rnorm(70,sd=0.5),rnorm(70,mean = 7,sd=0.5))

 ## resX <- onlinechangepoint(X, 
 ##                          model = "nng", 
 ##                          mu0=0.7,k0=1,alpha0=1/2,beta0=1, #initial parameters 
 ##                          bpmethod = "mean", 
 ##                          lambda=50, #exponential hazard 
 ##                          FILTER=1e-3) 

## tmpplot(resX,X)                         


vlp <- read.csv(file = "./vlp32c_R.csv", header = FALSE, sep = ",")
vlp <- vlp[,1:1800]
vlp1 <- data.matrix(vlp[22,])
ids <- 1:1800

data <- rbind(ids, vlp1)
data <- t(data)

indi <- is.na(data[,2]) == FALSE
data.clean = data[indi,]
data.diff = diff(data.clean[,2])
data.dect = 100*c(data.diff,data.clean[1,2] -  data.clean[dim(data.clean)[1],2] )

plot(data.clean[,1], data.clean[,2], type = "p", cex = 0.1, xlim=c(1, 1800))
x11()
plot(data.clean[,1], data.dect, type = "p", cex = 0.1, xlim=c(1, 1800), ylim = c(-10,10))




## vlp1 <- vlp1[700:1000]
vlp2 <- vlp1[is.na(vlp1) == FALSE]
vlp2 <- vlp2[vlp2!=0]

dif_vlp2 <- cbind(0,diff(vlp2))

start.time <- Sys.time()

data.dect <- data.dect + 10
data.dect <- data.dect[1:1000]
resVLP <- onlinechangepoint(data.dect, 
                          model = "nng", 
                          mu0=10, k0=0.1, alpha0=1,beta0=0.1, #initial parameters 
                          bpmethod = "mean", 
                          lambda=200, #exponential hazard 
                          FILTER=1e-3) 
end.time <- Sys.time()
end.time-start.time

tmpplot(resVLP,data.dect)
