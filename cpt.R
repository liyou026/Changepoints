source("~/project/Changepoints/online_detection/bayesian.r")
## X <- c(rnorm(100,sd=0.5),rnorm(70,mean=5,sd=0.5),rnorm(70,mean = 2,sd=0.5),rnorm(70,sd=0.5),rnorm(70,mean = 7,sd=0.5))

 ## resX <- onlinechangepoint(X, 
 ##                          model = "nng", 
 ##                          mu0=0.7,k0=1,alpha0=1/2,beta0=1, #initial parameters 
 ##                          bpmethod = "mean", 
 ##                          lambda=50, #exponential hazard 
 ##                          FILTER=1e-3) 

## tmpplot(resX,X)                         

derivative <- function(data){

    res <- c(0)
    for(int i in data[,1]){
        dif = (data[i+1,2] - data[i,2])/()
    }
}


vlp <- read.csv(file = "./vlp32c_Z.csv", header = FALSE, sep = ",")
vlp <- vlp[,1:1800]
vlp1 <- data.matrix(vlp[20,])
ids <- 1:1800

data <- rbind(ids, vlp1)
data <- t(data)

indi <- is.na(data[,2]) == FALSE
data.clean = data[indi,]
data.diff = diff(data.clean)
dr = data.diff[,2]/data.diff[,1]

d1 = data.clean[1,2]
dlast = data.clean[dim(data.clean)[1],2]
i1 = data.clean[1,1]
ilast = data.clean[dim(data.clean)[1],1]

dr1 = ( d1 - dlast )/(1800 - ilast + i1)
dr <- c(dr1,dr)
##dr <- dr*100
data.clean <- cbind(data.clean, dr)


plot(data.clean[,1], data.clean[,2], type = "p", cex = 0.1, xlim=c(1, 1800))
x11()
plot(data.clean[,1], data.clean[,3], type = "p", cex = 0.1, xlim=c(1, 1800), ylim = c(-10,10))

start.time <- Sys.time()


resVLP <- onlinechangepoint(data.clean[,2], 
                          model = "nng", 
                          mu0=-3, k0=0.1, alpha0=0.1,beta0=0.1, #initial parameters 
                          bpmethod = "mean", 
                          lambda=200, #exponential hazard 
                          FILTER=1e-3)
tmpplot(resVLP,data.clean[,2])


resZ <- onlinechangepoint(data.clean[,2]+5,
                          model = "pg", 
                          alpha0=1,beta0=1, 
                          bpmethod = "mean", 
                          lambda=10, #exponential hazard 
                          FILTER=1e-3)
Z <- c(rpois(100,lambda=5),rpois(70,lambda=7),rpois(70,lambda=5),rpois(70,lambda=6),rpois(70,lambda=5)) 
## online changepoint detection for series Z 
resZ <- onlinechangepoint(Z, 
                          model = "pg", 
                          alpha0=10,beta0=1, 
                          bpmethod = "mean", 
                          lambda=10, #exponential hazard 
                          FILTER=1e-3) 

end.time <- Sys.time()
end.time-start.time


