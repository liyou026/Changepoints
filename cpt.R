source("~/project/Changepoints/online_detection/bayesian.r")
X <- c(rnorm(100,sd=0.5),rnorm(70,mean=5,sd=0.5),rnorm(70,mean = 2,sd=0.5),rnorm(70,sd=0.5),rnorm(70,mean = 7,sd=0.5))

resX <- onlinechangepoint(X, 
                          model = "nng", 
                          mu0=0.7,k0=1,alpha0=1/2,beta0=1, #initial parameters 
                          bpmethod = "mean", 
                          lambda=50, #exponential hazard 
                          FILTER=1e-3) 

tmpplot(resX,X)


vlp <- read.csv(file = "./vlp32c.csv", header = FALSE, sep = ",")
vlp1 <- data.matrix(vlp[21,])
vlp1 <- vlp1[700:1000]
vlp2 <- vlp1[vlp1!=0]
dif_vlp2 <- diff(vlp2)

start.time <- Sys.time()
resVLP <- onlinechangepoint(vlp2, 
                          model = "nng", 
                          mu0=12,k0=1,alpha0=1,beta0=0.1, #initial parameters 
                          bpmethod = "mean", 
                          lambda=200, #exponential hazard 
                          FILTER=1e-3) 
end.time <- Sys.time()
end.time-start.time
tmpplot(resVLP,vlp2)
