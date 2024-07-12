library(lavaan)
library(psych)
library(rjags)
library(boot)

data <- read.table('robustsem.ex2.txt')
colnames(data) <- c('V1','V2','V3','V4','income','grade')

data1 <- data[-1, 1:4]
data1[] <- lapply(data1, as.numeric)


gcmodel<-'i =~ 1*V1 + 1*V2 + 1*V3  + 1*V4
	s =~ 0*V1 + 1*V2 + 2*V3 + 3*V4'

res1 <- growth(gcmodel, data=data1)
est1 <- parameterEstimates(res1)

#missing rate
proportion <- round(100*apply(data[-1,1:4], 2, function(x) mean(is.na(x))),3)

# descriptive statistics
s <- describe(data1)


data <- data1[,4]
skewness <- function(data, i) {
  d <- data[i]
  return(describe(d)$skew)
}

# bootstrap
bootstrap_result <- boot(data, skewness, R=1000)
bootstrap_se <- sd(bootstrap_result$t)  

# skewness and z
skew_value <- describe(data)$skew
z_value <- skew_value / bootstrap_se

# p-value
p_value <- 2 * (1 - pnorm(abs(z_value)))

print(paste("Z-value:", z_value))
round(z_value,2)
print(paste("P-value:", p_value))

#GCM-Median-MAR
y <- data1
N <- nrow(y)
Time <- ncol(y)
Lambda <- matrix(c(rep(1,Time), (c(1:Time)-1)), nrow = Time)


#------------------------------------------
# GCM
dat <- list("N" = N, "y" = y, "tau" = tau, "Time" = ncol(y))

# Set initial values
initial=list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 20231210)

jags.gmm.md3 <- jags.model( file = "gcm_median.txt", 
                            data=dat, inits= initial, n.chains=1, n.adapt=1000 )
params <- c("par")
samps.gmm.md3 <- coda.samples(jags.gmm.md3, params, n.iter = Niter)

smp.md3 <- window(samps.gmm.md3[[1]], start = burnIn)
geweke.md3 <- apply(smp.md3, 2, function(x) geweke.diag(x)$z)

summary(smp.md3)
summary(smp.md3)$quantiles[1:5,3]

geweke.md3
race_plot <- traceplot(samps.gmm.md3) 



#GCM-Median-MNAR
y <- data1
N <- nrow(y)
Time <- ncol(y)
Lambda <- matrix(c(rep(1,Time), (c(1:Time)-1)), nrow = Time)


#------------------------------------------
# GCM
dat <- list("N" = N, "y" = y, "tau" = tau, "Time" = ncol(y))

# Set initial values
initial=list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 20231210)

jags.gmm.md4 <- jags.model( file = "gcm_median_MNAR.txt", 
                            data=dat, inits= initial, n.chains=1, n.adapt=1000 )
params <- c("par")
samps.gmm.md4 <- coda.samples(jags.gmm.md4, params, n.iter = Niter)

smp.md4 <- window(samps.gmm.md4[[1]], start = burnIn)
geweke.md4 <- apply(smp.md4, 2, function(x) geweke.diag(x)$z)

summary(smp.md4)
summary(smp.md4)$quantiles[1:5,3]

geweke.md4











