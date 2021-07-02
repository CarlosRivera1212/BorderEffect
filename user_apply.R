# EXAMPLE FOR USERS OF HOW TO APPLY

library(dplyr)

# Define the kappa calculation function
kap_fun <- function(Y, We, M){
  In = diag(length(Y))
  (t(Y)%*%(In-M)%*%We%*%(Y))/((t(Y)%*%We%*%(In-M)%*%We%*%(Y)))
}


# Using "plantation" function to simulate experimental design
df = plantation(n_plant = 300, arr = 1, 
                width = 500, heigh = 375, 
                nx = 20, ny = 15,
                plot = T)
head(df)

# Calculation of the W matrix
REND.d=as.matrix(dist(df[,1:2], diag=T, upper=T))
REND.d.inv <-as.matrix(1/REND.d)
diag(REND.d.inv) <- 0
W=as.matrix(REND.d.inv)
We=W/sum(W)


# Experimental response data
## Simulation response Without-border
# df$yield = rnorm(300, 1.8, 0.06)

## Simulation response one-close-border
df$yield = rnorm(300,
               ifelse(df$br1,1.9,1.8),
               ifelse(df$br1,0.06,0.06))

## Simulation response two-close-border
# df$yield = rnorm(300,
#                ifelse(df$br2 | df$br1,
#                       1.9,1.8),
#                ifelse(df$br2 | df$br1,
#                       0.06,0.06))


# Linear model fitting for one-closed-border and treatments
mod = lm(yield ~ br1 + bk1+bk2+tau1+tau2+tau3, df)

# Simulation of new response according to the fitted model
yield_sim = simulate(mod, 500)

# Calculation of the M matrix
X=with(df,cbind(int,tau1,tau2,tau3,bk1,bk2))
Z=matrix(c(1,0,0,0,0,0,0,-1,0,1,1,-1,-1,1,-2,1,0,0,0,-2,1,1,-1,1),nrow=4,byrow=T)
P=X%*%t(Z)
Mp=P%*%(solve(t(P)%*%P))%*%t(P)

# Calculation of "Kappa" for new simulated responses
kap_sim = NULL
for (i in 1:ncol(yield_sim)) {
  Ys = yield_sim[,i] 
  ks = kap_fun(Ys, We, Mp)
  
  kap_sim[i] = ks
}

# Calculation of kappa from experimental data 
kap_exp = kap_fun(df$yield, We, Mp)

# Add experimental kappa and simulate kappa
k = c(kap_exp, kap_sim)

# Density plot of kappa simulation
plot(density(k))
abline(v = kap_exp)
text(kap_exp, 0, 'Kap_exp', pos = 3)
