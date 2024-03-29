#Serayen Govender


#NDLM SSM with forecasting
#Set forcast period (for_per) to 0 to run the NDLM normally

library(tidyverse)
#Load In data
setwd("C:/Users/Seray/Research Work/Data")
load("Wahlenbergia_Plot_C.RData")
load("ClimDatRenV2.RData")
setwd("C:/Users/Seray/Research Work/Code")
#Set Fynbos to 1 and Renostoveld as 0
dat <- data.Wahlenbergia.C  %>% mutate(Site = ifelse(Site == "Fynbos",1,0))
#dat <- cbind(dat[,1:4], dat[,5:10])
#Set Treatment to 1 and control as 0
dat <- dat  %>% mutate(CT = ifelse(CT == "Treatment",1,0))


#dat <-dat %>% add_column(seq_plotNo = seq(1,nrow(dat)))


#data grouped by site
dat_fyn <- dat %>% filter(Site == 1)
#dat_fyn$seq_plotNo <- seq(1:12)
dat_ren <- dat %>% filter(Site == 0)
#Data grouped by treatment/control
dat_T <- dat %>% filter(CT == 1)
dat_C <- dat %>% filter(CT == 0) 
#Data grouped by site and treatment/control
dat_T_fyn <- dat_fyn %>% filter(CT == 1)
dat_C_fyn <- dat_fyn %>% filter(CT == 0)
dat_T_Ren <- dat_T %>% filter(Site == 0)
dat_C_Ren <- dat_C %>% filter(Site == 0)


library(lme4)
library("jagsUI")
library("R2jags")
library("rjags")

#JAGS Code For SSM

sink("WahlenbergiaR.jag")
cat("
    model {
    # Priors
    
    for(i in 1:x) #Loop trough each plot
    {
    #mu.lam[i,1] ~ dunif(0,10)
    N[i,1] ~ dunif(0, 40000)
    
    }
    #Priors
    sig.proc ~ dunif(0,500)
    sig2.proc <- pow(sig.proc,2)
    tau.proc <- pow(sig.proc, -2)
    
    sig.obs ~ dunif(0,500)
    sig2.obs <- pow(sig.obs, 2)
    tau.obs <- pow(sig.obs, -2)
    
    
    sig.plot ~ dunif(0,500)
    sig2.plot <- pow(sig.plot , 2)
    tau.plot <- pow(sig2.plot, -2)
    
    beta[1] ~ dnorm(0, 0.1) #intercept
    beta[2] ~ dnorm(0, 0.1) #B Treatmment
    #beta[3] ~ dnorm(0, 0.00001) #B Climate1
    #beta[4] ~ dnorm(0, 0.00001) #B Climate2
    #beta[5] ~ dnorm(0, 0.00001) #B Climate3

    # for(i in 1:3)#For each Beta
    # {
    # for(j in 1:x)#For each Plot
    # {
    # beta[i,j] ~ dnorm(0, 0.0001)
    # }#end j
    # }#end i

    # Likelihood
    for(i in 1:(x)){

    #State Process
    eps.plot[i] ~ dnorm(0,tau.plot)
    for(t in 1:(T-1)) #For each time step after the first modeling each plot seperately
    {
    
    
    mu.lam[i,t] = beta[1] + beta[2]*z1[i] + eps.plot[i]
    lam[i,t] ~ dnorm(mu.lam[i,t], tau.proc)
    
    N[i,(t+1)] = N[i,(t)] * exp(lam[i,t]) 
    
    }#end t
    
    
    #Observation Process
    
    for(t in 1:(T))
    {
    y[i,t]  ~ dnorm(N[i,t], tau.obs)
    }#end t
    
    }#end i
    
    # #Forecasting
    #   for(i in 1:(x))
    #   {
    #   
    #     #eps.plot[i] ~ dnorm(0,tau.plot)
    #     #State Process
    #     for(t in 1:fp)
    #     {
    #       
    #       mu.lam[T+t] = beta[1]+ beta[2] *z1[i] +  eps.plot[i] #mean growth rate for ith plot
    #       lam[T+t] ~ dnorm(mu.lam[T+t], tau.proc)
    #       N[i, T+t] = N[i,T+(t-1)] * exp(lam[T+t]) 
    # 
    #       #y[i,T+1]  ~ dnorm(N[i,t], tau.obs)
    #     }#end t

    #
    # }#end i
    
    
    
    }#end model
    
    
    
    ",fill = TRUE)#end cat
sink()

#Renosterveld SSM
dat <- dat_ren
dat <-dat %>% add_column(seq_plotNo = seq(1,nrow(dat)))
#Data grouped by treatment/control
dat_T <- dat %>% filter(CT == 1)
dat_C <- dat %>% filter(CT == 0) 

#Setting up Variables for JAGS function in R 
Y = as.matrix(dat[, c(8:13)])  #Observed volume of population

#site = as.matrix(dat[,2]) #Site and treatment as categorical varibles
treat = as.matrix(dat[,7])


#Number of periods to forecast to
for_per = 0

#Climate data
#AirTC <- as.numeric(clim_avgs_ren[,1])
#RH <- as.numeric(clim_avgs_ren[,2])
#WS <- as.numeric(clim_avgs_ren[,3])
#Setup JAGS data
jags.data <- list(y = Y, T=6, x = dim(Y)[1], fp = for_per, z1 = as.numeric(treat) )

# Function to generate starting values
inits <- function()
{
  list(sig.proc = runif(1, 0, 5),sig.plot=runif(1, 0, 5), beta =runif(2,0,1),  sig.obs = runif(1, 0, 10), N=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1+(for_per))*dim(Y)[1])), nrow=dim(Y)[1]))
} 


# Parameters to be estimated
parameters <- c("lam", "beta", "mu.lam", "sig.obs", "sig.proc", "N","sig.plot")


# MCMC settings
nc <- 5 # Number of chains
ni <- 100000 # Number of draws from posterior (for each chain)
nb <- 50000 # Number of draws to discard as burn in
nt <- 3 # Thinning rate


#Start Gibbs sampler: Run model in JAGS and save results in object called ssm
#***Useful Data is saved in objects so it is not necessary to run this to check the rest of the code
ssm2 <- jags(jags.data, inits, parameters, "WahlenbergiaR.jag", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb) 

ssm2 <- autojags(ssm2, n.iter=1000)

setwd("C:/Users/Serayen/Research Work/Data")
#save(ssm2, file = "ElytropappusR_SSM.RData")
#load("ElytropappusR_SSM.RData")
setwd("C:/Users/Serayen/Research Work/Code")

N.out <- ssm2$BUGSoutput$mean$N

obs.errors <- Y - N.out
obs.errors
plot(obs.errors[3,], type = 'l')

par(mfrow=c(1,1))
plot(N.out[1,], type = 'l', ylim = c(0, max(c(Y[1,], N.out[1,]) )), ylab = "Volume", xlab = "Time Step" , main = "Estimated and \n Observed Population at \n Plot 1 Control")
lines(N.out[1,], type = 'l', col = 'blue')
lines(Y[1,], type ='l', col = 'red')


N.out <- ssm2$BUGSoutput$mean$N
#Predicted Data
tmp <- as_tibble(N.out) 
tmp <- rename(tmp, "1" = "V1", "2" = "V2" , "3" ="V3", "4" = "V4", "5" = "V5", "6" = "V6")
preds <- as_tibble(cbind(dat[,1:7],tmp))
preds_T <- preds %>% filter(CT == 1)
preds_C <- preds %>% filter(CT == 0) 

N.out_T <- as.matrix(preds_T[, c(8:(13+for_per))])
N.out_C <- as.matrix(preds_C[, c(8:(13+for_per))])

Y_T = as.matrix(dat_T[, c(8:13)])
Y_C = as.matrix(dat_C[, c(8:13)])

#Summary stats of estimations population volumes
N.sum <- ssm2$BUGSoutput$summary

N1.sum <- N.sum[seq(1,196, by = 12),]
N1.sum <- N1.sum[1:(6+for_per),]

plot(N.out[1,], type = 'l', ylim = c(0, max(N1.sum[,7])), ylab = "Volume", xlab = "Time Step" , main = "Estimated and \n Observed Population at \n Plot 1 Control")
polygon(c(seq(1:6),rev(seq(1:6))),c(N1.sum[,3],rev(N1.sum[,7])),col = "grey80", border = FALSE)
lines(N.out[1,], type = 'l', col = 'blue')
lines(Y[1,], type ='l', col = 'red')


########################################################
#Fynbos SSM

sink("WahlenbergiaF.jag")
cat("
    model {
    # Priors
    
    for(i in 1:x) #Loop trough each plot
    {
    #mu.lam[i,1] ~ dunif(0,10)
    N[i,1] ~ dunif(0, 40000)
    
    }
    #Priors
    sig.proc ~ dunif(0,500)
    sig2.proc <- pow(sig.proc,2)
    tau.proc <- pow(sig.proc, -2)
    
    sig.obs ~ dunif(0,500)
    sig2.obs <- pow(sig.obs, 2)
    tau.obs <- pow(sig.obs, -2)
    
    
    sig.plot ~ dunif(0,500)
    sig2.plot <- pow(sig.plot , 2)
    tau.plot <- pow(sig2.plot, -2)
    
    beta[1] ~ dnorm(0, 1000) #intercept
    beta[2] ~ dnorm(0, 1000) #B Treatmment
    #beta[3] ~ dnorm(0, 0.00001) #B Climate1
    #beta[4] ~ dnorm(0, 0.00001) #B Climate2
    #beta[5] ~ dnorm(0, 0.00001) #B Climate3

    # for(i in 1:3)#For each Beta
    # {
    # for(j in 1:x)#For each Plot
    # {
    # beta[i,j] ~ dnorm(0, 0.0001)
    # }#end j
    # }#end i

    # Likelihood
    for(i in 1:(x)){

    #State Process
    eps.plot[i] ~ dnorm(0,tau.plot)
    for(t in 1:(T-1)) #For each time step after the first modeling each plot seperately
    {
    
    
    mu.lam[i,t] = beta[1] + beta[2]*z1[i] + eps.plot[i]
    lam[i,t] ~ dnorm(mu.lam[i,t], tau.proc)
    
    N[i,(t+1)] = N[i,(t)] * exp(lam[i,t]) 
    
    }#end t
    
    
    #Observation Process
    
    for(t in 1:(T))
    {
    y[i,t]  ~ dnorm(N[i,t], tau.obs)
    }#end t
    
    }#end i
    
    # #Forecasting
    #   for(i in 1:(x))
    #   {
    #   
    #     #eps.plot[i] ~ dnorm(0,tau.plot)
    #     #State Process
    #     for(t in 1:fp)
    #     {
    #       
    #       mu.lam[T+t] = beta[1]+ beta[2] *z1[i] +  eps.plot[i] #mean growth rate for ith plot
    #       lam[T+t] ~ dnorm(mu.lam[T+t], tau.proc)
    #       N[i, T+t] = N[i,T+(t-1)] * exp(lam[T+t]) 
    # 
    #       #y[i,T+1]  ~ dnorm(N[i,t], tau.obs)
    #     }#end t

    #
    # }#end i
    
    
    
    }#end model
    
    
    
    ",fill = TRUE)#end cat
sink()

dat <- dat_fyn[1:12,]
dat <-dat %>% add_column(seq_plotNo = seq(1,nrow(dat)))
#Data grouped by treatment/control
dat_T <- dat %>% filter(CT == 1)
dat_C <- dat %>% filter(CT == 0) 

#Setting up Variables for JAGS function in R 
Y = as.matrix(dat[, c(8:13)])  #Observed volume of population

#site = as.matrix(dat[,2]) #Site and treatment as categorical varibles
treat = as.matrix(dat[,7])


#Number of periods to forecast to
for_per = 0

#Climate data
#AirTC <- as.numeric(clim_avgs_ren[,1])
#RH <- as.numeric(clim_avgs_ren[,2])
#WS <- as.numeric(clim_avgs_ren[,3])
#Setup JAGS data
jags.data <- list(y = Y, T=6, x = dim(Y)[1], fp = for_per, z1 = as.numeric(treat) )

# Function to generate starting values
inits <- function()
{
  list(sig.proc = runif(1, 0, 5),sig.plot=runif(1, 0, 5), beta =runif(2,0,1),  sig.obs = runif(1, 0, 10), N=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1+(for_per))*dim(Y)[1])), nrow=dim(Y)[1]))
} 


# Parameters to be estimated
parameters <- c("lam", "beta", "mu.lam", "sig.obs", "sig.proc", "N","sig.plot")


# MCMC settings
nc <- 5 # Number of chains
ni <- 100000 # Number of draws from posterior (for each chain)
nb <- 5000 # Number of draws to discard as burn in
nt <- 3 # Thinning rate


#Start Gibbs sampler: Run model in JAGS and save results in object called ssm
#***Useful Data is saved in objects so it is not necessary to run this to check the rest of the code
ssm3 <- jags(jags.data, inits, parameters, "WahlenbergiaF.jag", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb) 

ssm3 <- autojags(ssm3, n.iter=10000)

setwd("C:/Users/Seray/Research Work/Data")
#save(ssm3, file = "WahlenbergiaF_SSM.RData")
#load("WahlenbergiaF_SSM.RData")
setwd("C:/Users/Seray/Research Work/Code")

setwd("C:/Users/Seray/Research Work/Data")
#save(ssm3, file = "WahlenbergiaF2_SSM.RData")
#load("WahlenbergiaF2_SSM.RData")
setwd("C:/Users/Seray/Research Work/Code")

N.out <- ssm3$BUGSoutput$mean$N

obs.errors <- Y - N.out
obs.errors
plot(obs.errors[3,], type = 'l')

par(mfrow=c(1,1))
plot(N.out[1,], type = 'l', ylim = c(0, max(c(Y[1,], N.out[1,]) )), ylab = "Volume", xlab = "Time Step" , main = "Estimated and \n Observed Population at \n Plot 1 Control")
lines(N.out[1,], type = 'l', col = 'blue')
lines(Y[1,], type ='l', col = 'red')


N.out <- ssm3$BUGSoutput$mean$N
#Predicted Data
tmp <- as_tibble(N.out) 
tmp <- rename(tmp, "1" = "V1", "2" = "V2" , "3" ="V3", "4" = "V4", "5" = "V5", "6" = "V6")
preds <- as_tibble(cbind(dat[,1:7],tmp))
preds_T <- preds %>% filter(CT == 1)
preds_C <- preds %>% filter(CT == 0) 

N.out_T <- as.matrix(preds_T[, c(8:(13+for_per))])
N.out_C <- as.matrix(preds_C[, c(8:(13+for_per))])

Y_T = as.matrix(dat_T[, c(8:13)])
Y_C = as.matrix(dat_C[, c(8:13)])

#Data grouped by site and treatment/control
dat_T_Ren <- dat %>% filter(CT == 1)
dat_C_Ren <- dat %>% filter(CT == 0)

#Summary stats of estimations population volumes
N.sum <- ssm3$BUGSoutput$summary

N1.sum <- N.sum[seq(1,182, by = 12),]
N1.sum <- N1.sum[1:(6+for_per),]

plot(N.out[1,], type = 'l', ylim = c(0, max(N1.sum[,7])), ylab = "Volume", xlab = "Time Step" , main = "Estimated and \n Observed Population at \n Plot 1 Control")
polygon(c(seq(1:6),rev(seq(1:6))),c(N1.sum[,3],rev(N1.sum[,7])),col = "grey80", border = FALSE)
lines(N.out[1,], type = 'l', col = 'blue')
lines(Y[1,], type ='l', col = 'red')
#########################################
#-------------PLOTS---------------------#

#Function to add legend to plot
Add_legend <- function()
{
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',legend = c("Estimated", "Observed"), col = c("blue", "red"), lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1.8, seg.len=1.4, bty = 'n')
}


#Summary stats of estimations population volumes
period <- (6+for_per)
nPlots <- nrow(dat)
N.sum <- ssm2$BUGSoutput$summary
No_entries = (2*nPlots*(period-1)) + nPlots*period +1
deviance.sum <- N.sum[((nPlots*period)+1),]
N.sum<- rbind(N.sum[1:((nPlots*period)),], N.sum[((nPlots*period)+2):No_entries,])
No_entries = (2*nPlots*(period-1)) + nPlots*period 
#Summary Statistics for Confidence intervals By Plot Number
N_sum <- vector("list")
for(i in 1:nPlots)
{
  
  Ni.sum <-N.sum[seq(i,No_entries, by = nPlots),]
  N_sum[[i]] <- Ni.sum[1:period,] 
}


#Graphs for Renosterveld Treatment Plots
par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
for(i in 1:6)
{
  plotNo <- as.numeric(dat_T[i,14])
  Plot_CT <- "NULL"
  if(preds_T[i,4] == 1)
  {
    Plot_CT <- "Treatment"
  }else{
    Plot_CT <- "Control"   
  }
  
  Plot_Site<- "Null"
  if(preds_T[i,2] == 1)
  {
    Plot_Site <- "Fynbos"
  }else{
    Plot_Site <- "Renosterveld"   
  }
  
  title= paste( "Plot", toString(as.numeric(dat_T[i,1])) )
  #paste("Estimated and \n Observed Population at \n Plot", toString(as.numeric(dat_T_Ren[i,1])), "in", Plot_Site , Plot_CT)
  
  plot(N.out_T[i,], type = 'l', col = 'blue', ylim = c(0, max(c(N_sum[[plotNo]][1:6,7],Y_T[i,])) ) , ylab = "Volume", xlab = "Time Step",  main = title, cex.main = 1.3)
  polygon(c(seq(1:period),rev(seq(1:period))),c(N_sum[[plotNo]][,3],rev(N_sum[[plotNo]][,7])),col = "grey80", border = FALSE)
  lines(N.out_T[i,], type = 'l', col = 'blue', lwd = 2)
  lines(Y_T[i,], type ='l', col = 'red', lwd = 2)
}
Add_legend()


#Graphs for Renosterveld Control Plots
par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
for(i in 1:6)
{
  plotNo <- as.numeric(dat_C[i,14])
  Plot_CT <- "NULL"
  if(preds_C[i,4] == 1)
  {
    Plot_CT <- "Treatment"
  }else{
    Plot_CT <- "Control"   
  }
  
  Plot_Site<- "Null"
  if(preds_C[i,2] == 1)
  {
    Plot_Site <- "Fynbos"
  }else{
    Plot_Site <- "Renosterveld"   
  }
  
  title= paste( "Plot", toString(as.numeric(dat_C[i,1])) )
  #paste("Estimated and \n Observed Population at \n Plot", toString(as.numeric(dat_T_Ren[i,1])), "in", Plot_Site , Plot_CT)
  
  plot(N.out_C[i,], type = 'l', col = 'blue', ylim = c(0, max(c(N_sum[[plotNo]][1:6,7],Y_C[i,])) ) , ylab = "Volume", xlab = "Time Step",  main = title, cex.main = 1.3)
  polygon(c(seq(1:period),rev(seq(1:period))),c(N_sum[[plotNo]][,3],rev(N_sum[[plotNo]][,7])),col = "grey80", border = FALSE)
  lines(N.out_C[i,], type = 'l', col = 'blue', lwd = 2)
  lines(Y_C[i,], type ='l', col = 'red', lwd = 2)
}
Add_legend()


#Graphs for Fynbos Treatment Plots
period <- (6+for_per)
nPlots <- nrow(dat)
N.sum <- ssm3$BUGSoutput$summary
No_entries = (2*nPlots*(period-1)) + nPlots*period +1
deviance.sum <- N.sum[((nPlots*period)+1),]
N.sum<- rbind(N.sum[1:((nPlots*period)),], N.sum[((nPlots*period)+2):No_entries,])
No_entries = (2*nPlots*(period-1)) + nPlots*period 
#Summary Statistics for Confidence intervals By Plot Number
N_sum <- vector("list")
for(i in 1:nPlots)
{
  
  Ni.sum <-N.sum[seq(i,No_entries, by = nPlots),]
  N_sum[[i]] <- Ni.sum[1:period,] 
}



par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
for(i in 1:6)
{
  plotNo <- as.numeric(dat_T[i,14])
  Plot_CT <- "NULL"
  if(preds_T[i,4] == 1)
  {
    Plot_CT <- "Treatment"
  }else{
    Plot_CT <- "Control"   
  }
  
  Plot_Site<- "Null"
  if(preds_T[i,2] == 1)
  {
    Plot_Site <- "Fynbos"
  }else{
    Plot_Site <- "Renosterveld"   
  }
  
  title= paste( "Plot", toString(as.numeric(dat_T[i,1])) )
  #paste("Estimated and \n Observed Population at \n Plot", toString(as.numeric(dat_T_Ren[i,1])), "in", Plot_Site , Plot_CT)
  
  plot(N.out_T[i,], type = 'l', col = 'blue', ylim = c(0, max(c(N_sum[[plotNo]][1:6,7],Y_T[i,])) ) , ylab = "Volume", xlab = "Time Step",  main = title, cex.main = 1.3)
  polygon(c(seq(1:period),rev(seq(1:period))),c(N_sum[[plotNo]][,3],rev(N_sum[[plotNo]][,7])),col = "grey80", border = FALSE)
  lines(N.out_T[i,], type = 'l', col = 'blue', lwd = 2)
  lines(Y_T[i,], type ='l', col = 'red', lwd = 2)
}
Add_legend()

#Fynbos Control plots
par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
for(i in 1:6)
{
  plotNo <- as.numeric(dat_C[i,14])
  Plot_CT <- "NULL"
  if(preds_T[i,4] == 1)
  {
    Plot_CT <- "Treatment"
  }else{
    Plot_CT <- "Control"   
  }
  
  Plot_Site<- "Null"
  if(preds_T[i,2] == 1)
  {
    Plot_Site <- "Fynbos"
  }else{
    Plot_Site <- "Renosterveld"   
  }
  
  title= paste( "Plot", toString(as.numeric(dat_C[i,1])) )
  #paste("Estimated and \n Observed Population at \n Plot", toString(as.numeric(dat_T_Ren[i,1])), "in", Plot_Site , Plot_CT)
  
  plot(N.out_C[i,], type = 'l', col = 'blue', ylim = c(0, max(c(N_sum[[plotNo]][1:6,7],Y_T[i,])) ) , ylab = "Volume", xlab = "Time Step",  main = title, cex.main = 1.3)
  polygon(c(seq(1:period),rev(seq(1:period))),c(N_sum[[plotNo]][,3],rev(N_sum[[plotNo]][,7])),col = "grey80", border = FALSE)
  lines(N.out_C[i,], type = 'l', col = 'blue', lwd = 2)
  lines(Y_C[i,], type ='l', col = 'red', lwd = 2)
}
Add_legend()




##############################################
#--------------Get Stats---------------------#

#Put summary into list
Nplots <- nrow(dat)
nBeta <- 2
#Summary stats of estimations population volumes
period <- (6+for_per)
N.sum <- ssm3$BUGSoutput$summary
No_entries = (2*Nplots*(period-1)) + Nplots*period +1
deviance.sum <- N.sum[((Nplots*period)+1+nBeta),]
beta.out <- N.sum[((Nplots*period)+1):((Nplots*period)+nBeta),]
N.sum<- rbind(N.sum[1:((Nplots*period)),], N.sum[((Nplots*period)+nBeta+2):No_entries,])
No_entries = (2*Nplots*(period-1)) + Nplots*period - nBeta
#Summary Statistics for Confidence intervals By Plot Number
N_sum <- vector("list")
for(i in 1:Nplots)
{
  
  Ni.sum <-N.sum[seq(i,No_entries, by = Nplots),]
  N_sum[[i]] <- Ni.sum 
}


#Average statistics over plots

#Mean of average growth rate at t at a site, if it is control or treatment
avg_lam <- function(t,site, ct)
{
  
  #index of summary data
  sum_ind <- NULL
  if(site == 1 & ct ==1)
  {
    sum_ind <- ind_fyn_t
  }else if(site == 1 & ct ==0){
    sum_ind <- ind_fyn_c
  }else if(site == 0 & ct ==1){
    sum_ind <- ind_ren_t
  }else if(site == 0 & ct ==0){
    sum_ind <- ind_ren_c
  }
  
  sum<- 0
  for(i in 1:5)
  {
    sum <- sum + N_sum[[sum_ind[i]]][(period+t),1]
    #print(N_sum[[sum_ind[i]]][(period+t),1])
  }
  avg <- sum/5
  
  return(avg)
}

avg_lam(2,1,1)


avg_mu_lam <- function(t,site, ct)
{
  
  #index of summary data
  sum_ind <- NULL
  if(site == 1 & ct ==1)
  {
    sum_ind <- ind_fyn_t
  }else if(site == 1 & ct ==0){
    sum_ind <- ind_fyn_c
  }else if(site == 0 & ct ==1){
    sum_ind <- ind_ren_t
  }else if(site == 0 & ct ==0){
    sum_ind <- ind_ren_c
  }
  
  sum<- 0
  for(i in 1:6)
  {
    sum <- sum + N_sum[[sum_ind[i]]][(2* period -1)+t,1]
  }
  avg <- sum/6
  
  return(avg)
}


avg_mu_lam(2,1,1)

##########################################
library(xtable)
rowSum_N.out_T <- data.frame(rowSums(N.out_T))

rns <- NULL
for(i in 1:6)
{
  rns <- c(rns, paste("plot", toString(preds_T$Plot[i])))
}

rownames(rowSum_N.out_T) <- rns

colnames(rowSum_N.out_T) <- "Volume"


xtable(rowSum_N.out_T, caption = "Total estimated volume of graminoids in Fynbos Treatment plots over 6 periods", label = 'tab:vol_gram_fyn_t')


#Lambdas
lams <- ssm3$BUGSoutput$mean$lam

lams_mat <- cbind(dat[,7], dat[,1], as.matrix(lams))

lams_T <- lams_mat %>% filter(CT ==1)
lams_T <- lams_T[,-1]
xtable(lams_T, caption = "Estimates of growth rates $lambda$ of Microdon Polygaloides in treatment plots over 6 periods", label = 'tab:lam_Micro_t')

lams_C <- lams_mat %>% filter(CT ==0)
lams_C <- lams_C[,-1]
xtable(lams_C, caption = "Estimates of growth rates $lambda$ of Microdon Polygaloides in control plots over 6 periods", label = 'tab:lam_Micro_c')

nlam <- Nplots*5
lam.out <- N.sum[((Nplots*period)+2+nBeta):((Nplots*period)+nlam+nBeta+1),]
#lam.out[seq(i,No_entries, by = Nplots),]

lam.out[seq(1,nlam, by = 12),]
lam.out[seq(2,nlam, by = 12),]
l1 <- NULL
for(i in 1:12)
{
  l1 <-rbind(l1,lam.out[seq(i,nlam, by = 12),])
}
l1[,c(1,3,7)]

xtable(round(l1[,c(1,3,7)],3), caption = "$lambda$ credibility intervals")

#mu_lamdas
mu.lams <- ssm3$BUGSoutput$mean$mu.lam

mu.lams_mat <- cbind(dat[,7], dat[,1], as.matrix(mu.lams))


mu.lams_T <- mu.lams_mat %>% filter(CT ==1)
mu.lams_T <- mu.lams_T[,-1]
xtable(mu.lams_T, caption = "Mean growth rates $mu_{lambda}$ of Microdon Polygaloides in treatment plots over 6 periods", label = 'tab:mu.lam_Micro_t')

mu.lams_C <- mu.lams_mat %>% filter(CT ==0)
mu.lams_C <- mu.lams_C[,-1]
xtable(mu.lams_C, caption = "Mean growth  rates $mu_{lambda}$ of Microdon Polygaloides in control plots over 6 periods", label = 'tab:mu.lam_Micro_c')

#Betas
beta_tab <- beta.out[,c(1,2,3,7)]
xtable(beta_tab, caption = "Estimated Beta parameters", label = 'tab:mu.Beta_Micro')
######################













