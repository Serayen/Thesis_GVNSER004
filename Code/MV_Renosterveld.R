

#NDLM SSM with forecasting
#Set forcast period (for_per) to 0 to run the NDLM normally

library(tidyverse)
#Load In data
setwd("C:/Users/Seray/Research Work/Data")
#Reseeders
load("MicrodonDat_Plot_C.RData")
load("Othonna_Plot_C.RData")
#Resprouters
load("Elytropappus_Plot_C.RData")
load("Passerina_Plot_C.RData")
#parasite
load("Thesium_Plot_C.RData")
setwd("C:/Users/Seray/Research Work/Code")
#Set Fynbos to 1 and Renostoveld as 0
dat <- data.Microdon.C  %>% mutate(Site = ifelse(Site == "Fynbos",1,0))
#dat <- cbind(dat[,1:4], dat[,5:10])
#Set Treatment to 1 and control as 0
dat <- dat  %>% mutate(CT = ifelse(CT == "Treatment",1,0))
#dat <-dat %>% add_column(seq_plotNo = seq(1,12))



#Thesium data
dat.th <- data.Thesium.C%>% mutate(Site = ifelse(Site == "Fynbos",1,0))
dat.th <- dat.th  %>% mutate(CT = ifelse(CT == "Treatment",1,0))
dat.th_ren <- dat.th %>% filter(Site == 0)
dat.th_ren <-dat.th_ren %>% add_column(seq_plotNo = seq(1,12))
dat.th_fyn <- dat.th %>% filter(Site == 1)

#Othonna data
dat.oth <- data.Othonna.C%>% mutate(Site = ifelse(Site == "Fynbos",1,0))
dat.oth <- dat.oth  %>% mutate(CT = ifelse(CT == "Treatment",1,0))
dat.oth_ren <- dat.oth %>% filter(Site == 0)
dat.oth_ren <-dat.oth_ren %>% add_column(seq_plotNo = seq(1,12))
dat.oth_fyn <- dat.oth %>% filter(Site == 1)

#Elytropappus data
dat.el <- data.Elytropappus.C%>% mutate(Site = ifelse(Site == "Fynbos",1,0))
dat.el <- dat.el  %>% mutate(CT = ifelse(CT == "Treatment",1,0))
dat.el_ren <- dat.el %>% filter(Site == 0)
dat.el_ren <-dat.el_ren %>% add_column(seq_plotNo = seq(1,12))
dat.el_fyn <- dat.el %>% filter(Site == 1)

#Passerina data
dat.pas <- data.Passerina.C%>% mutate(Site = ifelse(Site == "Fynbos",1,0))
dat.pas <- dat.pas  %>% mutate(CT = ifelse(CT == "Treatment",1,0))
dat.pas_ren <- dat.pas %>% filter(Site == 0)
dat.pas_ren <-dat.pas_ren %>% add_column(seq_plotNo = seq(1,12))
dat.pas_fyn <- dat.pas %>% filter(Site == 1)




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

sink("MV_Microdon.jag")
cat("
    model {
    # Priors
    
    for(i in 1:x) #Loop trough each plot
    {
   
    for(j in 1:4)#for each data matrix
     {
      N[i,1,j] ~ dunif(0, 40000)
     }#end j
    }#end i
    
    

    
    for(j in  1:4)
    {
      for(k in 1:4)
      {
        Q[j,k] ~ dunif(0,5000)
        R[j,k] ~ dunif(0,5000)
      }#end k
    }#endj
    
    
    

    
    for(i in 1:4)
    {
     u[i] ~ dunif(0,10)
     a[i] ~ dunif(0,10)
    }
  
   
   for(i in 1:4)
    {
    for(j in 1:4)
    {
      beta[i,j] ~ dnorm(0, 0.01)
      Z[i,j] ~ dnorm(0, 0.01)
    }#end j
    
    }#end i
   
   
    mu = c(0,0,0,0)
   
    # Likelihood
    for(i in 1:(x)){

    #State Process
    #eps.plot[i] ~ dnorm(0,tau.plot)
    w[1:4,i,1] ~ dmnorm.vcov(mu[1:4], Q[1:4,1:4])
    for(t in 1:(T-1)) #For each time step after the first modeling each plot seperately
    {
    
    N[i,(t+1),1:4] = beta %*%  N[i,(t),1:4] + u + w[1:4,i,t]
    w[1:4,i,(t+1)] ~ dmnorm.vcov(mu[1:4], Q[1:4,1:4])
    
    }#end t
    
    
    #Observation Process
    for(t in 1:(T))
    {
    #v[1:2,i,t] ~ dmnorm(mu[1:5], R[1:5,1:5])
    y[i,t,1:4] ~ dmnorm.vcov(N[i,(t),1:4], R[1:4,1:4])
    #y[i,t,1:2] = Z %*% N[i,(t),1:2] + a + v[1:2,1,t]
    
    
    #y[i,t]  ~ dnorm(N[i,t], tau.obs)
    }#end t
    
    }#end i
    
    
    
    
    }#end model
    
    
    
    ",fill = TRUE)#end cat
sink()

#Renosterveld SSM
dat <- dat_ren
dat <-dat %>% add_column(seq_plotNo = seq(1,nrow(dat)))
#Data grouped by treatment/control
dat_T <- dat %>% filter(CT == 1)
dat_C <- dat %>% filter(CT == 0) 

#Other species
dat.th <- dat.th_ren
dat.oth <- dat.oth_ren
dat.el <- dat.el_ren
dat.pas <- dat.pas_ren

#dat <-dat %>% add_column(seq_plotNo = seq(1,nrow(dat)))
#Data grouped by treatment/control
dat.th_T <- dat.th %>% filter(CT == 1)
dat.th_C <- dat.th %>% filter(CT == 0)






#Setting up Variables for JAGS function in R 
y1 = as.matrix(dat[, c(8:13)])  #Observed volume of population
y2 = as.matrix(dat.oth[, c(8:13)])
y3 = as.matrix(dat.el[, c(8:13)])
y4 = as.matrix(dat.pas[, c(8:13)])
#y5 = as.matrix(dat.th[, c(8:13)])


Y = array(c(y1,y2,y3,y4), dim = c(dim(y1)[1], dim(y1)[2],4))

#site = as.matrix(dat[,2]) #Site and treatment as categorical varibles
treat = as.matrix(dat[,7])




#Setup JAGS data
jags.data <- list(y = Y, T=6, x = dim(Y)[1] )

# Function to generate starting values
inits <- function()
{
  N1=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1 )*dim(Y)[1])), nrow=dim(Y)[1])
  N2=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1)*dim(Y)[1])), nrow=dim(Y)[1])
  N3=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1)*dim(Y)[1])), nrow=dim(Y)[1])
  N4=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1)*dim(Y)[1])), nrow=dim(Y)[1])
  #N5=  matrix(c(runif(dim(Y)[1],0,5000),rep(NA,(dim(Y)[2]-1+(for_per))*dim(Y)[1])), nrow=dim(Y)[1])
  a = dim(Y)[1]
  b = dim(Y)[2]
  NT = array(c(N1,N2,N3,N4), dim = c(a,b,4) )
  
  list( Q = diag(runif(4, 0, 5),4,4), R = diag(runif(4, 0, 5),4,4), beta = matrix(runif(16,0,1),4,4), Z = matrix(runif(16,0,1),4,4),  N = NT,
        w = array(runif((4*dim(Y)[1]*dim(Y)[2]),0,1), dim = c(4,dim(Y)[1],dim(Y)[2]) ), 
        v = array(runif((4*dim(Y)[1]*dim(Y)[2]),0,1), dim = c(4,dim(Y)[1],dim(Y)[2]) ),
        u= runif(4,0,1) , a= runif(4,0,1)) 
  
} 


# Parameters to be estimated
parameters <- c("Q","R", "beta", "Z", "v", "w", "a", "u" , "N" )


# MCMC settings
nc <- 3 # Number of chains
ni <- 1000 # Number of draws from posterior (for each chain)
nb <- 500 # Number of draws to discard as burn in
nt <- 2 # Thinning rate


#Start Gibbs sampler: Run model in JAGS and save results in object called ssm
#***Useful Data is saved in objects so it is not necessary to run this to check the rest of the code
ssm2 <- jags(jags.data, inits, parameters, "MV_Microdon.jag", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb) 

ssm2 <- autojags(ssm2, n.iter=1000)


setwd("C:/Users/Seray/Research Work/Data")
#save(ssm2, file = "MVMicrodon_SSM.RData")
#load("MVMicrodon_SSM.RData")
setwd("C:/Users/Seray/Research Work/Code")

ssm2$BUGSoutput$mean$beta

NT.out <- ssm2$BUGSoutput$mean$N
N.out <- NT.out[,,1]



obs.errors <- Y[,,1] - N.out
obs.errors
plot(obs.errors[3,], type = 'l')

par(mfrow=c(1,1))
plot(N.out[1,], type = 'l', ylim = c(0, max(c(Y[1,,1], N.out[1,]) )), ylab = "Volume", xlab = "Time Step" , main = "Estimated and \n Observed Population at \n Plot 1 Control")
lines(N.out[1,], type = 'l', col = 'blue')
lines(Y[1,,1], type ='l', col = 'red')


#N.out <- ssm2$BUGSoutput$mean$N
#Predicted Data
tmp <- as_tibble(N.out) 
tmp <- rename(tmp, "1" = "V1", "2" = "V2" , "3" ="V3", "4" = "V4", "5" = "V5", "6" = "V6")
preds <- as_tibble(cbind(dat[,1:7],tmp))
preds_T <- preds %>% filter(CT == 1)
preds_C <- preds %>% filter(CT == 0) 

N.out_T <- as.matrix(preds_T[, c(8:(13))])
N.out_C <- as.matrix(preds_C[, c(8:(13))])

Y_T = as.matrix(dat_T[, c(8:13)])
Y_C = as.matrix(dat_C[, c(8:13)])

#Summary stats of estimations population volumes
N.sum <- ssm2$BUGSoutput$summary

N1.sum <- N.sum[seq(1,196, by = 12),]
N1.sum <- N1.sum[1:(6),]

plot(N.out[1,], type = 'l', ylim = c(0, max(N1.sum[,7])), ylab = "Volume", xlab = "Time Step" , main = "Estimated and \n Observed Population at \n Plot 1 Control")
polygon(c(seq(1:6),rev(seq(1:6))),c(N1.sum[,3],rev(N1.sum[,7])),col = "grey80", border = FALSE)
lines(N.out[1,], type = 'l', col = 'blue')
lines(Y[1,,1], type ='l', col = 'red')



#########################################
#-------------PLOTS---------------------#

#Summary stats of estimations population volumes
period <- (6)
nBeta <- 4
#Number of plots there were recordings in
nPlots <-12
N.sum <- ssm2$BUGSoutput$summary

N1.sum <- N.sum[1:72,]
N2.sum <- N.sum[73:144,]
N3.sum <- N.sum[145:216,]
N4.sum <- N.sum[217:288,]



# # mu.lam + lam + N + deviance
 No_entries = 12 *6
# #(2*12*(period-1)) + 12*period +1 + nBeta 
# deviance.sum <- N.sum[((12*period)+2),]
# N.sum<- rbind(N.sum[1:((12*period+2)),], N.sum[((12*period)+4):No_entries,])
# No_entries = (2*12*(period-1)) + 12*period  + nBeta 

#Summary Statistics for Confidence intervals By Plot Number
N_sum1 <- vector("list")
N_sum2 <- vector("list")
N_sum3 <- vector("list")
N_sum4 <- vector("list")
for(i in 1:12)
{
  
  Ni.sum <-N1.sum[seq(i,No_entries, by = 12),]
  N_sum1[[i]] <- Ni.sum[1:period,]
  
  Ni.sum <-N2.sum[seq(i,No_entries, by = 12),]
  N_sum2[[i]] <- Ni.sum[1:period,]
  
  Ni.sum <-N3.sum[seq(i,No_entries, by = 12),]
  N_sum3[[i]] <- Ni.sum[1:period,]
  
  Ni.sum <-N4.sum[seq(i,No_entries, by = 12),]
  N_sum4[[i]] <- Ni.sum[1:period,]
}

Add_legend <- function()
{
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',legend = c("Estimated", "Observed"), col = c("blue", "red"), lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1.8, seg.len=1.4, bty = 'n')
}

Plot_ren_T <- function(NS, NT.out, dt, TC)
{
  #Graphs for Renosterveld Treatment Plots
  par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
  
  N_sum <- NS
  dat <- dt
  N.out <- NT.out
  tmp <- as_tibble(N.out) 
  tmp <- rename(tmp, "1" = "V1", "2" = "V2" , "3" ="V3", "4" = "V4", "5" = "V5", "6" = "V6")
  preds <- as_tibble(cbind(dat[,1:7],tmp))
  preds_T <- preds %>% filter(CT == 1)
  preds_C <- preds %>% filter(CT == 0) 
  dat_T <- dat %>% filter(CT == 1)
  dat_C <- dat %>% filter(CT == 0)
  datTC <- dat_T 
  N.out_T <- NULL
  Y_T <- NULL
  if(TC == 1){
    N.out_T <- as.matrix(preds_T[, c(8:(13))])
    Y_T = as.matrix(dat_T[, c(8:13)])
    
  }
  else{
    N.out_T <- as.matrix(preds_C[, c(8:(13))])
    Y_T = as.matrix(dat_C[, c(8:13)])
    datTC <- dat_C
  }
  
  
  for(i in 1:6)
  {
  
    plotNo <- as.numeric(datTC[i,14])
    title= paste( "Plot", toString(as.numeric(datTC[i,1])) )
    
    plot(N.out_T[i,], type = 'l', col = 'blue', ylim = c(0, max(c(N_sum[[plotNo]][1:6,7],Y_T[i,])) ) , ylab = "Volume", xlab = "Time Step",  main = title, cex.main = 1.3)
    polygon(c(seq(1:period),rev(seq(1:period))),c(N_sum[[plotNo]][,3],rev(N_sum[[plotNo]][,7])),col = "grey80", border = FALSE)
    lines(N.out_T[i,], type = 'l', col = 'blue', lwd = 2)
    lines(Y_T[i,], type ='l', col = 'red', lwd = 2)
  }
  Add_legend()
}#end function

#(N_Sum, NT.out, dat, TC)
#Microdon Plots
#treat
Plot_ren_T(N_sum1, NT.out[,,1], dat, 1 )
#control
Plot_ren_T(N_sum1, NT.out[,,1], dat, 0 )

#Oth plots
#Treat
Plot_ren_T(N_sum2, NT.out[,,2], dat.oth, 1 )
#control
Plot_ren_T(N_sum2, NT.out[,,2], dat.oth, 0 )

#Elyt plots
#Treat
Plot_ren_T(N_sum3, NT.out[,,3], dat.el, 1 )
#control
Plot_ren_T(N_sum3, NT.out[,,3], dat.el, 0 )

#Pas plots
#Treat
Plot_ren_T(N_sum4, NT.out[,,4], dat.pas, 1 )
#control
Plot_ren_T(N_sum4, NT.out[,,4], dat.pas, 0 )



h <- as.vector(N.sum[341:356,][,1])
matrix(h,4,4)
ssm2$BUGSoutput$mean$beta

library(xtable)
xtable(ssm2$BUGSoutput$mean$beta)


ssm2$BUGSoutput
summary(ssm2[[2]][7])

library(mcmcplots)

denplot(ssm2, parms = "Q")
traplot(ssm2, parms = "Q")

      


