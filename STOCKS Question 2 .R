library(mosaic)
library(quantmod)
library(foreach)

##############
#SET UP/plots
#############

# Import a few stocks
mystocks = c("SPY", "TLT", "LQD","EEM","VNQ")
myprices = getSymbols(mystocks, from = "2007-01-01")

# Adjust for splits and dividends
SPYa = adjustOHLC(SPY)
TLTa = adjustOHLC(TLT)
LQDa = adjustOHLC(LQD)
EEMa=adjustOHLC(EEM)
VNQa=adjustOHLC(VNQ)

#####
# Look at close-to-close changes
plot(ClCl(TLTa))
plot(ClCl(LQDa))
plot(ClCl(SPYa))
plot(ClCl(EEMa))
plot(ClCl(VNQa))
####

# Combine close to close changes in a single matrix
all_returns = cbind(ClCl(SPYa),ClCl(TLTa),ClCl(LQDa),ClCl(EEMa),ClCl(VNQa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# These returns can be viewed as draws from the joint distribution
pairs(all_returns)
plot(all_returns[,1], type='l')

# Look at the market returns over time
plot(all_returns[,5], type='l')

# An autocorrelation plot: nothing there
acf(all_returns[,5])

# The sample correlation matrix
cor(all_returns)

#################
# END SETUP/PLOTS
#################

############
# EVEN SPLIT
############
# Now simulate many different possible scenarios  
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim1[,n_days], 0.05) - initial_wealth

##############
# END 20 SPLIT
##############

###########
# SAFE PORT. 
###########

initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(.2, 0.3, 0.5, 0, 0)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim1[,n_days], 0.05) - initial_wealth

###############
# END SAFE PORT
###############

############
# RISKY PORT. 
############

initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0, 0, 0, .9, .1)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim1[,n_days], 0.05) - initial_wealth

################
# END RISKY PORT
################












##################
#EXPERIMENTAL CODE
##################
c=seq(from=0, to=1,by=.05)

weights=resample(c,5,replace=FALSE)

initial_wealth = 10000

sim_i=c(sim1,sim2)

for i in 1:sim_i


foreach(t=1:1) %do% {
  weights=resample(c,5,replace=FALSE)
  weight_tracker=rep(0,n_days)
  sim <- paste("sim", t, sep = "")
  assign(sim,foreach(i=1:5000, .combine='rbind') %do% {
    total_wealth = initial_wealth
    holdings = weights * total_wealth
    n_days = 10
    wealthtracker = rep(0, n_days)
    for(today in 1:n_days) {
      return.today = resample(all_returns, 1, orig.ids=FALSE)
      holdings = holdings + holdings*return.today
      total_wealth = sum(holdings)
      wealthtracker[today] = total_wealth
    }
    weight_tracker=weights 
    wealthtracker
  })
}



d <- 5
for(i in 1:10) { 
  nam <- paste("B", i, sep = "")
  assign(nam)
}


head(sim1)

hist(sim1[,n_days], 25)
