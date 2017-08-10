---
title: "Exercise One"
author: "Ali Prasla, Shreya Trivedi, Reece Wooten, Deeksha Yellam"
date: "August 10, 2017"
output: 
  html_document:
    toc: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Bayesian Problems:

###Part A:

$RC = Random Choice Event$

$TC = True Choice Event$

$Y = "Yes" Choice Event$

$N = "No" Choice Event$

####Solve for $P(Y/TC)$

$P(Y/TC) = P(Y n TC) / P(TC)$

$P(TC) = .7$

$P(Y n TC) = P(Y) - P(Y n RC)$

$P(Y) = .65$

$P(Y n RC) = P(RC) * P(Y/RC)$

$P(RC) = .3$

$P(Y/RC) = .5$

P(Y n RC) = 
```{r}
YAndRC = .3 * .5
YAndRC
```


P(Y n TC) = 

```{r}
YAndTC = .65 - YAndRC
YAndTC
```

Finally,

P(Y/TC) =  
```{r}
YAndTC/.7
```

### Part B.
$P = Positive Test Event$
$D = Has Disease Event$

####Solve for $P(D/P)$

$P(D/P) = (P(P/D) * P(D))/P(P)$
$P(P/D) = .993$
$P(D) = .000025$
$P(P) = P(P n D) + P(P n D^c)$

$P(P n D) = P(P/D) * P(D)$
```{r}
PAndD = .993 * .000025
```

$P(P n D^c) = P(P/D^c) * P(D^c)$
$P(P/D^c) = (1 - .9999)$
```{r}
PAndNotD = (1-.9999) * (1-.000025)
```

```{r}
P = PAndD + PAndNotD
```

$P(D / P) = $
```{r}
print((.993 * .000025) / P)
```

Yes. We foresee problems with universial testing. This test effectively has a 20% false positive rate. Yet, to determine whether or not to implement this policy, it is important to look at other factors, like the deadliness of the disease and the efficacy of early treatment. 



```{r include=FALSE}
library(mosaic)
library(ggplot2)
library(dplyr)
library(reshape2)
```

## Green Buildings

In this study, we evaluate the economic impact of "going green" on a latest project for an Austin real-estate developer. We consider the recommendations made by the developer's stats guru and establish whether or not he was right by doing some exploratory data analysis on the greenbuildings dataset.

```{r include=FALSE}
green_buildings=read.csv("greenbuildings.csv")

green_buildings$cluster=factor(green_buildings$cluster)
attach(green_buildings)
#######
# CREATES NEW DATA SETS OF JUST GREEN/NON GREEN BUILDINGS
######
green_buildings_green=subset(green_buildings,green_rating==1)
green_buildings_not_green=subset(green_buildings,green_rating==0)
```



### Outliers

One of the first assumptions made by the stats guru is that buildings which had very low occupancy rates are outliers and removing them from the dataset to avoid a potential distortion of the analysis.

Below is a histogram of the occupancy rates of the various buildings in the dataset. We can see that there's a considerable number of buildings with less than 10% occupancy rate, including some green buildings. 

```{r}
breaks = seq(0,100,10)
figure=ggplot()+ggtitle('Leasing Rate Histogram')+
       geom_histogram(data=green_buildings_not_green,aes(x= leasing_rate,fill='Non Green Buildings'),color='white',breaks=breaks)+
       geom_histogram(data=green_buildings_green,aes(x=leasing_rate,fill='Green Buildings'),color='black',breaks=breaks)+
       scale_x_continuous(breaks = breaks)+
  scale_fill_manual(name="Bar Color",
                    values=c('Non Green Buildings'='snow3', 'Green Buildings'='olivedrab4'),guide='legend')
figure
```


Our recommendation is to not exclude the buildings with less than 10% occupancy as they represent a non-negligible portion of the data. It is possible that our estimate of rents may get skewed if we eliminate this chunk of data.

### Market rent of Green buildings Vs Non-Green buildings

The stats guru looked at the green buildings and non-green buildings separately and calculated median market rents for both categories.Below is a histogram of the rents of all buildings.

```{r}
green=median(green_buildings_green$Rent)
not=median(green_buildings_not_green$Rent)
figure=ggplot()+ ggtitle('Histogram of Rent')+
  geom_histogram(data=green_buildings_not_green, aes(x=Rent, fill='Non green buildings'))+
  geom_histogram(data=green_buildings_green,aes(x=Rent,fill='Green Buildings'))+geom_vline(aes(xintercept = not,color='Non green buildings median rent'))+geom_vline(aes(xintercept = green,color='Green buildings median rent'))+
  scale_fill_manual(name="Bar Color",
                      values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4','Non green buildings median rent'='dimgray', 'Green buildings median rent'='darkseagreen2'),guide='legend')+
  scale_color_manual(name="Line Color",
                     values=c('Non green buildings median rent'='dimgray', 'Green buildings median rent'='darkseagreen2'),guide='legend')
figure
```


We can see that there are some outliers whose rent exceeds $125 per sq.ft. and he was right in calculating the median rent instead of mean as the median is more robust to outliers. But, we cannot conclude that green buildings fetch higher rent just by looking at the difference in median rents between green and non-green buildings.

For example, if we look at the median value of rents for green vs. non-green buildings which are between 240,000-260,000 sqft in size, we notice that median rent for green buildings is actually lower than non-green buildings by $2.06 per sq.ft.

```{r}
green_buildings_25=subset(green_buildings_green, size<=260000 & size>=240000)
cat('Median value for green buildings in the 24k-26k sqft size range =',median(green_buildings_25$Rent))
```

```{r}
green_buildings_25_not=subset(green_buildings_not_green, size<=260000 & size>=240000)
cat('Median value for non-green buildings in the 24k-26k sqft size range =',median(green_buildings_25_not$Rent))
```


The stats guru's analysis ignores the possibility of interactions between green rating of a building and the various other variables that also affect the rents of buildings like age, renovated, class of building quality etc. The fact that green buildings seem to have higher median rents only establishes correlation between rent and green rating. It does not establish causality. The scatter plots below can help illustrate this effect.

```{r}
figure=ggplot()+ ggtitle('Rent Vs. Age')+
  geom_point(data=green_buildings_not_green, aes(y=Rent,x=age, color='Non green buildings'))+
  geom_point(data=green_buildings_green,aes(y=Rent,x=age,color='Green Buildings'))+
  scale_colour_manual(name="Dot Color",
                      values=c('Non green buildings'='snow3', 'Green Buildings'='olivedrab4'),guide='legend')
figure
```

In the above scatter plot, we can see that the green buildings are clustered around low age, which is understandable since the concept of a 'green' building is relatively new. And we can also see that new buildings have slightly higher rents than the older buildings, if we ignore outliers. So, a portion of the high rents we see in green buildings is probably only because they are new and not because they are green. 

We also observed that there is a high correlation between the construction class of buildings and the green rating. Close to 80% of the green buildings fall under class 'A'. Below chart shows a comparison of the median rent values by class and green rating. We see that class A buildings get higher rent than non-class A buildings. But there is no difference between the rents for a green vs. non-green class A building. 

```{r}

mediansGreen = green_buildings_green %>% group_by(class_a) %>% summarise(median = median(Rent))
mediansNotGreen = green_buildings_not_green %>% group_by(class_a) %>% summarise(median = median(Rent))


medians = cbind(mediansGreen,mediansNotGreen)

colnames(medians) = c("class_a","Green Buildings","class_a","Non-Green Buildings")
medians = medians[,2:ncol(medians)]
medians = melt(medians,id = "class_a")
breaks=seq(0,1,1)
figure=ggplot(medians,aes(x = class_a,y = value,fill=variable)) + geom_col(position = "dodge")+ ggtitle('Class_A Vs. Median Rent')+
  scale_fill_manual(values=c("olivedrab4", "snow3")) + scale_x_discrete() + geom_text(x = 0,y = 26.5,label = "Not Class A") + geom_text(x = 1,y = 29.2,label = "Class A")+ylab('Median Rent')
figure
```

### Multiple Regression Model

To see the effect of green rating of a building on rents, we need to control for the other variables that are correlated with green rating. A multiple regression model of Rent on the factors can explain this. We dropped factors like cluster, LEED, Energystar and total_dd_07 from the linear model, because these variables do not add any extra information than what is already in the other variables.

```{r}
fit_all=glm(Rent~.-cluster -LEED -Energystar -total_dd_07,data=green_buildings)
summary(fit_all)
```

From the summary of the linear model fit, we can see that green rating of a building is not at all significant factor in determining the rent of the building.

The stats guru also assumed that the rent per sqft is not affected by the size of the building. He just calculated the median value of rents per sqft for all the buildings and multiplied the additional median value for green buildings with the square footage of the building. Below chart shows a plot of the Rent per sq.ft. for a building as a function of the total square footage of the building. We see that bigger buildings tend to have a higher per sq.ft rent. So, we would be able to get a realistic figure if we included the size of the building into our rent estimate.

```{r}
figure=ggplot()+ggtitle('Rent Vs. Size')+
       geom_point(data=green_buildings_not_green,aes(y=Rent,x=size,color='Non_green_buildings'))+
       geom_point(data=green_buildings_green,aes(y=Rent,x=size,color='Green_buildings'))+
       scale_colour_manual(name="DotColor",values=c('Non_green_buildings'='snow3','Green_buildings'='olivedrab4'),guide='legend')
figure
```

### Economic Benefit Calculation
According to the stats expert, "Our expected baseline construction costs are $100 million, with a 5% expected premium for green certification. Thus we should expect to spend an extra $5 million on the green building. Based on the extra revenue we would make, we would recuperate these costs in $5000000/650000 = 7.7 years. Even if our occupancy rate were only 90%, we would still recuperate the costs in a little over 8 years. Thus from year 9 onwards, we would be making an extra $650,000 per year in profit. Since the building will be earning rents for 30 years or more, it seems like a good financial move to build the green building."

The above calculation assumes that the rent earned on the building will not change as the building becomes older. Rent earned on a ten year old building will not be the same as the rent earned on a new building. The calculation does not also take into account the time value of money.


In conclusion, the stats "guru" overly simplified the problem at hand and ignored a lot of factors that affect the rent earned on a building. If we build in all of the relevant factors listed in the dataset into our estimate of a building's rent, we will notice that a building's green rating is not a statistically significant factor in determining the rent earned on the building and will not on its own translate into any significant economic benefit to the real-estate developer.



## Portfolio Bootstrapping

In this question we have explored below five asset classes:

- US domestic equities (SPY: the S&P 500 stock index)
- US Treasury bonds (TLT)
- Investment-grade corporate bonds (LQD)
- Emerging-market equities (EEM)
- Real estate (VNQ)

We have taken our data from 2007-01-01. After exploring the data we created three portfolios for these assets assuming we have notional $100,000 to invest in one of these portfolios.

1)
First, we explored the properties of five assets we are considering.

Installing relevant libraries. _Quantmod_ helps us to download several years of daily data on these ETFs.
```{r include = FALSE}
library(mosaic)
library(quantmod)
library(foreach)
```


Importing the stocks we are considering
```{r include = FALSE}
mystocks = c("SPY", "TLT", "LQD","EEM","VNQ")
myprices = getSymbols(mystocks, from = "2007-01-01")
```


Adjusting for splits and dividends
```{r}
SPYa = adjustOHLC(SPY)
TLTa = adjustOHLC(TLT)
LQDa = adjustOHLC(LQD)
EEMa=adjustOHLC(EEM)
VNQa=adjustOHLC(VNQ)
```


Looking at close-to-close changes
```{r}
par(bg = "gray")
plot(ClCl(TLTa),main = "US Treasury Daily Returns",ylab = "Return",col = "navy")
plot(ClCl(LQDa),main = "Investment Grade Corporate Bonds Daily Returns",ylab = "Return",col = "navy")
plot(ClCl(SPYa),main = "S&P 500 Daily Returns",ylab = "Return",col = "navy")
plot(ClCl(EEMa),main = "Emerging Market ETF Daily Returns",ylab = "Return",col = "navy")
plot(ClCl(VNQa),main = "REIT Daily Returns",ylab = "Return",col = "navy")
``` 

Combining close to close changes in a single matrix
```{r}
all_returns = cbind(ClCl(SPYa),ClCl(TLTa),ClCl(LQDa),ClCl(EEMa),ClCl(VNQa))
all_returns = as.matrix(na.omit(all_returns))
```

We calculated the sharpe ratio of individual stocks.

```{r}
summary_stocks = cbind(t(t(colMeans(all_returns))),t(t(apply(all_returns,2,sd))))
summary_stocks = cbind(summary_stocks,summary_stocks[,1]/summary_stocks[,2])
colnames(summary_stocks) = c("Average Returns","ST.Dev of Returns","Sharpe Ratio")
summary_stocks
```

These returns can be viewed as draws from the joint distribution
```{r}
pairs(all_returns)
```


The correlation matrix between stocks
```{r}
cor(all_returns)
```

2)
Now we are going to create our first portfolio with even split. We will assign the weight of each asset as 20%. 

```{r}
initial_wealth = 100000
set.rseed(1)    #Setting seed
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
    #rebalance portfolio
    holdings = weights * total_wealth
  }
  wealthtracker
}

head(sim1)

```

Profit/loss of our portfolio:

```{r}
par(bg = "gray")
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=30,main = "Histogram of Portfolio Returns",xlab = "End of Period Return",ylab = "Frequency",col = "blue")
abline(v = mean(sim1[,n_days]) - initial_wealth,col = "red",lw = 3)
```

Calculating 5% value at risk for this portfolio

```{r}
quantile(sim1[,n_days], 0.05) - initial_wealth
```

Hence for this portfolio the average return is $100946.6 and value at risk is $6297.993  


3) Now we are going to create a safer portfolio which reduces our risk. To create this portfolio, we first calculated the average returns and risk involved with each asset had we invested all our wealth into that portfolio. Based on the results we tried different combinations and came up with weights which increased the return of portfolio and minimized the risk involved. Below are the weights:
TLT - 20%
LQD - 30%
SPY - 50%
EEM -  0%
VMQ -  0%

```{r}
initial_wealth = 100000
set.rseed(1)  #Setting seed
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
    weights = c(.2, 0.3, 0.5, 0, 0)
    holdings = weights * total_wealth
  }
  wealthtracker
}

head(sim1)

```


Profit/loss for this portfolio
```{r}
par(bg = "gray")
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=30,main = "Histogram of Portfolio Returns",xlab = "End of Period Return",ylab = "Frequency",col = "blue")
abline(v = mean(sim1[,n_days]) - initial_wealth,col = "red",lw = 3)
```

Calculating 5% value at risk
```{r}
quantile(sim1[,n_days], 0.05) - initial_wealth
```

Hence for this portfolio the average return is $100531 and value at risk is $3066.526

4) Next we will create an aggressive portfolio which increases our return as well as our risk. Ater trying several combinations, below is the weight we considered for this portfolio:

TLT -  0%
LQD -  0%
SPY -  0%
EEM - 90%
VMQ - 10%

```{r}
initial_wealth = 100000
set.rseed(1)      #Setting seed
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
    weights = c(0, 0, 0, .9, .1)
    holdings = weights * total_wealth
  }
  wealthtracker
}

head(sim1)

```

Profit/loss for this portfolio
```{r}
par(bg = "gray")
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=70,main = "Histogram of Portfolio Returns",xlab = "End of Period Return",ylab = "Frequency",col = "blue")
abline(v = mean(sim1[,n_days]) - initial_wealth,col = "red",lw = 3)
```

Calculating 5% value at risk
```{r}
quantile(sim1[,n_days], 0.05) - initial_wealth
```
Hence for this portfolio the average return is $101872.8 and value at risk is $12925.12 

Below is the summary of our results:

Portfolio 1 (Even split):
Mean Return - $100946.6
Value at Risk - $6297.993

Portfolio 2 (Safe):
Mean Return - $100531
Value at Risk - $3066.526 

Portfolio 3 (Risky):
Mean Return $101872.8
Value at Risk - 12925.12 


## NutrientH20 Market Segmentation

First, load the data.
```{r}
df = read.csv("social_marketing.csv")
```

View a sample of the data:
```{r}
head(df)
```
Goal of Data Analysis: Find categories preferred by users so that NutrientH20 can tailor an appropriate marketing message.

Solution: Find the percentage of tweets per category, scale and find the most prominent clusters.


First, change the data frame total tweet numbers to fractions and drop the chatter column. We decided to drop the chatter column because of that column's inability to help shape our marketing message. Because a large number of tweets are characterized as "chatter", this would merely add noise to our clustering model and fail to provide interpretable results.
```{r}
df$X = NULL
df = data.frame(as.matrix(df)/rowSums(as.matrix(df)))
tempDf = df
#drop chatter 
tempDf$chatter= NULL
head(df)
```


Second, let's load the libraries required. This includes registering parallel computing.
```{r include = FALSE}
#set up parallel computing
library(foreach)
library(doParallel)
library(LICORS)
library(clusterCrit)
##register parallel backend
registerDoParallel(cores = 4)
```


Next, run a function _ClusterTweeters_. That function, given a K and a dataframe will return the CH score of all that model. _withoutChatter_ is a matrix with the CH score and SSE(tot) of all the models tested between k = 1 and k = 15. This step included **%dopar%** to tell the foreach loop to compute the results in parallel.
```{r}
clusterTweeters = function(numK,df){
  model = LICORS::kmeanspp(as.matrix(df), k = numK,nstart = 10)
  #find ch value for the model
  c(clusterCrit::intCriteria(as.matrix(df),model$cluster,"Calinski_Harabasz"),model$tot.withinss)
}

withoutChatter = foreach(k = 1:15,.combine = 'rbind')%dopar%
{
  clusterTweeters(k,tempDf)
}
colnames(withoutChatter) = c('CH Score','Fit')
withoutChatter
```





```{r}
par(bg = "gray")
plot(1:nrow(withoutChatter),withoutChatter[,2],col = "red",main = "Fit vs. K",type = "l",xlab = "K",ylab = "SSE (tot)")
```
Using the elbow method, there is no clear shift in slope of the line as K increases. This curve looks closer to an exponential decay than an elbow. The elbow method, therefore, is not a good metric of choosing the optimal number of market segments. A heuristic such as CH score could find a better optimal K.

Let's plot the CH scores of all the models tested. 
```{r}
par(bg = "gray")
plot(1:nrow(withoutChatter),withoutChatter[,1],main = "K selections versus CH Score",xlab = "K",ylab = "CH Score",type = "l",col = "red")
abline(v = (which.max(withoutChatter)+1),col = "blue",lw = 2)
```
CH score indicates two optimal market segments. We believe that this is the best metric for determining the clusters because of the fit vs. simplicity trade off inherent within the heuristic and it's clear answer: two segments.



Next, let's analyze the important components of that each cluster. In other words, for each cluster's centriod, let's look for the centriod with the highest value.

```{r}
optimalModel = LICORS::kmeanspp(tempDf, (which.max(withoutChatter)+1),nstart = 10)
#plot parameters for each model
topN = 5
centriodMat = matrix(NA,2,topN)
par(bg = "gray",mfrow = c(2,1))
for (k in 1:2)
{
  sorted = sort(optimalModel$centers[k,],decreasing = TRUE)[1:topN]
  barplot(sorted,ylim = c(0,.23),col = "blue",main = paste("Figure",k,":"))
  centriodMat[k,] = names(sorted)
}

print(paste("Top",topN,"important centriod indicators per cluster: "))
print(centriodMat)
```

The one of the clusters looks to be composed of people who are very concious of their health. They seem to be focused on Personal Fitness and are interested in the outdoors. This indicates a very physically active group of people and would recommend that the marketing message ought to be related to personal health. This cluster contains this fraction of social media users:

```{r}
length(which(optimalModel$cluster == 1))/length(optimalModel$cluster)
```

The second cluster appears to be grouped for millenials. Politically engaged(current_events and politics), educated (college_uni) and active in photo sharing, this could be a young group of people. We would need to dig more into the profiles of this segment, but a marketing strategy focused on millenials could be appropriate. A visually appealing marketing campaign would be attractive to these social media followers.

This clusters contains this fraction of social media users:

```{r}
length(which(optimalModel$cluster == 2))/length(optimalModel$cluster)
```