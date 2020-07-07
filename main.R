library(quantmod)
library(PerformanceAnalytics)

#setting start and end dates 
from <- "2011-04-01"
to <- "2019-04-01"

#Calling relevant stock data
getSymbols("GS", from=from, to=to, src="yahoo")

#splitting the data in half, train --> 1st half, test --> 2nd half
train.data <- GS[1:(length(Cl(GS))/2),]
test.data <- GS[((length(Cl(GS))/2)+1):length(Cl(GS)),]

#getting SPY data for comparison of returns
getSymbols("SPY", from=from, to=to)
#splitting data into half 
train.spy <- SPY[1:(length(Cl(SPY))/2),]
test.spy <- SPY[((length(Cl(SPY))/2)+1):length(Cl(SPY)),]

#daily returns on SPY 
dretspy <- dailyReturn(train.spy)
#dretspy <- dailyReturn(test.spy)

#initializing data set 
stockData <- train.data
#stockData <- test.data
price <- Cl(stockData)
hlc <- cbind(Hi(stockData), Lo(stockData), Cl(stockData))
dret <- dailyReturn(stockData)

#Setting small and long periods for EMA
s <- 10 
l <- 50 
ADXv <- 10

#Price chart with relevant indicators 
lineChart(stockData)
addEMA(s)
addEMA(l, col="white")
addADX(l)

#calculating technical indicator values 
#EMAs
emas <- EMA(price,s)
#Lag of EMAs
lemas <- Lag(emas)
#Lag of Lag of EMAs
llemas <- Lag(lemas)
#EMAl
emal <- EMA(price,l)
#Lag of EMAl
lemal <- Lag(EMA(price,l))

#ADXl
adx <- ADX(hlc,l)
#Lag of ADX 
ladx <- Lag(adx$ADX)

#to identify the peaks and troughs of short ema I used the following vector
# if short ema is greater than its lagged value it went up --> 1
# if short ema is lesser than its lagged value it went down --> -1
# I applied the above rule on lag and second lag as we can only use past data for decisions
hl_emas <- ifelse(lemas>llemas,1,ifelse(lemas<llemas,-1,0))

#Since the values are 1 and -1, if the product of the value and its lag
#is negative, we can say there has been a change --> 1 
#else no change, EMA continued to rise/fall as before
#X here calculates this value
x <- ifelse(hl_emas*Lag(hl_emas)<0,1,0)


#Here is a dataset to see the relation between EMA lags and X 
emas_pricedata <- cbind(lemas,llemas,hl_emas, x, price)
names(emas_pricedata) <- c("lag_ema", "llag_ema", "UpDown", "X", "Price")


#initialize vectors 
trade <- c()
trade[1:(2*l)] <- 0
shares <- c()
shares[1:(2*l)] <- 0
wealth <- c()
wealth[1:(2*l)] <- 1000000
profit <- c()
profit[1:(2*l)] <- 0
#assuming transaction cost = 0.2% of traded value -- size*price
tradecost <- 0.002

#Market Entry: Enter in Long Position or Short Position 
for (i in (2*l+1):length(price)){
  if (shares[i-1] != 0){
    #Only run entry trigger if there was no entry earlier, i.e, no share balance
    break
  }
  else if (lemas[i] > lemal[i]){
    #Enter in long position if short EMA > long EMA
    trade[i] <- 1
  }
  else if (lemas[i] < lemal[i]){
    #Enter in short position if short EMA < long EMA
    trade[i] <- -1
  }
  else {
    #Do not enter yet
    trade[i] <- 0
  }
  #update values
  #assuming investment of all wealth
  size <- as.integer(floor(wealth[i-1]/price[i]))
  shares[i] <- shares[i-1] + trade[i]*size 
  cost <- abs(trade[i])*size*price[i]*tradecost
  gain <- shares[i]*dret[i]*price[i]
  wealth[i] <- wealth[i-1] + gain - cost
  profit[i] <- (wealth[i]/wealth[i-1])-1
  cat("Entered with ", shares[i], "shares @ $", price[i], "each")
  print(index(price[i]))
}

#Once last loop breaks after entry fix entry day
entryday <- length(shares)
#size is fixed at entry value of size 
size <- size 

#Trading rules 
#Buy when hl_emas changes from -1,-1,....-1, to 1 -----> trough 
# and ADX(m) < 20, weak trend prospects of reversal
#i.e. if hl_emas changes from -1 to 1 ---> peak --> buy

#Sell when hl_emal changes from 1,1,1....1, to -1 -----> peak 
# and ADX(m) < 20, weak trend prospects of reversal
#i.e. if hl_emas changes from 1 to -1 ---> peak --> sell 


for (i in (entryday+1):length(price)){
  if (x[i] == 1 & hl_emas[i]==1 & ladx[i] < ADXv){
    #hl_emas changed from -1 to 1 --> trough --> go long
    if (shares[i-1] > 0){
      #if you are in a long position, stay long
      trade[i] <- 0
    }
    else {
      #if you are in a short position, sell all and buy more 
      #switch to long position
      trade[i] <- 2
    }
  }
  else if (x[i]==1 & hl_emas==-1 & ladx[i] < ADXv){
    #hl_emas changed from 1 to -1 --> peak --> short
    if (shares[i-1] > 0){
      #if you are in a long position
      #sell all and switch to short --> sell shares held
      #also short sell same amount
      trade[i] <- -2
    }
    else {
      #if you are in a short position, stay short
      trade[i] <- 0
    }
  }
  else {
    #Hold
    trade[i] <- 0
  }
  shares[i] <- shares[i-1] + trade[i]*size 
  cost <- abs(trade[i])*size*price[i]*tradecost
  gain <- shares[i]*dret[i]*price[i]
  wealth[i] <- wealth[i-1] + gain - cost
  profit[i] <- (wealth[i]/wealth[i-1])-1
}

#Performance 
#no of trade signals (can be increased by relaxing ADX condition)
table(trade)
tradelab <- ifelse(trade==-2, "SELL 2X", 
                   ifelse(trade==2, "BUY 2X", 
                          ifelse(trade==0, "HOLD", 
                                 ifelse(trade==-1,"ENTER SHORT", 
                                        "ENTER LONG"))))
table(tradelab)
#returns on strategy with short sale
ret1 <- reclass(profit,price)
names(ret1) <- c("Strategy with Short Sell")
retall1 <- cbind(ret1,dretspy,dret)
names(retall1) <- c("Strategy with Short Sell", "SPY Daily Returns", "Stock Daily Returns")
#performance of strategy with short sale
charts.PerformanceSummary(ret1)
#returns on strategy against stock daily returns and SPY returns
charts.PerformanceSummary(retall1)



#Same strategy without short selling 
#Assumptions 
#No short sale
#Person can only enter in Long position
#can only sell shares held 
#Invest x% of initial wealth and use the rest for buying more later
#Exit market if price goes below trailing stop (updated based on wealth at each stage)


#initialize vectors 
trade <- c()
trade[1:(2*l)] <- 0
shares <- c()
shares[1:(2*l)] <- 0
wealth <- c()
wealth[1:(2*l)] <- 1000000
profit <- c()
profit[1:(2*l)] <- 0
cashWealth <- wealth
shareWealth <- c()
shareWealth[1:(2*l)] <- 0
#assuming transaction cost = 0.2% of traded value -- size*price
tradecost <- 0.002
EntryRatio <- 0.6

for (i in (2*l+1):length(price)){
  if (shares[i-1] > 0){
    #run entry trigger only if there is no share balance 
    cat("Entered with ", shares[i-1], "shares @ $", price[i-1], "each")
    print(index(price[i-1]))
    break
  }
  else if (lemas[i] > lemal[i]){
    #enter market 
    trade[i] <- 1
  }
  else {
    trade[i] <- 0
  }
  size <- as.integer(floor(EntryRatio*wealth[i-1]/price[i]))
  shares[i] <- shares[i-1] + trade[i]*size 
  cost <-abs(trade[i])*size*price[i]*tradecost
  gain <- shares[i]*dret[i]*price[i]
  cashWealth[i] <- cashWealth[i-1] - trade[i]*size*price[i] - cost
  shareWealth[i] <- shares[i]*price[i] + gain
  wealth[i] <- cashWealth[i] + shareWealth[i]
  profit[i] <- (wealth[i]/wealth[i-1])-1
}

#fix entry day and size
entryday <- length(shares)
size <- wealth[entryday]/price[entryday]
#Can buy shares with 60% cash held 
#(leaving room for future buys and transaction costs)
buyratio <- 0.6


for (i in (entryday+1):length(price)){
  if (x[i] == 1 & hl_emas[i]==1 & ladx[i] < ADXv){
    #BUY
    trade[i] <- 1
    size <- as.integer(floor(buyratio*cashWealth[i-1]/price[i]))
  }
  else if (x[i]==1 & hl_emas==-1 & ladx[i] < ADXv){
    #hl_emas changed from 1 to -1 --> peak --> short
    trade[i] <- -1
    #sell all shares held and wait for next buy
    size <- shares[i-1]
    }
  else {
    #hold 
    trade[i] <- 0
    size <- 0
  }
  shares[i] <- shares[i-1] + trade[i]*size 
  cost <-abs(trade[i])*size*price[i]*tradecost
  gain <- shares[i]*dret[i]*price[i]
  cashWealth[i] <- cashWealth[i-1] - trade[i]*size*price[i] - cost
  shareWealth[i] <- shares[i]*price[i] + gain
  wealth[i] <- cashWealth[i] + shareWealth[i]
  profit[i] <- (wealth[i]/wealth[i-1])-1
}

#Performance
table(trade)
tradelab <- ifelse(trade==-1, "SELL ALL", 
                   ifelse(trade==1, "BUY X", "HOLD"))
                                 
table(tradelab)
#returns on strategy without short sale
ret2 <- reclass(profit,price)
names(ret2) <- c("Strategy w/o short sell")
retall2 <- cbind(ret2,dretspy,dret)
names(retall2) <- c("Strategy w/o short sell", "SPY Daily Returns", "Stock Daily Returns")
#performance of strategy with no short sale
charts.PerformanceSummary(ret2)
#returns of strategy (no short sale) against daily stock returns and SPY returns
charts.PerformanceSummary(retall2)

#Comparison of strategy with and without short selling
retstock <- cbind(ret1, ret2)
names(retstock) <- c("Strategy with short sell", "Strategy w/o short sell")
charts.PerformanceSummary(retstock)

#Comparison of strategy with and without short selling to SPY
retstockspy <- cbind(ret1, ret2, dretspy)
names(retstockspy) <- c("Strategy with short sell", "Strategy w/o short sell", "SPY Daily returns")
charts.PerformanceSummary(retstockspy)


#Naive filter rule with no short sale  
trade <- c()
trade[1:2] <- 0
profit <- c()
profit[1:2] <- 0
wealth <- c()
wealth[1:2] <- 1000000
cashWealth <- wealth
shareWealth <- c()
shareWealth[1:2] <- 0
lagprice <- Lag(price)
change <- price/lagprice
lagchange <- Lag(change)
delta <- 0.005
buyratio <- 0.6


for (i in 3:length(price)){
  if (lagchange[i] > 1+delta){
    #BUY 
    trade[i] <- 1
    size <- floor(buyratio*cashWealth[i-1]/price[i])
  }
  else if (lagchange[i] < 1-delta){
    #SELL
    trade[i] <- -1
    size <- shares[i-1]
  }
  else {
    #HOLD
    trade[i] <- 0
    size <- 0
  }
  shares[i] <- shares[i-1] + trade[i]*size 
  cost <-abs(trade[i])*size*price[i]*tradecost
  gain <- shares[i]*dret[i]*price[i]
  cashWealth[i] <- cashWealth[i-1] - trade[i]*size*price[i] - cost
  shareWealth[i] <- shares[i]*price[i] + gain
  wealth[i] <- cashWealth[i] + shareWealth[i]
  profit[i] <- (wealth[i]/wealth[i-1])-1
}


table(trade)
#returns on naive filter rule
retnaive <- reclass(profit,price)
names(retnaive) <- c("Naive Filter")
retall3 <- cbind(ret1, ret2, retnaive)
names(retall3) <- c("Strategy with short sell", "Strategy w/o short sell", "Naive Filter")
#performance of naive rule
charts.PerformanceSummary(retnaive)
#comparison of naive rule, and strategy with and without short sale
charts.PerformanceSummary(retall3)


#buy and hold 
wealth <- c()
wealth[1] <- 1000000
size <- as.integer(round(wealth[1]/price[2]))
shares <- size 
profit <- c()
profit[1] <- 0 

for (i in 2:length(price)){
  gain <- shares*price[i]*dret[i]
  wealth[i] <- wealth[i-1] + gain
  profit[i] <- (wealth[i]/wealth[i-1])-1
}

#returns on buy and hold
retbuyhold <- reclass(profit, price)
names(retbuyhold) <- c("Buy and Hold")
retall4 <- cbind(ret1, ret2, retbuyhold, retnaive)
names(retall4) <- c("Strategy with Short Sell", "Strategy w/o short sell", "Buy and Hold", "Naive Filter")

#comparison of strategy with and without short sale, buy and hold and naive rule
charts.PerformanceSummary(retall4)




#All performance charts 

#performance of strategy with short sale
charts.PerformanceSummary(ret1)

#returns on strategy against stock daily returns and SPY returns
charts.PerformanceSummary(retall1)

#performance of strategy with no short sale
charts.PerformanceSummary(ret2)

#returns of strategy (no short sale) against daily stock returns and SPY returns
charts.PerformanceSummary(retall2)

#Comparison of strategy with and without short selling
charts.PerformanceSummary(retstock)

#performance of naive rule
charts.PerformanceSummary(retnaive)

#comparison of naive rule, and strategy with and without short sale
charts.PerformanceSummary(retall3)

#comparison of strategy with and without short sale, buy and hold and naive rule
charts.PerformanceSummary(retall4)

#Comparison of strategy with and without short selling to SPY
charts.PerformanceSummary(retstockspy)










