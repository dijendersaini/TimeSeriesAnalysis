# Group Case Study Submission #
#-----------------------------#
# Retail Giant case Study     #
#-----------------------------#

library("ggplot2")
library("forecast")
require("graphics")

#Set Working Directory
setwd("E:\\IIITB\\Course4\\GroupAssignment")
gStore.DS<- read.csv("Global Superstore.csv")

str(gStore.DS)

sum(is.na(gStore.DS))

# Finding which column has NA values
list.NA<-""
for (i in c(1:ncol(gStore.DS)))
{
  len<-length(grep("TRUE",is.na(gStore.DS[,i])))
  if(len > 0){
    list.NA<-paste(colnames(gStore.DS[i]),":",len,list.NA)
  }
}
list.NA # Postal.Code has 41296 NA values
# It is observed that postal code is not available for non-USA addresses

str(gStore.DS)

# Update to data types
# 1. Change Order.Date from factor to Date
# 2. Change Ship.Date from factor to Date
# 3. Change Postal.Code from int to factor

gStore.DS$Order.Date<-as.Date(gStore.DS$Order.Date,"%d-%m-%Y")
gStore.DS$Ship.Date<-as.Date(gStore.DS$Ship.Date,"%d-%m-%Y")
gStore.DS$Postal.Code<-as.factor(gStore.DS$Postal.Code)

# Sustetting the data based on Market and segment

levels(gStore.DS$Segment) # 3 Segments:"Consumer" "Corporate" "Home Office"
levels(gStore.DS$Market) # 7 Markets:"Africa" "APAC" "Canada" "EMEA" "EU" "LATAM" "US" 
#levels(gStore.DS$Category) # 3 Category:"Furniture" "Office Supplies" "Technology" 

# Analysis of TotalProfit, AvgSalesAmount, TotalSalesAmount, NumOfSales on Market and Segment

w<-aggregate(gStore.DS$Profit, by=list(gStore.DS$Market,gStore.DS$Segment), FUN=sum)
names(w)<-list("Market","Segment","TotalProfit")
x<-aggregate(gStore.DS$Sales, by=list(gStore.DS$Market,gStore.DS$Segment), FUN=mean)
names(x)<-list("Market","Segment","AvgSalesAmount")
y<-aggregate(gStore.DS$Sales, by=list(gStore.DS$Market,gStore.DS$Segment), FUN=sum)
names(y)<-list("Market","Segment","TotalSalesAmount")
z<-aggregate(gStore.DS$Sales, by=list(gStore.DS$Market,gStore.DS$Segment), FUN=length)
names(z)<-list("Market","Segment","NumOfSales")

Sales.Summary.Total<-data.frame(w,x,y,z)
Sales.Summary.Total<-Sales.Summary.Total[,c(1,2,3,6,9,12)]

# Adding Profit as a percentage of Sales amount to the dataframe
Sales.Summary.Total$ProfitPercent<-(Sales.Summary.Total$TotalProfit/Sales.Summary.Total$TotalSalesAmount)*100

# Analysis of TotalMonthlyProfit, AvgMonthlySales, TotalMonthlySales, NoOfMonthlySales on Market, Segment, Month and Year

v1<-aggregate(gStore.DS$Quantity, by=list(gStore.DS$Market,gStore.DS$Segment,format(as.Date(gStore.DS$Order.Date), "%Y%m")), FUN=sum)
names(v1)<-list("Market","Segment","Month","TotalMonthlyQty")
w1<-aggregate(gStore.DS$Profit, by=list(gStore.DS$Market,gStore.DS$Segment,format(as.Date(gStore.DS$Order.Date), "%Y%m")), FUN=mean)
names(w1)<-list("Market","Segment","Month","TotalMonthlyProfit")
x1<-aggregate(gStore.DS$Sales, by=list(gStore.DS$Market,gStore.DS$Segment,format(as.Date(gStore.DS$Order.Date), "%Y%m")), FUN=mean)
names(x1)<-list("Market","Segment","Month","AvgMonthlySales")
y1<-aggregate(gStore.DS$Sales, by=list(gStore.DS$Market,gStore.DS$Segment,format(as.Date(gStore.DS$Order.Date), "%Y%m")), FUN=sum)
names(y1)<-list("Market","Segment","Month","TotalMonthlySales")
z1<-aggregate(gStore.DS$Sales, by=list(gStore.DS$Market,gStore.DS$Segment,format(as.Date(gStore.DS$Order.Date), "%Y%m")), FUN=length)
names(z1)<-list("Market","Segment","Month","NoOfMonthlySales")


Sales.Summary.Monthly<-data.frame(v1,w1,x1,y1,z1)
Sales.Summary.Monthly<-Sales.Summary.Monthly[,c(1,2,3,4,8,12,16,20)]

# Add Monthly profit as a percentage of monthly sales
Sales.Summary.Monthly$MonthlyProfitPercent<-(Sales.Summary.Monthly$TotalMonthlyProfit/Sales.Summary.Monthly$TotalMonthlySales)*100
Sales.Summary.Monthly<-Sales.Summary.Monthly[order(Sales.Summary.Monthly$TotalMonthlySales,decreasing=TRUE),]

# Finding standard deviation of monthly profit percentage across Market and Segment
y2<-aggregate(Sales.Summary.Monthly$MonthlyProfitPercent, by=list(Sales.Summary.Monthly$Market,Sales.Summary.Monthly$Segment), FUN=mean)
names(y2)<-list("Market","Segment","AvgMonthlyProfitPercent")
z2<-aggregate(Sales.Summary.Monthly$MonthlyProfitPercent, by=list(Sales.Summary.Monthly$Market,Sales.Summary.Monthly$Segment), FUN=sd)
names(z2)<-list("Market","Segment","MonthlyProfitPercentSD")

# Adding SD of monthly profit percentage to the Sales.Summary.Total
Sales.Summary.Total<-data.frame(Sales.Summary.Total,y2,z2)
Sales.Summary.Total<-Sales.Summary.Total[,-c(8,9,11,12)]
Sales.Summary.Total$CVMonthlyProfitPercent<-Sales.Summary.Total$MonthlyProfitPercentSD/Sales.Summary.Total$AvgMonthlyProfitPercent
# Order on Total profit to find the most profitable and consistent Market Segment 
Sales.Summary.Total<-Sales.Summary.Total[order(Sales.Summary.Total$TotalProfit,decreasing=TRUE),]

# Plotting Market Segment Vs. Total Profit

# Market and Segment generating most profit
plot1<-ggplot(Sales.Summary.Total,aes(x=Sales.Summary.Total$Market,y=Sales.Summary.Total$TotalProfit,fill=Sales.Summary.Total$Segment))
plot1+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit")+ggtitle("Total Profit")

# Market and Segment having most profit margin
plot2<-ggplot(Sales.Summary.Total,aes(x=Sales.Summary.Total$Market,y=Sales.Summary.Total$ProfitPercent,fill=Sales.Summary.Total$Segment))
plot2+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit %age")+ggtitle("Profit percent")

# Market and Segment having most profit margin
plot3<-ggplot(Sales.Summary.Total,aes(x=Sales.Summary.Total$Market,y=Sales.Summary.Total$CVMonthlyProfitPercent,fill=Sales.Summary.Total$Segment))
plot3<-plot3+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Coeff. of variance of monthly profit")
plot3+ggtitle("Coeff. of variance in monthly profit Vs. Market Segment")

# Based on the maximum profits and consistent profit month on month, we have chosen these Market Segements
# 1. APAC Consumer
# 2. EU Consumer
# 3. APAC Corporate
# 4. EU Corporate
# 5. LATAM Consumer

# US Consumer also has high profits but the coefficient of variance of profit %age month on month is high
# Therefore we are ignoring it

#--------------------------------------------------------------------------------------------------------
#                                 APAC Consumer
#--------------------------------------------------------------------------------------------------------

APAC.Consumer.Sales<-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Market=="APAC")
                               &(Sales.Summary.Monthly$Segment=="Consumer"))
APAC.Consumer.Sales<-APAC.Consumer.Sales[order(APAC.Consumer.Sales$Month),]
APAC.Consumer.Sales$MonthNum<-c(1:nrow(APAC.Consumer.Sales))
# Preparing Time series data
APAC.Consumer.Profit.TS<-APAC.Consumer.Sales[,c("MonthNum","TotalMonthlyProfit")]
APAC.Consumer.Qty.TS<-APAC.Consumer.Sales[,c("MonthNum","TotalMonthlyQty")]

# Separating test data
RowCount<-nrow(APAC.Consumer.Sales)

APAC.Consumer.Profit.TS.test<-APAC.Consumer.Profit.TS[(RowCount-5):RowCount,] # Test data
APAC.Consumer.Profit.TS<-APAC.Consumer.Profit.TS[1:(RowCount-6),]
APAC.Consumer.Qty.TS.test<-APAC.Consumer.Qty.TS[(RowCount-5):RowCount,] # Test data
APAC.Consumer.Qty.TS<-APAC.Consumer.Qty.TS[1:(RowCount-6),]

xcol<-c(1)
ycol<-c(2)

# Plotting timeseries data for Profit
#--------------------------------------
APAC.Consumer.Profit.timeser<-ts(APAC.Consumer.Profit.TS[,ycol[1]])

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
APAC.Consumer.Profit.timeser.d<-ts(APAC.Consumer.Profit.TS[,ycol[1]],frequency=12)
APAC.Consumer.Profit.timeser.decompose <- decompose(APAC.Consumer.Profit.timeser.d)
plot(APAC.Consumer.Profit.timeser.decompose)

# Decomposotion showed that:
# 1. Trend in a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APAC.Consumer.Profit.timeser)


# Smoothening the curve 
w <-1
APAC.Consumer.Profit.timeser.smooth <- filter(APAC.Consumer.Profit.timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)
diff <- APAC.Consumer.Profit.timeser.smooth[w+2] - APAC.Consumer.Profit.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC.Consumer.Profit.timeser.smooth[i] <- APAC.Consumer.Profit.timeser.smooth[i+1] - diff
}
n <- length(APAC.Consumer.Profit.timeser)

timevals <- APAC.Consumer.Profit.TS[[xcol[1]]]
timevals.test <- APAC.Consumer.Profit.TS.test[[xcol[1]]]
diff <- APAC.Consumer.Profit.timeser.smooth[n-w] - APAC.Consumer.Profit.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Consumer.Profit.timeser.smooth[i] <- APAC.Consumer.Profit.timeser.smooth[i-1] + diff
}
lines(APAC.Consumer.Profit.timeser.smooth, col="blue", lwd=2)

APAC.Consumer.Profit.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(APAC.Consumer.Profit.timeser.smooth)))
colnames(APAC.Consumer.Profit.timeser.smoothdf) <- c('MonthNum', 'Profit')
APAC.Consumer.Profit.timeser.smoothdf$MonthNum<-as.numeric(APAC.Consumer.Profit.timeser.smoothdf$MonthNum)
APAC.Consumer.Profit.timeser.smoothdf$Profit<-as.numeric(APAC.Consumer.Profit.timeser.smoothdf$Profit)
str(APAC.Consumer.Profit.timeser.smoothdf)

str(APAC.Consumer.Profit.TS)

lmfit <- lm(APAC.Consumer.Profit.timeser.smoothdf$Profit ~ sin(0.5*APAC.Consumer.Profit.timeser.smoothdf$MonthNum) *
              poly(APAC.Consumer.Profit.timeser.smoothdf$MonthNum,2) 
            + cos(0.5*APAC.Consumer.Profit.timeser.smoothdf$MonthNum) * 
              poly(APAC.Consumer.Profit.timeser.smoothdf$MonthNum,2)
            + sin(0.05*APAC.Consumer.Profit.timeser.smoothdf$MonthNum)*
              APAC.Consumer.Profit.timeser.smoothdf$MonthNum, 
            data=APAC.Consumer.Profit.timeser.smoothdf)
summary(lmfit)
accuracy(lmfit)

 
trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- APAC.Consumer.Profit.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")
par("mar") #5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))
armafit <- auto.arima(resi)
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(APAC.Consumer.Profit.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
accuracy(autoarima)

#--------------------- Forecasting APAC Consumer Profit------------------

APACConProfitForecast <- HoltWinters(APAC.Consumer.Profit.timeser, beta=FALSE, gamma=FALSE)
APACConProfitForecast
plot(APACConProfitForecast)

APACConProfitForecast6months <- forecast.HoltWinters(APACConProfitForecast,h=6)
APACConProfitForecast6months
plot(APACConProfitForecast6months)
# Comparing with Test data
accuracy(APACConProfitForecast6months,APAC.Consumer.Profit.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month profit-----------
forecast(APAC.Consumer.Profit.timeser,h=6)
forecast(autoarima,h=6)
#--------------------- Forecasting EU_CORPORATE_Sales----------------------

# Plotting timeseries data for Quantity
#---------------------------------------
APAC.Consumer.Qty.timeser<-ts(APAC.Consumer.Qty.TS[,ycol[1]])

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
APAC.Consumer.Qty.timeser.d<-ts(APAC.Consumer.Qty.TS[,ycol[1]],frequency=12)
APAC.Consumer.Qty.timeser.decompose <- decompose(APAC.Consumer.Qty.timeser.d)
plot(APAC.Consumer.Qty.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APAC.Consumer.Qty.timeser)

# Smoothening the curve 
w <-1
APAC.Consumer.Qty.timeser.smooth <- filter(APAC.Consumer.Qty.timeser, 
                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                           method='convolution', sides=2)
diff <- APAC.Consumer.Qty.timeser.smooth[w+2] - APAC.Consumer.Qty.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC.Consumer.Qty.timeser.smooth[i] <- APAC.Consumer.Qty.timeser.smooth[i+1] - diff
}
n <- length(APAC.Consumer.Qty.timeser)

timevals <- APAC.Consumer.Qty.TS[[xcol[1]]]
diff <- APAC.Consumer.Qty.timeser.smooth[n-w] - APAC.Consumer.Qty.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Consumer.Qty.timeser.smooth[i] <- APAC.Consumer.Qty.timeser.smooth[i-1] + diff
}
lines(APAC.Consumer.Qty.timeser.smooth, col="blue", lwd=2)

APAC.Consumer.Qty.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(APAC.Consumer.Qty.timeser.smooth)))
colnames(APAC.Consumer.Qty.timeser.smoothdf) <- c('MonthNum', 'Profit')
APAC.Consumer.Qty.timeser.smoothdf$MonthNum<-as.numeric(APAC.Consumer.Qty.timeser.smoothdf$MonthNum)
APAC.Consumer.Qty.timeser.smoothdf$Profit<-as.numeric(APAC.Consumer.Qty.timeser.smoothdf$Profit)
str(APAC.Consumer.Qty.timeser.smoothdf)

lmfit <- lm(APAC.Consumer.Qty.timeser.smoothdf$Profit ~ sin(0.6*APAC.Consumer.Qty.timeser.smoothdf$MonthNum) *
              poly(APAC.Consumer.Qty.timeser.smoothdf$MonthNum,2) 
            + cos(0.6*APAC.Consumer.Qty.timeser.smoothdf$MonthNum) * 
              poly(APAC.Consumer.Qty.timeser.smoothdf$MonthNum,2) +
              + sin(0.05*APAC.Consumer.Qty.timeser.smoothdf$MonthNum)*
              APAC.Consumer.Qty.timeser.smoothdf$MonthNum, 
            data=APAC.Consumer.Qty.timeser.smoothdf)
summary(lmfit)
 
trend <- predict(lmfit, data.frame(x=timevals))

trend2<-predict(lmfit, data.frame(x=c(47,48)))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- APAC.Consumer.Qty.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(APAC.Consumer.Qty.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#--------------------- Forecasting APAC Consumer Quantity------------------

APACConQtyForecast <- HoltWinters(APAC.Consumer.Qty.timeser, beta=FALSE, gamma=FALSE)
APACConQtyForecast
plot(APACConQtyForecast)

APACConQtyForecast6months <- forecast.HoltWinters(APACConQtyForecast,h=6)
APACConQtyForecast6months
plot(APACConQtyForecast6months)
# Comparing with Test data
accuracy(APACConQtyForecast6months,APAC.Consumer.Qty.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month quantity-----------
forecast(APAC.Consumer.Qty.timeser,h=6)
forecast(autoarima,h=6)


#--------------------------------------------------------------------------------------------------------
#                                 EU Consumer
#--------------------------------------------------------------------------------------------------------

EU.Consumer.Sales<-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Market=="EU")
                            &(Sales.Summary.Monthly$Segment=="Consumer"))
EU.Consumer.Sales<-EU.Consumer.Sales[order(EU.Consumer.Sales$Month),]
EU.Consumer.Sales$MonthNum<-c(1:nrow(EU.Consumer.Sales))
# Preparing Time series data
EU.Consumer.Profit.TS<-EU.Consumer.Sales[,c("MonthNum","TotalMonthlyProfit")]
EU.Consumer.Qty.TS<-EU.Consumer.Sales[,c("MonthNum","TotalMonthlyQty")]

# Separating test data
RowCount<-nrow(EU.Consumer.Sales)

EU.Consumer.Profit.TS.test<-EU.Consumer.Profit.TS[(RowCount-5):RowCount,] # Test data
EU.Consumer.Profit.TS<-EU.Consumer.Profit.TS[1:(RowCount-6),]
EU.Consumer.Qty.TS.test<-EU.Consumer.Qty.TS[(RowCount-5):RowCount,] # Test data
EU.Consumer.Qty.TS<-EU.Consumer.Qty.TS[1:(RowCount-6),]

xcol<-c(1)
ycol<-c(2)

# Plotting timeseries data for Profit
#--------------------------------------
EU.Consumer.Profit.timeser<-ts(EU.Consumer.Profit.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
EU.Consumer.Profit.timeser.d<-ts(EU.Consumer.Profit.TS[,ycol[1]],frequency=12)
EU.Consumer.Profit.timeser.decompose <- decompose(EU.Consumer.Profit.timeser.d)
plot(EU.Consumer.Profit.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(EU.Consumer.Profit.timeser)

# Smoothening the curve 
w <-1
EU.Consumer.Profit.timeser.smooth <- filter(EU.Consumer.Profit.timeser, 
                                              filter=rep(1/(2*w+1),(2*w+1)), 
                                              method='convolution', sides=2)
diff <- EU.Consumer.Profit.timeser.smooth[w+2] - EU.Consumer.Profit.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  EU.Consumer.Profit.timeser.smooth[i] <- EU.Consumer.Profit.timeser.smooth[i+1] - diff
}
n <- length(EU.Consumer.Profit.timeser)

timevals <- EU.Consumer.Profit.TS[[xcol[1]]]
diff <- EU.Consumer.Profit.timeser.smooth[n-w] - EU.Consumer.Profit.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU.Consumer.Profit.timeser.smooth[i] <- EU.Consumer.Profit.timeser.smooth[i-1] + diff
}
lines(EU.Consumer.Profit.timeser.smooth, col="blue", lwd=2)

#Modelling
#----------

EU.Consumer.Profit.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(EU.Consumer.Profit.timeser.smooth)))
colnames(EU.Consumer.Profit.timeser.smoothdf) <- c('MonthNum', 'Profit')
EU.Consumer.Profit.timeser.smoothdf$MonthNum<-as.numeric(EU.Consumer.Profit.timeser.smoothdf$MonthNum)
EU.Consumer.Profit.timeser.smoothdf$Profit<-as.numeric(EU.Consumer.Profit.timeser.smoothdf$Profit)
str(EU.Consumer.Profit.timeser.smoothdf)

lmfit <- lm(EU.Consumer.Profit.timeser.smoothdf$Profit ~ sin(0.6*EU.Consumer.Profit.timeser.smoothdf$MonthNum) *
              poly(EU.Consumer.Profit.timeser.smoothdf$MonthNum,3) 
            + cos(0.6*EU.Consumer.Profit.timeser.smoothdf$MonthNum) * 
              poly(EU.Consumer.Profit.timeser.smoothdf$MonthNum,3) +
              poly(EU.Consumer.Profit.timeser.smoothdf$MonthNum,2),
            data=EU.Consumer.Profit.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- EU.Consumer.Profit.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(EU.Consumer.Profit.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting EU Consumer Profit------------------

EUConProfitForecast <- HoltWinters(EU.Consumer.Profit.timeser, beta=FALSE, gamma=FALSE)
EUConProfitForecast
plot(EUConProfitForecast)

EUConProfitForecast6months <- forecast.HoltWinters(EUConProfitForecast,h=6)
EUConProfitForecast6months
plot(EUConProfitForecast6months)
# Comparing with Test data
accuracy(EUConProfitForecast6months,EU.Consumer.Profit.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month profit-----------
forecast(EU.Consumer.Profit.timeser,h=6)
forecast(autoarima,h=6)

  # Plotting timeseries data for Quantity
  #---------------------------------------
  EU.Consumer.Qty.timeser<-ts(EU.Consumer.Qty.TS[,ycol[1]])
  # Decompose timeseries to see the components
  # Added frequency = 12 because decomposition can't happen with frequency = 1
  EU.Consumer.Qty.timeser.d<-ts(EU.Consumer.Qty.TS[,ycol[1]],frequency=12)
  EU.Consumer.Qty.timeser.decompose <- decompose(EU.Consumer.Qty.timeser.d)
  plot(EU.Consumer.Qty.timeser.decompose)
  # Decomposotion showed that:
  # 1. Trend in a linear positive slope line
  # 2. Seasonality is a low wavelength sine curve
  
  # Plot the timeseries
  plot(EU.Consumer.Qty.timeser)
  
  # Smoothening the curve 
  w <-1
  EU.Consumer.Qty.timeser.smooth <- filter(EU.Consumer.Qty.timeser, 
                                             filter=rep(1/(2*w+1),(2*w+1)), 
                                             method='convolution', sides=2)
  diff <- EU.Consumer.Qty.timeser.smooth[w+2] - EU.Consumer.Qty.timeser.smooth[w+1]
  for (i in seq(w,1,-1)) {
    EU.Consumer.Qty.timeser.smooth[i] <- EU.Consumer.Qty.timeser.smooth[i+1] - diff
  }
  n <- length(EU.Consumer.Qty.timeser)
  
  timevals <- EU.Consumer.Qty.TS[[xcol[1]]]
  diff <- EU.Consumer.Qty.timeser.smooth[n-w] - EU.Consumer.Qty.timeser.smooth[n-w-1]
  for (i in seq(n-w+1, n)) {
    Eu.Consumer.Qty.timeser.smooth[i] <- EU.Consumer.Qty.timeser.smooth[i-1] + diff
  }
  lines(EU.Consumer.Qty.timeser.smooth, col="blue", lwd=2)
  
  EU.Consumer.Qty.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(EU.Consumer.Qty.timeser.smooth)))
  colnames(EU.Consumer.Qty.timeser.smoothdf) <- c('MonthNum', 'Profit')
  EU.Consumer.Qty.timeser.smoothdf$MonthNum<-as.numeric(EU.Consumer.Qty.timeser.smoothdf$MonthNum)
  EU.Consumer.Qty.timeser.smoothdf$Profit<-as.numeric(EU.Consumer.Qty.timeser.smoothdf$Profit)
  str(EU.Consumer.Qty.timeser.smoothdf)
  
  lmfit <- lm(EU.Consumer.Qty.timeser.smoothdf$Profit ~ sin(0.6*EU.Consumer.Qty.timeser.smoothdf$MonthNum) *
                poly(EU.Consumer.Qty.timeser.smoothdf$MonthNum,2) 
              + cos(0.6*EU.Consumer.Qty.timeser.smoothdf$MonthNum) * 
                poly(EU.Consumer.Qty.timeser.smoothdf$MonthNum,2) +
                EU.Consumer.Qty.timeser.smoothdf$MonthNum,
              data=EU.Consumer.Qty.timeser.smoothdf)
  summary(lmfit)
  
  trend <- predict(lmfit, data.frame(x=timevals))
  
  lines(timevals, trend, col="red", lwd=2)
  
  # Manual Arima
  #-------------
  resi <- EU.Consumer.Qty.timeser - trend
  plot(resi, col='red')
  
  acf(resi)
  acf(resi, type="partial")
  
  par("mar") #5.1 4.1 4.1 2.1
  armafit <- auto.arima(resi)
  par(mar=c(1,1,1,1))
  tsdiag(armafit)
  par(mar=c(5.1,4.1,4.1,2.1))
  armafit #ARIMA(0,0,0) with zero mean   
  
  # Auto Arima
  #-------------
  autoarima <- auto.arima(EU.Consumer.Qty.timeser)
  autoarima
  tsdiag(autoarima)
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")

#--------------------- Forecasting EU Consumer Quantity------------------
EUConQtyForecast <- HoltWinters(EU.Consumer.Qty.timeser, beta=FALSE, gamma=FALSE)
EUConQtyForecast
plot(EUConQtyForecast)

EUConQtyForecast6months <- forecast.HoltWinters(EUConQtyForecast,h=6)
EUConQtyForecast6months
plot(EUConQtyForecast6months)
# Comparing with Test data
accuracy(EUConQtyForecast6months,EU.Consumer.Qty.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month Quantity-----------
forecast(EU.Consumer.Qty.timeser,h=6)
forecast(autoarima,h=6)

#--------------------------------------------------------------------------------------------------------
#                                 APAC Corporate
#--------------------------------------------------------------------------------------------------------

APAC.Corporate.Sales<-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Market=="APAC")
                          &(Sales.Summary.Monthly$Segment=="Corporate"))
APAC.Corporate.Sales<-APAC.Corporate.Sales[order(APAC.Corporate.Sales$Month),]
APAC.Corporate.Sales$MonthNum<-c(1:nrow(APAC.Corporate.Sales))
# Preparing Time series data
APAC.Corporate.Profit.TS<-APAC.Corporate.Sales[,c("MonthNum","TotalMonthlyProfit")]
APAC.Corporate.Qty.TS<-APAC.Corporate.Sales[,c("MonthNum","TotalMonthlyQty")]

# Separating test data
RowCount<-nrow(APAC.Corporate.Sales)

APAC.Corporate.Profit.TS.test<-APAC.Corporate.Profit.TS[(RowCount-5):RowCount,] # Test data
APAC.Corporate.Profit.TS<-APAC.Corporate.Profit.TS[1:(RowCount-6),]
APAC.Corporate.Qty.TS.test<-APAC.Corporate.Qty.TS[(RowCount-5):RowCount,] # Test data
APAC.Corporate.Qty.TS<-APAC.Corporate.Qty.TS[1:(RowCount-6),]

xcol<-c(1)
ycol<-c(2)

# Plotting timeseries data for Profit
#--------------------------------------
APAC.Corporate.Profit.timeser<-ts(APAC.Corporate.Profit.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
APAC.Corporate.Profit.timeser.d<-ts(APAC.Corporate.Profit.TS[,ycol[1]],frequency=12)
APAC.Corporate.Profit.timeser.decompose <- decompose(APAC.Corporate.Profit.timeser.d)
plot(APAC.Corporate.Profit.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APAC.Corporate.Profit.timeser)

# Smoothening the curve 
w <-1
APAC.Corporate.Profit.timeser.smooth <- filter(APAC.Corporate.Profit.timeser, 
                                            filter=rep(1/(2*w+1),(2*w+1)), 
                                            method='convolution', sides=2)
diff <- APAC.Corporate.Profit.timeser.smooth[w+2] - APAC.Corporate.Profit.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC.Corporate.Profit.timeser.smooth[i] <- APAC.Corporate.Profit.timeser.smooth[i+1] - diff
}
n <- length(APAC.Corporate.Profit.timeser)

timevals <- APAC.Corporate.Profit.TS[[xcol[1]]]
diff <- APAC.Corporate.Profit.timeser.smooth[n-w] - APAC.Corporate.Profit.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Corporate.Profit.timeser.smooth[i] <- APAC.Corporate.Profit.timeser.smooth[i-1] + diff
}
lines(APAC.Corporate.Profit.timeser.smooth, col="blue", lwd=2)

#Modelling
#----------

APAC.Corporate.Profit.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(APAC.Corporate.Profit.timeser.smooth)))
colnames(APAC.Corporate.Profit.timeser.smoothdf) <- c('MonthNum', 'Profit')
APAC.Corporate.Profit.timeser.smoothdf$MonthNum<-as.numeric(APAC.Corporate.Profit.timeser.smoothdf$MonthNum)
APAC.Corporate.Profit.timeser.smoothdf$Profit<-as.numeric(APAC.Corporate.Profit.timeser.smoothdf$Profit)
str(APAC.Corporate.Profit.timeser.smoothdf)

lmfit <- lm(APAC.Corporate.Profit.timeser.smoothdf$Profit ~ sin(0.6*APAC.Corporate.Profit.timeser.smoothdf$MonthNum) *
              poly(APAC.Corporate.Profit.timeser.smoothdf$MonthNum,3) 
            + cos(0.6*APAC.Corporate.Profit.timeser.smoothdf$MonthNum) * 
              poly(APAC.Corporate.Profit.timeser.smoothdf$MonthNum,3) +
              APAC.Corporate.Profit.timeser.smoothdf$MonthNum,
            data=APAC.Corporate.Profit.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- APAC.Corporate.Profit.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(APAC.Corporate.Profit.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting APAC Corporate Profit------------------
APACCorProfitForecast <- HoltWinters(APAC.Corporate.Profit.timeser, beta=FALSE, gamma=FALSE)
APACCorProfitForecast
plot(APACCorProfitForecast)

APACCorProfitForecast6months <- forecast.HoltWinters(APACCorProfitForecast,h=6)
APACCorProfitForecast6months
plot(APACCorProfitForecast6months)
# Comparing with Test data
accuracy(APACCorProfitForecast6months,APAC.Corporate.Profit.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month profit-----------
forecast(APAC.Corporate.Profit.timeser,h=6)
forecast(autoarima,h=6)

# Plotting timeseries data for Quantity
#---------------------------------------
APAC.Corporate.Qty.timeser<-ts(APAC.Corporate.Qty.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
APAC.Corporate.Qty.timeser.d<-ts(APAC.Corporate.Qty.TS[,ycol[1]],frequency=12)
APAC.Corporate.Qty.timeser.decompose <- decompose(APAC.Corporate.Qty.timeser.d)
plot(APAC.Corporate.Qty.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APAC.Corporate.Qty.timeser)

# Smoothening the curve 
w <-1
APAC.Corporate.Qty.timeser.smooth <- filter(APAC.Corporate.Qty.timeser, 
                                            filter=rep(1/(2*w+1),(2*w+1)), 
                                            method='convolution', sides=2)
diff <- APAC.Corporate.Qty.timeser.smooth[w+2] - APAC.Corporate.Qty.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC.Corporate.Qty.timeser.smooth[i] <- APAC.Corporate.Qty.timeser.smooth[i+1] - diff
}
n <- length(APAC.Corporate.Qty.timeser)

timevals <- APAC.Corporate.Qty.TS[[xcol[1]]]
diff <- APAC.Corporate.Qty.timeser.smooth[n-w] - APAC.Corporate.Qty.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Corporate.Qty.timeser.smooth[i] <- APAC.Corporate.Qty.timeser.smooth[i-1] + diff
}
lines(APAC.Corporate.Qty.timeser.smooth, col="blue", lwd=2)

APAC.Corporate.Qty.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(APAC.Corporate.Qty.timeser.smooth)))
colnames(APAC.Corporate.Qty.timeser.smoothdf) <- c('MonthNum', 'Profit')
APAC.Corporate.Qty.timeser.smoothdf$MonthNum<-as.numeric(APAC.Corporate.Qty.timeser.smoothdf$MonthNum)
APAC.Corporate.Qty.timeser.smoothdf$Profit<-as.numeric(APAC.Corporate.Qty.timeser.smoothdf$Profit)
str(APAC.Corporate.Qty.timeser.smoothdf)

lmfit <- lm(APAC.Corporate.Qty.timeser.smoothdf$Profit ~ sin(0.6*APAC.Corporate.Qty.timeser.smoothdf$MonthNum) *
              poly(APAC.Corporate.Qty.timeser.smoothdf$MonthNum,2) 
            + cos(0.6*APAC.Corporate.Qty.timeser.smoothdf$MonthNum) * 
              poly(APAC.Corporate.Qty.timeser.smoothdf$MonthNum,2) +
              APAC.Corporate.Qty.timeser.smoothdf$MonthNum,
            data=APAC.Corporate.Qty.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- APAC.Corporate.Qty.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(APAC.Corporate.Qty.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting APAC Corporate Quantity------------------
APACCorQtyForecast <- HoltWinters(APAC.Corporate.Qty.timeser, beta=FALSE, gamma=FALSE)
APACCorQtyForecast
plot(APACCorQtyForecast)

APACCorQtyForecast6months <- forecast.HoltWinters(APACCorQtyForecast,h=6)
APACCorQtyForecast6months
plot(APACCorQtyForecast6months)
# Comparing with Test data
accuracy(APACCorQtyForecast6months,APAC.Corporate.Qty.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month Quantity-----------
forecast(APAC.Corporate.Qty.timeser,h=6)
forecast(autoarima,h=6)

#--------------------------------------------------------------------------------------------------------
#                                 EU Corporate
#--------------------------------------------------------------------------------------------------------

EU.Corporate.Sales<-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Market=="EU")
                           &(Sales.Summary.Monthly$Segment=="Corporate"))
EU.Corporate.Sales<-EU.Corporate.Sales[order(EU.Corporate.Sales$Month),]
EU.Corporate.Sales$MonthNum<-c(1:nrow(EU.Corporate.Sales))
# Preparing Time series data
EU.Corporate.Profit.TS<-EU.Corporate.Sales[,c("MonthNum","TotalMonthlyProfit")]
EU.Corporate.Qty.TS<-EU.Corporate.Sales[,c("MonthNum","TotalMonthlyQty")]

# Separating test data
RowCount<-nrow(EU.Corporate.Sales)

EU.Corporate.Profit.TS.test<-EU.Corporate.Profit.TS[(RowCount-5):RowCount,] # Test data
EU.Corporate.Profit.TS<-EU.Corporate.Profit.TS[1:(RowCount-6),]
EU.Corporate.Qty.TS.test<-EU.Corporate.Qty.TS[(RowCount-5):RowCount,] # Test data
EU.Corporate.Qty.TS<-EU.Corporate.Qty.TS[1:(RowCount-6),]

xcol<-c(1)
ycol<-c(2)

# Plotting timeseries data for Profit
#--------------------------------------
EU.Corporate.Profit.timeser<-ts(EU.Corporate.Profit.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
EU.Corporate.Profit.timeser.d<-ts(EU.Corporate.Profit.TS[,ycol[1]],frequency=12)
EU.Corporate.Profit.timeser.decompose <- decompose(EU.Corporate.Profit.timeser.d)
plot(EU.Corporate.Profit.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(EU.Corporate.Profit.timeser)

# Smoothening the curve 
w <-1
EU.Corporate.Profit.timeser.smooth <- filter(EU.Corporate.Profit.timeser, 
                                             filter=rep(1/(2*w+1),(2*w+1)), 
                                             method='convolution', sides=2)
diff <- EU.Corporate.Profit.timeser.smooth[w+2] - EU.Corporate.Profit.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  EU.Corporate.Profit.timeser.smooth[i] <- EU.Corporate.Profit.timeser.smooth[i+1] - diff
}
n <- length(EU.Corporate.Profit.timeser)

timevals <- EU.Corporate.Profit.TS[[xcol[1]]]
diff <- EU.Corporate.Profit.timeser.smooth[n-w] - EU.Corporate.Profit.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU.Corporate.Profit.timeser.smooth[i] <- EU.Corporate.Profit.timeser.smooth[i-1] + diff
}
lines(EU.Corporate.Profit.timeser.smooth, col="blue", lwd=2)

#Modelling
#----------

EU.Corporate.Profit.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(EU.Corporate.Profit.timeser.smooth)))
colnames(EU.Corporate.Profit.timeser.smoothdf) <- c('MonthNum', 'Profit')
EU.Corporate.Profit.timeser.smoothdf$MonthNum<-as.numeric(EU.Corporate.Profit.timeser.smoothdf$MonthNum)
EU.Corporate.Profit.timeser.smoothdf$Profit<-as.numeric(EU.Corporate.Profit.timeser.smoothdf$Profit)
str(EU.Corporate.Profit.timeser.smoothdf)

lmfit <- lm(EU.Corporate.Profit.timeser.smoothdf$Profit ~ sin(0.6*EU.Corporate.Profit.timeser.smoothdf$MonthNum) *
              poly(EU.Corporate.Profit.timeser.smoothdf$MonthNum,3) 
            + cos(0.6*EU.Corporate.Profit.timeser.smoothdf$MonthNum) * 
              poly(EU.Corporate.Profit.timeser.smoothdf$MonthNum,3) +
              EU.Corporate.Profit.timeser.smoothdf$MonthNum,
            data=EU.Corporate.Profit.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- EU.Corporate.Profit.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(EU.Corporate.Profit.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting EU Corporate Profit------------------
EUCorProfitForecast <- HoltWinters(EU.Corporate.Profit.timeser, beta=FALSE, gamma=FALSE)
EUCorProfitForecast
plot(EUCorProfitForecast)

EUCorProfitForecast6months <- forecast.HoltWinters(EUCorProfitForecast,h=6)
EUCorProfitForecast6months
plot(EUCorProfitForecast6months)
# Comparing with Test data
accuracy(EUCorProfitForecast6months,EU.Corporate.Profit.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month profit-----------
forecast(EU.Corporate.Profit.timeser,h=6)
forecast(autoarima,h=6)


# Plotting timeseries data for Quantity
#---------------------------------------
EU.Corporate.Qty.timeser<-ts(EU.Corporate.Qty.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
EU.Corporate.Qty.timeser.d<-ts(EU.Corporate.Qty.TS[,ycol[1]],frequency=12)
EU.Corporate.Qty.timeser.decompose <- decompose(EU.Corporate.Qty.timeser.d)
plot(EU.Corporate.Qty.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(EU.Corporate.Qty.timeser)

# Smoothening the curve 
w <-1
EU.Corporate.Qty.timeser.smooth <- filter(EU.Corporate.Qty.timeser, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)
diff <- EU.Corporate.Qty.timeser.smooth[w+2] - EU.Corporate.Qty.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  EU.Corporate.Qty.timeser.smooth[i] <- EU.Corporate.Qty.timeser.smooth[i+1] - diff
}
n <- length(EU.Corporate.Qty.timeser)

timevals <- EU.Corporate.Qty.TS[[xcol[1]]]
diff <- EU.Corporate.Qty.timeser.smooth[n-w] - EU.Corporate.Qty.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU.Corporate.Qty.timeser.smooth[i] <- EU.Corporate.Qty.timeser.smooth[i-1] + diff
}
lines(EU.Corporate.Qty.timeser.smooth, col="blue", lwd=2)

EU.Corporate.Qty.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(EU.Corporate.Qty.timeser.smooth)))
colnames(EU.Corporate.Qty.timeser.smoothdf) <- c('MonthNum', 'Profit')
EU.Corporate.Qty.timeser.smoothdf$MonthNum<-as.numeric(EU.Corporate.Qty.timeser.smoothdf$MonthNum)
EU.Corporate.Qty.timeser.smoothdf$Profit<-as.numeric(EU.Corporate.Qty.timeser.smoothdf$Profit)
str(EU.Corporate.Qty.timeser.smoothdf)

lmfit <- lm(EU.Corporate.Qty.timeser.smoothdf$Profit ~ sin(0.6*EU.Corporate.Qty.timeser.smoothdf$MonthNum) *
              poly(EU.Corporate.Qty.timeser.smoothdf$MonthNum,2) 
            + cos(0.6*EU.Corporate.Qty.timeser.smoothdf$MonthNum) * 
              poly(EU.Corporate.Qty.timeser.smoothdf$MonthNum,2) +
              EU.Corporate.Qty.timeser.smoothdf$MonthNum,
            data=EU.Corporate.Qty.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- EU.Corporate.Qty.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(EU.Corporate.Qty.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting EU Corporate Quantity------------------
EUCorQtyForecast <- HoltWinters(EU.Corporate.Qty.timeser, beta=FALSE, gamma=FALSE)
EUCorQtyForecast
plot(EUCorQtyForecast)

EUCorQtyForecast6months <- forecast.HoltWinters(EUCorQtyForecast,h=6)
EUCorQtyForecast6months
plot(EUCorQtyForecast6months)
# Comparing with Test data
accuracy(EUCorQtyForecast6months,EU.Corporate.Qty.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month Quantity-----------
forecast(EU.Corporate.Qty.timeser,h=6)
forecast(autoarima,h=6)

#--------------------------------------------------------------------------------------------------------
#                                 LATAM Consumer
#--------------------------------------------------------------------------------------------------------

LATAM.Consumer.Sales<-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Market=="LATAM")
                             &(Sales.Summary.Monthly$Segment=="Consumer"))
LATAM.Consumer.Sales<-LATAM.Consumer.Sales[order(LATAM.Consumer.Sales$Month),]
LATAM.Consumer.Sales$MonthNum<-c(1:nrow(LATAM.Consumer.Sales))
# Preparing Time series data
LATAM.Consumer.Profit.TS<-LATAM.Consumer.Sales[,c("MonthNum","TotalMonthlyProfit")]
LATAM.Consumer.Qty.TS<-LATAM.Consumer.Sales[,c("MonthNum","TotalMonthlyQty")]

# Separating test data
RowCount<-nrow(LATAM.Consumer.Sales)

LATAM.Consumer.Profit.TS.test<-LATAM.Consumer.Profit.TS[(RowCount-5):RowCount,] # Test data
LATAM.Consumer.Profit.TS<-LATAM.Consumer.Profit.TS[1:(RowCount-6),]
LATAM.Consumer.Qty.TS.test<-LATAM.Consumer.Qty.TS[(RowCount-5):RowCount,] # Test data
LATAM.Consumer.Qty.TS<-LATAM.Consumer.Qty.TS[1:(RowCount-6),]

xcol<-c(1)
ycol<-c(2)

# Plotting timeseries data for Profit
#--------------------------------------
LATAM.Consumer.Profit.timeser<-ts(LATAM.Consumer.Profit.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
LATAM.Consumer.Profit.timeser.d<-ts(LATAM.Consumer.Profit.TS[,ycol[1]],frequency=12)
LATAM.Consumer.Profit.timeser.decompose <- decompose(LATAM.Consumer.Profit.timeser.d)
plot(LATAM.Consumer.Profit.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(LATAM.Consumer.Profit.timeser)

# Smoothening the curve 
w <-1
LATAM.Consumer.Profit.timeser.smooth <- filter(LATAM.Consumer.Profit.timeser, 
                                               filter=rep(1/(2*w+1),(2*w+1)), 
                                               method='convolution', sides=2)
diff <- LATAM.Consumer.Profit.timeser.smooth[w+2] - LATAM.Consumer.Profit.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  LATAM.Consumer.Profit.timeser.smooth[i] <- LATAM.Consumer.Profit.timeser.smooth[i+1] - diff
}
n <- length(LATAM.Consumer.Profit.timeser)

timevals <- LATAM.Consumer.Profit.TS[[xcol[1]]]
diff <- LATAM.Consumer.Profit.timeser.smooth[n-w] - LATAM.Consumer.Profit.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  LATAM.Consumer.Profit.timeser.smooth[i] <- LATAM.Consumer.Profit.timeser.smooth[i-1] + diff
}
lines(LATAM.Consumer.Profit.timeser.smooth, col="blue", lwd=2)

#Modelling
#----------

LATAM.Consumer.Profit.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(LATAM.Consumer.Profit.timeser.smooth)))
colnames(LATAM.Consumer.Profit.timeser.smoothdf) <- c('MonthNum', 'Profit')
LATAM.Consumer.Profit.timeser.smoothdf$MonthNum<-as.numeric(LATAM.Consumer.Profit.timeser.smoothdf$MonthNum)
LATAM.Consumer.Profit.timeser.smoothdf$Profit<-as.numeric(LATAM.Consumer.Profit.timeser.smoothdf$Profit)
str(LATAM.Consumer.Profit.timeser.smoothdf)

lmfit <- lm(LATAM.Consumer.Profit.timeser.smoothdf$Profit ~ sin(0.6*LATAM.Consumer.Profit.timeser.smoothdf$MonthNum) *
              poly(LATAM.Consumer.Profit.timeser.smoothdf$MonthNum,3) 
            + cos(0.6*LATAM.Consumer.Profit.timeser.smoothdf$MonthNum) * 
              poly(LATAM.Consumer.Profit.timeser.smoothdf$MonthNum,3) +
              LATAM.Consumer.Profit.timeser.smoothdf$MonthNum,
            data=LATAM.Consumer.Profit.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- LATAM.Consumer.Profit.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(LATAM.Consumer.Profit.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting LATAM Consumer Profit------------------
LATAMConProfitForecast <- HoltWinters(LATAM.Consumer.Profit.timeser, beta=FALSE, gamma=FALSE)
LATAMConProfitForecast
plot(LATAMConProfitForecast)

LATAMConProfitForecast6months <- forecast.HoltWinters(LATAMConProfitForecast,h=6)
LATAMConProfitForecast6months
plot(LATAMConProfitForecast6months)
# Comparing with Test data
accuracy(LATAMConProfitForecast6months,LATAM.Consumer.Profit.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month profit-----------
forecast(LATAM.Consumer.Profit.timeser,h=6)
forecast(autoarima,h=6)

# Plotting timeseries data for Quantity
#---------------------------------------
LATAM.Consumer.Qty.timeser<-ts(LATAM.Consumer.Qty.TS[,ycol[1]])
# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
LATAM.Consumer.Qty.timeser.d<-ts(LATAM.Consumer.Qty.TS[,ycol[1]],frequency=12)
LATAM.Consumer.Qty.timeser.decompose <- decompose(LATAM.Consumer.Qty.timeser.d)
plot(LATAM.Consumer.Qty.timeser.decompose)
# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(LATAM.Consumer.Qty.timeser)

# Smoothening the curve 
w <-1
LATAM.Consumer.Qty.timeser.smooth <- filter(LATAM.Consumer.Qty.timeser, 
                                            filter=rep(1/(2*w+1),(2*w+1)), 
                                            method='convolution', sides=2)
diff <- LATAM.Consumer.Qty.timeser.smooth[w+2] - LATAM.Consumer.Qty.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {
  LATAM.Consumer.Qty.timeser.smooth[i] <- LATAM.Consumer.Qty.timeser.smooth[i+1] - diff
}
n <- length(LATAM.Consumer.Qty.timeser)

timevals <- LATAM.Consumer.Qty.TS[[xcol[1]]]
diff <- LATAM.Consumer.Qty.timeser.smooth[n-w] - LATAM.Consumer.Qty.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  LATAM.Consumer.Qty.timeser.smooth[i] <- LATAM.Consumer.Qty.timeser.smooth[i-1] + diff
}
lines(LATAM.Consumer.Qty.timeser.smooth, col="blue", lwd=2)

LATAM.Consumer.Qty.timeser.smoothdf <- as.data.frame(cbind(timevals, as.vector(LATAM.Consumer.Qty.timeser.smooth)))
colnames(LATAM.Consumer.Qty.timeser.smoothdf) <- c('MonthNum', 'Profit')
LATAM.Consumer.Qty.timeser.smoothdf$MonthNum<-as.numeric(LATAM.Consumer.Qty.timeser.smoothdf$MonthNum)
LATAM.Consumer.Qty.timeser.smoothdf$Profit<-as.numeric(LATAM.Consumer.Qty.timeser.smoothdf$Profit)
str(LATAM.Consumer.Qty.timeser.smoothdf)

lmfit <- lm(LATAM.Consumer.Qty.timeser.smoothdf$Profit ~ sin(0.6*LATAM.Consumer.Qty.timeser.smoothdf$MonthNum) *
              poly(LATAM.Consumer.Qty.timeser.smoothdf$MonthNum,2) 
            + cos(0.6*LATAM.Consumer.Qty.timeser.smoothdf$MonthNum) * 
              poly(LATAM.Consumer.Qty.timeser.smoothdf$MonthNum,2) +
              LATAM.Consumer.Qty.timeser.smoothdf$MonthNum,
            data=LATAM.Consumer.Qty.timeser.smoothdf)
summary(lmfit)

trend <- predict(lmfit, data.frame(x=timevals))

lines(timevals, trend, col="red", lwd=2)

# Manual Arima
#-------------
resi <- LATAM.Consumer.Qty.timeser - trend
plot(resi, col='red')

acf(resi)
acf(resi, type="partial")

par("mar") #5.1 4.1 4.1 2.1
armafit <- auto.arima(resi)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit #ARIMA(0,0,0) with zero mean   

# Auto Arima
#-------------
autoarima <- auto.arima(LATAM.Consumer.Qty.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting LATAM Consumer Quantity------------------
LATAMConQtyForecast <- HoltWinters(LATAM.Consumer.Qty.timeser, beta=FALSE, gamma=FALSE)
LATAMConQtyForecast
plot(LATAMConQtyForecast)

LATAMConQtyForecast6months <- forecast.HoltWinters(LATAMConQtyForecast,h=6)
LATAMConQtyForecast6months
plot(LATAMConQtyForecast6months)
# Comparing with Test data
accuracy(LATAMConQtyForecast6months,LATAM.Consumer.Qty.TS.test$TotalMonthlyProfit)

#-----Using Forecast function to calculate next six month Quantity-----------
forecast(LATAM.Consumer.Qty.timeser,h=6)
forecast(autoarima,h=6)
