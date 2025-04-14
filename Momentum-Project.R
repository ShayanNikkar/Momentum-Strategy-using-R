##importing the libraries
library("zoo")
library("reshape2")
library("data.table")
library("plotly")
library("ggplot2")

##loading the csv file for prices
p1 <- read.csv("./exam2024-25-odd.csv" , head = TRUE)
p1$month <- as.yearmon(p1$month)
p1 <- zoo(p1[,2:ncol(p1)] , p1$month) ## converting to time series


## Question 2 (a)
N <- NCOL(p1) 
cat("There are" , N , "stocks in the dataset \n")
## Question 2 (b)
T <- NROW(p1)
cat("There are" , T , "months in the dataset \n")

##Question 2 (c)
## we define function which gives n-month pas return
m_return <- function(n) {
  returns <- NULL
  for (t in 1:nrow(p1)){
    if (t > n+1){ 
      r <- ((coredata(p1[t-1,])/coredata(p1[t-(n+1),])) -1) * 100 ## compute the past ret
      returns <- rbind(returns , r)
    }
    else{
      returns <-rbind(returns , rep(NA,ncol(p1))) ## set NA for months that we can not observe the n-month past ret
    }
  }
  returns <- zoo(returns, order.by = index(p1)) ## convert to time series
  return(returns[-1:-(n+1),]) ## omit the months with NA from the new dataset
} 
## assign n=3,6,9,12 to the m_ret function
r_3 <- m_return(3)
r_6 <- m_return(6)
r_9 <- m_return(9)
r_12 <- m_return(12)

## computing the mean and standard deviation of Nov 2024 for each period
mean_r3 <- mean(r_3[nrow(r_3),],na.rm=TRUE)
sd_r3 <- sd(r_3[nrow(r_3),],na.rm=TRUE)
mean_r6 <- mean(r_6[nrow(r_6),],na.rm=TRUE)
sd_r6 <- sd(r_6[nrow(r_6),],na.rm=TRUE)
mean_r9 <- mean(r_9[nrow(r_9),],na.rm=TRUE)
sd_r9 <- sd(r_9[nrow(r_9),],na.rm=TRUE)
mean_r12 <- mean(r_12[nrow(r_12),],na.rm=TRUE)
sd_r12 <- sd(r_12[nrow(r_12),],na.rm=TRUE)

##convert it to a dataframe
Nov24 <- data.frame( Average_return= c(mean_r3,mean_r6 , mean_r9 , mean_r12),
                     Std.Dev.Return = c(sd_r3 , sd_r6 , sd_r9 , sd_r12)
                     )
rownames(Nov24) <- c("J=3", "J=6" , "J=9", "J=12")


##Question 2(d)

##ranking function gives us the rank of assets based on their past return
## number 1 belongs to the asset with lowest past ret and 100 belongs to the asset with highest past ret
ranking <- function(x){
  ranks <- NULL
  for ( t in 1:nrow(x)){
    ind <- rank(as.numeric(x[t,]) , ties.method = "min")
    ranks <- rbind(ranks , ind)
  }
   ranks <- zoo(ranks,order.by = index(x))
   colnames(ranks) <- colnames(x)
   return(ranks)
}
## get the ranking function of each 3/6/9/12 month past returns
rank_3 <- ranking(r_3)
rank_6 <- ranking(r_6)
rank_9 <- ranking(r_9)
rank_12 <- ranking(r_12)

##The monthly return of each stock is computed and converted to a new data frame called Actualreturn
Actualreturn <-NULL
for (t in 2:nrow(p1)){
  r <- ((coredata(p1[t,])/coredata(p1[t-1,])) -1) * 100
  Actualreturn <- rbind(Actualreturn , r)
}
Actualreturn <- zoo(Actualreturn, order.by = index(p1[2:nrow(p1) ,])) 
rm(r)

##Now based on the ranking of the stocks in past returns we determine 10 winners(highest past returns)
## and 10 losers(lowest pas returns) in each month and compute the equal-weighted average return of each group
##the function winner_loser gets the ranking as input, computes the average of actual returns of each group(winner & losers)
winners <- NULL
losers <- NULL
winner_loser <- function ( rank_f){
  ret_f <- window(Actualreturn , start = start(rank_f)) ##starting both ranking and returns dataset from the same date
  for (t in 1:nrow(rank_f)){
    a <- which(rank_f[t,] <= 10) ## save index of loser stocks in a
    b <- which(rank_f[t,] >=91) ##save index of winner stocks in b
    m_los <- mean(ret_f[t,a] , na.rm=TRUE) ## value-weighted average of past losers' current returns
    m_win <- mean(ret_f[t,b] , na.rm=TRUE) ## value-weighted average of past winners' current returns
    winners <- c(winners , m_win)
    losers <- c(losers , m_los)
    win_los_r <- cbind(winners , losers) ## put the winners and losers columns next to each
  }
  win_los_r <-zoo(win_los_r , order.by = index(rank_f))
  colnames(win_los_r) <- c("Winners" , "Losers")
  return(win_los_r)
}
## assign the rank dataset for each 3/6/9/12 month to the function. 
## the output is the average current return of the past winners and the average current return of the past losers
win_los_3 <- winner_loser(rank_3)
win_los_6 <- winner_loser(rank_6)
win_los_9 <- winner_loser(rank_9)
win_los_12 <- winner_loser(rank_12)

## computing the winners decile average return and losers decile average return for 3/6/9/12 month
mean_win_3 <- mean(win_los_3 [ , 1] , na.rm=TRUE)
mean_los_3 <- mean(win_los_3 [ ,2], na.rm=TRUE)
mean_win_6 <- mean(win_los_6 [ ,1] , na.rm=TRUE)
mean_los_6 <- mean(win_los_6 [ ,2], na.rm=TRUE)
mean_win_9 <- mean(win_los_9 [ ,1] , na.rm=TRUE)
mean_los_9 <- mean(win_los_9 [ ,2], na.rm=TRUE)
mean_win_12 <- mean(win_los_12 [ ,1] , na.rm=TRUE)
mean_los_12 <- mean(win_los_12 [ ,2], na.rm=TRUE)

##convert the results in to a dataframe called Decile_Portfolio_Returns
Decile_Portfolio_Returns <- data.frame(Winners_Decile_Avg_Ret = c(mean_win_3,mean_win_6,mean_win_9,mean_win_12),
                                       Losers_Decile_Avg_Ret = c(mean_los_3,mean_los_6,mean_los_9,mean_los_12)
                                       )
rownames(Decile_Portfolio_Returns) = c("J=3", "J=6", "J=9" , "J=12")


#Question 2(e)

## computing the cumulative return for losers and winners portfo in a loop
for(i in c(3,6,9,12)){
  x <- get(paste0("win_los_",i))
  y <- cbind((cumprod(1+(x[,1]/100))-1) * 100,(cumprod(1+(x[,2]/100))-1) * 100 )
  colnames(y) <-c("Cumret_Winners", "Cumret_Losers")
  assign(paste0("cum_ret_",i) , y)
}       
rm(x,y)

## plotting the cumulative returns of winners and losers portfo for each J=3,6,9,12 month 
par(mfrow = c(2, 2))
plot(cum_ret_3[,1] , main = "Cumulative Return" , col='green' , xlab = "Year" , ylab = "cum_ret_3",
     ylim = c(0,12000))
lines(cum_ret_3[,2] , col='red')
legend("topleft", legend = c("Winners", "Losers"), 
       col = c("green", "red"),lty=1 , cex=0.7)
grid()

plot(cum_ret_6[,1] , main = "Cumulative Return" , col='green' , xlab = "Year" , ylab = "cum_ret_6",
     ylim = c(0,12000))
lines(cum_ret_6[,2] , col='red')
legend("topleft", legend = c("Winners", "Losers"), 
       col = c("green", "red"),lty=1 , cex=0.7)
grid()

plot(cum_ret_9[,1] , main = "Cumulative Return" , col='green' , xlab = "Year" , ylab = "cum_ret_9",
     ylim = c(0,18000))
lines(cum_ret_9[,2] , col='red')
legend("topleft", legend = c("Winners", "Losers"), 
       col = c("green", "red"),lty=1 , cex=0.7)
grid()

plot(cum_ret_12[,1] , main = "Cumulative Return" , col='green' , xlab = "Year" , ylab = "cum_ret_12",
     ylim = c(0,8000))
lines(cum_ret_12[,2] , col='red')
legend("topleft", legend = c("Winners", "Losers"), 
       col = c("green", "red"),lty=1 , cex=0.7)
grid()


##Question 2(f)

##creating the (Long Winner-Short Loser) portfo for each J=3,6,9,12 month and computing the cumulative return of each
long_short_3 <- win_los_3$Winners - win_los_3$Losers
cum_long_short_3 <- (cumprod(1+(long_short_3/100))-1) * 100
long_short_6 <- win_los_6$Winners - win_los_6$Losers
cum_long_short_6 <- (cumprod(1+(long_short_6/100))-1) * 100
long_short_9 <- win_los_9$Winners - win_los_9$Losers
cum_long_short_9 <- (cumprod(1+(long_short_9/100))-1) * 100
long_short_12 <- win_los_12$Winners - win_los_12$Losers
cum_long_short_12 <- (cumprod(1+(long_short_12/100))-1) * 100

##plotting the cumulative return for the above portfolio for J=3,6,9,12 month
par(mfrow = c(2, 2))
plot(cum_long_short_3 , main = "Long winner - Short Loser" , col='blue' , xlab = "Year" , ylab = "cum_ret_3" 
     )
grid()

plot(cum_long_short_6, main = "Long winner - Short Loser" , col='blue' , xlab = "Year" , ylab = "cum_ret_6"
     )
grid()

plot(cum_long_short_9 , main = "Long winner - Short Loser" , col='blue' , xlab = "Year" , ylab = "cum_ret_9"
     )
grid()

plot(cum_long_short_12, main = "Long winner - Short Loser" , col='blue' , xlab = "Year" , ylab = "cum_ret_12"
     )
grid()


##Question 3(a)
## importing the Fama_French dataset
f1 <- read.csv("./fama-french-factors.csv" , head = TRUE)
## change it to time series
f1$month <- as.yearmon(f1$month) 
f1 <- zoo(f1[,2:ncol(f1)] , f1$month) 

## subtracting risk-free rate from market, small-minus-big and high-minus-low returns and saving the results in new columns
f1$rm <- f1$Mkt - f1$RF
f1$rsmb <- f1$SMB - f1$RF
f1$rhml <- f1$HML - f1$RF

##subtracting the risk free rate from our portfolio return
rp_3 <- long_short_3 - f1$RF
rp_6 <- long_short_6 - f1$RF
rp_9 <- long_short_9 - f1$RF
rp_12 <- long_short_12 - f1$RF

## running the regression 
fama_french <- function(lhs) {
  rhs <- window(f1 , start= start(lhs))
  m <- lm(lhs ~ rhs[,5:7]) ## columns 5 to 7 are new columns which risk free rate is deducted 
  alpha <- coef(m)[1]
  betas <- coef(m)[2:length(coef(m))]
  resids <- residuals(m)
  t_stats <- summary(m)$coefficients[,3] ## extracting the t-value
  table <- rbind(c(alpha,t(betas)),t(t_stats))
  colnames(table) <- c("alpha" , "Beta_MKT" , "Beta_SMB" , "Beta_HML")
  return(table)
}
## running the fama_french function for all J=3,6,,9,12 
fam_3 <- fama_french(rp_3)
fam_6 <- fama_french(rp_6)
fam_9 <- fama_french(rp_9)
fam_12 <- fama_french(rp_12)
## creating final table to put all results in
final_table <- rbind(fam_3,fam_6,fam_9,fam_12)
rownames(final_table) <- c("J=3 coeffs" , "J=3 t_stats" , "J=6 coeffs" , "J=6 t_stats",
                           "J=9 coeffs" , "J=9 t_stats" , "J=12 coeffs" , "J=12 t_stats")



