library(jrvFinance)
library(data.table)
library(optimx)
library(ggplot2)

# Start question 1

## 1a
q1_table = fread("~/Downloads/TeamAssignment2_Q1.csv")
settlement_date = as.Date("2023-12-15")
freq = 2
conv = "ACT/ACT"

q1_table$yield <- sapply(seq_len(nrow(q1_table)), function(i) {
  bond.yield(settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, q1_table$clean.price[i], conv)
})

q1_table$mod_duration <- sapply(seq_len(nrow(q1_table)), function(i) {
  bond.duration(settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, q1_table$yield[i], conv, modified = TRUE)
})

bond.convexity <-
  function(settle,mature,coupon,freq=2,yield,convention,comp.freq=freq) {
    z <- as.data.frame(bond.TCF(settle,mature,coupon,freq,convention))
    cf <- z$cf
    t <- z$t
    r <- yield
    m <- comp.freq
    return ( 1/sum( cf/(1+r/m)^(t*m) ) * sum( t*(t+1/m)*cf/(1+r/m)^(t*m+2) ) )
  }

# Apply the convexity function to each bond
q1_table$convexity <- sapply(seq_len(nrow(q1_table)), function(i) {
  bond.convexity(settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, q1_table$yield[i], conv)
})
print(q1_table)

## 1b
# Calculate the market value of each bond
q1_table$full.price <- sapply(seq_len(nrow(q1_table)), function(i) {
  clean.price <- q1_table$clean.price[i]
  accrued_interest <- bond.TCF(settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, conv)$accrued
  clean.price + accrued_interest
})
# Next, calculate the market value
q1_table[, market.value := .(full.price * face.value / 100)]

# Calculate the total market value of the portfolio
total_market_value <- sum(q1_table$market.value)
# Calculate the weighted average modified duration of the portfolio
q1_table[, weight := market.value / total_market_value]

portfolio_mod_duration <- sum(q1_table$weight * q1_table$mod_duration)
portfolio_convexity <- sum(q1_table$weight * q1_table$convexity)


## 1c
delta_y = 0.001
percentage_change = -portfolio_mod_duration * delta_y
print(paste("Percentage Change in Market Value (First Order Approximation):", round(percentage_change * 100,3), "%"))

## 1d
delta_y = 0.001
# Second order approximation of the percentage change in market value
percentage_change_second_order = -portfolio_mod_duration * delta_y + 0.5 * portfolio_convexity * (delta_y ^ 2)
print(paste("Percentage Change in Market Value (Second Order Approximation):", round(percentage_change_second_order * 100,3), "%"))


## 1e
# Increase the yield of each bond by 10 basis points
q1_table$new_yield <- q1_table$yield + 0.001

# Recalculate the price of each bond using the new yield
q1_table$new_price <- sapply(seq_len(nrow(q1_table)), function(i) {
  bond.price(settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, q1_table$new_yield[i], conv)+bond.TCF(
    settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, conv)$accrued
})

# Calculate the new market value of each bond
q1_table$new_market_value <- q1_table$new_price * q1_table$face.value / 100

# Calculate the total market value before and after the yield change
total_market_value_before <- sum(q1_table$market.value)
total_market_value_after <- sum(q1_table$new_market_value)

# Calculate the actual percentage change in market value
actual_percentage_change <- (total_market_value_after - total_market_value_before) / total_market_value_before * 100

# Print the result
print(paste("Actual Percentage Change in Market Value:", round(actual_percentage_change, 3), "%"))

## 1f
### Approximation
# Define the yield changes (in decimal form)
yield_changes <- c(0.0015, 0.0013, 0.0011, 0.0009)  # 15 bp, 13 bp, 11 bp, 9 bp

q1_table$yield_changes = yield_changes
q1_table = q1_table[,c(1,2,4,5,6,7,9,10,14)]

q1_table[, new_yield := yield + yield_changes]
##New Price calculation
q1_table$new_price <- sapply(seq_len(nrow(q1_table)), function(i) {
  bond.price(settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, q1_table$new_yield[i], conv)+bond.TCF(
    settlement_date, q1_table$maturity[i], q1_table$coupon[i], freq, conv)$accrued
})

q1_table$new_market_value <- q1_table$new_price * q1_table$face.value / 100

### calculate the diff
total_market_value_before <- sum(q1_table$market.value)
total_market_value_after <- sum(q1_table$new_market_value)

actual_percentage_change <- (total_market_value_after - total_market_value_before) / total_market_value_before * 100

### simulate the change of the portfolio value with first order and second order
# First order approximation using modified duration
first_order_change <- sum(q1_table$weight * q1_table$mod_duration * q1_table$yield_changes)
# Second order approximation using convexity
second_order_change <- sum(q1_table$weight * (-q1_table$mod_duration * q1_table$yield_changes + 0.5 * q1_table$convexity * (q1_table$yield_changes ^ 2)))

# Print results
print(paste("First Order Approximation of Percentage Change in Market Value:", round(first_order_change * 100, 3), "%"))
print(paste("Second Order Approximation of Percentage Change in Market Value:", round(second_order_change * 100, 3), "%"))

# Compare with the actual percentage change calculated in 1f
print(paste("Actual Percentage Change in Market Value:", round(actual_percentage_change, 3), "%"))

### we can see that the second order simulate better than the first order approximattion!!

# End question 1

#---------------------------------------

# Start question 2

#### 2a
data <- fread("~/Downloads/TeamAssignment2_Q2_bond.csv")
settle <- as.Date("2023-12-15")
data[, maturity := as.Date(maturity)]
data[, ttm := as.numeric((maturity - settle) / 365)]
data[, p.full := pclean + bond.TCF(settle, maturity, coupon, 2, "ACT/ACT")$accrued, by = 1:nrow(data)]
data = data[, c(1,2,4,5)]
bond = data

# Define your spot rate function
gfun4 <- function(ttm, parm) {
  parm[1] + parm[2] * ttm + parm[3] * ttm^2 + parm[4] * ttm^3 + parm[5] * ttm^4 + parm[6] * ttm^5
}

#### ------------Solution 1 Approach------Try to avoid it! it took me roughly half hour to run it------------####
# ssr <- function(parm) {
#   for (i in 1:nrow(data)) {
#     # CALCULATE data$phat[i]
#     # as the security is a coupon bond, its price is the sum of the PVs of all cash flows
#     cfi  <- bond.TCF(settle,data$maturity[i],data$coupon[i],2,"ACT/ACT")$cf  # cfi is a vector of all future cash flows
#     cf.d <- coupons.dates(settle,data$maturity[i],2)                         # cf.d is a vector of dates for the future cash flows
#     ttmi <- as.numeric(cf.d-settle)/365                                      # ttmi is a vector of all ttms of these future cash flows
#     rates<- sapply(ttmi,gfun4,parm=parm)                                     # rates is a vector of Z(ttmi), where Z(ttmi) is the spot rate from gfun(ttm,parm)
#     disfac<- 1/(1+rates/2)^(ttmi*2)                                          # disfac is a vector of discount factors for $1 paid at ttmi years                                                                                                    #   i.e., disfac[ttm] = 1/(1+rates(ttm)/2)^(2*ttm)
#     data$phat[i] <- sum(disfac * cfi)                                        # sum of PVs of future cash flows, which is the model price of this security
#   }
#   ztest <- sum((data$p.full-data$phat)^2)
#   #   sometimes, optimx sends in a parm which results in phat not computable.
#   #   in that case, return a large positive number, 9x10^20, so optimx will stay away from going there
#   if (is.nan(ztest)) ztest <- 9e20                                              
#   return(ztest)
# }
# 
# methods.fast <- c('BFGS','Nelder-Mead','L-BFGS-B','nlm','nlminb','Rvmmin')
# 
# opt4x <- optimx(start,ssr,method=methods.fast,
#                 control = list(maxit=10000))
# 
# print(opt4x)


#### ------------Solution 2 Approach------------------####
# Pre-calculate cash flows and time-to-maturities (Solution 2 optimization)
cf.mat <- matrix(0, nrow = nrow(bond), ncol = 61)
ttm.mat <- matrix(0, nrow = nrow(bond), ncol = 61)
ncf.mat <- numeric(nrow(bond))
for (i in 1:nrow(bond)) {
  cfi <- bond.TCF(settle, bond$maturity[i], bond$coupon[i], 2, "ACT/ACT")$cf
  cf_dates <- coupons.dates(settle, bond$maturity[i], 2)
  ttmi <- as.numeric(cf_dates - settle) / 365
  ncf.mat[i] <- length(cfi)
  cf.mat[i, 1:ncf.mat[i]] <- cfi
  ttm.mat[i, 1:ncf.mat[i]] <- ttmi
}

ssr <- function(parm) {
  total_error <- 0
  for (i in 1:nrow(bond)) {
    cfi <- cf.mat[i, 1:ncf.mat[i]]
    ttmi <- ttm.mat[i, 1:ncf.mat[i]]
    rates <- sapply(ttmi, gfun4, parm = parm)
    disfac <- 1 / (1 + rates / 2) ^ (ttmi * 2)
    phat <- sum(disfac * cfi)
    error <- (bond$p.full[i] - phat)^2
    total_error <- total_error + error
  }
  return(total_error)
}

start <- c(0.01, 0, 0, 0, 0, 0)
#methods.fast <- c('BFGS','Nelder-Mead','L-BFGS-B','nlm','nlminb','Rvmmin')
opt4x <- suppressWarnings(optimx(start, ssr, method = 'Nelder-Mead', control = list(maxit = 10000)))

print(opt4x)


opt4x[which.min(opt4x$value),]

## Coefficient -> 0.04434334 -2.301289e-03 4.067661e-04 -3.228110e-05 1.218913e-06 -1.729954e-08


Formula_spot <- function(ttm) {
  alpha0 <-  0.04434334
  alpha1 <- -2.301289e-03
  alpha2 <- 4.067661e-04
  alpha3 <- -3.228110e-05
  alpha4 <-  1.218913e-06
  alpha5 <-  -1.729954e-08
  
  spot_rate <- alpha0 + alpha1 * ttm + alpha2 * ttm^2 + alpha3 * ttm^3 + alpha4 * ttm^4 + alpha5 * ttm^5
  return(spot_rate)
}


## 2b

for (i in 1:nrow(data)) {
  # CALCULATE data$phat[i]
  # as the security is a coupon bond, its price is the sum of the PVs of all cash flows
  cfi  <- bond.TCF(settle,data$maturity[i],data$coupon[i],2,"ACT/ACT")$cf  # cfi is a vector of all future cash flows
  cf.d <- coupons.dates(settle,data$maturity[i],2)                         # cf.d is a vector of dates for the future cash flows
  ttmi <- as.numeric(cf.d-settle)/365                                      # ttmi is a vector of all ttms of these future cash flows
  rates<- sapply(ttmi,Formula_spot)                                     # rates is a vector of Z(ttmi), where Z(ttmi) is the spot rate from gfun(ttm,parm)
  disfac<- 1/(1+rates/2)^(ttmi*2)                                          # disfac is a vector of discount factors for $1 paid at ttmi years                                                                                                    #   i.e., disfac[ttm] = 1/(1+rates(ttm)/2)^(2*ttm)
  data$phat[i] <- sum(disfac * cfi)                                        # sum of PVs of future cash flows, which is the model price of this security
}
ztest <- sum((data$p.full-data$phat)^2)    
data

ggplot(data, aes(x = maturity)) +
  geom_point(aes(y = p.full, colour = "Actual Price"), size = 1) +
  geom_point(aes(y = phat, colour = "Predicted Price"), size = 1) +
  labs(title = "Comparison of Actual and Predicted Prices of Coupon Strips",
       x = "Maturity Date", y = "Price") +
  scale_colour_manual("", 
                      breaks = c("Actual Price", "Predicted Price"),
                      values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2c

ci_data <- fread("~/Downloads/TeamAssignment2_Q2_ci.csv")
ci_data[, maturity := as.Date(maturity)]
ci_data[, ttm := as.numeric(maturity - settle) / 365]
ci_data[, predicted_price := {
  # Assuming the cash flow for a coupon strip is just its face value at maturity
  cfi <- 100  # Face value
  ttm <- as.numeric(maturity - settle) / 365
  rate <- Formula_spot(ttm)
  disfac <- 1 / (1 + rate / 2)^(ttm * 2)
  predicted_price <- disfac * cfi
}]
ci_result = ci_data[, .(maturity, actual_price = price, predicted_price)]

ci_ssr <- sum((result$actual_price - result$predicted_price)^2)

## plot to to see the result
ggplot(ci_result, aes(x = maturity)) +
  geom_point(aes(y = actual_price, colour = "Actual Price"), size = 1) +
  geom_point(aes(y = predicted_price, colour = "Predicted Price"), size = 1) +
  labs(title = "Comparison of Actual and Predicted Prices of Coupon Strips",
       x = "Maturity Date", y = "Price") +
  scale_colour_manual("", 
                      breaks = c("Actual Price", "Predicted Price"),
                      values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")




# 2d
sp_data <- fread("~/Downloads/TeamAssignment2_Q2_sp.csv")
sp_data[, maturity := as.Date(maturity)]
settle <- as.Date("2023-12-15")

# Calculate ttm for principal strips
sp_data[, ttm := as.numeric(maturity - settle) / 365]

# Calculate predicted prices using the spot rate function
sp_data[, predicted_price := {
  face_value <- 100  # Face value
  rate <- Formula_spot(ttm)  # Calculate the spot rate using your model
  disfac <- 1 / (1 + rate / 2)^(ttm * 2)  # Discount factor
  predicted_price <- disfac * face_value  # Present value of the face value
}]

# Compare actual and predicted prices
sp_result = sp_data[, .(maturity, actual_price = price, predicted_price)]
sp_ssr <- sum((result$actual_price - result$predicted_price)^2)

ggplot(sp_result, aes(x = maturity)) +
  geom_point(aes(y = actual_price, colour = "Actual Price"), size = 1) +
  geom_point(aes(y = predicted_price, colour = "Predicted Price"), size = 1) +
  labs(title = "Comparison of Actual and Predicted Prices of Coupon Strips",
       x = "Maturity Date", y = "Price") +
  scale_colour_manual("", 
                      breaks = c("Actual Price", "Predicted Price"),
                      values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")


## 2e Discuss how the prediction errors differ for coupon strips (in 2c) and principal strips (in 2d).


# It's interesting to see that the prediction errors for coupon and principal strips are quite
# similar, each with an SSR of 2156.06. This contrasts sharply with the much lower SSR of 84.29
# for the regular bond data, where the model clearly performs better. Why such a difference? 
# Well, it boils down to what the model was originally trained on: the unique world of regular bonds. 

# Regular bonds have a more complex structure in terms of cash flows and discounting, 
# and our model was tailored to capture these intricacies. On the other hand, strips are 
# much simpler– they just have one payment at maturity. This simplicity can actually make 
# them trickier for a model that’s used to dealing with more complexity. So, when we apply 
# this bond-focused model to strips, it doesn’t quite hit the mark as precisely.

# That said, the results we got for the strips are still pretty respectable. 
# The graphs we looked at earlier show that the model does a decent job, even if it's 
# not as spot-on as it is with the regular bonds. 

# In summary, our model is a bit of a star when it comes to regular bonds, but it needs a
# bit of tweaking to fully understand the nuances of coupon and principal strips. It’s a 
# great reminder that financial instruments can vary widely, and a one-size-fits-all 
# approach might not always capture these differences. We might need to consider some 
# adjustments or even different modeling techniques to really nail the predictions for 
# these types of financial products.


# End question 2

#--------------------------------------

# Start question 3
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}
zcb.price <- function(zcb.yield,ttm,freq=2) {
  return( 100/(1+zcb.yield/freq)^(freq*ttm) )
}
zcb.yield <- function(zcb.price,ttm,freq=2) {
  return( freq * ( (100/zcb.price)^(1/(freq*ttm)) - 1 ) )
}


# 3a 
settle <- DATE(2021, 12, 30)

df <- fread("~/Downloads/TeamAssignment2_Q3.csv")
df[, maturity := as.Date2(maturity) ]
df[, ttm      := as.numeric(maturity-settle)/365 ]


df$disfac <- rep(1,nrow(df))
df$disfac[1] <- 100/(100+df$par.rate[1]*100)
for (i in 2:nrow(df)) {
  cumdf <- sum(df$disfac[1:i-1])
  df$disfac[i] <- ( 100 - df$par.rate[i]*100 * cumdf ) / 
    (100 + df$par.rate[i]*100 )
}

df[, spot := zcb.yield(disfac*100,ttm)]
df
#3b 

tmp <- data.frame(x = df$ttm,
                  y = df$spot)
spline_df <- as.data.frame(spline(tmp$x, tmp$y, xout = tmp$x, method = "natural"))
newttm1 <- as.numeric(DATE(2022,06,30)-settle)/365
newttm2 <- as.numeric(DATE(2023,06,30)-settle)/365
newttm3 <- as.numeric(DATE(2024,06,30)-settle)/365
newttm4 <- as.numeric(DATE(2025,06,30)-settle)/365
newttm5 <- as.numeric(DATE(2026,06,30)-settle)/365
newttm6 <- as.numeric(DATE(2027,06,30)-settle)/365
newttm7 <- as.numeric(DATE(2028,06,30)-settle)/365
newttm8 <- as.numeric(DATE(2029,06,30)-settle)/365
newttm9 <- as.numeric(DATE(2030,06,30)-settle)/365
newttm10 <- as.numeric(DATE(2031,06,30)-settle)/365

new_df<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm1, method="natural") )
new_df2<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm2, method="natural") )
new_df3<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm3, method="natural") )
new_df4<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm4, method="natural") )
new_df5<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm5, method="natural") )
new_df6<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm6, method="natural") )
new_df7<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm7, method="natural") )
new_df8<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm8, method="natural") )
new_df9<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm9, method="natural") )
new_df10<- as.data.frame(spline(tmp$x, tmp$y, xout = newttm10, method="natural") )


DF_newttm <- zcb.price(new_df$y,newttm1)/100
DF_newttm2 <- zcb.price(new_df2$y,newttm2)/100
DF_newttm3 <- zcb.price(new_df3$y,newttm3)/100
DF_newttm4 <- zcb.price(new_df4$y,newttm4)/100
DF_newttm5 <- zcb.price(new_df5$y,newttm5)/100
DF_newttm6 <- zcb.price(new_df$y,newttm6)/100
DF_newttm7 <- zcb.price(new_df2$y,newttm7)/100
DF_newttm8 <- zcb.price(new_df3$y,newttm8)/100
DF_newttm9 <- zcb.price(new_df4$y,newttm9)/100
DF_newttm10 <- zcb.price(new_df5$y,newttm10)/100


low <- 100000*DF_newttm
low2 <- 100000*DF_newttm2
low3 <- 100000*DF_newttm3
low4 <- 100000*DF_newttm4
low5 <- 100000*DF_newttm5
low6 <- 100000*DF_newttm6
low7 <- 100000*DF_newttm7
low8 <- 100000*DF_newttm8
low9 <- 100000*DF_newttm9
low10 <- 100000*DF_newttm10

answer2 <- sum(low, low2, low3, low4, low5, low6, low7, low8, low9, low10)
answer2



# End question 3

