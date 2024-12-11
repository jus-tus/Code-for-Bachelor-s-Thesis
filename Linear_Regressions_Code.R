# ---
# title: "Linear Regressions"
# author: "Justus Felkel"
# date: "12.12.2024"
# ---

# load packages 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(lubridate)
library(zoo)
library(dynlm)
library(data.table)
library(zoo)       
library(plm)         
library(lmtest)     
library(gridExtra)
library(DescTools)

#setting the working directory
setwd("C:/Users/Justus/BachelorsThesisJustus/Inflation-adjusted transactions/Bachelor's Thesis JF Code")

#load created r scripts
source("InflationAdjusted_MerchantCategory_CardSpending.R")
source("Monetary_Policy_Shocks_Creation.R")

######################################################################################################################
#Regression of current policy shocks on past shocks - Wong critique
################################################################################################################################

#create lags
mp_shocks_wong1 <- mp_shocks3 %>%
  arrange(date) %>%  
  mutate(
    lag1 = dplyr::lag(policy_shock, 1),
    lag2 = dplyr::lag(policy_shock, 2),
    lag3 = dplyr::lag(policy_shock, 3),
    lag4 = dplyr::lag(policy_shock, 4)
  )

mp_shocks_wong2 <- mp_shocks_wong1 %>%
  mutate(
    policy_shock = ifelse(is.na(policy_shock), 0, policy_shock),
    lag1 = ifelse(is.na(lag1), 0, lag1),
    lag2 = ifelse(is.na(lag2), 0, lag2),
    lag3 = ifelse(is.na(lag3), 0, lag3),
    lag4 = ifelse(is.na(lag4), 0, lag4)
  )

wong_regression <- dynlm(policy_shock ~ lag1 + lag2 + lag3 + lag4, data = mp_shocks_wong2)
summary(wong_regression)

######################################################################################################################
#Functions from Schmidheiny & Siegloch (2023)
################################################################################################################################

# function for reverse cumulative sum of vector
# summing up from end to start
revcumsum <- function(x){
  x <- rev(cumsum(rev(x)))
}

# function to calculate standard errors of cumulative sum
# b: a coefficient vector
# vcov: a variance covariance matrix
secumsum <- function(vcov){
  L <- dim(vcov)[1]
  # result vector with standard errors
  se <- c()
  # loop over elements
  for (i in c(1:L)){
    # Variance of cumulative sum from 1 to i
    # V[ax] = a*V[x]*a', a=[1, ..., 1]
    # create vector for summation
    a <- matrix(rep(1,i), nrow = 1)
    V <- a %*% vcov[1:i,1:i] %*% t(a)
    se[i] <- sqrt(V)
  }
  return(se)
}

# function to calculate standard errors of reverse cumulative sum
# summing up from end to start
# b: a coefficient vector
# vcov: a variance covariance matrix
serevcumsum <- function(vcov){
  L <- dim(vcov)[1]
  # result vector with standard errors
  se <- c()
  # loop over elements
  for (i in c(L:1)){
    # Variance of cumulative sum from i to L
    # V[ax] = a*V[x]*a', a=[1, ..., 1]
    a <- matrix(rep(1,L-i+1), nrow = 1)
    V <- a %*% vcov[i:L,i:L] %*% t(a)
    se[i] <- sqrt(V)
  }
  return(se)
}

######################################################################################################################
#Weekly Regression - Baseline
################################################################################################################################

#start with June 2022 and thus remove the first 1375 rows from the dataset starting with 2020 and also last 77 rows to end with September 2024
weekly_dat1 <- weekly_mercat_yoy5[-c(1:1375), ]
weekly_dat2 <- head(weekly_dat1, -77)

#merge policy shock with card spending dataset
weekly_dat3 <- weekly_dat2 %>% 
  left_join(mp_shocks, by = "date")

#set NAs to zero
weekly_dat3$policy_shock[is.na(weekly_dat3$policy_shock)] <- 0

#set as year month
weekly_dat3$yearmonth <- as.yearmon(weekly_dat3$date)

#set as panel data
weekly_dat4 <- pdata.frame(weekly_dat3, index = c("merchant_category", "date"))


##
#discretionary vs non discretionary
discretionary1 <- weekly_dat3 %>% 
  filter(merchant_category %in% c("Accommodation",  "Food and beverage services", "Entertainment and sports", "Other", "Personal Services", "Professional Services", "Retail: Other goods"))

essential1 <- weekly_dat3 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Human health services", "Transport services", "Retail: Fuel stations")  )

discretionary2 <- pdata.frame(discretionary1, index = c("merchant_category", "date"))
essential2 <- pdata.frame(essential1, index = c("merchant_category", "date"))

# estimate distributed-lag model in first differences with 0 leads and 12 lags
# no constant in first differences, i.e. no linear trend in levels
estim_test1 <- 
  plm(diff(yoy_change) ~ policy_shock  + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = weekly_dat4,
      model = "pooling")
summary(estim_test1)

estim_discretionary1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = discretionary2,
      model = "pooling")
summary(estim_discretionary1)

estim_essential1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = essential2,
      model = "pooling")
summary(estim_essential1)

#now creating robust standard errors and plotting the effects

#TOTAL
# gamma coefficients ESTIM_TEST1
gamma_test1 <- estim_test1$coefficients[1:13]

# store variance-covariance matrix
vcov_test1 <- vcovHC(estim_test1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
           cumsum(gamma_test1[1:13])),   # lags
  se = c(
    0,                                            # reference period
         secumsum(vcov_test1[1:13, 1:13])))  # lags
beta_test1

beta_test1[, estimator := "First difference"]

plot_beta_test1 <- ggplot(beta_test1, aes(x = week_to_reform, y = coef,
                                                         group = estimator, color = estimator,
                                                         shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) + 
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )

#print the plot
plot_beta_test1

#ggsave("regression1.png", plot = plot_beta_test1, width = 12, height = 8, dpi = 300)


#ESSENTIAL
# gamma coefficients
gamma_test_ess1 <- estim_essential1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_ess1 <- vcovHC(estim_essential1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_ess1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_ess1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_ess1[1:13, 1:13])))  # lags
beta_test_ess1

beta_test_ess1[, estimator := "First difference"]

#plot
plot_beta_test_ess1 <- ggplot(beta_test_ess1, aes(x = week_to_reform, y = coef,
                                          group = estimator, color = estimator,
                                          shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )

#DISCRETIONARY
# gamma coefficients
gamma_test_dis1 <- estim_discretionary1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_dis1 <- vcovHC(estim_discretionary1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_dis1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_dis1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_dis1[1:13, 1:13])))  # lags
beta_test_dis1

beta_test_dis1[, estimator := "First difference"]

plot_beta_test_dis1 <- ggplot(beta_test_dis1, aes(x = week_to_reform, y = coef,
                                          group = estimator, color = estimator,
                                          shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )

#Plot together
plot_reg_together1 <- grid.arrange(
  plot_beta_test1,
  plot_beta_test_ess1, 
  plot_beta_test_dis1,
  ncol = 2     # Number of columns
)
#ggsave("regressioncombined1.png", plot = plot_reg_together1, width = 12, height = 8, dpi = 300)




######################################################################################################################
#Monthly Regression - Baseline
################################################################################################################################

#remove october and november 2024
monthly_merch1 <- head(monthly_merchant7, -22)

#create columns month and year
monthly_merch2 <- monthly_merch1 %>%
  mutate(month = month(date),
         year = year(date))

romonth1 <- monthly_merch2 %>%
  left_join(mp_shocks3, by = c("month","year"))

#set NAs to zero
romonth1$policy_shock[is.na(romonth1$policy_shock)] <- 0

romonth1$yearmonth <- as.yearmon(romonth1$date.x)

romonth2 <- pdata.frame(romonth1, index = c("merchant_category", "date.x"))

#discretionary vs non discretionary
discretionarymonth1 <- romonth1 %>% 
  filter(merchant_category %in% c("Accommodation",  "Food and beverage services", "Entertainment and sports", "Other", "Personal Services", "Professional Services", "Retail: Other goods"))

essentialmonth1 <- romonth1 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Human health services", "Transport services", "Retail: Fuel stations")  )

discretionarymonth2 <- pdata.frame(discretionarymonth1, index = c("merchant_category", "date.x"))
essentialmonth2 <- pdata.frame(essentialmonth1, index = c("merchant_category", "date.x"))

#regression now 
estim_mont1 <- 
  plm(diff(yoy_growth) ~ policy_shock + lag(policy_shock, k=c(1:3)) + as.factor(yearmonth) - 1,
      data = romonth2,
      model = "pooling")
summary(estim_mont1)

estim_discretionary_month1 <- 
  plm(diff(yoy_growth) ~ policy_shock + lag(policy_shock, k=c(1:3)) + as.factor(yearmonth) - 1,
      data = discretionarymonth2,
      model = "pooling")
summary(estim_discretionary_month1)

estim_essential_month1 <- 
  plm(diff(yoy_growth) ~ policy_shock + lag(policy_shock, k=c(1:3)) + as.factor(yearmonth) - 1,
      data = essentialmonth2,
      model = "pooling")
summary(estim_essential_month1)


#Plotting now

#TOTAL
# gamma coefficients
gamma_test_month1 <- estim_mont1$coefficients[1:4]

# store variance-covariance matrix
vcov_test_month1 <- vcovHC(estim_mont1, type = "sss", cluster = "group")[1:4,1:4]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_month1 <- data.table(
  week_to_reform = c(-1:3), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_month1[1:4])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_month1[1:4, 1:4])))  # lags
beta_test_month1

beta_test_month1[, estimator := "First difference"]

plot_beta_test_month1 <- ggplot(beta_test_month1, aes(x = week_to_reform, y = coef,
                                                  group = estimator, color = estimator,
                                                  shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) + 
  scale_color_manual(values = c("darkblue")) + 
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Months Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-150, 50), breaks = seq(-150, 50, by = 25)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )

#ESSENTIAL
# gamma coefficients
gamma_test_month_ess1 <- estim_essential_month1$coefficients[1:4]

# store variance-covariance matrix
vcov_test_month_ess1 <- vcovHC(estim_essential_month1, type = "sss", cluster = "group")[1:4,1:4]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_month_ess1 <- data.table(
  week_to_reform = c(-1:3), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_month_ess1[1:4])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_month_ess1[1:4, 1:4])))  # lags
beta_test_month_ess1

beta_test_month_ess1[, estimator := "First difference"]

plot_beta_test_month_ess1 <- ggplot(beta_test_month_ess1, aes(x = week_to_reform, y = coef,
                                                      group = estimator, color = estimator,
                                                      shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) + 
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Months Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-150, 50), breaks = seq(-150, 50, by = 25)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )

#DISCRETIONARY
# gamma coefficients
gamma_test_month_dis1 <- estim_discretionary_month1$coefficients[1:4]

# store variance-covariance matrix
vcov_test_month_dis1 <- vcovHC(estim_discretionary_month1, type = "sss", cluster = "group")[1:4,1:4]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_month_dis1 <- data.table(
  week_to_reform = c(-1:3), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_month_dis1[1:4])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_month_dis1[1:4, 1:4])))  # lags
beta_test_month_dis1

beta_test_month_dis1[, estimator := "First difference"]

plot_beta_test_month_dis1 <- ggplot(beta_test_month_dis1, aes(x = week_to_reform, y = coef,
                                                              group = estimator, color = estimator,
                                                              shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Months Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-150, 50), breaks = seq(-150, 50, by = 25)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )

#ggsave("regressioncombined1.png", plot = plot_reg_together1, width = 12, height = 8, dpi = 300)


######################################################################################################################
#Weekly Regression - Winsorising at a 90% level
################################################################################################################################


win_dat1 <- weekly_dat3 %>%
  group_by(merchant_category) %>%
  arrange(date) %>%
  mutate(yoy_change = Winsorize(yoy_change)) %>% 
  ungroup()

win_dat2 <- pdata.frame(win_dat1, index = c("merchant_category", "date"))

#discretionary vs non discretionary
discretionary_win1 <- win_dat1 %>% 
  filter(merchant_category %in% c("Accommodation",  "Food and beverage services", "Entertainment and sports", "Other", "Personal Services", "Professional Services", "Retail: Other goods"))

essential_win1 <- win_dat1 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Human health services", "Transport services", "Retail: Fuel stations")  )

discretionary_win2 <- pdata.frame(discretionary_win1, index = c("merchant_category", "date"))
essential_win2 <- pdata.frame(essential_win1, index = c("merchant_category", "date"))

#regression now
estim_winsor1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = win_dat2,
      model = "pooling")
summary(estim_winsor1)

estim_discretionary_win1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = discretionary_win2,
      model = "pooling")
summary(estim_discretionary_win1)

estim_essential_win1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = essential_win2,
      model = "pooling")
summary(estim_essential_win1)


#Plotting

#TOTAL
# gamma coefficients
gamma_test_win1 <- estim_winsor1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_win1 <- vcovHC(estim_winsor1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_win1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_win1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_win1[1:13, 1:13])))  # lags
beta_test_win1

beta_test_win1[, estimator := "First difference"]

plot_beta_test_win1 <- ggplot(beta_test_win1, aes(x = week_to_reform, y = coef,
                                                  group = estimator, color = estimator,
                                                  shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) + 
  scale_color_manual(values = c("darkblue")) + 
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )

#ESSENTIAL
# gamma coefficients
gamma_test_esswin1 <- estim_essential_win1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_esswin1 <- vcovHC(estim_essential_win1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_esswin1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_esswin1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_esswin1[1:13, 1:13])))  # lags
beta_test_esswin1

beta_test_esswin1[, estimator := "First difference"]

#plot
plot_beta_test_esswin1 <- ggplot(beta_test_esswin1, aes(x = week_to_reform, y = coef,
                                                  group = estimator, color = estimator,
                                                  shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +
  scale_color_manual(values = c("darkblue")) +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  # Angle x-axis text for better readability
    legend.position = "none"
  )


#DISCRETIONARY
# gamma coefficients
gamma_test_diswin1 <- estim_discretionary_win1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_diswin1 <- vcovHC(estim_discretionary_win1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_diswin1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_diswin1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_diswin1[1:13, 1:13])))  # lags
beta_test_diswin1

beta_test_diswin1[, estimator := "First difference"]

plot_beta_test_diswin1 <- ggplot(beta_test_diswin1, aes(x = week_to_reform, y = coef,
                                                  group = estimator, color = estimator,
                                                  shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )



######################################################################################################################
#Weekly Regression - Excluding outlying categories
################################################################################################################################

#excluding the categories which are outliers, namely Entertainment and Sports as well as Transport Services
normalones1 <- playingaround3 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Accommodation", "Human health services", "Other", "Personal Services", "Retail: Fuel stations", "Retail: Other goods", "Food and beverage services","Professional Services"))
normalones2 <- pdata.frame(normalones1, index = c("merchant_category", "date"))

#discretionary vs non discretionary
discretionary_outlierer1 <- playingaround6 %>% 
  filter(merchant_category %in% c("Accommodation",  "Food and beverage services", "Other", "Personal Services", "Professional Services", "Retail: Other goods"))

essential_outlierer1 <- playingaround6 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Human health services", "Retail: Fuel stations")  )

discretionary_outlierer2 <- pdata.frame(discretionary_outlierer1, index = c("merchant_category", "date"))
essential_outlierer2 <- pdata.frame(essential_outlierer1, index = c("merchant_category", "date"))

#regression now
estim_outlierer1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = normalones2,
      model = "pooling")
summary(estim_outlierer1)

estim_discretionary_outlierer1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = discretionary_outlierer2,
      model = "pooling")
summary(estim_discretionary_outlierer1)

estim_essential_outlierer1 <- 
  plm(diff(yoy_change) ~ policy_shock + lag(policy_shock, k=c(1:12)) + as.factor(yearmonth) - 1,
      data = essential_outlierer2,
      model = "pooling")
summary(estim_essential_outlierer1)


#Plotting

#TOTAL
# gamma coefficients
gamma_test_exclwe1 <- estim_outlierer1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_exclwe1 <- vcovHC(estim_outlierer1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_exclwe1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_exclwe1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_exclwe1[1:13, 1:13])))  # lags
beta_test_exclwe1

beta_test_exclwe1[, estimator := "First difference"]

plot_beta_test_exclwe1 <- ggplot(beta_test_exclwe1, aes(x = week_to_reform, y = coef,
                                                  group = estimator, color = estimator,
                                                  shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )

#ESSENTIAL
# gamma coefficients
gamma_test_exclweess1 <- estim_essential_outlierer1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_exclweess1 <- vcovHC(estim_essential_outlierer1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_exclweess1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_exclweess1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_exclweess1[1:13, 1:13])))  # lags
beta_test_exclweess1

beta_test_exclweess1[, estimator := "First difference"]

#plot
plot_beta_test_exclweess1 <- ggplot(beta_test_exclweess1, aes(x = week_to_reform, y = coef,
                                                        group = estimator, color = estimator,
                                                        shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    legend.position = "none"
  )

#DISCRETIONARY
# gamma coefficients
gamma_test_exclwedis1 <- estim_discretionary_outlierer1$coefficients[1:13]

# store variance-covariance matrix
vcov_test_exclwedis1 <- vcovHC(estim_discretionary_outlierer1, type = "sss", cluster = "group")[1:13,1:13]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_exclwedis1 <- data.table(
  week_to_reform = c(-1:12), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_exclwedis1[1:13])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_exclwedis1[1:13, 1:13])))  # lags
beta_test_exclwedis1

beta_test_exclwedis1[, estimator := "First difference"]

plot_beta_test_exclwedis1 <- ggplot(beta_test_exclwedis1, aes(x = week_to_reform, y = coef,
                                                        group = estimator, color = estimator,
                                                        shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Weeks Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-850, 150), breaks = seq(-850, 150, by = 100)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )




######################################################################################################################
#Monthly Regression - Excluding outlying categories
################################################################################################################################

#excluding the categories which are outliers, namely Entertainment and Sports as well as Transport Services
normalones_month1 <- romonth1 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Accommodation", "Human health services", "Other", "Personal Services", "Retail: Fuel stations", "Retail: Other goods", "Food and beverage services","Professional Services")  )

normalones_month2 <- pdata.frame(normalones_month1, index = c("merchant_category", "date.x"))

#discretionary vs non discretionary
discretionary_outlierer_month1 <- romonth1 %>% 
  filter(merchant_category %in% c("Accommodation",  "Food and beverage services", "Other", "Personal Services", "Professional Services", "Retail: Other goods"))


essential_outlierer_month1 <- romonth1 %>% 
  filter(merchant_category %in% c("Retail: Food, beverage, tobacco", "Human health services","Retail: Fuel stations")  )

discretionary_outlierer_month2 <- pdata.frame(discretionary_outlierer_month1, index = c("merchant_category", "date.x"))
essential_outlierer_month2 <- pdata.frame(essential_outlierer_month1, index = c("merchant_category", "date.x"))

#regression now
estim_outlierer_month1 <- 
  plm(diff(yoy_growth) ~ policy_shock + lag(policy_shock, k=c(1:3)) + as.factor(yearmonth) - 1,
      data = normalones_month2,
      model = "pooling")
summary(estim_outlierer_month1)

estim_discretionary_outlierer_month1 <- 
  plm(diff(yoy_growth) ~ policy_shock + lag(policy_shock, k=c(1:3)) + as.factor(yearmonth) - 1,
      data = discretionary_outlierer_month2,
      model = "pooling")
summary(estim_discretionary_outlierer_month1)

estim_essential_outlierer_month1 <- 
  plm(diff(yoy_growth) ~ policy_shock + lag(policy_shock, k=c(1:3)) + as.factor(yearmonth) - 1,
      data = essential_outlierer_month2,
      model = "pooling")
summary(estim_essential_outlierer_month1)


#TOTAL
# gamma coefficients
gamma_test_monthout1 <- estim_outlierer_month1$coefficients[1:4]

# store variance-covariance matrix
vcov_test_monthout1 <- vcovHC(estim_outlierer_month1, type = "sss", cluster = "group")[1:4,1:4]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_monthout1 <- data.table(
  week_to_reform = c(-1:3), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_monthout1[1:4])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_monthout1[1:4, 1:4])))  # lags
beta_test_monthout1

beta_test_monthout1[, estimator := "First difference"]

plot_beta_test_monthout1 <- ggplot(beta_test_monthout1, aes(x = week_to_reform, y = coef,
                                                      group = estimator, color = estimator,
                                                      shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Months Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-150, 50), breaks = seq(-150, 50, by = 25)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )

#ESSENTIAL
# gamma coefficients
gamma_test_month_outess1 <- estim_essential_outlierer_month1$coefficients[1:4]

# store variance-covariance matrix
vcov_test_month_outess1 <- vcovHC(estim_essential_outlierer_month1, type = "sss", cluster = "group")[1:4,1:4]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_month_outess1 <- data.table(
  week_to_reform = c(-1:3), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_month_outess1[1:4])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_month_outess1[1:4, 1:4])))  # lags
beta_test_month_outess1

beta_test_month_outess1[, estimator := "First difference"]

plot_beta_test_month_outess1 <- ggplot(beta_test_month_outess1, aes(x = week_to_reform, y = coef,
                                                              group = estimator, color = estimator,
                                                              shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Months Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-150, 50), breaks = seq(-150, 50, by = 25)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )

#DISCRETIONARY
# gamma coefficients
gamma_test_month_outdis1 <- estim_discretionary_outlierer_month1$coefficients[1:4]

# store variance-covariance matrix
vcov_test_month_outdis1 <- vcovHC(estim_discretionary_outlierer_month1, type = "sss", cluster = "group")[1:4,1:4]

# gamma
# cumulative sum of gamma coefficients starting at zero in period -1
beta_test_month_outdis1 <- data.table(
  week_to_reform = c(-1:3), 
  coef = c(
    0,                                      # reference period
    cumsum(gamma_test_month_outdis1[1:4])),   # lags
  se = c(
    0,                                            # reference period
    secumsum(vcov_test_month_outdis1[1:4, 1:4])))  # lags
beta_test_month_outdis1

beta_test_month_outdis1[, estimator := "First difference"]

plot_beta_test_month_outdis1 <- ggplot(beta_test_month_outdis1, aes(x = week_to_reform, y = coef,
                                                              group = estimator, color = estimator,
                                                              shape = estimator)) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c(19)) +  
  scale_color_manual(values = c("darkblue")) +  # Use only one color for FD
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 12, by = 1)) +
  xlab(paste0("Months Relative to Monetary Policy Shock")) +
  scale_y_continuous(limits = c(-150, 50), breaks = seq(-150, 50, by = 25)) +
  ylab("Effect on Card Spending [%]") +
  theme_bw()  +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 24),
    axis.title.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20), 
    legend.position = "none"
  )

