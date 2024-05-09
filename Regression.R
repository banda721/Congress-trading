library(ggplot2)
library(dbplyr)
library(tidyverse)
library(readxl)
library(MASS)
library(AER)
library(modEvA)
library(performance)
library(stargazer)
library(survival)

setwd("C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project")
## Transaction data
all_transactions<-read.csv('C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project\\all_transactions.csv')
unique(all_transactions$senator)

## Subset all the Stock asset type disclosures
transactions<-all_transactions[all_transactions$asset_type %in% "Stock", ]
summary(transactions$senator)

## SP 500 data
sp<-read.csv('C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project\\constituents1.csv')
names(sp)
## Change sectors to the committee names
sp <- sp %>%
  mutate(Sector = recode(GICS.Sector, "Information Technology" = "Commerce, Science, and Transportation",
                         "Financials"="Finance", "Communication" = "Commerce, Science, and Transportation", 
                         "Consumer Discretionary" = 'Commerce, Science, and Transportation',
                         "Health Care"="Health, Education, Labor, and Pensions",
                         'Consumer Staples'="Small Business and Entrepreneurship", 
                         'Real Estate'="Banking, Housing, and Urban Affairs", 
                         'Materials' = "Energy and Natural Resources", 
                         "Energy" = "Energy and Natural Resources", 
                         "Industrials" = "Energy and Natural Resources", 
                         "Utilities" = "Energy and Natural Resources" ))
names(sp)

## Senate committees
senatecomm<-read_excel("C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project\\Senate_assignments_103-117.xlsx", skip = 1)


## 113-117th session of congress
senac<-senatecomm[senatecomm$Congress >= "116", ]
names(senac)[4] <- "senator"


## Rename To ticker
names(sp)[1] <- "ticker"

# First merger of data adding together SP500 data with all transaction data
combo<-merge(transactions,sp,by='ticker')
unique(combo$senator)

combo <- subset(combo, senator != "Joe Manchin, Iii")


## Changing to last name, first name
x <- combo$senator
y <- as.person(x)
s<-format(y, include=c("family","given"), braces=list(family=c("",",")))

## Change sp column to correct name formatting
combo$senator<-s

#######################################################################################################
## Merge combination of sp and senate transaction with senate committee's
senators<- merge(combo,senac, by="senator")
names(senators)

## Remove unnecessary columns
senators_final<- senators#[ -c(4,5,9,14,21,22,27,31,32,39,40,41,42,43) ]

# Rename the 'Committee Name' column to 'Committee_Name'
colnames(senators_final)[colnames(senators_final) == 'Committee Name'] <- 'Committee_Name'

# Add a new variable based on the condition
senators_final$count <- ifelse(senators_final$Committee_Name == senators_final$sector, "1", "0")
senators_final$count=as.numeric(senators_final$count)
sum(senators_final$count)


#write.csv(senators_final, file = "senators_final13.csv", row.names = FALSE)

################################################################################################################################
all_trades<-read.csv('C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project\\All_Trades.csv')
names(all_trades)
summary(all_trades)
str(all_trades)
stargazer(all_trades,type="text")
names(All_Trades_surv)

str(all_trades$transaction_date)
all_trades$transaction_date <- as.Date(all_trades$transaction_date, format = "%m/%d/%Y")
str(all_trades)

Poisson=glm(count~party+Committee.Seniority+Committee_Rank+Amount+Committee.Period.of.Service, family="poisson",data=All_Trades_surv)
summary(Poisson)
stargazer(Poisson,type="text")

Poisson=glm(count~party+Committee.Seniority+Committee_Rank+Amount+Committee.Period.of.Service, family="poisson",data=All_Trades_surv)
summary(Poisson)
stargazer(Poisson,type="text")

Poisson1=glm(count~party+Committee.Seniority+Committee_Rank+Amount+Committee.Period.of.Service+Rank.Within.Party, family="poisson",data=All_Trades_surv)
summary(Poisson1)
stargazer(Poisson1,type="text")

########################################Survival##########################################################################################

All_Trades_surv<-read.csv('C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project\\All_Trades_surv.csv')
All_Trades_surv$transaction_date <- as.Date(All_Trades_surv$transaction_date, format = "%m/%d/%Y")

trades_summary <- All_Trades_surv %>%
  group_by(year = lubridate::year(transaction_date)) %>%
  summarise(sum_trades = sum(Amount), count_trades = n(), sum_suspicious_trades = sum(count))

trades_summary

surv<-read.csv('C:\\Users\\gihan\\OneDrive\\Gihan\\BGSU_MAFE\\Books\\Econometrics\\Project\\Survival data.csv')
surv$Time=as.numeric(surv$Time)
surv$count=as.numeric(surv$count)


summary(surv$Time)
summary(surv$count)
stargazer(surv,type="text")

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(surv$Time,surv$count) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis
kmsurvival2 <- survfit(Surv(surv$Time,surv$count) ~ surv$Sus.Trade_return)
summary(kmsurvival2)
plot(kmsurvival2, xlab="Time", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(surv$Time,surv$count) ~ surv$party+surv$Committee.Rank+log(surv$Amount)
               , method="breslow")
summary(coxph)

coxph2 <- coxph(Surv(surv$Time,surv$count) ~ surv$party+surv$Committee.Rank+log(surv$Amount)
               +surv$Rank.Within.Party, method="breslow")
summary(coxph2)

coxph3 <- coxph(Surv(surv$Time,surv$count) ~ surv$party+surv$Committee.Rank+log(surv$Amount)
                +surv$Rank.Within.Party+surv$Committee.Period.of.Service, method="breslow")
summary(coxph3)

coxph4 <- coxph(Surv(surv$Time,surv$count) ~ surv$party+surv$Committee.Rank+log(surv$Amount)
                +surv$Rank.Within.Party+surv$Committee.Period.of.Service+surv$Committee.Seniority, method="breslow")
summary(coxph4)

