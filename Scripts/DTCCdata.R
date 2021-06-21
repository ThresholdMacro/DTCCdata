## ---------------------------
##
## Script name: DTCCdata
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: 2021-06-09
## Copyright (c) Hedge Analytics Ltd, 2021
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(httr)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(jrvFinance)
library(data.table)
library(diffr)
library(stringr)
library(ggplot2)
library(ggthemes)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

IRTrade<-data.frame(read.csv("Data/DTCC2021_06_18.csv"),stringsAsFactors = FALSE)
ReportDate<-substr("DTCC2021_06_18.csv", 5, 15)
ReportDate<-as.Date(ymd(str_replace(ReportDate, "_", "-")))

IRNewUSD<-
  IRTrade %>% filter (Product.ID == "InterestRate:IRSwap:FixedFloat",
                      Action == "NEW",
                      Transaction.Type == "Trade",
                      Notional.Currency.1 == "USD",
                      Payment.Frequency.Period.1 %in% c("3M", "6M"),
                      Execution.Venue.Type %in% c("ON","OFF"),
                      Cleared == "C", #essential for swaps, but swaptions are not cleared 
                      # "U" uncleared swaps produce some very odd rate levels
                      Block.Trade.Election.Indicator == "N",
                      Other.Payment.Amount == "",
                      as.Date(Effective.Date) >= date(ReportDate)
                      )



IRNewUSD <- IRNewUSD[order(IRNewUSD$Effective.Date, decreasing = FALSE),]
IRNewUSD <- IRNewUSD[, colSums(IRNewUSD != 0, na.rm = TRUE) > 0]

IRNewUSD$Tenor<-round(decimal_date(ymd(IRNewUSD$Expiration.Date)) - decimal_date(ymd(IRNewUSD$Effective.Date)),1)
IRNewUSD$SpotFwd<-round(decimal_date(ymd(IRNewUSD$Effective.Date)) - decimal_date(ymd(ReportDate)),1)

## amalgamate fixed rate column bcz don't care whether it is paid or received. 

df<-data.frame('F1'=IRNewUSD$Fixed.Rate.1,'F2'=IRNewUSD$Fixed.Rate.2)
IRNewUSD$Fixed.Rate <- rowSums(df, na.rm = TRUE)
IRNewUSD$Notional.Amount.1<- as.numeric(str_replace_all(IRNewUSD$Notional.Amount.1, "[^[:alnum:]]", ""))

SpotTrade<-
  IRNewUSD %>% filter (SpotFwd <= 0)

SpotTrade<-SpotTrade[order(SpotTrade$Tenor, decreasing = FALSE),]
TenorByBucket <-
  cut(
    SpotTrade$Tenor,
    breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
    labels = c(
      "0-1",
      "1-3",
      "3-4",
      "4-5",
      "5-7",
      "7-10",
      "10-15",
      "15-20",
      "20-25",
      "25-30",
      "30-40",
      "40-50",
      "50-100"
    ),
    right = FALSE
  )

SpotTradedNotional <-
  data.frame(
    'Tenor.by.year.bucket' = TenorByBucket,
    'Notional' = SpotTrade$Notional.Amount.1,
    'Rate' = SpotTrade$Fixed.Rate
  )

sum(SpotTrade$Notional.Amount.1)
# Barplot
p<-ggplot(SpotTradedNotional, aes(x=Tenor.by.year.bucket, y=Notional))+
geom_bar(stat = "identity")+
  labs(title = paste0("Notional spot US$ swaps by maturity reported as traded on ",ReportDate),
       caption = "source: DTCC, Hedge Analytics") +
  scale_y_continuous(name = "US$", labels = scales::comma)

p + theme_economist_white() +
  scale_colour_economist()


SpotRateByBucket <-
  SpotTradedNotional %>%
  group_by(Tenor.by.year.bucket) %>%
  summarise_all(list(mean = mean, sum = sum))

#SpotRate15Jun2021<-SpotRateByBucket

p<-ggplot(SpotRateByBucket, aes(x=Tenor.by.year.bucket, y=Rate, group = 1))+
  geom_point()+
  geom_line(stat = "identity")+
  labs(title = paste0("Average traded spot rates for US$ swaps by maturity reported as traded on ",ReportDate),
       caption = "source: DTCC, Hedge Analytics") +
  scale_y_continuous(name = "Percent", labels = scales::percent_format())
p + theme_economist_white() +
  scale_colour_economist()

###################### - other stuff to look at ########################

SwaptionUSD<-
  IRTrade %>% filter (Product.ID == "InterestRate:Option:Swaption",
                      Action %in% c("NEW", "CORRECT"),
                      Transaction.Type == "Trade",
                      Notional.Currency.1 == "USD",
                      as.Date(Effective.Date) >= date(ReportDate)
  )
SwaptionUSD <- SwaptionUSD[, colSums(SwaptionUSD != 0, na.rm = TRUE) > 0]

IROISUSD<-
  IRTrade %>% filter (Product.ID == "InterestRate:IRSwap:OIS",
                      Action %in% c("NEW", "CORRECT"),
                      Transaction.Type == "Trade",
                      Notional.Currency.1 == "USD",
                      Block.Trade.Election.Indicator == "N",
                      Other.Payment.Amount == "",
                      as.Date(Effective.Date) >= date(ReportDate)
  )

IROISUSD <- IROISUSD[order(IROISUSD$Effective.Date, decreasing = FALSE),]
IROISUSD <- IROISUSD[, colSums(IROISUSD != 0, na.rm = TRUE) > 0]
IROISUSD$Tenor<-round(decimal_date(ymd(IROISUSD$Expiration.Date)) - decimal_date(ymd(IROISUSD$Effective.Date)),1)
IROISUSD$SpotFwd<-round(decimal_date(ymd(IROISUSD$Effective.Date)) - decimal_date(ymd(ReportDate)),1)
df<-data.frame('F1'=IROISUSD$Fixed.Rate.1,'F2'=IROISUSD$Fixed.Rate.2)
IROISUSD$Fixed.Rate <- rowSums(df, na.rm = TRUE)
IROISUSD$Notional.Amount.1<- as.numeric(str_replace_all(IROISUSD$Notional.Amount.1, "[^[:alnum:]]", ""))
IROISUSD$Notional.Amount.2<- as.numeric(str_replace_all(IROISUSD$Notional.Amount.2, "[^[:alnum:]]", ""))

IROISUSDSpotTrade<-
  IROISUSD %>% filter (SpotFwd <= 0)

IROISUSDTenorByBucket <-
  cut(
    IROISUSDSpotTrade$Tenor,
    breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
    labels = c(
      "0-1",
      "1-3",
      "3-4",
      "4-5",
      "5-7",
      "7-10",
      "10-15",
      "15-20",
      "20-25",
      "25-30",
      "30-40",
      "40-50",
      "50-100"
    ),
    right = FALSE
  )

IROISUSDTradedNotional <-
  data.frame(
    'Tenor.by.year.bucket' = IROISUSDTenorByBucket,
    'Notional' = IROISUSDSpotTrade$Notional.Amount.2,
    'Spread' = IROISUSDSpotTrade$Fixed.Rate
  )


p<-ggplot(IROISUSDTradedNotional, aes(x=Tenor.by.year.bucket, y=Notional))+
  geom_bar(stat = "identity")+
  labs(title = paste0("Notional spot USD OIS swaps  by maturity reported as traded on ",ReportDate),
       caption = "source: DTCC, Hedge Analytics") +
  scale_y_continuous(name = "US$", labels = scales::comma)

p + theme_economist_white() +
  scale_colour_economist()

OISSpotRateByBucket <-
  IROISUSDTradedNotional %>%
  group_by(Tenor.by.year.bucket) %>%
  summarise_all(list(mean = mean, sum = sum))

CCBasis<-
  IRTrade %>% filter (Product.ID == "InterestRate:CrossCurrency:Basis",
                      Action %in% c("NEW", "CORRECT"),
                      Transaction.Type == "Trade",
                      Other.Payment.Amount == "",
                      Leg.1...Floating.Rate.Index == "JPY-LIBOR-BBA",
                      as.Date(Effective.Date) >= date(ReportDate)
  )

CCBasis <- CCBasis[order(CCBasis$Effective.Date, decreasing = FALSE),]
CCBasis <- CCBasis[, colSums(CCBasis != 0, na.rm = TRUE) > 0]
CCBasis$Tenor<-round(decimal_date(ymd(CCBasis$Expiration.Date)) - decimal_date(ymd(CCBasis$Effective.Date)),1)
CCBasis$SpotFwd<-round(decimal_date(ymd(CCBasis$Effective.Date)) - decimal_date(ymd(ReportDate)),1)
CCBasis$Notional.Amount.1<- as.numeric(str_replace_all(CCBasis$Notional.Amount.1, "[^[:alnum:]]", ""))
CCBasis$Notional.Amount.2<- as.numeric(str_replace_all(CCBasis$Notional.Amount.2, "[^[:alnum:]]", ""))

CCBasisSpotTrade<-
  CCBasis %>% filter (SpotFwd <= 0)

CCBasisTenorByBucket <-
  cut(
    CCBasisSpotTrade$Tenor,
    breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
    labels = c(
      "0-1",
      "1-3",
      "3-4",
      "4-5",
      "5-7",
      "7-10",
      "10-15",
      "15-20",
      "20-25",
      "25-30",
      "30-40",
      "40-50",
      "50-100"
    ),
    right = FALSE
  )

CCBasisTradedNotional <-
  data.frame(
    'Tenor.by.year.bucket' = CCBasisTenorByBucket,
    'Notional' = CCBasisSpotTrade$Notional.Amount.2,
    'Spread' = CCBasisSpotTrade$Spread.1
  )


p<-ggplot(CCBasisTradedNotional, aes(x=Tenor.by.year.bucket, y=Notional))+
  geom_bar(stat = "identity")+
  labs(title = paste0("Notional spot CC Basis by maturity reported as traded on ",ReportDate),
       caption = "source: DTCC, Hedge Analytics") +
  scale_y_continuous(name = "US$", labels = scales::comma)

p + theme_economist_white() +
  scale_colour_economist()

InflSwap<-
  IRTrade %>% filter (Product.ID == "InterestRate:IRSwap:Inflation",
                      Action %in% c("NEW", "CORRECT"),
                      Transaction.Type == "Trade",
                      Notional.Currency.1 == "USD",
                      as.Date(Effective.Date) >= date(ReportDate)
  )

InflSwap <- InflSwap[order(InflSwap$Effective.Date, decreasing = FALSE),]
InflSwap <- InflSwap[, colSums(InflSwap != 0, na.rm = TRUE) > 0]
InflSwap$Tenor<-round(decimal_date(ymd(InflSwap$Expiration.Date)) - decimal_date(ymd(InflSwap$Effective.Date)),1)
InflSwap$SpotFwd<-round(decimal_date(ymd(InflSwap$Effective.Date)) - decimal_date(ymd(ReportDate)),1)
df<-data.frame('F1'=InflSwap$Fixed.Rate.1,'F2'=InflSwap$Fixed.Rate.2)
InflSwap$Fixed.Rate <- rowSums(df, na.rm = TRUE)
InflSwap <- InflSwap[which(InflSwap$Fixed.Rate < 1),] #clumsy attempt to exclude error entries in DTCC
InflSwap$Notional.Amount.1<- as.numeric(str_replace_all(InflSwap$Notional.Amount.1, "[^[:alnum:]]", ""))
InflSwap$Notional.Amount.2<- as.numeric(str_replace_all(InflSwap$Notional.Amount.2, "[^[:alnum:]]", ""))

InfSpotTrade<-
  InflSwap %>% filter (SpotFwd <= 0)

InfSpotTrade<-InfSpotTrade[order(InfSpotTrade$Tenor, decreasing = FALSE),]
InfTenorByBucket <-
  cut(
    InfSpotTrade$Tenor,
    breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
    labels = c(
      "0-1",
      "1-3",
      "3-4",
      "4-5",
      "5-7",
      "7-10",
      "10-15",
      "15-20",
      "20-25",
      "25-30",
      "30-40",
      "40-50",
      "50-100"
    ),
    right = FALSE
  )

InfSpotTradedNotional <-
  data.frame(
    'Tenor.by.year.bucket' = InfTenorByBucket,
    'Notional' = InfSpotTrade$Notional.Amount.1,
    'Rate' = InfSpotTrade$Fixed.Rate
  )

sum(InfSpotTrade$Notional.Amount.1)

p<-ggplot(InfSpotTradedNotional, aes(x=Tenor.by.year.bucket, y=Notional))+
  geom_bar(stat = "identity")+
  labs(title = paste0("Notional spot US$ inflation swaps by maturity reported as traded on ",ReportDate),
       caption = "source: DTCC, Hedge Analytics") +
  scale_y_continuous(name = "US$", labels = scales::comma)

p + theme_economist_white() +
  scale_colour_economist()

InfSpotRateByBucket <-
  InfSpotTradedNotional %>%
  group_by(Tenor.by.year.bucket) %>%
  summarise_all(mean)
