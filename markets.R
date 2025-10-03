## Class work
library(readr)
library(tidyverse)
GROV_Data <- read_csv("Documents/MASTERS/METRICS/session/data/GROV Data.csv")
View(GROV_Data)
bep <- read_csv("~/Documents/MASTERS/METRICS/session/data/BEP Data.csv")
bird <- read_csv("~/Documents/MASTERS/METRICS/session/data/BIRD Data.csv")


bep_clean <- bep %>% 
  rename(
    date=Date,
    open=Open,
    hi=High,
    lo=Low,
    clo=Close,
    adjclo=`Adj Close`,
    vol=Volume
  ) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  separate("date",c("year","month","day"), sep="-", remove = FALSE)

grov_clean <- GROV_Data %>% 
  rename(
    date=Date,
    open=Open,
    hi=High,
    lo=Low,
    clo=Close,
    adjclo=`Adj Close`,
    vol=Volume
  ) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  separate("date", c("year","month","day"), sep="-", remove = FALSE)

bird_clean <- bird %>% 
  rename(
    date=Date,
    open=Open,
    hi=High,
    lo=Low,
    clo=Close,
    adjclo=`Adj Close`,
    vol=Volume
  ) %>% 
  mutate(
    date = as.Date(date, format="%m/%d/%Y")
  ) %>% 
  separate("date", c("year","month","day"), sep="-", remove = FALSE)

