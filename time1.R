library(tidyverse)
library(vroom)
library(DataExplorer)
library(patchwork)
library(tidymodels)
library(dplyr)
library(poissonreg)
library(glmnet)
library(ggplot2)
library(gridExtra)
library(embed)
library(discrim)
library(naivebayes)
library(bonsai)
library(lightgbm)
library(themis)
library(forecast)


# Load in Data
data <- vroom("STAT348/time/train.csv") 
testdata <- vroom("STAT348/time/test.csv") 


storeItem <- data %>%
  filter(store==4, item==4)


g1 <- storeItem %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

g2 <- storeItem %>%
  pull(sales) %>%
  ggAcf()
  
g3 <- storeItem %>%
  pull(sales) |> 
  forecast::ggAcf( lag.max=2*365)

storeItem <- data %>%
  filter(store==1, item==4)


g4 <- storeItem %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

g5 <- storeItem %>%
  pull(sales) %>%
  ggAcf()

g6 <- storeItem %>%
  pull(sales) |> 
  forecast::ggAcf( lag.max=2*365)

grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 3, nrow = 2)
