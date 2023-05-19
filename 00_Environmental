library(tidyverse)

## environmental data processing ##
## original data downloads from NOAA & Yeager et al 2017 ##
temp.baamax <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,4:15) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="baa_max_", values_to = "baa_max") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                          "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, baa_max)
temp.dhwmax <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,16:27) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="dhw_max_", values_to = "dhw_max") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, dhw_max)
temp.sstmax <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,28:39) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="sst_max_", values_to = "sst_max") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, sst_max)
temp.sstmean <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,40:51) %>% 
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="sst_mean_", values_to = "sst_mean") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, sst_mean)
temp.sstmin <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,52:63) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="sst_min_", values_to = "sst_min") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, sst_min)
temp.sstamax <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,64:75) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="ssta_max_", values_to = "ssta_max") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, ssta_max)
temp.sstamean <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,76:87) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="ssta_mean_", values_to = "ssta_mean") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, ssta_mean)
temp.sstamin <- read.csv("data/temp.csv", header=T) %>% select(., 1:3,88:99) %>%
  pivot_longer(., cols=4:15, names_to = "mon", names_prefix="ssta_min_", values_to = "ssta_min") %>% 
  mutate(., Month=recode(mon, "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                       "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)) %>%
  select(., Longitude = X, Latitude = Y, Year, Month, ssta_min)

tempenv <- inner_join(temp.baamax, temp.dhwmax) %>% 
  inner_join(., temp.sstmax) %>%
  inner_join(., temp.sstmean) %>%
  inner_join(., temp.sstmin) %>%
  inner_join(., temp.sstamax) %>%
  inner_join(., temp.sstamean) %>%
  inner_join(., temp.sstamin)

env.tempannual <- read.csv("data/envtempannual.csv", header=T) %>% 
  select(., -Site) %>%
  rename(., Site = Merged.Site)

env.tempannual1 <- env.tempannual %>%
  select(., c(1,2,3:7,8,31:33)) %>%
  left_join(tempenv)
env.tempannual2 <- env.tempannual %>%
  select(., c(1,10,3:7,9,31:33)) %>% 
  rename(., Month = Month1, Year = Year1) %>%
  left_join(tempenv)
env.tempannual3 <- env.tempannual %>%
  select(., c(1,12,3:7,11,31:33)) %>% 
  rename(., Month = Month2, Year = Year2) %>%
  left_join(tempenv)
env.tempannual4 <- env.tempannual %>%
  select(., c(1,14,3:7,13,31:33)) %>% 
  rename(., Month = Month3, Year = Year3) %>%
  left_join(tempenv)
env.tempannual5 <- env.tempannual %>%
  select(., c(1,16,3:7,15,31:33)) %>% 
  rename(., Month = Month4, Year = Year4) %>%
  left_join(tempenv)
env.tempannual6 <- env.tempannual %>%
  select(., c(1,18,3:7,17,31:33)) %>% 
  rename(., Month = Month5, Year = Year5) %>%
  left_join(tempenv)
env.tempannual7 <- env.tempannual %>%
  select(., c(1,20,3:7,19,31:33)) %>% 
  rename(., Month = Month6, Year = Year6) %>%
  left_join(tempenv)
env.tempannual8 <- env.tempannual %>%
  select(., c(1,22,3:7,21,31:33)) %>% 
  rename(., Month = Month7, Year = Year7) %>%
  left_join(tempenv)
env.tempannual9 <- env.tempannual %>%
  select(., c(1,24,3:7,23,31:33)) %>% 
  rename(., Month = Month8, Year = Year8) %>%
  left_join(tempenv)
env.tempannual10 <- env.tempannual %>%
  select(., c(1,26,3:7,25,31:33)) %>% 
  rename(., Month = Month9, Year = Year9) %>%
  left_join(tempenv)
env.tempannual11 <- env.tempannual %>%
  select(., c(1,28,3:7,27,31:33)) %>% 
  rename(., Month = Month10, Year = Year10) %>%
  left_join(tempenv)
env.tempannual12 <- env.tempannual %>%
  select(., c(1,30,3:7,29,31:33)) %>% 
  rename(., Month = Month11, Year = Year11) %>%
  left_join(tempenv)

env.tempannualbaa <- env.tempannual1 %>% 
  select(., 1:12) %>% 
  cbind(., env.tempannual2$baa_max) %>%
  cbind(., env.tempannual3$baa_max) %>%
  cbind(., env.tempannual4$baa_max) %>%
  cbind(., env.tempannual5$baa_max) %>%
  cbind(., env.tempannual6$baa_max) %>%
  cbind(., env.tempannual7$baa_max) %>%
  cbind(., env.tempannual8$baa_max) %>%
  cbind(., env.tempannual9$baa_max) %>%
  cbind(., env.tempannual10$baa_max) %>%
  cbind(., env.tempannual11$baa_max) %>%
  cbind(., env.tempannual12$baa_max)

colnames(env.tempannualbaa)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualbaa <- env.tempannualbaa %>% 
  pivot_longer(., 12:23, names_to = "month_baa", values_to = "baa_max") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., sum_baa_max = sum(baa_max), max_baa_max = max(baa_max))

env.tempannualdhw <- env.tempannual1 %>% 
  select(., 1:11,13) %>% 
  cbind(., env.tempannual2$dhw_max) %>%
  cbind(., env.tempannual3$dhw_max) %>%
  cbind(., env.tempannual4$dhw_max) %>%
  cbind(., env.tempannual5$dhw_max) %>%
  cbind(., env.tempannual6$dhw_max) %>%
  cbind(., env.tempannual7$dhw_max) %>%
  cbind(., env.tempannual8$dhw_max) %>%
  cbind(., env.tempannual9$dhw_max) %>%
  cbind(., env.tempannual10$dhw_max) %>%
  cbind(., env.tempannual11$dhw_max) %>%
  cbind(., env.tempannual12$dhw_max)

colnames(env.tempannualdhw)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualdhw <- env.tempannualdhw %>% 
  pivot_longer(., 12:23, names_to = "month_dhw", values_to = "dhw_max") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., sum_dhw_max = sum(dhw_max), max_dhw_max = max(dhw_max))

env.tempannualsst_max <- env.tempannual1 %>% 
  select(., 1:11,14) %>% 
  cbind(., env.tempannual2$sst_max) %>%
  cbind(., env.tempannual3$sst_max) %>%
  cbind(., env.tempannual4$sst_max) %>%
  cbind(., env.tempannual5$sst_max) %>%
  cbind(., env.tempannual6$sst_max) %>%
  cbind(., env.tempannual7$sst_max) %>%
  cbind(., env.tempannual8$sst_max) %>%
  cbind(., env.tempannual9$sst_max) %>%
  cbind(., env.tempannual10$sst_max) %>%
  cbind(., env.tempannual11$sst_max) %>%
  cbind(., env.tempannual12$sst_max)

colnames(env.tempannualsst_max)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualsstmax <- env.tempannualsst_max %>% 
  pivot_longer(., 12:23, names_to = "month_sst", values_to = "sst_max") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., max_sst_max = max(sst_max), mean_sst_max = mean(sst_max), sd_sst_max = sd(sst_max))

env.tempannualsst_mean <- env.tempannual1 %>% 
  select(., 1:11,15) %>% 
  cbind(., env.tempannual2$sst_mean) %>%
  cbind(., env.tempannual3$sst_mean) %>%
  cbind(., env.tempannual4$sst_mean) %>%
  cbind(., env.tempannual5$sst_mean) %>%
  cbind(., env.tempannual6$sst_mean) %>%
  cbind(., env.tempannual7$sst_mean) %>%
  cbind(., env.tempannual8$sst_mean) %>%
  cbind(., env.tempannual9$sst_mean) %>%
  cbind(., env.tempannual10$sst_mean) %>%
  cbind(., env.tempannual11$sst_mean) %>%
  cbind(., env.tempannual12$sst_mean)

colnames(env.tempannualsst_mean)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualsstmean <- env.tempannualsst_mean %>% 
  pivot_longer(., 12:23, names_to = "month_sst", values_to = "sst_mean") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., mean_sst_mean = mean(sst_mean), sd_sst_mean = sd(sst_mean))

env.tempannualsst_min <- env.tempannual1 %>% 
  select(., 1:11,16) %>% 
  cbind(., env.tempannual2$sst_min) %>%
  cbind(., env.tempannual3$sst_min) %>%
  cbind(., env.tempannual4$sst_min) %>%
  cbind(., env.tempannual5$sst_min) %>%
  cbind(., env.tempannual6$sst_min) %>%
  cbind(., env.tempannual7$sst_min) %>%
  cbind(., env.tempannual8$sst_min) %>%
  cbind(., env.tempannual9$sst_min) %>%
  cbind(., env.tempannual10$sst_min) %>%
  cbind(., env.tempannual11$sst_min) %>%
  cbind(., env.tempannual12$sst_min)

colnames(env.tempannualsst_min)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualsstmin <- env.tempannualsst_min %>% 
  pivot_longer(., 12:23, names_to = "month_sst", values_to = "sst_min") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., min_sst_min = min(sst_min), mean_sst_min = mean(sst_min), sd_sst_min = sd(sst_min))

env.tempannualssta_max <- env.tempannual1 %>% 
  select(., 1:11, 17) %>% 
  cbind(., env.tempannual2$ssta_max) %>%
  cbind(., env.tempannual3$ssta_max) %>%
  cbind(., env.tempannual4$ssta_max) %>%
  cbind(., env.tempannual5$ssta_max) %>%
  cbind(., env.tempannual6$ssta_max) %>%
  cbind(., env.tempannual7$ssta_max) %>%
  cbind(., env.tempannual8$ssta_max) %>%
  cbind(., env.tempannual9$ssta_max) %>%
  cbind(., env.tempannual10$ssta_max) %>%
  cbind(., env.tempannual11$ssta_max) %>%
  cbind(., env.tempannual12$ssta_max)

colnames(env.tempannualssta_max)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualsstamax <- env.tempannualssta_max %>% 
  pivot_longer(., 12:23, names_to = "month_ssta", values_to = "ssta_max") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., sum_ssta_max = sum(ssta_max), max_ssta_max = max(ssta_max), mean_ssta_max = mean(ssta_max), sd_ssta_max = sd(ssta_max))

env.tempannualssta_mean <- env.tempannual1 %>% 
  select(., 1:11,18) %>% 
  cbind(., env.tempannual2$ssta_mean) %>%
  cbind(., env.tempannual3$ssta_mean) %>%
  cbind(., env.tempannual4$ssta_mean) %>%
  cbind(., env.tempannual5$ssta_mean) %>%
  cbind(., env.tempannual6$ssta_mean) %>%
  cbind(., env.tempannual7$ssta_mean) %>%
  cbind(., env.tempannual8$ssta_mean) %>%
  cbind(., env.tempannual9$ssta_mean) %>%
  cbind(., env.tempannual10$ssta_mean) %>%
  cbind(., env.tempannual11$ssta_mean) %>%
  cbind(., env.tempannual12$ssta_mean)

colnames(env.tempannualssta_mean)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualsstamean <- env.tempannualssta_mean %>% 
  pivot_longer(., 12:23, names_to = "month_ssta", values_to = "ssta_mean") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., sum_ssta_mean = sum(ssta_mean), mean_ssta_mean = mean(ssta_mean), sd_ssta_mean = sd(ssta_mean))

env.tempannualssta_min <- env.tempannual1 %>% 
  select(., 1:11,19) %>% 
  cbind(., env.tempannual2$ssta_min) %>%
  cbind(., env.tempannual3$ssta_min) %>%
  cbind(., env.tempannual4$ssta_min) %>%
  cbind(., env.tempannual5$ssta_min) %>%
  cbind(., env.tempannual6$ssta_min) %>%
  cbind(., env.tempannual7$ssta_min) %>%
  cbind(., env.tempannual8$ssta_min) %>%
  cbind(., env.tempannual9$ssta_min) %>%
  cbind(., env.tempannual10$ssta_min) %>%
  cbind(., env.tempannual11$ssta_min) %>%
  cbind(., env.tempannual12$ssta_min)

colnames(env.tempannualssta_min)[12:23] <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

env.annualsstamin <- env.tempannualssta_min %>% 
  pivot_longer(., 12:23, names_to = "month_ssta", values_to = "ssta_min") %>% 
  group_by(., Code, Year, Site, Month, Replicate, Latitude, Longitude) %>%
  summarise(., sum_ssta_min = sum(ssta_min), min_ssta_min = min(ssta_min), mean_ssta_min = mean(ssta_min), sd_ssta_min = sd(ssta_min))

rm(env.tempannual1);rm(env.tempannual2);rm(env.tempannual3);rm(env.tempannual4);rm(env.tempannual5);rm(env.tempannual6);rm(env.tempannual7);rm(env.tempannual8);rm(env.tempannual9);rm(env.tempannual10);rm(env.tempannual11);rm(env.tempannual12)
rm(env.tempannualbaa);rm(env.tempannualdhw);rm(env.tempannualsst_max);rm(env.tempannualsst_mean);rm(env.tempannualsst_min);rm(env.tempannualssta_max);rm(env.tempannualssta_mean);rm(env.tempannualssta_min)

annualtempenv <- inner_join(env.annualbaa, env.annualdhw) %>% 
  inner_join(., env.annualsstmax) %>%
  inner_join(., env.annualsstmean) %>%
  inner_join(., env.annualsstmin) %>%
  inner_join(., env.annualsstamax) %>%
  inner_join(., env.annualsstamean) %>%
  inner_join(., env.annualsstamin) %>%
  select(., 2,4,6:30)

envdata <- read.csv("data/envdata.csv", header=T) %>%
  left_join(., tempenv) %>%
  left_join(., annualtempenv)
dataset <- merge(coraldata, envdata)
