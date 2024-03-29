library(tidyverse)
library(brms)
library(performance)
library(nlme)
library(MuMIn)

## dataset processing ##
## read datasets, select -columns with environmental data ##
dataset <- read.csv("data/dataset.csv", header=T) %>%
  select(., -c(6,24:40))

## dataset with at least 3 time points ##
datasetrep <- read.csv("data/dataset.csv", header=T) %>%
  filter(., REMOVED !="Y") %>%
  select(., -c(6,24:40))
  
## beta scaling for proportion data based off Smithson and Verkuilen (2006) ##
dataset$CoralMin <- ((dataset$Hardcoral * (nrow(dataset)-1)) + (1/2)) / (nrow(dataset))
dataset$AlgaeMin <- ((dataset$Macroalgae * (nrow(dataset)-1)) + (1/2)) / (nrow(dataset))

datasetrep$CoralMin <- ((datasetrep$Hardcoral * (nrow(datasetrep)-1)) + (1/2)) / (nrow(datasetrep))
datasetrep$AlgaeMin <- ((datasetrep$Macroalgae * (nrow(datasetrep)-1)) + (1/2)) / (nrow(datasetrep))

## priors ##
wiprior <- c(set_prior("normal(0,0.25)", class="b"),
            set_prior("cauchy(0,5)", class="sd", group="Location"),
            set_prior("cauchy(0,5)", class="sd", group="Location:Site"),
            set_prior("normal(0,0.25)", class="sds", coef="s(Year, by = Country)"),
            set_prior("normal(0,0.25)", class="sds", coef="s(Depth)"))
           
## main models ##
brm.wip.bycountry <- brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), prior=wiprior, data=dataset, family=Beta, cores=2, iter=10000, file="brm.wip.bycountry")
brma.wip.bycountry <- brm(AlgaeMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), prior=wiprior, data=dataset, family=Beta, cores=2, iter=10000, file="brma.wip.bycountry")
bayes_R2(brm.wip.bycountry)
bayes_R2(brma.wip.bycountry)

## models compared ##
brm.method <- brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Method) + (1|Location/Site), data=dataset, family=Beta, cores=2)
brm.location <- brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location), data=dataset, family=Beta, cores=2)
brm.site <- brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Site), data=dataset, family=Beta, cores=2)
brm.bycountry.rs1 <- brm(CoralMin ~ s(Year) + s(Depth) + Country + (Year|Site), data=dataset, family=Beta, cores=2)
brm.bycountry.rs2 <- brm(CoralMin ~ s(Year) + s(Depth) + Country + (Depth|Site), data=dataset, family=Beta, cores=2)

brm.linear <- brm(CoralMin ~ Year + Depth + Country + (1|Location/Site), data=dataset, family=Beta, cores=2, file="brm.linear")
brm.t2 <- brm(CoralMin ~ t2(Year, Depth, by=Country) + (1| Location/Site), data=dataset, family=Beta, cores=2, file="brm.t2")
brm.country <- brm(CoralMin ~ s(Year) + s(Depth) + Country + (1|Location/Site), data=dataset, family=Beta, cores=2, file="brm.country")
brm.bycountry <- brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), data=dataset, family=Beta, cores=2, file="brm.bycountry")
compare_performance(brm.linear, brm.t2, brm.country, brm.bycountry)

## models with countries removed ##
dataset.nojp <- dataset %>% filter(., Country !="Japan")
dataset.nomy <- dataset %>% filter(., Country !="Myanmar")

brm.nojp <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), data=dataset.nojp, family=Beta, cores=2, file="brm.nojpbc")
brm.nomy <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), data=dataset.nomy, family=Beta, cores=2, file="brm.nomybc")

rm(dataset.npjp);rm(dataset.nomy)

## models for individual time periods ##
dataset.time1 <- dataset %>% filter(., Year<1998)
dataset.time2 <- dataset %>% filter(., Year>1997 & Year<2010)
dataset.time3 <- dataset %>% filter(., Year>2009)

brm.time1 <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), data=dataset.time1, family=Beta, cores=2, file="brm.time1")
brm.time2 <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), data=dataset.time2, family=Beta, cores=2, file="brm.time2")
brm.time3 <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location/Site), data=dataset.time3, family=Beta, cores=2, file="brm.time3")

rm(dataset.time1);rm(dataset.time2);rm(dataset.time3)

## creating country data, methods data for reports, analyses ##
dataset.brunei <- dataset %>% filter(., Country=="Brunei")
dataset.cambodia <- dataset %>% filter(., Country=="Cambodia")
dataset.hongkong <- dataset %>% filter(., Country=="Hong Kong")
dataset.indonesia <- dataset %>% filter(., Country=="Indonesia")
dataset.japan <- dataset %>% filter(., Country=="Japan")
dataset.malaysia <- dataset %>% filter(., Country=="Malaysia")
dataset.wmalaysia <- dataset.malaysia %>% filter(., Longitude<107)
dataset.emalaysia <- dataset.malaysia %>% filter(., Longitude>107)
dataset.myanmar <- dataset %>% filter(., Country=="Myanmar")
dataset.philippines <- dataset %>% filter(., Country=="Philippines")
dataset.singapore <- dataset %>% filter(., Country=="Singapore")
dataset.southkorea <- dataset %>% filter(., Country=="South Korea")
dataset.taiwan <- dataset %>% filter(., Country=="Taiwan")
dataset.thailand <- dataset %>% filter(., Country=="Thailand")
dataset.vietnam <- dataset %>% filter(., Country=="Vietnam")

datasetLT.brunei <- datasetrep %>% filter(., Country=="Brunei")
datasetLT.cambodia <- datasetrep %>% filter(., Country=="Cambodia")
datasetLT.hongkong <- datasetrep %>% filter(., Country=="Hong Kong")
datasetLT.indonesia <- datasetrep %>% filter(., Country=="Indonesia")
datasetLT.japan <- datasetrep %>% filter(., Country=="Japan")
datasetLT.malaysia <- datasetrep %>% filter(., Country=="Malaysia")
datasetLT.myanmar <- datasetrep %>% filter(., Country=="Myanmar")
datasetLT.philippines <- datasetrep %>% filter(., Country=="Philippines")
datasetLT.singapore <- datasetrep %>% filter(., Country=="Singapore")
datasetLT.southkorea <- datasetrep %>% filter(., Country=="South Korea")
datasetLT.taiwan <- datasetrep %>% filter(., Country=="Taiwan")
datasetLT.thailand <- datasetrep %>% filter(., Country=="Thailand")
datasetLT.vietnam <- datasetrep %>% filter(., Country=="Vietnam")

## individual country models for report ##
brm.br <- brm(CoralMin ~ s(Year) + s(Depth) + (1| Location) + (1| Site), data=dataset.brunei, family=Beta, cores=2)
brm.ca <- brm(CoralMin ~ Year + Depth + (1| Location) + (1| Site), data=dataset.cambodia, family=Beta, cores=2)
brm.hk <- brm(CoralMin ~ Year + Depth + (1| Location) + (1| Site), data=dataset.hongkong, family=Beta, cores=2)
brm.in <- brm(CoralMin ~ s(Year) + s(Depth) + (1| Location) + (1| Site), data=dataset.indonesia, family=Beta, cores=2)
brm.in2 <- brm(CoralMin ~ s(Year) + Depth + (1| Location) + (1| Site), data=dataset.indonesia, family=Beta, cores=2)
brm.jp <- brm(CoralMin ~ t2(Year,Depth, by=Site) + (1| Location), data=dataset.japan, family=Beta, cores=2)
brm.jp2 <- brm(CoralMin ~ s(Year) + s(Depth) + (1| Location) + (1| Site), data=dataset.japan, family=Beta, cores=2)
brm.ma <- brm(CoralMin ~ s(Year) + s(Depth) + (1| Location) + (1| Site), data=dataset.malaysia, family=Beta, cores=2)
brm.ma2 <- brm(CoralMin ~ s(Year) + Depth + (1| Location) + (1| Site), data=dataset.malaysia, family=Beta, cores=2)
brm.my <- brm(CoralMin ~ Year + Depth + (1| Location) + (1| Site), data=dataset.myanmar, family=Beta, cores=2)
brm.ph <- brm(CoralMin ~ s(Year) + s(Depth) + (1| Location) + (1| Site), data=dataset.philippines, family=Beta, cores=2)
brm.sg <- brm(CoralMin ~ s(Year) + Depth + (1| Location) + (1| Site), data=dataset.singapore, family=Beta, cores=2)
brm.sk <- brm(CoralMin ~ Year + Depth + (1| Location) + (1| Site), data=dataset.southkorea, family=Beta, cores=2)
brm.sk2 <- brm(CoralMin ~ Year + (1| Location) + (1| Site), data=dataset.southkorea, family=Beta, cores=2)
brm.tw <- brm(CoralMin ~ s(Year) + Depth + (1| Location) + (1| Site), data=dataset.taiwan, family=Beta, cores=2)
brm.tw2 <- brm(CoralMin ~ Year + Depth + (1| Location) + (1| Site), data=dataset.taiwan, family=Beta, cores=2)
brm.th <- brm(CoralMin ~ s(Year) + s(Depth) + (1| Location) + (1| Site), data=dataset.thailand, family=Beta, cores=2)
brm.vn <- brm(CoralMin ~ s(Year) + Depth + (1| Location) + (1| Site), data=dataset.vietnam, family=Beta, cores=2)
brm.vn2 <- brm(CoralMin ~ Year + Depth + (1| Location) + (1| Site), data=dataset.vietnam, family=Beta, cores=2)

## bayes_R2 function from https://avehtari.github.io/bayes_R2/bayes_R2.html ##
###
bayes_R2 <- function(fit) {
  mupred <- rstanarm::posterior_epred(fit)
  var_mupred <- apply(mupred, 1, var)
  if (family(fit)$family == "binomial" && NCOL(y) == 1) {
    sigma2 <- apply(mupred*(1-mupred), 1, mean)
  } else {
    sigma2 <- as.matrix(fit, pars = c("sigma"))^2
  }
  var_mupred / (var_mupred + sigma2)
}
###

## environmental correlations ##
envdata <- read.csv("data/dataset.csv", header=T)

envdata$CoralMin <- ((envdata$Hardcoral * (nrow(envdata)-1)) + (1/2)) / (nrow(envdata))
envdata$AlgaeMin <- ((envdata$Macroalgae * (nrow(envdata)-1)) + (1/2)) / (nrow(envdata))

## check for NAs, remove them ##
envdata.narm <- envdata[complete.cases(envdata[ , 62:92]),]
envdata.hc1 <- envdata.narm1[complete.cases(envdata.narm1[ , 11]),]
envdata.ma1 <- envdata.hc[complete.cases(envdata.narm[ , 16]),]

envdata.hc <- scale(envdata.hc1[,c(39:51,53:55,58,62:92)], center=T)
envdata.ma <- scale(envdata.ma1[,c(39:51,53:55,58,62:92)], center=T)

rm(envdata.narm);rm(envdata.hc1);rm(envdata.ma1)

## create max model, dredge, check model averages, read summaries ##
maxmodel.hc <- lme(Hardcoral ~ Year + Country + Depth + pop_est_5km + pop_est_100km + 
                   reef_area_5km + reef_area_100km + npp_mean + npp_min + npp_max + npp_sd + npp_interann_sd + 
                   wave_mean + wave_sd + wave_interann_sd + dist_market + baa_max + dhw_max + 
                   sst_max + sst_mean + sst_min + ssta_max + ssta_mean + ssta_min,
                   random = ~ 1| Location/Site, data=envdata.hc)
                   
dredgemodel.hc <- dredge(maxmodel.hc, m.lim=c(NA, 6), trace=T)
hcmodel.average.d10 <- model.avg(dredgemodel.hc, subset = delta <10)
hcmodel.average.d6 <- model.avg(dredgemodel.hc, subset = delta <6)
hcmodel.average.d2 <- model.avg(dredgemodel.hc, subset = delta <2)
hcmodel.average.95 <- model.avg(dredgemodel.hc, cumsum(weight) <=0.95)

summary(hcmodel.averagec.d6)

maxmodel.ma <- lme(Macroalgae ~ Year + Country + Depth + pop_est_5km + pop_est_100km + 
                         reef_area_5km + reef_area_100km + npp_mean + npp_min + npp_max + npp_sd + npp_interann_sd + 
                         wave_mean + wave_sd + wave_interann_sd + dist_market + baa_max + dhw_max + 
                         sst_max + sst_mean + sst_min + ssta_max + ssta_mean + ssta_min,
                         random = ~ 1| Location/Site, data=envdata.ma)

dredgemodel.ma <- dredge(maxmodel.ma, m.lim=c(NA, 6), trace=T)
mamodel.average.d10 <- model.avg(dredgemodel.ma, subset = delta <10)
mamodel.average.d6 <- model.avg(dredgemodel.ma, subset = delta <6)
mamodel.average.d2 <- model.avg(dredgemodel.ma, subset = delta <2)
mamodel.average.95 <- model.avg(dredgemodel.ma, cumsum(weight) <=0.95)

summary(mamodel.averagec.d6)

## bleaching ##
dataset.bpa1 <- dataset %>% filter(., Year>1995 & Year<1998) %>% mutate(., Period = "BPA", Bleaching = "Pre")
dataset.bpa2 <- dataset %>% filter(., Year==1998) %>% mutate(., Period = "BPA", Bleaching = "During")
dataset.bpa3 <- dataset %>% filter(., Year>1998 & Year<2001) %>% mutate(., Period = "BPA", Bleaching = "Post")
dataset.bpafin <- bind_rows(dataset.bpa1,dataset.bpa2,dataset.bpa3)
dataset.bpafin2 <- dataset.bpafin %>% filter(., Year>1996 & Year <2000)

dataset.bpb1 <- dataset %>% filter(., Year>2007 & Year<2010) %>% mutate(., Period = "BPB", Bleaching = "Pre")
dataset.bpb2 <- dataset %>% filter(., Year==2010) %>% mutate(., Period = "BPB", Bleaching = "During")
dataset.bpb3 <- dataset %>% filter(., Year>2010 & Year<2013) %>% mutate(., Period = "BPB", Bleaching = "Post")
dataset.bpbfin <- bind_rows(dataset.bpb1,dataset.bpb2,dataset.bpb3)
dataset.bpbfin2 <- dataset.bpbfin %>% filter(., Year>2008 & Year <2012)

dataset.bpc1 <- dataset %>% filter(., Year>2013 & Year<2016) %>% mutate(., Period = "BPC", Bleaching = "Pre")
dataset.bpc2 <- dataset %>% filter(., Year==2016) %>% mutate(., Period = "BPC", Bleaching = "During")
dataset.bpc3 <- dataset %>% filter(., Year>2016 & Year<2019) %>% mutate(., Period = "BPC", Bleaching = "Post")
dataset.bpcfin <- bind_rows(dataset.bpc1,dataset.bpc2,dataset.bpc3)
dataset.bpcfin2 <- dataset.bpcfin %>% filter(., Year>2014 & Year <2018)

dataset.bpfin <- bind_rows(dataset.bpafin,dataset.bpbfin,dataset.bpcfin)
dataset.bpfin2 <- bind_rows(dataset.bpafin2,dataset.bpbfin2,dataset.bpcfin2)

brm.bp <- brm(CoralMin ~ Period * Bleaching + Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.2 <- brm(CoralMin ~ Period + Bleaching + Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.3 <- brm(CoralMin ~ Bleaching * Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.3a <- brm(CoralMin ~ Country * Bleaching + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.4 <- brm(CoralMin ~ Bleaching + Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
compare_performance(brm.bp,brm.bp.2,brm.bp.3, brm.bp.4)
compare_performance(brm.bp.3a,brm.bp.3)

## lag ##
envdataYear <- envdata %>% mutate(Year1 = Year + 1, Year2 = Year + 2, Year3 = Year + 3, Year5 = Year + 5, Year10 = Year +10)

env.lag0 <- brm(CoralMin ~ dhw_max + Year + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag1 <- brm(CoralMin ~ dhw_max + Year1 + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag2 <- brm(CoralMin ~ dhw_max + Year2 + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag3 <- brm(CoralMin ~ dhw_max + Year3 + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag1s <- brm(CoralMin ~ s(dhw_max) + s(Year1) + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag0s <- brm(CoralMin ~ s(dhw_max) + s(Year) + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag2s <- brm(CoralMin ~ s(dhw_max) + s(Year2) + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
env.lag3s <- brm(CoralMin ~ s(dhw_max) + s(Year3) + Depth + Country + (1|Location/Site), data=envdataYear, family=Beta, cores=2)
