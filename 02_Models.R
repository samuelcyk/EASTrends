library(tidyverse)
library(brms)
library(performance)
library(RColorBrewer)

#### read data & subset ####
datafull <- read.csv("newdata/Final2.csv", header=T) %>% mutate(., Days = Year+(as.numeric(as.character(Month))/12))
datafull$CoralMin <- ((datafull$Hardcoral * (nrow(datafull)-1)) + (1/2)) / (nrow(datafull))
envdata <- read.csv("newdata/envdata.csv", header=T)

datarep <- datafull %>% filter(., REMOVED!= "Y")
envdatarep <- merge(datarep, envdata)
envdatafull <- merge(datafull, envdata)
#write.csv(envdatarep, file="newdata/envdatarep.csv")
#write.csv(envdatafull, file="newdata/envdatafull.csv")

dataset <- read.csv("newdata/compiledfullnew_edit_FINAL.csv", header=T) %>%
  select(., -c(7,8,27:40,54,58,59,61:63)) %>%
  rename(., Site="Merged.Site")

datasetrep <- read.csv("newdata/compiledfullnew_edit_FINAL.csv", header=T) %>%
  filter(., REMOVED !="Y") %>%
  select(., -c(7,26:39,53,57,58,60:62)) %>%
  rename(., Site="Merged.Site")


#### priors ####
wprior <- c(set_prior("normal(0,1)", class="b"),
               set_prior("cauchy(0,5)", class="sd", group="Location"),
               set_prior("cauchy(0,5)", class="sd", group="Site"),
               set_prior("normal(0,1)", class="sds", coef="s(Year)"),
               set_prior("normal(0,1)", class="sds", coef="s(Depth)")
)

wprior1 <- c(set_prior("normal(0,1)", class="b"),
            set_prior("cauchy(0,5)", class="sd", group="Location"),
            set_prior("cauchy(0,5)", class="sd", group="Site"),
            set_prior("normal(0,1)", class="sds", coef="s(Year,by=Country)"),
            set_prior("normal(0,1)", class="sds", coef="s(Depth)")
)

wiprior <- c(set_prior("normal(0.5,0.5)", class="b"),
               set_prior("cauchy(0,5)", class="sd", group="Location"),
               set_prior("cauchy(0,5)", class="sd", group="Site"),
               set_prior("normal(0.5,0.5)", class="sds", coef="s(Year)"),
               set_prior("normal(0.5,0.5)", class="sds", coef="s(Depth)")
)

wiprior1 <- c(set_prior("normal(0.5,0.5)", class="b"),
             set_prior("cauchy(0,5)", class="sd", group="Location"),
             set_prior("cauchy(0,5)", class="sd", group="Site"),
             set_prior("normal(0.5,0.5)", class="sds", coef="s(Year,by=Country)"),
             set_prior("normal(0.5,0.5)", class="sds", coef="s(Depth)")
)

#### Main models ####
brm.method <- brm(CoralMin ~ s(Year) + s(Depth) + Country + (1|Method) + (1|Location) + (1|Site), data=dataset, family=Beta, cores=2)

brm.wip.country <- brm(CoralMin ~ s(Year) + s(Depth) + Country + (1|Location) + (1| Site), prior=wiprior, data=dataset, family=Beta, cores=2, file="brm.wip.country")
brm.wip.bycountry <- brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location) + (1| Site), prior=wiprior1, data=dataset, family=Beta, cores=2, file="brm.wip.bycountry")

compare_performance(brm.wip.country,brm.wip.bycountry)


brma.wip.country <- brm(AlgaeMin ~ s(Year) + s(Depth) + Country + (1|Location) + (1| Site), prior=wimprior, data=dataset, family=Beta, cores=2, file="brma.wip.country")
brma.wip.bycountry <- brm(AlgaeMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location) + (1| Site), prior=wimprior1, data=dataset, family=Beta, cores=2, file="brma.wip.bycountry")

compare_performance(brm.wip.country,brm.wip.bycountry)

#### Time separated models ####

dataset.time1 <- dataset %>% filter(., Year<2000) %>% mutate(., Period = "P1")
dataset.time2 <- dataset %>% filter(., Year>1999 & Year<2010) %>% mutate(., Period = "P2")
dataset.time3 <- dataset %>% filter(., Year>2009) %>% mutate(., Period = "P3")

brm.time1 <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location) + (1|Site), prior=wiprior1, data=dataset.time1, family=Beta, cores=2, file="brm.time1.rds")
brm.time2 <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location) + (1|Site), prior=wiprior1, data=dataset.time2, family=Beta, cores=2, file="brm.time2.rds")
brm.time3 <-brm(CoralMin ~ s(Year, by=Country) + s(Depth) + Country + (1|Location) + (1|Site), prior=wiprior1, data=dataset.time3, family=Beta, cores=2, file="brm.time3.rds")

#### Bleaching models ####

dataset.bpa1 <- dataset %>% filter(., Year>1995 & Year<1998) %>% mutate(., Period = "BPA", Bleaching = "Pre")
dataset.bpa2 <- dataset %>% filter(., Year==1998) %>% mutate(., Period = "BPA", Bleaching = "During")
dataset.bpa3 <- dataset %>% filter(., Year>1998 & Year<2001) %>% mutate(., Period = "BPA", Bleaching = "Post")
dataset.bpafin <- bind_rows(dataset.bpa1,dataset.bpa2,dataset.bpa3)
dataset.bpb1 <- dataset %>% filter(., Year>2007 & Year<2010) %>% mutate(., Period = "BPB", Bleaching = "Pre")
dataset.bpb2 <- dataset %>% filter(., Year==2010) %>% mutate(., Period = "BPB", Bleaching = "During")
dataset.bpb3 <- dataset %>% filter(., Year>2010 & Year<2013) %>% mutate(., Period = "BPB", Bleaching = "Post")
dataset.bpbfin <- bind_rows(dataset.bpb1,dataset.bpb2,dataset.bpb3)
dataset.bpc1 <- dataset %>% filter(., Year>2013 & Year<2016) %>% mutate(., Period = "BPC", Bleaching = "Pre")
dataset.bpc2 <- dataset %>% filter(., Year==2016) %>% mutate(., Period = "BPC", Bleaching = "During")
dataset.bpc3 <- dataset %>% filter(., Year>2016 & Year<2019) %>% mutate(., Period = "BPC", Bleaching = "Post")
dataset.bpcfin <- bind_rows(dataset.bpc1,dataset.bpc2,dataset.bpc3)
dataset.bpfin <- bind_rows(dataset.bpafin,dataset.bpbfin,dataset.bpcfin)

brm.bp <- brm(CoralMin ~ Period * Bleaching + Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.2 <- brm(CoralMin ~ Period + Bleaching + Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.3 <- brm(CoralMin ~ Bleaching * Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
brm.bp.3a <- brm(CoralMin ~ Country * Bleaching + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2, file="brm.bp.3a.rds")
brm.bp.4 <- brm(CoralMin ~ Bleaching + Country + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2)
compare_performance(brm.bp,brm.bp.2,brm.bp.3, brm.bp.4)
compare_performance(brm.bp.3a,brm.bp.3)
