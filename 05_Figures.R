library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggnewscale)
library(ggspatial)
library(patchwork)

max(dataset$Latitude); max(dataset$Longitude)
min(dataset$Latitude); min(dataset$Longitude)

theme_base <- theme_set(theme_bw(base_size = 10) + theme(legend.position="bottom")) #create theme
theme_noleg <- theme_set(theme_bw(base_size = 10) + theme(legend.position="none")) #create theme

countrycols.seq <- c("Brunei"="#f86060", "Cambodia"="#f8a560", "Hong Kong"="#f8c560", "Indonesia"="#edd146", 
                     "Japan"="#eded46", "Malaysia"="#b8e75a", "Myanmar"="#4dc64d", "Philippines"="#3a9595",
                     "Singapore"="#44779f", "South Korea"="#4b67a6", "Taiwan"="#644eaa", "Thailand"="#8345a5",
                     "Vietnam"="#c74d92")
                     
#### Figure 1a ####

EA <- ne_countries(continent = "asia", scale = "large", returnclass = "sf")
EA_check <- EA[, c("admin", "name", "formal_en", "region_un", "subregion", "region_wb")]
basemap <- ggplot(data = EA) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(85, 150), ylim = c(-15, 50), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25)
  
basemap_wsites <- basemap + new_scale_color() +
  geom_point(data = dataset, aes(x = Longitude, y = Latitude, color = Country), size = 1.2, alpha = 0.75) + 
  scale_color_manual(values = countrycols.seq, name="Country") + theme(legend.position="none")

#### Figure 1b ####
hist.survey <- ggplot(dataset.survey, aes(Year)) + geom_histogram(aes(fill=Country), bins=20) + 
  facet_wrap(~ Country, scales="free_y", ncol=5) + labs(x="Year") +
  scale_fill_manual(values = countrycols.seq, name="Country") + theme(axis.text.x = element_text(angle = 45, hjust=1))

dataset.brunei.s <- filter(dataset.brunei, Replicate==1)
hist.br <- ggplot(dataset.brunei.s, aes(Year)) + geom_histogram(fill="#f86060", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Brunei", y="Counts", x = "") +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.cambodia.s <- filter(dataset.cambodia, Replicate==1)
hist.ca <- ggplot(dataset.cambodia.s, aes(Year)) + geom_histogram(fill="#f8a560", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Cambodia", y="", x = "") + 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.hongkong.s <- filter(dataset.hongkong, Replicate==1)
hist.hk <- ggplot(dataset.hongkong.s, aes(Year)) + geom_histogram(fill="#f8c560", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Hong Kong", y="", x = "") + 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.indonesia.s <- filter(dataset.indonesia, Replicate==1)
hist.in <- ggplot(dataset.indonesia.s, aes(Year)) + geom_histogram(fill="#edd146", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Indonesia", y="", x = "")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.japan.s <- filter(dataset.japan, Replicate==1)
hist.jp <- ggplot(dataset.japan.s, aes(Year)) + geom_histogram(fill="#eded46", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Japan", y="Counts", x = "")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.malaysia.s <- filter(dataset.malaysia, Replicate==1)
hist.ml <- ggplot(dataset.malaysia.s, aes(Year)) + geom_histogram(fill="#b8e75a", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Malaysia", y="", x = "")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.myanmar.s <- filter(dataset.myanmar, Replicate==1)
hist.my <- ggplot(dataset.myanmar.s, aes(Year)) + geom_histogram(fill="#4dc64d", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Myanmar", y="", x = "")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.philippines.s <- filter(dataset.philippines, Replicate==1)
hist.ph <- ggplot(dataset.philippines.s, aes(Year)) + geom_histogram(fill="#3a9595", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Philippines", y="", x = "")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.singapore.s <- filter(dataset.singapore, Replicate==1)
hist.sg <- ggplot(dataset.singapore.s, aes(Year)) + geom_histogram(fill="#44779f", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Singapore", y="Counts", x = "")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.southkorea.s <- filter(dataset.southkorea, Replicate==1)
hist.sk <- ggplot(dataset.southkorea.s, aes(Year)) + geom_histogram(fill="#4b67a6", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "South Korea", y="", x = "Year")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.taiwan.s <- filter(dataset.taiwan, Replicate==1)
hist.tw <- ggplot(dataset.taiwan.s, aes(Year)) + geom_histogram(fill="#644eaa", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Taiwan", y="", x = "Year")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.thailand.s <- filter(dataset.thailand, Replicate==1)
hist.th <- ggplot(dataset.thailand.s, aes(Year)) + geom_histogram(fill="#8345a5", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Thailand", y="", x = "Year")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dataset.vietnam.s <- filter(dataset.vietnam, Replicate==1)
hist.vn <- ggplot(dataset.vietnam.s, aes(Year)) + geom_histogram(fill="#c74d92", bins=20) + 
  scale_x_continuous(limits = c(1980,2020)) + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title= "Vietnam", y="", x = "Year")+ 
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 1 ####
layout.hist <- "
AABBCCDD
EEFFGGHH
IIJJKKLL
MM######"

hist.set <- hist.br + hist.ca + hist.hk + hist.in + 
  hist.jp + hist.ml + hist.my + hist.ph + 
  hist.sg + hist.sk + hist.tw + hist.th + hist.vn + 
  plot_layout(design = layout.hist)
  
 
ggsave(hist.set, width=15, height=15, units="cm", dpi=300, filename="newfigspub/hist.nogrid.tiff")
ggsave(hist.set, width=15, height=15, units="cm", dpi=300, filename="newfigspub/hist.nogrid.svg")

ggsave(basemap_wsites, width=15, height=15, units="cm", dpi=300, filename="newfigspub/map.tiff")
ggsave(basemap_wsites, width=15, height=15, units="cm", dpi=300, filename="newfigspub/map.svg")


#### Model plots for Fig 2/3####
brmplots <- plot(marginal_effects(brm.wip.bycountry), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmsplot1 <- brmplots$Year + 
  scale_x_continuous(limits = c(1983,2020)) +
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Coral Cover")
brmsplot2 <- brmplots$Depth + 
  scale_x_continuous(limits = c(0,20)) + 
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Coral Cover")
brmsplot3 <- brmplots$Country + geom_point(aes(color=Country), size=4) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(y="Coral Cover") + 
  scale_color_manual(values = countrycols.seq, name="Country")
brmsplot4 <- brmplots$`Year:Country`  + 
  scale_x_continuous(limits = c(1983,2020)) +
  labs(y="Coral Cover") + 
  scale_fill_manual(values = countrycols.seq, name="Country") + 
  scale_color_manual(values = countrycols.seq, name="Country")

brmaplots <- plot(marginal_effects(brma.wip.bycountry), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmaplot1 <- brmaplots$Year + 
  scale_x_continuous(limits = c(1983,2020)) +
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Macroalgal Cover")
brmaplot2 <- brmaplots$Depth + 
  scale_x_continuous(limits = c(0,20)) + 
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Macroalgal Cover")
brmaplot3 <- brmaplots$Country + geom_point(aes(color=Country), size=4) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(y="Macroalgal Cover") + 
  scale_color_manual(values = countrycols.seq, name="Country")
brmaplot4 <- brmaplots$`Year:Country` +
  scale_x_continuous(limits = c(1983,2020)) +
  labs(y="Macroalgal Cover") + 
  scale_fill_manual(values = countrycols.seq, name="Country") + 
  scale_color_manual(values = countrycols.seq, name="Country")

#### Figure 2 a ####
loessplot <- ggplot(dataset) + 
#  geom_point(aes(Year, Hardcoral), alpha=0.05, color="#d7191c") + 
  geom_smooth(aes(Year, Hardcoral), color="#d6604d")  + 
  geom_text(aes(2016.5,0.35, label="HC")) +
#  geom_point(aes(Year, Macroalgae), alpha=0.05, color="#1a9641") + 
  geom_smooth(aes(Year, Macroalgae), color="#4393c3") +
  geom_text(aes(2016.5,0.08, label="MA")) +
  scale_x_continuous(limits = c(1983,2019), expand=c(0,0)) +
  scale_y_continuous(expand =c(0,0)) +
  labs(y="Mean Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 2 b ####
brmsplot1 <- brmsplot1 + scale_y_continuous(limits = c(0,0.6), expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 2 c ####
brmsplot2 <- brmsplot2 + scale_y_continuous(limits = c(0,0.6), expand=c(0,0)) + scale_x_continuous(limits = c(0,20), expand=c(0,0))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 2 d ####
dataset.graphing <- dataset %>% group_by(Year) %>%
  summarise(HardcoralMean = mean(Hardcoral), HardcoralSD = sd(Hardcoral), MacroalgalMean = mean(Macroalgae, na.rm=T), MacroalgalSD = sd(Macroalgae, na.rm=T))

meanplot <- ggplot(dataset.graphing, aes(Year)) + 
  geom_line(aes(x=Year, HardcoralMean), color="#d6604d") + 
#  geom_point(aes(x=Year, HardcoralMean), color="#d7191c") + 
  geom_errorbar(aes(ymin=pmax(HardcoralMean-HardcoralSD,0), ymax=HardcoralMean+HardcoralSD), alpha =0.5, color="#d6604d") +
  geom_line(aes(x=Year, MacroalgalMean), color="#4393c3") + 
  geom_text(aes(2016.5,0.42, label="HC", size=0.8)) +
  geom_text(aes(2016.5,0.022, label="MA", size=0.8)) +
#  geom_point(aes(x=Year, MacroalgalMean), color="#1a9641") + 
  geom_errorbar(aes(ymin=pmax(MacroalgalMean-MacroalgalSD,0), ymax=MacroalgalMean+MacroalgalSD), alpha =0.5, color="#4393c3") +
  scale_y_continuous(limits=c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8), expand=c(0,0)) + 
  scale_x_continuous(limits = c(1983,2019), expand=c(0,0)) +
  labs(y="Mean Proportion") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 2 e ####
brmaplot1 <- brmaplot1 + scale_y_continuous(limits = c(0,0.12), expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 2 f ####
brmaplot2 <- brmaplot2 + scale_y_continuous(limits = c(0,0.12), expand=c(0,0)) + scale_x_continuous(limits= c(0, 20),expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 2 ####
layout.cover <- "
AABBCC
DDEEFF"

coverplot2 <- loessplot + brmsplot1 + brmsplot2 +
  meanplot + brmaplot1 + brmaplot2 + plot_annotation(tag_levels = 'a') + 
  plot_layout(design = layout.cover)

ggsave(coverplot2, width=22.5, height=15, units="cm", dpi=300, filename="newfigspub/coverplot2.nogrid.tiff")
ggsave(coverplot2, width=22.5, height=15, units="cm", dpi=300, filename="newfigspub/coverplot2.nogrid.svg")

#### Figure 3 a ####
boxplot <- ggplot(dataset, aes(Country, Hardcoral)) + 
  geom_boxplot(aes(fill=Country, alpha=.75), outlier.size = 0.6) + 
  scale_fill_manual(values = countrycols.seq, name="Country") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        )

#### Figure 3 d ####
boxplotma <- ggplot(dataset, aes(Country, Macroalgae)) + 
  geom_boxplot(aes(fill=Country, alpha=.75), outlier.size = 0.6) + 
  scale_fill_manual(values = countrycols.seq, name="Country") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        )

#### Figure 3 b ####
loesscountryplot <- ggplot(dataset) + 
#  geom_point(aes(Year, Hardcoral, colour=Country), alpha=0.05) + 
  geom_smooth(aes(Year, Hardcoral, colour=Country), method="loess", alpha= 0.25) + 
  scale_y_continuous(limits=c(0,1), breaks = c(0, 0.25,0.5,0.75,1.0)
                     , expand=c(0,0)
                     ) + 
  scale_x_continuous(limits = c(1983,2019)
                     , expand=c(0,0)
                     ) +
  scale_colour_manual(values = countrycols.seq, name="Country") +
  labs(y="Mean Proportion") 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

loesscountryplot.n <- ggplot(dataset) + 
  #  geom_point(aes(Year, Hardcoral, colour=Country), alpha=0.05) + 
  geom_smooth(aes(Year, Hardcoral, colour=Country), method="loess", se=F) + 
  stat_smooth(geom='ribbon', method="loess", aes(Year, Hardcoral, fill=Country, ymin = ifelse(..ymin.. < 0, 0, ..ymin..)), alpha = 0.15) +
  scale_y_continuous(limits=c(0,1), breaks = c(0, 0.25,0.5,0.75,1.0)
                     , expand=c(0,0)
  ) + 
  scale_x_continuous(limits = c(1983,2019)
                     , expand=c(0,0)
  ) +
  scale_colour_manual(values = countrycols.seq, name="Country") +
  labs(y="Mean Proportion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 3 e ####
loesscountryplot.ma <- ggplot(dataset) + 
#  geom_point(aes(Year, Macroalgae, colour=Country), alpha=0.05) + 
  geom_smooth(aes(Year, Macroalgae, colour=Country), method="gam", alpha= 0.25) + 
  scale_y_continuous(limits=c(0,1), breaks = c(0, 0.25,0.5,0.75,1.0)
                     , expand=c(0,0)
                     ) + 
  scale_x_continuous(limits = c(1983,2019)
                     , expand=c(0,0)
                     ) +
  scale_colour_manual(values = countrycols.seq, name="Country") +
  labs(y="Mean Proportion") + theme(legend.position = "none"
                                    , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                    )

loesscountryplot.nma <- ggplot(dataset) + 
  #  geom_point(aes(Year, Macroalgae, colour=Country), alpha=0.05) + 
  geom_smooth(aes(Year, Macroalgae, colour=Country), method="gam", se=F) + 
  stat_smooth(geom='ribbon', method="gam", aes(Year, Macroalgae, fill=Country, ymin = ifelse(..ymin.. < 0, 0, ..ymin..)), alpha = 0.15) +
  scale_y_continuous(limits=c(0,1), breaks = c(0, 0.25,0.5,0.75,1.0)
                     , expand=c(0,0)
  ) + 
  scale_x_continuous(limits = c(1983,2019)
                     , expand=c(0,0)
  ) +
  scale_colour_manual(values = countrycols.seq, name="Country") +
  labs(y="Mean Proportion") + theme(legend.position = "none"
                                    , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

#### Figure 3 c ####
brmsplot3 <- brmsplot3 + theme(legend.position = "none"
                               , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                               )

#### Figure 3 f ####
brmaplot3 <- brmaplot3 + theme(legend.position = "none"
                               , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                               )

#### Figure 3 ####
coverplot3 <- boxplot + loesscountryplot.n + brmsplot3 +
  boxplotma + loesscountryplot.nma + brmaplot3 + plot_annotation(tag_levels = 'a') + 
  plot_layout(design = layout.cover, guides="collect") 

ggsave(coverplot3, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/coverplot3.nogrid.tiff")
ggsave(coverplot3, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/coverplot3.nogrid.svg")

#### Model plots for Fig 4####

brmplots.t1 <- plot(marginal_effects(brm.time1), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmplots.t11 <- brmplots.t1$Year + 
  scale_x_continuous(breaks=seq(1980,1997,5),expand=c(0, 0)) + 
  scale_y_continuous(limits=c(0,0.65),breaks=seq(0,0.6,0.1),expand=c(0, 0)) +
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Hardcoral Cover")

brmplots.t2 <- plot(marginal_effects(brm.time2), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmplots.t21 <- brmplots.t2$Year + 
  scale_x_continuous(breaks=seq(2000,2009,5),expand=c(0, 0)) + 
  scale_y_continuous(limits=c(0,0.65),breaks=seq(0,0.6,0.1),expand=c(0, 0)) +
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Hardcoral Cover")

brmplots.t3 <- plot(marginal_effects(brm.time3), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmplots.t31 <- brmplots.t3$Year + 
  scale_x_continuous(breaks=seq(2010,2020,5),expand=c(0, 0)) + 
  scale_y_continuous(limits=c(0,0.65),breaks=seq(0,0.6,0.1),expand=c(0, 0)) +
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Hardcoral Cover")

#### Figure 4 a ####
brmplots.t11 <- brmplots.t11 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 4 b ####
brmplots.t21 <- brmplots.t21 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 4 c ####
brmplots.t31 <- brmplots.t31 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 4 f ####
maxmincover <- ggplot(datasetrep.nulldiff, aes(max.year, min.year)) + 
  geom_point(aes(colour = hc.diff)) + 
  geom_abline(aes(intercept=0,slope=1), linetype="dotted") + 
  scale_color_gradient2(name = "Difference in coral cover", low = "#2b83ba", mid = "#ffffbf", midpoint = 0.5, high = "#d7191c") +
  labs(x="Year of highest mean coral cover", y="Year of lowest mean coral cover") + 
  theme(panel.grid.minor = element_blank())

#### Figure 4 d ####
#bleachingcols.seq <- c("Pre"="#fdae61", "During"="#ffffbf", "Post"="#abdda4")
dataset.bpfin$Bleaching <- factor(dataset.bpfin$Bleaching, levels=c("Pre", "During", "Post"))
brm.bp.3a <- brm(CoralMin ~ Country * Bleaching + (1| Location) + (1| Site), data=dataset.bpfin, family=Beta, cores=2, file="brm.bp.3a.rds")
brm.bp.3aplots <- plot(marginal_effects(brm.bp.3a), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brm.bp.3aplots1 <- brm.bp.3aplots$Country + 
  theme(axis.text.x = element_text(hjust=1)) + 
  labs(y="Hardcoral Cover")
brm.bp.3aplots2 <- brm.bp.3aplots$Bleaching + 
  geom_point(aes(color=Bleaching), size=4) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(y="Hardcoral Cover", x="") + 
  scale_x_discrete(labels=c("Pre-bleaching", "During bleaching", "Post-bleaching")) +
  scale_color_manual(breaks=c("Pre", "During", "Post"),
                     #labels=c("Pre-bleaching", "During bleaching", "Post-bleaching"),
                     values=c("#a6cee3", "#fdbf6f", "#fb9a99"), #blue #orange #red
                     name="Bleaching Periods") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 4 e ####
brm.bp.3aplots3 <- brm.bp.3aplots$`Country:Bleaching` + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(y="Hardcoral Cover", x="Country") + 
  scale_color_manual(values=c("#a6cee3", "#fdbf6f", "#fb9a99"), 
                     name="Bleaching Periods",
                     breaks=c("Pre", "During", "Post"),
                     labels=c("Pre-bleaching", "During bleaching", "Post-bleaching")) + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Figure 4 ####
nullplot <- brmplots.t11 + brmplots.t21 + brmplots.t31 +
  brm.bp.3aplots2 + brm.bp.3aplots3 + maxmincover + plot_annotation(tag_levels = 'a') + 
  plot_layout(design = layout.cover, guides="collect") 

ggsave(nullplot, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/nullplot.nogrid.tiff")
ggsave(nullplot, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/nullplot.nogrid.eps")


#### Supplementary plots ####
mcplot <- plot(brm.wip.bycountry, ask=F, N=9)
mcaplot <- plot(brma.wip.bycountry, ask=F, N=9)

layout.supp <- "
AABB
CCDD"

brmsplot4 <- brmsplot4 + scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
brmaplot4 <- brmaplot4 + scale_y_continuous(limits = c(0,0.5), expand=c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(brmsplot4, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot0a.tiff")
ggsave(brmsplot4, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot0a.svg")

ggsave(brmaplot4, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot0b.tiff")
ggsave(brmaplot4, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot0b.svg")

brmplots.nojp <- plot(marginal_effects(brm.nojpbc), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmplots.nojp1 <- brmplots.nojp$Year + theme(axis.text.x = element_text(hjust=1)) + labs(y="Coral Cover") + 
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
brmplots.nojp2 <-brmplots.nojp$Depth + theme(axis.text.x = element_text(hjust=1)) + labs(y="Coral Cover")+ 
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
brmplots.nojp3 <- brmplots.nojp$`Year:Country` + 
  scale_x_continuous(limits = c(1983,2020)) +
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) +
  labs(y="Coral Cover") + 
  scale_fill_manual(values = countrycols.seq, name="Country") + 
  scale_color_manual(values = countrycols.seq, name="Country") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
brmplots.nojp4 <- brmplots.nojp$Country + geom_point(aes(color=Country), size=4) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) +
  labs(y="Coral Cover") + labs(x="Locality") +  
  scale_color_manual(values = countrycols.seq, name="Country") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="none")

brmplots.nomy <- plot(marginal_effects(brm.nomybc), point_args = c(alpha = 0.5, size = 1.5), points=F, ask=F)
brmplots.nomy1 <- brmplots.nomy$Year + theme(axis.text.x = element_text(hjust=1)) + labs(y="Coral Cover") + 
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
brmplots.nomy2 <- brmplots.nomy$Depth + theme(axis.text.x = element_text(hjust=1)) + labs(y="Coral Cover") + 
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
brmplots.nomy3 <- brmplots.nomy$`Year:Country` + 
  scale_x_continuous(limits = c(1983,2020)) +
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) +
  labs(y="Coral Cover") + 
  scale_fill_manual(values = countrycols.seq, name="Country") + 
  scale_color_manual(values = countrycols.seq, name="Country")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
brmplots.nomy4 <- brmplots.nomy$Country + geom_point(aes(color=Country), size=4) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_y_continuous(limits = c(0,1.0), expand=c(0,0)) +
  labs(y="Coral Cover") +  labs(x="Locality") +
  scale_color_manual(values = countrycols.seq, name="Country")  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="none")

supplot1 <- brmplots.nojp1 + brmplots.nojp2 + brmplots.nojp3 + brmplots.nojp4 + plot_annotation(tag_levels = 'a') + 
  plot_layout(design = layout.supp, guides="collect") 

supplot2 <- brmplots.nomy1 + brmplots.nomy2 + brmplots.nomy3 + brmplots.nomy4 + plot_annotation(tag_levels = 'a') + 
  plot_layout(design = layout.supp, guides="collect") 

ggsave(supplot1, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot1.tiff")
ggsave(supplot1, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot1.svg")

ggsave(supplot2, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot2.tiff")
ggsave(supplot2, width=22.5, height=17.5, units="cm", dpi=300, filename="newfigspub/supplot2.svg")
