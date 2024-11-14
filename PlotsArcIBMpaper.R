# %%%%%%%%%%%%%%%%%%%%%%%%% #
# ARCodIBM PLOTS            #
# %%%%%%%%%%%%%%%%%%%%%%%%% #
# Created Feb 2022, Carmen L David


setwd("~/Documents/WHOI/R")

#read libraries#### 
library(ncdf4)
library(hdf5r)
library(plot3D)
library(misc3d)
library(ggOceanMaps)
library(rgeos)
library(sp)
library(rgdal)
library(ggplot2)
library(ggh4x)



# =========== #
# Figure 1 ####
# =========== #
# Read release locations - all 17 areas
df_map <- read.table("Data/Init_17polys_0deg05_areaID.txt", header = T)
df_map <- df_map[df_map$area %in% c("SV", "NWW", "LS", "CUK"),]
Arealabel <- dplyr::recode_factor(df_map$area, "NWW" = "Baffin Bay", "CUK"="Chukchi Sea",
                                  "SV"="Svalbard", "LS"="Laptev Sea")
df_map$area <- Arealabel
df_map$area <- factor(df_map$area, levels=c("Baffin Bay","Chukchi Sea", "Laptev Sea","Svalbard"))

df_map$lon <- ifelse(df_map$lon>180, df_map$lon-360, df_map$lon)

coord_labs=data.frame(labels = c("0º", "90º", "180º", "270º", "60º", "70º",  "80º"), 
                      lon=c(0,92,180,-92, 131, 131, 131),  
                      lat=c(66, 66.5, 67, 67, 66, 71, 81),
                      angle = c(0, 0, 0, 0, 310, 310, 310))
count_labs =data.frame(labels = c("Greenland", "Canada", "USA", "Russia", "Norway"), 
                       lon=c(-40, -125, -145, 115, 25),  
                       lat=c(73, 67.5, 68, 68.5, 67.5))

p_base <- basemap(65, bathymetry = TRUE, projection.grid = F, legends = T) +
  geom_text(data = transform_coord(count_labs[,2:3]), aes(x = lon, y = lat, angle=0), 
            color = "grey90",label= count_labs$labels) +
  geom_text(data = transform_coord(coord_labs[,2:3]), aes(x = lon, y = lat, angle=coord_labs$angle), 
            color = "grey90",label= coord_labs$labels) 

p_base + 
  geom_point(data = transform_coord(df_map[,2:3]),
             aes(x = lon, y = lat, colour=df_map$area), size = 0.3, show.legend = T) + 
  guides(colour = guide_legend("Region", override.aes = list(size=5)))

dev.copy2pdf(file = "Plots/Paper_Jan2024/Figure1.pdf", width = 7, height = 5)


# Figure 1 first submission
# with areas defined as polygons
all_poly <- list(
  SV = data.frame(lon = c(20.0, 13.5, 8.5, 14,   27,   30, 27.8, 26.8), 
                  lat = c(76.5, 76.5, 79,  80.5, 80.8, 80, 78.8, 77.5)),
  LS = data.frame(lon = c(118, 100, 110, 119), 
                  lat = c(72, 72,  76.5, 75)),
  CUK = data.frame(lon = c(-175, -180, -176,  -170.8, -170), 
                   lat = c(67.0, 69.0, 68.3, 67.0, 66.0)),
  BAF = data.frame(lon = c(-80, -70, -65, -80), 
                   lat = c(78, 78, 76, 76)))

# Chukchi Sea
basemap(data = all_poly$CUK , bathymetry = T, shapefiles = "Arctic", legends = F) +
  geom_point(data = transform_coord(df_pts[df_pts$area == "CUK",2:3]), 
             aes(x = lon, y = lat), color = "chartreuse3", size=0.5) 
dev.copy2pdf(file = "plots/paper_Jan2022/ArcticMaps_CUK.pdf", width = 6, height = 6)

# Svalbard
basemap(data = all_poly$SV, bathymetry = T, shapefiles = "Arctic", legends = F) +
  geom_point(data = transform_coord(df_pts[df_pts$area == "SV",2:3]), 
             aes(x = lon, y = lat), color = "mediumpurple2", size=0.5) 
dev.copy2pdf(file = "plots/paper_Jan2022/ArcticMaps_SV.pdf", width = 6, height = 6)

# Baffin Bay
basemap(data = all_poly$BAF, bathymetry = T, shapefiles = "Arctic", legends = F) +
  geom_point(data = transform_coord(df_pts[df_pts$area == "NWW",2:3]), 
             aes(x = lon, y = lat), color = "coral3", size=0.5) 
dev.copy2pdf(file = "plots/paper_Jan2022/ArcticMaps_BAF.pdf", width = 6, height = 6)

# Laptev Sea
basemap(data = all_poly$LS, bathymetry = T, shapefiles = "Arctic", legends = F) +
  geom_point(data = transform_coord(df_pts[df_pts$area == "LS",2:3]), 
             aes(x = lon, y = lat), color = "lightseagreen", size=0.5) 
dev.copy2pdf(file = "plots/paper_Jan2022/ArcticMaps_LS.pdf", width = 6, height = 6)




# =========== #
# Figure 2 ####
# =========== #
# PWL per area color-coded by month

df_plot <- read.table("Output/Baseline1Sep_4areasM.txt", header = T)
df_plot$Month <- factor(df_plot$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months


ggplot(df_plot, aes(x=lmm, fill=Month)) + 
  geom_density(alpha=.5, size=0.5) + 
  labs(x = "Prewinter length (mm)", y = "Density", fill = "Spawning month") + 
  theme_classic() + 
  theme(legend.position="top", strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(color="black", fill="grey90"))+
  theme(panel.spacing = unit(0.8, "lines")) +
  facet_wrap2(~Area, axes = "all", remove_labels = 'y')
dev.copy2pdf(file = "plots/paper_Jan2024/Figure3m.pdf", width = 6, height = 5.5)



# ============ #
# Figure 3  ####
# ============ #
# PWL~Ice #
ggplot(df_plot, aes(x=IceAbs15, y=lmm)) +
  geom_point(shape=21, size=0.5, aes(colour=Month))+
  geom_smooth(method="lm")+ 
  labs(y = "Prewinter length (mm)", x = "Days with ice cover < 15%") + 
  theme_classic() + 
  theme(legend.position="top", strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(color="black", fill="grey90"))+
  facet_wrap(~Area, nrow = 2)
dev.copy2pdf(file = "Plots/Paper_Jan2024/Figure3m.pdf", width = 7, height = 5)


# =========== #
# Figure 4 ####
# =========== #
# PWL~Hd #
ggplot(df_plot, aes(x=hd, y=lmm)) +
  geom_point(shape=21, size=0.5, aes(colour=as.factor(stage)))+
  geom_smooth(method=lm , color="black", fill="#69b3a2", level=0.90) +
  labs(y = "Prewinter length (mm)", x = "Embryonic development duration (days)") + 
  theme_classic() + 
  theme(legend.position="none", strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(color="black", fill="grey90")) +
  facet_wrap(~Month, nrow = 2)
dev.copy2pdf(file = "Plots/Paper_Jan2024/Figure4m.pdf", width = 7, height = 5)



# =========== #
# Figure 5 ####
# =========== #
df_t <- read.table("Output/HighTemp1Sep_4areasM.txt")
df_t$Month <- factor(df_t$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df_t$Year <- factor(df_t$Year)
df_t$Area <- factor(df_t$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))


# estimate difference from baseline
df_t$Anomaly <- df_t$BEM_highT-df_t$BEM
dfm <- df_t[,c("Anomaly","Area","Year","Month")]
dfm$colbar <- ifelse(dfm$Area=="Svalbard","a","b")


dfplot <- cbind(aggregate(Anomaly~Area, data = dfm, mean), 
                aggregate(Anomaly~Area, data = dfm, sd)$Anomaly)
names(dfplot) <- c("Area", "Mean", "sd")
dfplot$colbar <- ifelse(dfplot$Area=="Svalbard","a","b")

library(forcats) # for fct_rev() in reordering $AREA

ggplot()+
  geom_col(data = dfplot, aes(y = fct_rev(Area), x = Mean, fill = colbar), width = 0.4) +
  geom_errorbarh(data = dfplot, aes(y = fct_rev(Area), xmin=Mean-sd, xmax=Mean+sd),
                 size = 0.6, colour="gray40") +
  geom_point(data = dfplot, aes(y = fct_rev(Area), x = Mean), colour = "gray40", size = 2) +
  geom_vline(xintercept=0, linetype="dashed", colour = "gray40") +
  xlab("Prewinter length difference (mm)")+
  ylab("Region") +
  labs(fill='') +
  scale_fill_discrete(guide = guide_legend(reverse=TRUE), labels=c('Negative', 'Positive'))+
  theme(legend.position="none")+
  theme_bw(base_size = 12)
dev.copy2pdf(file = "plots/paper_Jan2024/Figure5m.pdf", width = 5, height = 3.5)




# =========== #
# Figure 6 ####
# =========== #
df_hs <- read.table("Output/HatchSize1Sep_4areasM.txt", header = T)
df_hs$Month <- factor(df_hs$Month, levels=c("Jan","Feb", "Mar")) # order the months
  
p1 <- ggplot(df_hs, aes(x=lmm, fill = Scenario)) + 
  #geom_density(alpha=.15, fill="#FF6666", size=2) + 
  geom_density(alpha=.5, size=0.5) + 
  labs(x = "Prewinter length (mm)", y = "Density") + 
  scale_fill_discrete(name = "Size \n at hatch", labels = c("4 mm", "5 mm", "6 mm", "7 mm")) +
  theme_classic() + theme(legend.position="right", strip.text.x = element_text(face = "bold"),
                          strip.background = element_rect(color="black", fill="grey90"))+
  facet_wrap(~Month)

p2 <- ggplot(data = df_hs, aes(x = lmm, fill=Month)) + 
  #geom_density(alpha=.15, fill="#FF6666", size=2) + 
  geom_density(alpha=.5, size=0.5) +
  labs(x ="Prewinter length (mm)", y = "Density") + 
  theme_classic() + 
  theme(legend.position="right", strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(color="black", fill="grey90"))+
  facet_wrap2(~Scenario, axes = "all", remove_labels = 'y',
              labeller = as_labeller(c("4SL"="4 mm", "5SL"="5 mm", "6SL"="6 mm", "7SL"="7 mm"))) 

ggpubr::ggarrange(p1,p2, nrow=2, common.legend = F, labels = c("a)", "b)"))
dev.copy2pdf(file = "plots/paper_Jan2024/Figure6m.pdf", width = 6, height = 7)



# =========== #
# Figure 7 ####
# =========== #
# Metamorphosis rate Baseline & Low food & High T
df_lf <- read.table("Output/LowFood1Sep_4areasM.txt", header = T)

df_lf$Scenario <- factor(df_lf$Scenario, levels=c("P70", "P52"))
df_lf$Month <- factor(df_lf$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df_lf$Year <- factor(df_lf$Year)
df_lf$Area <- factor(df_lf$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))


# Probability to reach juvenile (>30 mm)
problf <- data.frame(proba = ifelse(df_lf$lmm>30, "yes", "no"),
                     Scenario = df_lf$Scenario,
                     Area = df_lf$Area, 
                     Month = df_lf$Month,
                     Year = df_lf$Year)
sumjuv <- table(problf)
lf_plot <- as.data.frame(sumjuv[2,,,,]/(sumjuv[1,,,,]+sumjuv[2,,,,]))
rm(problf, sumjuv)

# Require loading DF from Figure 7
df_t <- read.table("Output/HighTemp1Sep_4areasM.txt", header = T)
df_t$Month <- factor(df_t$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df_t$Year <- factor(df_t$Year)
df_t$Area <- factor(df_t$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

df_t <- df_t[c("BEM_highT","Area", "Year", "Month")] 
problf <- data.frame(proba = ifelse(df_t$BEM_highT>30, "yes", "no"),
                     Area = df_t$Area, 
                     Month = df_t$Month,
                     Year = df_t$Year)
sumjuv <- table(problf)
lt_plot <- as.data.frame(sumjuv[2,,,]/(sumjuv[1,,,]+sumjuv[2,,,]))
rm(problf, sumjuv)

lf_plot <- rbind(lf_plot, cbind(Scenario="HighT",lt_plot))

ggplot(data = lf_plot, aes(x = Month, y = Freq, col = Area)) + 
  geom_point(cex = 2.5, alpha=0.8) + 
  labs(x = "Spawning month", y = "Recruitment success", color = "Region") + 
  theme_bw()+
  theme(legend.position="top", strip.text.x = element_text(face = "bold"))+
  facet_wrap(~Scenario, labeller = as_labeller(c("P70"="Baseline", "P52"="Low Food", "HighT"="High Temp"))) 
dev.copy2pdf(file = "plots/paper_Jan2024/Figure7m.pdf", width = 7.5, height = 5)



# ============ #
# Figure S1 ####
# ============ #
# Specifics of BIOMAS produced by Z.Feng


# =========== #
# Figure S2 ####
# =========== #
df_plot$Year <- factor(df_plot$Year)
ggplot(df_plot, aes(y=lmm, x=Year, fill=Year)) + 
  geom_boxplot(color="black", alpha=0.8, width = 0.6, outlier.size = 0.7) + 
  labs(y = "Pre-winter length (mm)") + 
  theme_classic() + 
  theme(legend.position="bottom",axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  facet_wrap(~Area, nrow = 2)
dev.copy2pdf(file = "plots/paper_Jan2024/FigureS2.pdf", width = 6, height = 5)


# =========== #
# Figure S3 ####
# =========== #
# validation Baf & Laptev



# =========== #
# Figure S4 ####
# =========== #
# requires p_base from Figure 1
df_map <- df_plot
df_map$lon <- ifelse(df_map$lon>180, df_map$lon-360, df_map$lon)
p1 <- p_base + geom_point(data = transform_coord(df_map[df_map$Month == "Dec",1:2]),size=0.5, 
                          aes(x = lon, y = lat, col = as.factor(df_map[df_map$Month == "Dec",]$Area))) +
  theme(legend.position = "right", legend.box = "horizontal") + ggtitle("December") +
  guides(colour = guide_legend("Region", override.aes = list(size=4)))
leg <- cowplot::get_legend(p1)
p1 <- p1 + theme(legend.position = "none")
p2 <- p_base + geom_point(data = transform_coord(df_map[df_map$Month == "Jan",1:2]), size=0.5,
                          aes(x = lon, y = lat, col = as.factor(df_map[df_map$Month == "Jan",]$Area))) +
  theme(legend.position = "none") + ggtitle("January")
p3 <- p_base + geom_point(data = transform_coord(df_map[df_map$Month == "Feb",1:2]),size=0.5, 
                          aes(x = lon, y = lat, col = as.factor(df_map[df_map$Month == "Feb",]$Area))) +
  theme(legend.position = "none") + ggtitle("February")
p4 <- p_base + geom_point(data = transform_coord(df_map[df_map$Month == "Mar",1:2]), size=0.5,
                          aes(x = lon, y = lat, col = as.factor(df_map[df_map$Month == "Mar",]$Area))) +
  theme(legend.position = "none") + ggtitle("March")
p5 <- p_base + geom_point(data = transform_coord(df_map[df_map$Month == "Apr",1:2]), size=0.5,
                          aes(x = lon, y = lat, col = as.factor(df_map[df_map$Month == "Apr",]$Area))) +
  theme(legend.position = "none") + ggtitle("April")


ggpubr::ggarrange(p1,p2,p3,p4,p5,leg, ncol = 2, nrow = 3)
dev.copy2pdf(file = "Plots/Paper_Jan2024/FigureS4.pdf", width = 9, height = 11)


# ============ #
# Figure S5 ####
# ============ #
# plots with environmental variables after hatch
# calculates per individual fish along dispersal pathways
# Read dfm (mortality) from Analyse_ArcIBM_paper.R Line 350
df_env <- read.table("Output/EndDate1Sep/IceTempAfterHatch1Sep4Areas.txt")

df_env <- df_env[df_env$Month != "May",]
df_env$Month <- factor(df_env$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df_env$Year <- factor(df_env$Year)
Arealabel <- dplyr::recode_factor(df_env$Area, "NWW" = "Baffin Bay", "CUK"="Chukchi Sea",
                                  "SV"="Svalbard", "LS"="Laptev Sea")
df_env$Area <- Arealabel
df_env$Area <- factor(df_env$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))


p1 <- ggplot(dfm, aes(x=Area, y=LarvaT9, fill = Area)) + 
  geom_boxplot(alpha=0.5, outlier.shape = 1, outlier.size = 1) + 
  labs(y = "Days with T>9º C (larval stage)", x = "Area") + 
  theme_classic() + theme(legend.position="none")
p2 <- ggplot(df_env, aes(x=Area, y=DaysIce, fill = Area)) + 
  geom_boxplot(alpha=0.5, outlier.shape = 1, outlier.size = 1) + 
  labs(y = "Days with ice >15%", x = "Area") + 
  theme_classic() + theme(legend.position="none")
p3 <- ggplot(dfm, aes(x=Area, y=EggsT3, fill = Area)) + 
  geom_boxplot(alpha=0.5, outlier.shape = 1, outlier.size = 1) + 
  labs(y = "Days with T>3º C (egg stage)", x = "Area") + 
  theme_classic() + theme(legend.position="none")
p4 <- ggplot(df_env, aes(x=Area, y=HatchTmax, fill = Area)) + 
  geom_boxplot(alpha=0.5, outlier.shape = 1, outlier.size = 1) + 
  labs(y = "Max temp since hatch", x = "Area") + 
  theme_classic() + theme(legend.position="none")

ggpubr::ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
dev.copy2pdf(file = "Plots/Paper_Jan2024/FigureS5.pdf", width = 8, height = 6)




# =========== #
# Table 2  ####
# =========== #
df <- read.table("Output/Baseline1Sep_4areasM.txt", header = T)
df$Month <- factor(df$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df$Year <- factor(df$Year)
df$Area <- factor(df$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

hist(df$trajm)
hist(df$Bathy)
hist(df$Temp)
hist(df$IceAbs15)

library(dplyr)
sum1Sep <- df %>% group_by(Area) %>% summarize(meanHD = mean(hd),
                                               sdHD = sd(hd),
                                               meanL = mean(lmm),
                                                sdL = sd(lmm),
                                                meanT = median(Temp),
                                                sdT = sd(Temp),
                                                meanIce = median(IceAbs15),
                                                sdIce = sd(IceAbs15),
                                                meanDist = median(trajm),
                                                sdDist = sd(trajm),
                                                meanB = median(Bathy),
                                                sdB = sd(Bathy))
writexl::write_xlsx(sum1Sep,"SummaryEnv_1SepM.xlsx")


# =========== #
# Table 3  ####
# =========== #
# requires lf_plot from Figure 4
# Scenario:P70=Baseline; P52=Low-Food

# requires df_hs from Figure 3
Arealabel <- dplyr::recode_factor(df_hs$Area, "NWW" = "Baffin Bay", "CUK"="Chukchi Sea",
                                  "SV"="Svalbard", "LS"="Laptev Sea")
df_hs$Area <- Arealabel
df_hs$Area <- factor(df_hs$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

probjuv <- data.frame(proba = ifelse(df_hs$lmm>30, "yes", "no"),
                      Scenario = df_hs$Scenario,
                      Area = df_hs$Area, 
                      Month = df_hs$Month,
                      Year = df_hs$Year)
sumjuv <- table(probjuv)
juvhs <- as.data.frame(sumjuv[2,,,,]/(sumjuv[1,,,,]+sumjuv[2,,,,]))
rm(probjuv, sumjuv) # remove not needed data

# Baseline3 with SL=6mm only
base3 <- df_hs[df_hs$Scenario=="6SL",]
probjuv <- data.frame(proba = ifelse(base3$lmm>30, "yes", "no"),
                      Area = base3$Area, 
                      Month = base3$Month,
                      Year = base3$Year)
sumjuv <- table(probjuv)
base3 <- as.data.frame(sumjuv[2,,,]/(sumjuv[1,,,]+sumjuv[2,,,]))
rm(probjuv, sumjuv) # remove not needed data


# High Temp scenario
#requires df_t
probjuv <- data.frame(proba = ifelse(df_t$BEM_highT>30, "yes", "no"),
                      Area = df_t$Area, 
                      Month = df_t$Month,
                      Year = df_t$Year)
sumjuv <- table(probjuv)
juvT <- as.data.frame(sumjuv[2,,,]/(sumjuv[1,,,]+sumjuv[2,,,]))
rm(probjuv, sumjuv) # remove not needed data

library(dplyr)
sumScenarios <- cbind(lf_plot[lf_plot$Scenario=="P70",] %>% group_by(Area) %>% 
                        summarize(BaseM = round(mean(Freq), digits = 2),
                                                BaseSD = round(sd(Freq), digits = 2)),
                      lt_plot %>% group_by(Area) %>% 
                        summarize(HighTempM = round(mean(Freq), digits = 2),
                                  HighTempSD = round(sd(Freq), digits = 2)),
                      lf_plot[lf_plot$Scenario=="P52",] %>% group_by(Area) %>% 
                        summarize(LowFoodM = round(mean(Freq), digits = 2),
                                                LowFoodSD = round(sd(Freq), digits = 2)),
                      base3 %>% group_by(Area) %>% 
                        summarize(Base3M = round(mean(Freq), digits = 2),
                                  Base3SD = round(sd(Freq), digits = 2)),
                      juvhs %>% group_by(Area) %>% 
                        summarize(HatchSizeM = round(mean(Freq), digits = 2),
                                  HatchSizeSD = round(sd(Freq), digits = 2)))
                                                
writexl::write_xlsx(sumScenarios,"Table3_Summary_ScenariosM.xlsx")


# =========== #
# Table 4  ####
# =========== #
# linear models from Analyse ArcIBM_stats.R


# =========== #
# Table 5  ####
# =========== #
# Test juvenile threshold length
# Test end date for simulations
df_jl <- rbind(read.table("ProbaJuv1Sep_4areas.txt", header = T),
               read.table("ProbaJuv15Sep_4areas.txt", header = T))
df_jl <- rbind(read.table("Output/ProbaJuv1Sep_4areasM.txt", header = T),
               read.table("Output/ProbaJuv15Sep_4areasM.txt", header = T))
names(df_jl)

aggregate(L25mm ~ EndDate, data = df_jl, mean)

aggregate(L25mm ~ EndDate*Month, data = df_jl, mean)

aggregate(L25mm ~ EndDate, data = df_jl[df_jl$Month=="Dec",], mean)

juvtab <- data.frame(
  aggregate(L25mm ~ EndDate, data = df_jl, mean),
  SD25 = aggregate(L25mm ~ EndDate, data = df_jl, sd)[,2],
  L30mm = aggregate(L30mm ~ EndDate, data = df_jl, mean)[,2],
  SD30 = aggregate(L30mm ~ EndDate, data = df_jl, sd)[,2],
  L35mm = aggregate(L35mm ~ EndDate, data = df_jl, mean)[,2],
  SD35 = aggregate(L35mm ~ EndDate, data = df_jl, sd)[,2])

write.table(juvtab, "TestEndDate_JuvLengthM.txt")
writexl::write_xlsx(juvtab,"TestEndDate_JuvLengthM.xlsx")

# =========== #
# Table S3  ####
# =========== #
# requires df_hs from Figure 6
# Probability to reach juvenile (>30 mm)
probhs <- data.frame(proba = ifelse(df_hs$lmm>30, "yes", "no"),
                     HL = df_hs$Scenario,
                     Area = df_hs$Area, 
                     Month = df_hs$Month,
                     Year = df_hs$Year)
sumjuv <- table(probhs)
hs_plot <- as.data.frame(sumjuv[2,,,,]/(sumjuv[1,,,,]+sumjuv[2,,,,]))
rm(probhs, sumjuv)

hlsum <- data.frame(aggregate(Freq~HL*Area, data = hs_plot, mean),
                    SD = aggregate(Freq~HL*Area, data = hs_plot, sd)$Freq)
hlsum$Freq <- round(hlsum$Freq, 2)
hlsum$SD <- round(hlsum$SD, 2)
write.csv(hlsum, "HatchSizeSummary.csv")

