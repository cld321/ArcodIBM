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



# =========== #
# Figure 1 ####
# =========== #
# Read release locations - all 17 areas
df_plot <- read.table("Data/Init_17polys_0deg05_areaID.txt", header = T)
df_map <- df_plot[df_plot$area %in% c("SV", "NWW", "LS", "CUK"),]
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

# Figure 1 ####
  p_base + 
  geom_point(data = transform_coord(df_map[,2:3]),
             aes(x = lon, y = lat, colour=df_map$area), size = 0.3, show.legend = T) + 
  guides(colour = guide_legend("Region", override.aes = list(size=5)))

dev.copy2pdf(file = "Plots/Paper_Jan2023/Figure1.pdf", width = 7, height = 5)


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
df_plot <- read.table("Output/Areas4/Baseline1Sep_4areas.txt", header = T)
df_plot$Month <- factor(df_plot$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months

# larval length by month/year
# Histograms
ggplot(df_plot, aes(x=lmm, fill=Month)) + 
  geom_histogram(aes(y = ..count..), color="grey30", alpha=0.2, binwidth = 2) + 
  labs(x = "Prewinter length (mm)", y = "Counts", fill = "Spawning month") + 
  theme_classic() + 
  theme(legend.position="top", strip.background = element_rect(color="black", fill="grey90"))+
  facet_wrap(~Area)
dev.copy2pdf(file = "plots/paper_Jan2023/Figure2.pdf", width = 5.5, height = 5.5)

# Distribution plot
ggplot(df_plot, aes(x=lmm, fill=Month)) + 
  geom_density(alpha=.5, size=0.5) + 
  labs(x = "Prewinter length (mm)", y = "Density", fill = "Spawning month") + 
  theme_classic() + 
  theme(legend.position="top", strip.background = element_rect(color="black", fill="grey90"))+
  facet_wrap(~Area)
dev.copy2pdf(file = "plots/paper_Dec2023/Figure2.pdf", width = 5.5, height = 5.5)


# plot for PPTs #
ggplot(df_plot, aes(x=lmm, colour=Month)) + 
  geom_density(alpha=.15, size=1.5) + 
  labs(x = "Prewinter length (mm)", y = "Frequency") + 
  scale_color_grey(name = "Spawning month", labels = c("Dec","Jan","Feb", "Mar", "Apr")) +
  theme_classic() + theme(legend.position="top")+
  facet_wrap(~Area)
dev.copy2pdf(file = "plots/Hist_PPT.pdf", width = 5.5, height = 5.5)


# =========== #
# Figure 3 ####
# =========== #
df_hs <- read.table("Output/Areas4/HatchSize_4areas.txt", header = T)
df_hs$Month <- factor(df_hs$Month, levels=c("Jan","Feb", "Mar")) # order the months
  
p1 <- ggplot(df_hs, aes(x=lmm, fill = Scenario)) + 
  #geom_density(alpha=.15, fill="#FF6666", size=2) + 
  geom_density(alpha=.5, size=0.5) + 
  labs(x = "Prewinter length (mm)", y = "Density") + 
  scale_color_discrete(name = "Size \n at hatch", labels = c("4 mm", "5 mm", "6 mm", "7 mm")) +
  theme_classic() + theme(legend.position="right")+
  facet_wrap(~Month)

p2 <- ggplot(data = df_hs, aes(x = lmm, fill=Month)) + 
  #geom_density(alpha=.15, fill="#FF6666", size=2) + 
  geom_density(alpha=.5, size=0.5) +
  labs(x ="Prewinter length (mm)", y = "Density") + 
  theme_classic() + 
  facet_wrap(~Scenario, labeller = as_labeller(c("4SL"="4 mm", "5SL"="5 mm", "6SL"="6 mm", "7SL"="7 mm"))) 

ggpubr::ggarrange(p1,p2, nrow=2, common.legend = F, labels = c("a)", "b)"))
dev.copy2pdf(file = "plots/paper_Dec2023/Figure3.pdf", width = 5, height = 7)



# =========== #
# Figure 4 ####
# =========== #

df_lf <- read.table("Output/Areas4/LowFood1Sep_4areas.txt", header = T)
df_lf <- df_lf[df_lf$Scenario!="P35",]
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

ggplot(data = lf_plot, aes(x = Month, y = Freq, col = Area)) + 
  geom_point(cex = 2.5, alpha=0.8) + labs(x = "Spawning month", y = "MR", color = "Region") + 
  theme_bw()+theme(legend.position="top")+
  facet_wrap(~Scenario, labeller = as_labeller(c("P70"="Baseline", "P52"="Low Food"))) 
dev.copy2pdf(file = "plots/paper_Jan2023/Figure4.pdf", width = 5, height = 4)



# =========== #
# Figure 5 ####
# =========== #
df1 <- read.table("Output/Areas4/Baseline1Sep_4areas.txt", header = T)
df1$Month <- factor(df1$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df1$Year <- factor(df1$Year)
df1$Area <- factor(df1$Area, levels=c("Baffin Bay","Chukchi Sea", "Laptev Sea","Svalbard"))
probjuv <- data.frame(proba = ifelse(df1$lmm>30, "yes", "no"),
                      Area = df1$Area, 
                      Month = df1$Month,
                      Year = df1$Year)
sumjuv <- table(probjuv)
juvplot <- as.data.frame(sumjuv[2,,,]/(sumjuv[1,,,]+sumjuv[2,,,]))
rm(probjuv, sumjuv) # remove not needed data

ice_all <- data.frame(Ice = df1$IceAbs15,
                      Area = df1$Area, 
                      Month = df1$Month,
                      Year = df1$Year)
df_ice <-  aggregate(Ice~Area*Month*Year, data = ice_all, mean)

# define new df_plot: Freq+Ice
df_plot <- cbind(juvplot, Ice = df_ice$Ice)


# Fit reggression on each area
library(ggformula) # needed for geom_spline
ggplot(df_plot, aes(x=Ice, y=Freq)) + 
  geom_spline(df=5, cex=1.2, alpha = 0.7) + 
  geom_point(aes(colour = Month), cex=1.4, alpha = 0.7) +
  labs(x = "Days with ice cover > 15%", y = "MR", color = "Spawning Month") + 
  theme_classic() + theme(legend.position="top")+
  facet_wrap(~Area, nrow = 2)
dev.copy2pdf(file = "plots/paper_Jan2023/Figure5.pdf", width = 6, height = 5)

# Plot for PPT #
ggplot(df_plot, aes(x=Ice, y=Freq)) + 
  geom_spline(df=5, cex=1.2, alpha = 0.7, color = "blue") + 
  geom_point(aes(colour = Month), cex=1.4, alpha = 0.7) +
  scale_colour_grey() +
  labs(x = "Days with ice cover > 15%", y = "MR", color = "Spawning Month") + 
  theme_classic() + theme(legend.position="top")+
  facet_wrap(~Area, nrow = 2)
dev.copy2pdf(file = "plots/MR.pdf", width = 6, height = 5)

# =========== #
# Figure 6 ####
# =========== #
df <- read.table("Output/EndDate1Sep/HighTempScenarios1Sep.txt")
df <- df[df$Month != "May",]
df$Month <- factor(df$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df$Year <- factor(df$Year)
Arealabel <- dplyr::recode_factor(df$Area, "NWW" = "Baffin Bay", "CUK"="Chukchi Sea",
                                  "SV"="Svalbard", "LS"="Laptev Sea")
df$Area <- Arealabel
df$Area <- factor(df$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

# estimate difference from baseline
df$Anomaly <- df$BEM_highT-df$BEM
dfm <- df[,c("Anomaly","Area","Year","Month")]
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
  xlab("Prewinter length difference (mm)")+
  ylab("Region") +
  labs(fill='') +
  scale_fill_discrete(guide = guide_legend(reverse=TRUE), labels=c('Negative', 'Positive'))+
  theme(legend.position="none")+
  theme_bw(base_size = 12)
dev.copy2pdf(file = "plots/paper_Jan2023/Figure6.pdf", width = 5, height = 3.5)

dev.copy2pdf(file = "plots/HighTemp.pdf", width = 5, height = 3.5)



# =========== #
# Figure S2 ####
# =========== #
df_plot <- read.table("Output/Areas4/Baseline1Sep_4areas.txt", header = T)
df_plot$Month <- factor(df_plot$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
df_plot$Year <- factor(df_plot$Year)
df_plot$Area <- factor(df_plot$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))


ggplot(df_plot, aes(y=lmm, x=Year, fill=Year)) + 
  geom_boxplot(color="black", alpha=0.8, width = 0.6, outlier.size = 0.7) + 
  labs(y = "Pre-winter length (mm)") + 
  theme_classic() + 
  theme(legend.position="bottom",axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  facet_wrap(~Area, nrow = 2)
dev.copy2pdf(file = "plots/paper_Jan2023/FigureS2.pdf", width = 6, height = 5)




# =========== #
# Figure S3 ####
# =========== #
# requires p_base from Figure 1
df_map <- read.table("Output/Areas4/Baseline1Sep_4areas.txt", header = T)
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
dev.copy2pdf(file = "Plots/Paper_Jan2023/FigureS2.pdf", width = 9, height = 11)



# ============ #
# Figure S4 ####
# ============ #




# =========== #
# Table 2 ####
# =========== #
df1 <- read.table("Output/Areas4/Baseline1Sep_4areas.txt", header = T)

hist(df1$trajm)
hist(df1$Bathy)
hist(df1$Temp)
hist(df1$IceAbs15)

library(dplyr)
sum1Sep <- df1 %>% group_by(Area) %>% summarize(mean = mean(lmm),
                                                sd = sd(lmm),
                                                meanT = median(Temp),
                                                sdT = sd(Temp),
                                                meanIce = median(IceAbs15),
                                                sdIce = sd(IceAbs15),
                                                meanDist = median(trajm),
                                                sdDist = sd(trajm),
                                                meanB = median(Bathy),
                                                sdB = sd(Bathy))
writexl::write_xlsx(sum1Sep,"Summary_1Sep.xlsx")

# =========== #
# Table 3 ####
# =========== #
# requires lf_plot from Figure 4
# Scenario:P70=Baseline; P52=Low-Food

# requires df_hs from Figure 3
Arealabel <- dplyr::recode_factor(df_hs$Area, "NWW" = "Baffin Bay", "CUK"="Chukchi Sea",
                                  "SV"="Svalbard", "LS"="Laptev Sea")
df_hs$Area <- Arealabel
df_hs$Area <- factor(df_hs$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

probjuv <- data.frame(proba = ifelse(df_hs$lmm>30, "yes", "no"),
                      Area = df_hs$Area, 
                      Month = df_hs$Month,
                      Year = df_hs$Year)
sumjuv <- table(probjuv)
juvhs <- as.data.frame(sumjuv[2,,,]/(sumjuv[1,,,]+sumjuv[2,,,]))
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
dfT <- read.table("Output/EndDate1Sep/HighTempScenarios1Sep.txt")
dfT <- dfT[dfT$Month != "May",]
dfT$Month <- factor(dfT$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr")) # order the months
dfT$Year <- factor(dfT$Year)
Arealabel <- dplyr::recode_factor(dfT$Area, "NWW" = "Baffin Bay", "CUK"="Chukchi Sea",
                                  "SV"="Svalbard", "LS"="Laptev Sea")
dfT$Area <- Arealabel
dfT$Area <- factor(dfT$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

probjuv <- data.frame(proba = ifelse(dfT$BEM_highT>30, "yes", "no"),
                      Area = dfT$Area, 
                      Month = dfT$Month,
                      Year = dfT$Year)
sumjuv <- table(probjuv)
juvT <- as.data.frame(sumjuv[2,,,]/(sumjuv[1,,,]+sumjuv[2,,,]))
rm(probjuv, sumjuv, dfT) # remove not needed data

library(dplyr)
sumScenarios <- cbind(lf_plot[lf_plot$Scenario=="P70",] %>% group_by(Area) %>% 
                        summarize(BaseM = round(mean(Freq), digits = 2),
                                                BaseSD = round(sd(Freq), digits = 2)),
                      juvT %>% group_by(Area) %>% 
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
                                                
writexl::write_xlsx(sumScenarios,"Table3_Summary_Scenarios.xlsx")



# =========== #
# Table 4 ####
# =========== #
# Test juvenile threshold length
# Test end date for simulations
df_jl <- rbind(read.table("ProbaJuv1Sep_4areas.txt", header = T),
               read.table("ProbaJuv15Sep_4areas.txt", header = T))
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

write.table(juvtab, "TestEndDate_JuvLength.txt")
writexl::write_xlsx(juvtab,"TestEndDate_JuvLength_Jan2023.xlsx")


