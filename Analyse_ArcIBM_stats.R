# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#    ARCIBM MODEL OUTPUT         #
#    STATISTICAL MODELS          #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Revised on Nov 2024

setwd("~/Documents/WHOI/R")

# Read data ####
df <- read.table("Output/Baseline1Sep_4areasM.txt", header = T)
# Order factors
df$Month <- factor(df$Month, levels=c("Dec","Jan","Feb", "Mar", "Apr"))
df$Year <- factor(df$Year)
df$Area <- factor(df$Area, levels=c("Baffin Bay","Chukchi Sea","Laptev Sea", "Svalbard"))

# =============== #
# NAMES IN DATA   #
# =============== #
names(df)
# Lat, Lon are position at 1 Sep, the ending of simulations
# stage: 4=juvenile, 3=larva, 2=yolk-sac larva, 1=egg
# lmm: modelled prewinter length in mm
# trajm: length of trajectory (or dispersal distance since egg release) in km
# Bathy: average bathymetry during entire dispersal
# Temp:average temperature (upper 0 - 60 m) during entire dispersal trajectories
# IcePerc: percentage of days with ice cover >15%, relative to total number of dispersal days
# IceAbs15: number of days with ice cover>15% (this is the official threshold for ice cover)
# IceAbs50: number of days with ice cover>50% 
# Area: Release (spawning) area
# Year: Simulation year
# Month: Start month
# hd : duration of embryonic development in days, from spawning till hatching


# ================== #
# ENVIRONMENTAL VARS #
# ================== #

hist(df$lmm)
hist(df$IceAbs15, breaks = 50)
hist(df$hd, breaks = 50)

hist(df$trajm)
hist(sqrt(df$trajm))

hist(df$Bathy)
hist((df$Bathy)^(1/3))

# Transform variables for normality
df$dist <- sqrt(df$trajm)
df$bat <- df$Bathy^(1/3)
df$ice <- df$IceAbs15


# ================== #
# LINEAR MODELS   ####
# ================== #

# ONE FACTOR
mod1 <- lm(lmm~ice, data=df) # 1st best
summary(mod1)
mod2 <- lm(lmm~dist, data=df)
summary(mod2)
mod3 <- lm(lmm~bat, data=df)
summary(mod3)
mod4 <- lm(lmm~hd, data=df) # 2nd best
summary(mod4)
AIC(mod1,mod2,mod3,mod4)


# TWO FACTORS
mod1 <- lm(lmm~ice+hd, data=df) # 1st best
summary(mod1)
mod2 <- lm(lmm~ice+dist, data=df)
summary(mod2)
mod3 <- lm(lmm~ice+bat, data=df) # 2nd best
summary(mod3)
mod4 <- lm(lmm~hd+dist, data=df)
summary(mod4)
mod5 <- lm(lmm~hd+bat, data=df)
summary(mod5)
mod6 <- lm(lmm~bat+dist, data=df)
summary(mod6)
AIC(mod1,mod2,mod3,mod4,mod5,mod6)

# TWO FACTORS with interactions
mod1 <- lm(lmm~ice*hd, data=df) # 1st best
summary(mod1)
mod2 <- lm(lmm~ice*dist, data=df)
summary(mod2)
mod3 <- lm(lmm~ice*bat, data=df) # 2nd best
summary(mod3)
mod4 <- lm(lmm~hd*dist, data=df)
summary(mod4)
mod5 <- lm(lmm~hd*bat, data=df)
summary(mod5)
mod6 <- lm(lmm~bat*dist, data=df)
summary(mod6)
AIC(mod1,mod2,mod3,mod4,mod5,mod6)


# 3 FACTORS
mod2 <- lm(lmm~ice+hd+dist, data=df) # 2nd best, R2=0.71
summary(mod2)
mod3 <- lm(lmm~ice+dist+bat, data=df)
summary(mod3)
mod4 <- lm(lmm~ice+hd+bat, data=df) # 1nd best, R2=0.72
summary(mod4)
mod5 <- lm(lmm~hd+dist+bat, data=df)
summary(mod3)
AIC(mod1,mod2,mod3,mod4,mod5)


# FULL MODEL
mod1 <- lm(lmm~ice+hd+dist+bat, data=df) # R2=0.73
summary(mod1)
AIC(mod1)


# LM interactions ####
# With / without interactions : signif diff
mod1 <- lm(lmm~ice+hd, data=df) # 1st best
mod2 <- lm(lmm~ice*hd, data=df)
AIC(mod1,mod2)
summary(mod1) # R2=0.6564
summary(mod2) # R2=0.6636

mod3 <- lm(lmm~ice+bat, data=df) # 2nd best
mod4 <- lm(lmm~ice*bat, data=df)
AIC(mod3,mod4)
summary(mod4)



# =============== #
# Mixed models ####
# =============== #
# LME
library(nlme)

mod1 <- lme(lmm~ice, random=~1|Year/Month, method="REML", data=df)
mod2 <- lme(lmm~ice, random=~1|Area/Month, method="REML", data=df) # best model
mod3 <- lme(lmm~ice, random=~1|Area/Year, method="REML", data=df)
mod4 <- lme(lmm~ice, random=~1|Month, method="REML", data=df)
mod5 <- lme(lmm~ice, random=~1|Area, method="REML", data=df)
mod6 <- lme(lmm~ice, random=~1|Year, method="REML", data=df)
AIC(mod1, mod2, mod3, mod4,mod5,mod6)


mod1 <- lme(lmm~hd, random=~1|Year/Month, method="REML", data=df)
mod2 <- lme(lmm~hd, random=~1|Area/Month, method="REML", data=df) # best model
mod3 <- lme(lmm~hd, random=~1|Area/Year, method="REML", data=df)
mod4 <- lme(lmm~hd, random=~1|Month, method="REML", data=df)
#mod5 <- lme(lmm~hd, random=~1|Area, method="REML", data=df) # convergence error
mod6 <- lme(lmm~hd, random=~1|Year, method="REML", data=df)
AIC(mod,mod1, mod2, mod3, mod4,mod6)


# Random effects on 2-factor model
mod <- lm(lmm~ice*hd, data=df) # initial linear model
mod1 <- lme(lmm~ice*hd, random=~1|Year/Month, method="REML", data=df)
mod2 <- lme(lmm~ice*hd, random=~1|Area/Month, method="REML", data=df)
mod3 <- lme(lmm~ice+hd, random=~1|Area/Year, method="REML", data=df) #convergence error, use additive model
mod4 <- lme(lmm~ice*hd, random=~1|Year, method="REML", data=df)
mod5 <- lme(lmm~ice*hd, random=~1|Month, method="REML", data=df)
mod6 <- lme(lmm~ice*hd, random=~1|Area, method="REML", data=df)
AIC(mod, mod1, mod2, mod3, mod4,mod5,mod6)

# Random effects on 3-factor model
mod <- lm(lmm~ice+bat+hd, data=df)
mod1 <- lme(lmm~ice+bat+hd, random=~1|Year/Month, method="REML", data=df)
mod2 <- lme(lmm~ice+bat+hd, random=~1|Area/Month, method="REML", data=df) # 1st best
mod3 <- lme(lmm~ice+bat+hd, random=~1|Area/Year, method="REML", data=df)
mod4 <- lme(lmm~ice+bat+hd, random=~1|Year, method="REML", data=df)
mod5 <- lme(lmm~ice+bat+hd, random=~1|Month, method="REML", data=df)
mod6 <- lme(lmm~ice+bat+hd, random=~1|Area, method="REML", data=df)
AIC(mod, mod1, mod2, mod3, mod4,mod5,mod6)


# Random effects on 4-factor model
mod <- lm(lmm~ice+dist+hd+bat, data=df)
mod1 <- lme(lmm~ice+dist+hd+bat, random=~1|Year/Month, method="REML", data=df)
mod2 <- lme(lmm~ice+dist+hd+bat, random=~hd|Area/Month, method="REML", data=df) # covergence error
mod3 <- lme(lmm~ice+dist+hd+bat, random=~1|Area/Year, method="REML", data=df)
mod4 <- lme(lmm~ice+dist+hd+bat, random=~1|Year, method="REML", data=df)
mod5 <- lme(lmm~ice+dist+hd+bat, random=~1|Month, method="REML", data=df)
mod6 <- lme(lmm~ice+dist+hd+bat, random=~1|Area, method="REML", data=df)
AIC(mod, mod1, mod2, mod3, mod4,mod5,mod6)
anova.lme(mod2, mod5)
summary(mod2)
plot(mod2)



# ====================== #
# LM with life traits ####
# ====================== #
# ice and hd(edd)
mod <- lm(lmm~hd*Month, data=df)
summary(mod)
mod1 <- lm(lmm~ice*hd, data=df) # Initial model
mod2 <- lm(lmm~ice*hd+Month, data=df) 
mod3 <- lm(lmm~ice*hd+Year, data=df) 
mod4 <- lm(lmm~ice*hd+Area, data=df) 
mod5 <- lm(lmm~ice+hd*Month, data=df) # *** 3rd best
mod6 <- lm(lmm~ice*Month + hd*Month, data=df) # *** 2nd best
mod7 <- lm(lmm~ice*Area + hd*Area, data=df) 
mod8 <- lm(lmm~ice*Month + hd*Area, data=df) 
mod9 <- lm(lmm~ice*Area + hd*Month, data=df) # *** 1st best
summary(mod9)
AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9)

# 4-factor
mod1 <- lm(lmm~ice*Area+hd*Month+bat+dist, data=df) # 1
mod11 <- lm(lmm~ice+Area+hd+Month+bat+dist, data=df)
mod12 <- lm(lmm~ice+Area+hd+bat*Month+dist, data=df)
mod13 <- lm(lmm~ice+Area+hd+bat+dist*Month, data=df)
mod14 <- lm(lmm~ice+hd+bat*Area+dist+Month, data=df)
mod15 <- lm(lmm~ice+hd+bat+dist*Area+Month, data=df)
AIC(mod1,mod11,mod12,mod13,mod14,mod15)

# 3-factor
mod2 <- lm(lmm~ice*Area+hd*Month+bat, data=df) # 2
mod22 <- lm(lmm~ice+Area+hd+Month+bat, data=df)
mod23 <- lm(lmm~ice+Area+hd+bat*Month, data=df)
mod24 <- lm(lmm~ice+hd+Month+bat*Area, data=df)
mod25 <- lm(lmm~ice+hd*Month+bat*Area, data=df)
AIC(mod2,mod22,mod23,mod24,mod25)

# 2-factor
mod3 <- lm(lmm~ice*Area+hd*Month, data=df) # 3
mod31 <- lm(lmm~ice+Area+hd+Month, data=df) 
mod32 <- lm(lmm~ice*Month+hd*Month, data=df)
mod33 <- lm(lmm~ice*Month+hd*Area, data=df) 
AIC(mod3,mod31,mod32,mod33)

mod4 <- lm(lmm~ice*Month+hd, data=df) 
mod44 <- lm(lmm~ice+hd*Month, data=df) 

mod5 <- lm(lmm~hd*Month, data=df) 

# Models with or without interactions
AIC(mod1,mod11,mod2,mod22,mod3,mod31,mod4,mod44,mod5)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod44)
summary(mod5)
anova(mod1,mod55)
plot(mod1)



# ======== #
# ANOVA ####
# ======== #
# Hatch size ####
df_hd <- read.table("Output/HatchSize1Sep_4areasM.txt", header = T)
df_hd$Year <- factor(df_hd$Year)
# Reduced dataset
# Prewinter length ~ Scenario
df_lmm <-  aggregate(lmm~Area*Month*Year*Scenario, data = df_hd, mean)
hist(df_lmm$lmm)
res <- bartlett.test(lmm ~ Scenario, data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario,Area), data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario,Year), data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario, Month), data = df_lmm)
res

res.aov <- aov(lmm ~ Scenario, data = df_lmm)
res.aov <- aov(lmm ~ Scenario*Area, data = df_lmm)
res.aov <- aov(lmm ~ Scenario*Year, data = df_lmm)
res.aov <- aov(lmm ~ Scenario*Month, data = df_lmm)
summary(res.aov)

# Recruitment rate ~ Scenario
# Probability to reach juvenile (>30 mm)
probhs <- data.frame(proba = ifelse(df_hs$lmm>30, "yes", "no"),
                     HL = df_hs$Scenario,
                     Area = df_hs$Area, 
                     Month = df_hs$Month,
                     Year = df_hs$Year)
sumjuv <- table(probhs)
hs_plot <- as.data.frame(sumjuv[2,,,,]/(sumjuv[1,,,,]+sumjuv[2,,,,]))
rm(probhs, sumjuv)

kruskal.test(Freq ~ HL, data = hs_plot)


# Low-food scenario ####
# Reduced dataset
df_lf <- read.table("Output/LowFood1Sep_4areasM.txt", header = T)
df_lf$Year <- factor(df_lf$Year)

# Prewinter length ~ Scenario
df_lmm <-  aggregate(lmm~Area*Month*Year*Scenario, data = df_lf, mean)
hist(df_lmm$lmm)
res <- bartlett.test(lmm ~ Scenario, data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario,Area), data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario,Year), data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario, Month), data = df_lmm)
res

res.aov <- aov(lmm ~ Scenario, data = df_lmm)
summary(res.aov)
kruskal.test(lmm ~ Scenario, data = df_lmm)

# Recruitment rate ~ Scenario
# Probability to reach juvenile (>30 mm)
probhs <- data.frame(proba = ifelse(df_lf$lmm>30, "yes", "no"),
                     HL = df_lf$Scenario,
                     Area = df_lf$Area, 
                     Month = df_lf$Month,
                     Year = df_lf$Year)
sumjuv <- table(probhs)
hs_plot <- as.data.frame(sumjuv[2,,,,]/(sumjuv[1,,,,]+sumjuv[2,,,,]))
rm(probhs, sumjuv)

kruskal.test(Freq ~ HL, data = hs_plot)


# High-Temp ####
df_t <- read.table("Output/HighTemp1Sep_4areasM.txt", header = T)
df_t$Year <- factor(df_t$Year)

dfm <- reshape::melt(df_t, id = c("Area", "Year", "Month"))
names(dfm) <- c("Area", "Year", "Month", "Scenario", "lmm")


# Prewinter length ~ Scenario
df_lmm <-  aggregate(lmm~Area*Month*Year*Scenario, data = dfm, mean)
hist(df_lmm$lmm)
res <- bartlett.test(lmm ~ Scenario, data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario,Area), data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario,Year), data = df_lmm)
res <- bartlett.test(lmm ~ interaction(Scenario, Month), data = df_lmm)
res

res.aov <- aov(lmm ~ Scenario, data = df_lmm)
res.aov <- aov(lmm ~ Scenario*Area, data = df_lmm)
res.aov <- aov(lmm ~ Scenario*Year, data = df_lmm)
res.aov <- aov(lmm ~ Scenario*Month, data = df_lmm)
summary(res.aov)


# Recruitment rate ~ Scenario
# Probability to reach juvenile (>30 mm)
probhs <- data.frame(proba = ifelse(dfm$lmm>30, "yes", "no"),
                     HL = dfm$Scenario,
                     Area = dfm$Area, 
                     Month = dfm$Month,
                     Year = dfm$Year)
sumjuv <- table(probhs)
hs_plot <- as.data.frame(sumjuv[2,,,,]/(sumjuv[1,,,,]+sumjuv[2,,,,]))
rm(probhs, sumjuv)

kruskal.test(Freq ~ HL, data = hs_plot)

