source("1-load_data.r")

library(dplyr)   # for data handling: %>%, mutate, filter
library(ggplot2) # for nice graphics: ggplot
library(tidyr)   # for data handling: gather, spread

# merge tow and gis 
all_strata <- merge(gis, tow, by=c("Cruise", "Station"))

# create Season variable
# check for which months are in data set
unique(all_strata$Mon) # should be only 2-6 and 9-12
all_season <- mutate(all_strata, 
                     Season = factor(ifelse(Mon <= 7, "Spring", "Fall"), levels=c("Spring", "Fall"))) %>%
  select(-CatchNum, -CatchWt, -ID) # drop these because catches all zeros and ID will be used later

# exploratory plots for relationship between wingspread and doorspread
wdplot <- ggplot(all_season, aes(x=MEAN_DOOR_SPREAD_METERS, y=MEAN_WING_SPRD_METERS, color=Season)) +
  geom_point(alpha=0.2, na.rm=TRUE) +
  facet_wrap(~Year) + 
  geom_hline(yintercept = 6, linetype = 2) +
  geom_vline(xintercept = 6, linetype = 2) +
  theme_bw()
print(wdplot)
ggsave("output/wdplot1_door_vs_wing_spread_initial.png")

# have some problems with wing and door spread of close to zero - replace with NA
# use these cutoff values, no strong reason for them other than looking at the plot
min_door_area <- 0.02
min_wing_area <- 0.01
min_wing_spread <- 6
min_door_spread <- 6
all_season <- all_season %>%
  mutate(AREA_SWEPT_DOORS_MEAN_KM2 = replace(AREA_SWEPT_DOORS_MEAN_KM2, 
                                             AREA_SWEPT_DOORS_MEAN_KM2 < min_door_area, NA),
         AREA_SWEPT_WINGS_MEAN_KM2 = replace(AREA_SWEPT_WINGS_MEAN_KM2, 
                                             AREA_SWEPT_WINGS_MEAN_KM2 < min_wing_area, NA),
         AREA_SWEPT_WINGS_MEAN_KM2 = replace(AREA_SWEPT_WINGS_MEAN_KM2, 
                                             MEAN_WING_SPRD_METERS < min_wing_spread, NA),
         MEAN_WING_SPRD_METERS = replace(MEAN_WING_SPRD_METERS, 
                                         MEAN_WING_SPRD_METERS < min_wing_spread, NA),
         AREA_SWEPT_DOORS_MEAN_KM2 = replace(AREA_SWEPT_DOORS_MEAN_KM2, 
                                             MEAN_DOOR_SPREAD_METERS < min_door_spread, NA),
         MEAN_DOOR_SPREAD_METERS = replace(MEAN_DOOR_SPREAD_METERS, 
                                           MEAN_DOOR_SPREAD_METERS < min_door_spread, NA) )

wdplot2 <- ggplot(all_season, aes(x=MEAN_DOOR_SPREAD_METERS, y=MEAN_WING_SPRD_METERS, color=Season)) +
  geom_point(alpha=0.2, na.rm=TRUE) +
  facet_wrap(~Year) + 
  theme_bw()
print(wdplot2)
ggsave("output/wdplot2_door_vs_wing_spread_cleaned.png")

# density plot of tow area
area_swept_wings_distibution <- ggplot(all_season, aes(AREA_SWEPT_WINGS_MEAN_KM2)) +
  geom_histogram(binwidth = 0.0005, na.rm=TRUE) +
  geom_density(aes(y=0.0005 * ..count..), na.rm = TRUE) +
  geom_vline(xintercept = standard_wing_swept_area, linetype=2) +
  theme_bw()
print(area_swept_wings_distibution)
ggsave("output/area_swept_wings_distibution.png")

summary(all_season$AREA_SWEPT_WINGS_MEAN_KM2) # need to fill 504 NAs

# create flag for using original wing spread data
all_season <- mutate(all_season, orig_wing_spread_flag = ifelse(is.na(AREA_SWEPT_WINGS_MEAN_KM2),0,1))
sum(all_season$orig_wing_spread_flag)

# fill missing with door area relationship first

# figure out which scenario has lowest AIC
f0 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS"
f1 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Season"
f2 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + as.factor(Year)"
f3 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Depth"
f4 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Season + as.factor(Year)"
f5 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Season * as.factor(Year)"
f6 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Season + Depth"
f7 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + as.factor(Year) + Depth"
f8 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Season + as.factor(Year) + Depth"
f9 <- "MEAN_WING_SPRD_METERS ~ MEAN_DOOR_SPREAD_METERS + Season * as.factor(Year) + Depth"

model0 <- glm(f0, data = all_season)
model1 <- glm(f1, data = all_season)
model2 <- glm(f2, data = all_season)
model3 <- glm(f3, data = all_season)
model4 <- glm(f4, data = all_season)
model5 <- glm(f5, data = all_season)
model6 <- glm(f6, data = all_season)
model7 <- glm(f7, data = all_season)
model8 <- glm(f8, data = all_season)
model9 <- glm(f9, data = all_season)

s0 <- summary(model0)
s1 <- summary(model1)
s2 <- summary(model2)
s3 <- summary(model3)
s4 <- summary(model4)
s5 <- summary(model5)
s6 <- summary(model6)
s7 <- summary(model7)
s8 <- summary(model8)
s9 <- summary(model9)

aics <- c(s0$aic, s1$aic, s2$aic, s3$aic, s4$aic, s5$aic, s6$aic, s7$aic, s8$aic, s9$aic)
fxns <- c(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)
aic_table <- data.frame(Model = c(0:9),
                        AIC = aics, 
                        "Delta AIC" = aics - min(aics),
                        Definition = fxns)
aic_table 
write.csv(aic_table, "output/aic_table.csv", row.names = FALSE)

# make data frames of predicted values for best model (model 9)
p0 <- data.frame(MEAN_DOOR_SPREAD_METERS = seq(20, 45),
                 MEAN_WING_SPRD_METERS = predict(model0, 
                                                 data.frame(MEAN_DOOR_SPREAD_METERS = seq(20, 45))))
p9 <- data.frame(predict(model9))
all_season$p0 <- predict(model0, all_season)
all_season$p9 <- predict(model9, all_season)

# how different are they really? not much
wdplot3 <- wdplot2 +
  geom_point(data=all_season, 
             aes(x=MEAN_DOOR_SPREAD_METERS, y=p0), color="black", size=0.5, na.rm=TRUE) +
  geom_point(data=filter(all_season, Season=="Spring"), 
             aes(x=MEAN_DOOR_SPREAD_METERS, y=p9), color="red", size=0.5, na.rm=TRUE) +
  geom_point(data=filter(all_season, Season=="Fall"), 
             aes(x=MEAN_DOOR_SPREAD_METERS, y=p9), color="green", size=0.5, na.rm=TRUE)
print(wdplot3)
ggsave("output/wdplot3_compare_fits_with_data.png")

wdplot4 <-   ggplot(data=all_season, aes(x=MEAN_DOOR_SPREAD_METERS, y=p0)) +
  geom_point(color="black", size=0.5, na.rm=TRUE) +
  geom_point(data=filter(all_season, Season=="Spring"), 
             aes(x=MEAN_DOOR_SPREAD_METERS, y=p9), color="red", size=0.5, na.rm=TRUE) +
  geom_point(data=filter(all_season, Season=="Fall"), 
             aes(x=MEAN_DOOR_SPREAD_METERS, y=p9), color="green", size=0.5, na.rm=TRUE) +
  facet_wrap(~Year) +
  ylab("MEAN_WING_SPRD_METERS") +
  theme_bw()
print(wdplot4)
ggsave("output/wdplot4_compare_fits_without_data.png")

# is tow distance a function of depth? No
all_season <- all_season %>%
  mutate(tow_distance = 1000 * AREA_SWEPT_WINGS_MEAN_KM2 / MEAN_WING_SPRD_METERS)
tow_distance_vs_depth <- ggplot(all_season, aes(x=Depth, y=tow_distance, color=Season)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  facet_wrap(~Year) +
  geom_hline(yintercept = 1.852, linetype = 2) +
  theme_bw()
print(tow_distance_vs_depth)
ggsave("output/tow_distance_vs_depth.png")

# so fill values with wing spread : door spread model 9
door_fill <- all_season %>% 
  mutate(Area_Swept_Wings_Fill_Doorspread_Flag = 
           ifelse(is.na(AREA_SWEPT_WINGS_MEAN_KM2),
                  ifelse(!is.na(AREA_SWEPT_DOORS_MEAN_KM2), 1, 0), 0)) %>%
  mutate(Area_Swept_Wings_Fill_Doorspread = 
           ifelse(Area_Swept_Wings_Fill_Doorspread_Flag == 1, 
                  p9 * 1.852 / 1000, NA))
filter(door_fill, Area_Swept_Wings_Fill_Doorspread_Flag == 1)  # check to see it worked

wdplot7 <- wdplot2 +
  geom_point(data=filter(door_fill, Area_Swept_Wings_Fill_Doorspread_Flag == 1), 
             aes(x=MEAN_DOOR_SPREAD_METERS, y=p9), color="black")
print(wdplot7)  # variable number of holes filled this way by year (as seen in tow_na_table.csv)
ggsave("output/wdplot7_wdplot2_with_doorfills.png")

wdplot8 <- ggplot(door_fill, aes(x=AREA_SWEPT_DOORS_MEAN_KM2, y=AREA_SWEPT_WINGS_MEAN_KM2, color=Season)) +
  geom_point(alpha=0.2, na.rm=TRUE) +
  geom_point(aes(x=AREA_SWEPT_DOORS_MEAN_KM2, y=Area_Swept_Wings_Fill_Doorspread), 
             color="black", na.rm=TRUE) +
  facet_wrap(~Year) +
  theme_bw()
print(wdplot8)
ggsave("output/wdplot8_doorarea_vs_wingarea.png")

# fill remaining missing values with standard wing spread area
global_fill <- door_fill %>%
  mutate(global_fill_flag = ifelse((orig_wing_spread_flag == 0) & (Area_Swept_Wings_Fill_Doorspread_Flag == 0), 1, 0))
global_fill <- global_fill %>%
  mutate(Area_Swept_Wings_Fill_Global = ifelse(global_fill_flag == 1, standard_wing_swept_area, 0))
head(global_fill)

final_tow <- global_fill %>%
  mutate(wing_area_swept = pmax(AREA_SWEPT_WINGS_MEAN_KM2,
                                Area_Swept_Wings_Fill_Doorspread,
                                Area_Swept_Wings_Fill_Global,
                                na.rm = TRUE)) %>%
  mutate(wing_area_swept_type = ifelse(orig_wing_spread_flag == 1, "Measured",
                                       ifelse(Area_Swept_Wings_Fill_Doorspread_Flag == 1, "DoorFill",
                                              ifelse(global_fill_flag == 1, "GlobalFill", "Empty"))))

# make table of replacements by survey and type
fill_table_seasonyeartype <- final_tow %>%
  group_by(Season, Year, wing_area_swept_type) %>%
  summarise(n = n()) %>%
  spread(wing_area_swept_type, n, fill = 0) 
fill_table_seasonyear <- final_tow %>%
  group_by(Season, Year) %>%
  summarise(Stations = n())
fill_table_combined <- merge(fill_table_seasonyeartype, fill_table_seasonyear, by=c("Season", "Year"))
fill_table_sum <- data.frame(Season = "Both",
                             Year = "All",
                             DoorFill = sum(fill_table_combined$DoorFill),
                             GlobalFill = sum(fill_table_combined$GlobalFill),
                             Measured = sum(fill_table_combined$Measured),
                             Stations = sum(fill_table_combined$Stations) )
fill_table <- rbind(fill_table_combined, fill_table_sum) %>%
  mutate(sumcheck = Stations - Measured - DoorFill - GlobalFill) # sumcheck should be zero if all is good
write.csv(fill_table,"output/fill_table.csv", row.names = FALSE)

