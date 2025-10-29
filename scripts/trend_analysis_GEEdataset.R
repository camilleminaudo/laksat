# LAKSAT project
# Camille

# Description
# this scripts loads GEE outputs and calcualte color indices


rm(list = ls()) # clear workspace
cat("/014") # clear console

# ---- packages ----
library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(grid)
library(egg)
require(dplyr)
library(sf)
library(data.table)
library(readxl)
library(colorscience)
library(trend)
library(data.table)


# --------- Functions --------- 


get_sens.slope <- function(df_select, var_name, smooth.it){
  
  if(missing(smooth.it)){smooth.it = F}
  # aggregate
  Annual.Ave <- aggregate(x = df_select, by = list(year(df_select$date)),
                          mean, na.rm = TRUE)
  
  if (smooth.it){
    mysmooth <-  smooth.spline(Annual.Ave[,1], Annual.Ave[,2], spar = 0.7)
    my_ts <- ts(data = mysmooth$y, start = min(Annual.Ave$Group.1), end = max(Annual.Ave$Group.1), frequency = 1)
  } else {
    my_ts <- ts(data = Annual.Ave[[var_name]], start = min(Annual.Ave$Group.1), end = max(Annual.Ave$Group.1), frequency = 1)
  }
  
  sens.slope <- sens.slope(my_ts, conf.level = 0.95)
  
  return(sens.slope)
}



get_aggregated_stats <- function(df_color, sat_code){
  df_color_summer <- df_color[which(month(df_color$date)>5 & month(df_color$date)<11),]
  
  df_lake <- NULL
  for(id in unique(df_color_summer$ID)){
    color_id <- df_color_summer[which(df_color_summer$ID==id),]
    
    # ggplot(color_id, aes(date, dwl))+geom_point()+theme_article()
    
    # ggplot(color_id, aes(s_star, dwl))+geom_point()+theme_article()
    
    if(dim(color_id)[1]>30){
      
      mysens_slope <- get_sens.slope(df_select = color_id, var_name = "dwl", smooth.it = F)
      
      trend_type <- "no"
      if(abs(mysens_slope$statistic) >= 1.96 & mysens_slope$estimates < 0){trend_type <- "-"}
      if(abs(mysens_slope$statistic) >= 1.96 & mysens_slope$estimates > 0){trend_type <- "+"}
      
      
      df_lake <- rbind(df_lake,
                       data.frame(satellite = sat_code,
                                  ID = color_id$ID[1],
                                  elevation_masl  = color_id$ALTITUDE_M[1],
                                  catchmt_area_ha = color_id$CATCHMT_HA[1],
                                  depth_max_m = color_id$MAXDEPTH_M[1],
                                  lake_size_ha = color_id$SIZE_HA[1],
                                  wrt_months = color_id$WRT_MONTHS[1],
                                  n_summer = dim(color_id)[1],
                                  dwl_med = median(color_id$dwl),
                                  dwl_sd = sd(color_id$dwl),
                                  s_star_med = median(color_id$dwl),
                                  s_star_sd = sd(color_id$dwl),
                                  trend_type = trend_type,
                                  sens_slope = mysens_slope$estimates,
                                  sens_slope.Z = mysens_slope$statistic))
    }
  }
  return(df_lake)
}






# --------- Load datasets --------- 
setwd("C:/Projects/myGit/laksat/results")



# ---------------- landsat 5 -----------------------


color_l5 <- read.csv(file = "color_landsat5.csv")
color_l5$date <- as.Date(color_l5$date)
color_l5$doy <- as.numeric(strftime(color_l5$date, format = "%j"))

# ggplot(color_l5, aes(doy, dwl))+geom_point()+theme_article()
trends_l5 <- get_aggregated_stats(df_color = color_l5, sat_code = "L5")



# ---------------- landsat 7 -----------------------

color_l7 <- read.csv(file = "color_landsat7.csv")
color_l7$date <- as.Date(color_l7$date)
color_l7$doy <- as.numeric(strftime(color_l7$date, format = "%j"))

# ggplot(color_l7, aes(doy, dwl))+geom_point()+theme_article()

trends_l7 <- get_aggregated_stats(df_color = color_l7, sat_code = "L7")

ggplot(trends_l7, aes(dwl_med))+geom_density()+theme_article()+ggtitle("LANDSAT 7, June-October")

ggplot(trends_l7, aes(dwl_med, elevation_masl, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+ggtitle("LANDSAT 7, June-October")+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 7, June-October")

ggplot(trends_l7, aes(dwl_med, dwl_sd, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 7, June-October")

table(df_lake$trend_type)






# ---------------- landsat 8 -----------------------


color_l8 <- read.csv(file = "color_landsat8.csv")
color_l8$date <- as.Date(color_l8$date)
color_l8$doy <- as.numeric(strftime(color_l8$date, format = "%j"))

# ggplot(color_l8, aes(doy, dwl))+geom_point()+theme_article()
trends_l8 <- get_aggregated_stats(df_color = color_l8, sat_code = "L8")


ggplot(trends_l8, aes(dwl_med))+geom_density()+theme_article()+ggtitle("LANDSAT 8, June-October")


ggplot(trends_l8, aes(dwl_med, dwl_sd, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 8, June-October")


ggplot(trends_l8, aes(dwl_med, elevation_masl, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+ggtitle("LANDSAT 8, June-October")+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 8, June-October")





# ---------------- landsat 9 -----------------------

color_l9 <- read.csv(file = "color_landsat9.csv")
color_l9$date <- as.Date(color_l9$date)
color_l9$doy <- as.numeric(strftime(color_l9$date, format = "%j"))

# ggplot(color_l9, aes(doy, dwl))+geom_point()+theme_article()
trends_l9 <- get_aggregated_stats(df_color = color_l9, sat_code = "L9")


ggplot(trends_l9, aes(dwl_med))+geom_density()+theme_article()+ggtitle("LANDSAT 9, June-October")


ggplot(trends_l9, aes(dwl_med, dwl_sd, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 9, June-October")


ggplot(trends_l9, aes(dwl_med, elevation_masl, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+ggtitle("LANDSAT 9, June-October")+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 9, June-October")






# ---------------- Sentinel 2 -----------------------

color_s2 <- read.csv(file = "color_sentinel2.csv")
color_s2$date <- as.Date(color_s2$date)
color_s2$doy <- as.numeric(strftime(color_s2$date, format = "%j"))

# ggplot(color_s2, aes(doy, dwl))+geom_point()+theme_article()
trends_s2 <- get_aggregated_stats(df_color = color_s2, sat_code = "s2")


ggplot(trends_s2, aes(dwl_med))+geom_density()+theme_article()+ggtitle("SENTINEL 2, June-October")


ggplot(trends_s2, aes(dwl_med, dwl_sd, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("SENTINEL 2, June-October")


ggplot(trends_s2, aes(dwl_med, elevation_masl, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+ggtitle("SENTINEL 2, June-October")+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("SENTINEL 2, June-October")


# ---------------- ALL SATELLITES -----------------------

df_all <- rbind(trends_l5, trends_l7, trends_l8, trends_l9, trends_s2)

ggplot(df_all, aes(dwl_med, fill = satellite))+geom_density(alpha=0.5)+theme_article()

ggplot(df_all, aes(s_star_med, fill = satellite))+geom_density(alpha=0.5)+theme_article()

ggplot(df_all, aes(dwl_med, dwl_sd, colour = satellite))+geom_point(size=3, alpha=0.6)+theme_article()+
  ggtitle("ALL SATELLITES, June-October")+facet_wrap(satellite~.)


ggplot(df_all, aes(ID, dwl_med, colour = satellite))+geom_point(size=3, alpha=0.6)+theme_article()+
  ggtitle("ALL SATELLITES, June-October")


df_all_sprd_dwl_med <- spread(df_all[,c("ID","satellite","dwl_med")], satellite, dwl_med)

ggplot(df_all_sprd_dwl_med, aes(L5, L7))+geom_point()+theme_article()+geom_abline(slope=1)
ggplot(df_all_sprd_dwl_med, aes(L7, L8))+geom_point()+theme_article()+geom_abline(slope=1)
ggplot(df_all_sprd_dwl_med, aes(L8, L9))+geom_point()+theme_article()+geom_abline(slope=1)
ggplot(df_all_sprd_dwl_med, aes(L8, s2))+geom_point()+theme_article()+geom_abline(slope=1)
ggplot(df_all_sprd_dwl_med, aes(L9, s2))+geom_point()+theme_article()+geom_abline(slope=1)
ggplot(df_all_sprd_dwl_med, aes(L7, s2))+geom_point()+theme_article()+geom_abline(slope=1)




