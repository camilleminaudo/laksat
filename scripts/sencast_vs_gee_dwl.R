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


# ---------------- functions -----------------------



getFUI <- function(vectDWL){
  
  ## Code for connnecting dWL to forel-ule index 
  fui.lookup <- tibble(dWL = c(471:583), fui = NA)
  
  fui.lookup$fui[fui.lookup$dWL <= 583] = 21
  fui.lookup$fui[fui.lookup$dWL <= 581] = 20
  fui.lookup$fui[fui.lookup$dWL <= 579] = 19
  fui.lookup$fui[fui.lookup$dWL <= 577] = 18
  fui.lookup$fui[fui.lookup$dWL <= 575] = 17
  fui.lookup$fui[fui.lookup$dWL <= 573] = 16
  fui.lookup$fui[fui.lookup$dWL <= 571] = 15
  fui.lookup$fui[fui.lookup$dWL <= 570] = 14
  fui.lookup$fui[fui.lookup$dWL <= 569] = 13
  fui.lookup$fui[fui.lookup$dWL <= 568] = 12
  fui.lookup$fui[fui.lookup$dWL <= 567] = 11
  fui.lookup$fui[fui.lookup$dWL <= 564] = 10
  fui.lookup$fui[fui.lookup$dWL <= 559] = 9
  fui.lookup$fui[fui.lookup$dWL <= 549] = 8
  fui.lookup$fui[fui.lookup$dWL <= 530] = 7
  fui.lookup$fui[fui.lookup$dWL <= 509] = 6
  fui.lookup$fui[fui.lookup$dWL <= 495] = 5
  fui.lookup$fui[fui.lookup$dWL <= 489] = 4
  fui.lookup$fui[fui.lookup$dWL <= 485] = 3
  fui.lookup$fui[fui.lookup$dWL <= 480] = 2
  fui.lookup$fui[fui.lookup$dWL <= 475 & fui.lookup$dWL >470] = 1
  
  
  fui <- fui.lookup[as.vector(sapply(vectDWL,function(x) which.min(abs(x - fui.lookup$dWL)))), 'fui']
  
  return(as.vector(fui$fui))
}

# Actual Forel-Ule Colors
fui.colors <- c(
  "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660","#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04")





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



get_aggregated_stats <- function(df_color){
  df_color_summer <- df_color[which(month(df_color$date)>5 & month(df_color$date)<11),]
  
  df_lake <- NULL
  for(id in unique(df_color_summer$ID)){
    color_id <- df_color_summer[which(df_color_summer$ID==id),]
    
    # ggplot(color_id, aes(date, dwl_nm))+geom_point()+theme_article()
    
    # ggplot(color_id, aes(s_star, dwl))+geom_point()+theme_article()
    
    if(dim(color_id)[1]> 5){
      
      mysens_slope <- get_sens.slope(df_select = color_id, var_name = "dwl_nm", smooth.it = F)
      
      trend_type <- "no"
      if(abs(mysens_slope$statistic) >= 1.96 & mysens_slope$estimates < 0){trend_type <- "-"}
      if(abs(mysens_slope$statistic) >= 1.96 & mysens_slope$estimates > 0){trend_type <- "+"}
      
      
      df_lake <- rbind(df_lake,
                       data.frame(satellite = "SENTINEL 2",
                                  ID = color_id$ID[1],
                                  n_summer = dim(color_id)[1],
                                  
                                  dwl_med = median(color_id$dwl),
                                  dwl_sd = sd(color_id$dwl),
                                 
                                  trend_type = trend_type,
                                  sens_slope = mysens_slope$estimates,
                                  sens_slope.Z = mysens_slope$statistic))
    }
  }
  return(df_lake)
}



# ---------------- load  data -----------------------

setwd("C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/LAKeSAT/sencast")
tab_highqual <- read.csv(file = "DWL_all_chebychev.csv")
tab_highqual_bands <- read.csv(file = "High_quality_data_center_POLY.csv")

unique(tab_highqual_bands$year)

tab_highqual$date <- ""
tab_highqual$date[tab_highqual$year == "2016"] <- "2016-09-28"
tab_highqual$date[tab_highqual$year == "2017"] <- "2017-09-13"
tab_highqual$date[tab_highqual$year == "2018"] <- "2018-09-23"
tab_highqual$date[tab_highqual$year == "2019"] <- "2019-09-08"
tab_highqual$date[tab_highqual$year == "2020"] <- "2020-09-02"
tab_highqual$date[tab_highqual$year == "2021"] <- "2021-08-28"
tab_highqual$date[tab_highqual$year == "2022"] <- "2022-09-17"
tab_highqual$date[tab_highqual$year == "2023_08"] <- "2023-08-08"
tab_highqual$date[tab_highqual$year == "2023_09"] <- "2023-09-27"
tab_highqual$date[tab_highqual$year == "2024"] <- "2024-08-27"

tab_highqual$date <- as.Date(tab_highqual$date)
tab_highqual$year <- year(tab_highqual$date)



tab_highqual_bands$date <- ""
tab_highqual_bands$date[tab_highqual_bands$year == "2016"] <- "2016-09-28"
tab_highqual_bands$date[tab_highqual_bands$year == "2017"] <- "2017-09-13"
tab_highqual_bands$date[tab_highqual_bands$year == "2018"] <- "2018-09-23"
tab_highqual_bands$date[tab_highqual_bands$year == "2019"] <- "2019-09-08"
tab_highqual_bands$date[tab_highqual_bands$year == "2020"] <- "2020-09-02"
tab_highqual_bands$date[tab_highqual_bands$year == "2021"] <- "2021-08-28"
tab_highqual_bands$date[tab_highqual_bands$year == "2022"] <- "2022-09-17"
tab_highqual_bands$date[tab_highqual_bands$year == "2023/08"] <- "2023-08-08"
tab_highqual_bands$date[tab_highqual_bands$year == "2023/09"] <- "2023-09-27"
tab_highqual_bands$date[tab_highqual_bands$year == "2024"] <- "2024-08-27"

tab_highqual_bands$date <- as.Date(tab_highqual_bands$date)

tab_highqual_bands$year <- year(tab_highqual_bands$date)

tab_highqual_bands$uniqID <- paste0("lake",tab_highqual_bands$lake_id," ",tab_highqual_bands$date)
tab_highqual$uniqID <- paste0("lake",tab_highqual$lake_id," ",tab_highqual$date)


tab_highqual_joined <- merge(tab_highqual, tab_highqual_bands, by = "uniqID")
names(tab_highqual_joined)

tab_highqual_joined <- tab_highqual_joined[,c(1,3,5,4,seq(9,14),seq(19,33))]
names(tab_highqual_joined)[seq(2,4)] <- c("ID","date","dwl_nm")
head(tab_highqual_joined)


tab_highqual_joined$FUI <- getFUI(tab_highqual_joined$dwl_nm)

setwd("C:/Projects/myGit/laksat/results")
write.csv(tab_highqual_joined, file = "Rrs_color_s2_polymer_PYRlakes.csv")



# ---------------- Spatial variability in lake color -----------------------

# calculate median DWL for all lakes

df_DWLmed <- tab_highqual_joined %>% 
  summarise(med=median(dwl_nm), .by = ID, na.rm = T)
df_DWLmed$FUI <- getFUI(df_DWLmed$med)


# look at spatial variability in lake color

p_densDWL <- ggplot(df_DWLmed, aes (med))+geom_density()+theme_article()+xlab("Median dominant wavelength")

df_FUI <- as.data.frame(table(df_DWLmed$FUI))
df_FUI$prop <- df_FUI$Freq/sum(df_FUI$Freq)*100
df_FUI$color <- fui.colors[as.numeric(as.vector(df_FUI$Var1))]

p_FUI <- ggplot(df_FUI)+geom_segment(aes(x= Var1, y = 0, yend = Freq, color = Var1), size = 5)+theme_article()+
  xlab("median Forel Ule Index")+
  ylab("Counts")+
  scale_color_manual(values = c(df_FUI$color))+theme(legend.title = element_blank())

require(ggpubr)
ggpubr::ggarrange(plotlist = c(p_densDWL, p_FUI), labels = c("a","b"))


ggplot(tab_highqual_joined, aes (FUI, colour = FUI))+geom_histogram()+theme_article()+scale_color_manual(fui.colors)

p_densTSS <- ggplot(tab_highqual_joined)+
  geom_density(aes (tsm_vantrepotte665))+
  geom_density(aes (tsm_vantrepotte665))+
  theme_article()
p_densCHL <- ggplot(tab_highqual_joined, aes (tsm_vantrepotte665))+geom_density()+theme_article()


p_DWL_TSS <- ggplot(tab_highqual_joined, aes (tsm_vantrepotte665, dwl_nm))+geom_point()+theme_article()#+scale_x_log10()+scale_y_log10()


# ---------------- Temporal variability in lake color -----------------------

# look at temporal variability in lake color by lake

df_DWL_iqr <- tab_highqual_joined %>% 
  summarise(iqr=IQR(dwl_nm), .by = ID)

df_DWLmed$iqr <- df_DWL_iqr$iqr[match(df_DWL_iqr$ID, df_DWL_iqr$ID)]


ggplot(df_DWLmed, aes(med, iqr))+geom_point()+theme_article()


# Calculate trends

df_trends <- get_aggregated_stats(df_color = tab_highqual_joined)

ggplot(df_trends, aes(as.factor(trend_type)))+geom_histogram(stat="count")



ggplot(df_trends, aes(dwl_med))+geom_density()+theme_article()


ggplot(df_trends, aes(dwl_med, elevation_masl, colour = trend_type))+geom_point(size=3, alpha=0.6)+theme_article()+ggtitle("LANDSAT 7, June-October")+
  scale_colour_manual(values = c("darkblue","red","grey"))+ggtitle("LANDSAT 7, June-October")






# ---------------- Correlation between POLYMER and GEE (LaSRC correction) -----------------------


setwd("C:/Projects/myGit/laksat/results")

color_s2 <- read.csv(file = "color_sentinel2.csv")
color_s2$date <- as.Date(color_s2$date)

tab_highqual$dwl_gee <- color_s2$dwl[match(paste0(tab_highqual$date," lake",tab_highqual$lake_id), 
                                           paste0(color_s2$date, " lake",color_s2$ID))]

tab_highqual$SIZE_HA <- color_s2$SIZE_HA[match(paste0(tab_highqual$date," lake",tab_highqual$lake_id), 
                                           paste0(color_s2$date, " lake",color_s2$ID))]

tab_highqual$WRT_MONTHS <- color_s2$WRT_MONTHS[match(paste0(tab_highqual$date," lake",tab_highqual$lake_id), 
                                               paste0(color_s2$date, " lake",color_s2$ID))]

ggplot(tab_highqual, aes(centre_domwl_nm, dwl_gee))+geom_point()+theme_article()+
  geom_abline(slope = 1)+
  scale_color_viridis_c(option = "A", direction = -1)+
  xlab("POLYMER High quality")+ylab("GEE low quality")






tab_highqual_bands$Rw443_gee <- color_s2$ultrablue[match(paste0(tab_highqual_bands$date," lake",tab_highqual_bands$lake_id), 
                                           paste0(color_s2$date, " lake",color_s2$ID))]


tab_highqual_bands$Rw490_gee <- color_s2$blue[match(paste0(tab_highqual_bands$date," lake",tab_highqual_bands$lake_id), 
                                                     paste0(color_s2$date, " lake",color_s2$ID))]

tab_highqual_bands$Rw560_gee <- color_s2$green[match(paste0(tab_highqual_bands$date," lake",tab_highqual_bands$lake_id), 
                                                paste0(color_s2$date, " lake",color_s2$ID))]

tab_highqual_bands$Rw665_gee <- color_s2$red[match(paste0(tab_highqual_bands$date," lake",tab_highqual_bands$lake_id), 
                                                paste0(color_s2$date, " lake",color_s2$ID))]



p_ublue <- ggplot(tab_highqual_bands, aes(Rw443, Rw443_gee))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_blue <- ggplot(tab_highqual_bands, aes(Rw490, Rw490_gee))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_green <- ggplot(tab_highqual_bands, aes(Rw560,Rw560_gee))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_red <- ggplot(tab_highqual_bands, aes(Rw665,Rw665_gee))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()

ggarrange(p_ublue, p_blue, p_green, p_red, ncol = 2)







