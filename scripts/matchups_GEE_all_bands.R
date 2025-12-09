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


# -------------------  functions ------------------- 

get_aNDWI <- function(mytable){
  aNDWI <- (mytable$blue+mytable$green+mytable$red-mytable$nir-mytable$swir1-mytable$swir2)/
    (mytable$blue+mytable$green+mytable$red+mytable$nir+mytable$swir1+mytable$swir2)
  return(aNDWI)
}

trim_with_aNDWI <- function(mytable){
  p95 <- quantile(x = mytable$aNDWI, 0.95)
  mytable <- mytable[mytable$aNDWI<p95,]
}


# -------------------  Load GEE table ------------------- 

setwd("C:/Projects/myGit/laksat/results")
mytable_l5 <- read.csv(file = "color_landsat5.csv")
mytable_l7 <- read.csv(file = "color_landsat7.csv")
mytable_l8 <- read.csv(file = "color_landsat8.csv")
mytable_l9 <- read.csv(file = "color_landsat9.csv")
mytable_s2 <- read.csv(file = "color_sentinel2.csv")

mytable_l5$date <- as.Date(mytable_l5$date)
mytable_l7$date <- as.Date(mytable_l7$date)
mytable_l8$date <- as.Date(mytable_l8$date)
mytable_l9$date <- as.Date(mytable_l9$date)
mytable_s2$date <- as.Date(mytable_s2$date)

mytable_l5<-mytable_l5[which(mytable_l5$dwl>400 & mytable_l5$dwl < 600),]
mytable_l7<-mytable_l7[which(mytable_l7$dwl>400 & mytable_l7$dwl < 600),]
mytable_l8<-mytable_l8[which(mytable_l8$dwl>400 & mytable_l8$dwl < 600),]
mytable_l9<-mytable_l9[which(mytable_l9$dwl>400 & mytable_l9$dwl < 600),]
mytable_s2<-mytable_s2[which(mytable_s2$dwl>400 & mytable_s2$dwl < 600),]

mytable_l5 <- mytable_l5[mytable_l5$s_star<=1,]
mytable_l7 <- mytable_l7[mytable_l7$s_star<=1,]
mytable_l8 <- mytable_l8[mytable_l8$s_star<=1,]
mytable_l9 <- mytable_l9[mytable_l9$s_star<=1,]
mytable_s2 <- mytable_s2[mytable_s2$s_star<=1,]

mytable_l5$aNDWI <- get_aNDWI(mytable = mytable_l5)
mytable_l7$aNDWI <- get_aNDWI(mytable = mytable_l7)
mytable_l8$aNDWI <- get_aNDWI(mytable = mytable_l8)
mytable_l9$aNDWI <- get_aNDWI(mytable = mytable_l9)
mytable_s2$aNDWI <- get_aNDWI(mytable = mytable_s2)

mytable_l5 <- trim_with_aNDWI(mytable_l5)
mytable_l7 <- trim_with_aNDWI(mytable_l7)
mytable_l8 <- trim_with_aNDWI(mytable_l8)
mytable_l9 <- trim_with_aNDWI(mytable_l9)
mytable_s2 <- trim_with_aNDWI(mytable_s2)


maxDelay = 7

# -------------------- landsat 5 comparison with landsat 7 -------------------- 

table_matchups_l5l7 <- NULL
for (id in unique(mytable_l5$ID)){
  mytable_l5_id <- mytable_l5[which(mytable_l5$ID == id),]
  mytable_l7_id <- mytable_l7[which(mytable_l7$ID == id),]
  
  # first we only work on temporal range overlaps
  mytable_l5_id <- mytable_l5_id[which(mytable_l5_id$date >= min(mytable_l7_id$date) & 
                                         mytable_l5_id$date <= max(mytable_l7_id$date)),]
  
  for(indl5 in seq_along(mytable_l5_id$date)){
    d = as.Date(mytable_l5_id$date[indl5])
    indl7 <- which.min(abs(mytable_l7_id$date - d))
    daydiff <- as.numeric(abs(mytable_l7_id$date[indl7] - d))
    
    if (daydiff < maxDelay){
      table_matchups_l5l7 <- rbind(table_matchups_l5l7,
                                 data.frame(lake = id,
                                            date_l5 = d,
                                            blue_l5 = mytable_l5_id$blue[indl5],
                                            green_l5 = mytable_l5_id$green[indl5],
                                            red_l5 = mytable_l5_id$red[indl5],
                                            nir_l5 = mytable_l5_id$nir[indl5],
                                            swir1_l5 = mytable_l5_id$swir1[indl5],
                                            swir2_l5 = mytable_l5_id$swir2[indl5],
                                            dwl_l5 = mytable_l5_id$dwl[indl5],
                                            fui_l5 = mytable_l5_id$fui[indl5],
                                            s_star_l5 = mytable_l5_id$s_star[indl5],
                                            days_difference = daydiff,
                                            date_l7 = d,
                                            blue_l7 = mytable_l7_id$blue[indl7],
                                            green_l7 = mytable_l7_id$green[indl7],
                                            red_l7 = mytable_l7_id$red[indl7],
                                            nir_l7 = mytable_l7_id$nir[indl7],
                                            swir1_l7 = mytable_l7_id$swir1[indl7],
                                            swir2_l7 = mytable_l7_id$swir2[indl7],
                                            dwl_l7 = mytable_l7_id$dwl[indl7],
                                            fui_l7 = mytable_l7_id$fui[indl7] ,
                                            s_star_l7 = mytable_l7_id$s_star[indl7] ))
    }
  }
}
ggplot(table_matchups_l5l7, aes(days_difference))+geom_density()+theme_article()

p_blue <- ggplot(table_matchups_l5l7, aes(blue_l5, blue_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)+geom_smooth(method = 'lm')
p_green <- ggplot(table_matchups_l5l7, aes(green_l5, green_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_red <- ggplot(table_matchups_l5l7, aes(red_l5,red_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_nir <- ggplot(table_matchups_l5l7, aes(nir_l5,nir_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir1 <- ggplot(table_matchups_l5l7, aes(swir1_l5,swir1_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir2 <- ggplot(table_matchups_l5l7, aes(swir2_l5,swir2_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()

ggarrange(p_blue, p_green, p_red,
          p_nir, p_swir1, p_swir2, ncol = 3)

ggplot(table_matchups_l5l7, aes(dwl_l5, dwl_l7))+geom_point()+
  theme_article()+geom_abline(slope=1)+ggtitle("LANDSAT 7 vs LANDSAT 5")



# -------------------- landsat 7 comparison with landsat 8 -------------------- 

table_matchups_l7l8 <- NULL
for (id in unique(mytable_l7$ID)){
  mytable_l7_id <- mytable_l7[which(mytable_l7$ID == id),]
  mytable_l8_id <- mytable_l8[which(mytable_l8$ID == id),]
  
  # first we only work on temporal range overlaps
  mytable_l7_id <- mytable_l7_id[which(mytable_l7_id$date >= min(mytable_l8_id$date) & 
                                         mytable_l7_id$date <= max(mytable_l8_id$date)),]
  
  for(indl7 in seq_along(mytable_l7_id$date)){
    d = as.Date(mytable_l7_id$date[indl7])
    indl8 <- which.min(abs(mytable_l8_id$date - d))
    daydiff <- as.numeric(abs(mytable_l8_id$date[indl8] - d))
    
    if (daydiff < 7){
      table_matchups_l7l8 <- rbind(table_matchups_l7l8,
                                 data.frame(lake = id,
                                            date_l7 = d,
                                            blue_l7 = mytable_l7_id$blue[indl7],
                                            green_l7 = mytable_l7_id$green[indl7],
                                            red_l7 = mytable_l7_id$red[indl7],
                                            nir_l7 = mytable_l7_id$nir[indl7],
                                            swir1_l7 = mytable_l7_id$swir1[indl7],
                                            swir2_l7 = mytable_l7_id$swir2[indl7],
                                            dwl_l7 = mytable_l7_id$dwl[indl7],
                                            fui_l7 = mytable_l7_id$fui[indl7],
                                            s_star_l7 = mytable_l7_id$s_star[indl7],
                                            days_difference = daydiff,
                                            date_l8 = d,
                                            blue_l8 = mytable_l8_id$blue[indl8],
                                            green_l8 = mytable_l8_id$green[indl8],
                                            red_l8 = mytable_l8_id$red[indl8],
                                            nir_l8 = mytable_l8_id$nir[indl8],
                                            swir1_l8 = mytable_l8_id$swir1[indl8],
                                            swir2_l8 = mytable_l8_id$swir2[indl8],
                                            dwl_l8 = mytable_l8_id$dwl[indl8],
                                            fui_l8 = mytable_l8_id$fui[indl8] ,
                                            s_star_l8 = mytable_l8_id$s_star[indl8] ))
    }
  }
}
# ggplot(table_matchups_l7l8, aes(days_difference))+geom_density()+theme_article()


p_blue <- ggplot(table_matchups_l7l8, aes(blue_l7, blue_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_green <- ggplot(table_matchups_l7l8, aes(green_l7, green_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_red <- ggplot(table_matchups_l7l8, aes(red_l7,red_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_nir <- ggplot(table_matchups_l7l8, aes(nir_l7,nir_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir1 <- ggplot(table_matchups_l7l8, aes(swir1_l7,swir1_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir2 <- ggplot(table_matchups_l7l8, aes(swir2_l7,swir2_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()

ggarrange(p_blue, p_green, p_red,
          p_nir, p_swir1, p_swir2, ncol = 3)

ggplot(table_matchups_l7l8, aes(dwl_l7, dwl_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)




# -------------------- landsat 7 comparison with sentinel 2 -------------------- 

table_matchups_l7s2 <- NULL
for (id in unique(mytable_l7$ID)){
  mytable_l7_id <- mytable_l7[which(mytable_l7$ID == id),]
  mytable_s2_id <- mytable_s2[which(mytable_s2$ID == id),]
  
  # first we only work on temporal range overlaps
  mytable_l7_id <- mytable_l7_id[which(mytable_l7_id$date >= min(mytable_s2_id$date) & 
                                         mytable_l7_id$date <= max(mytable_s2_id$date)),]
  
  for(indl7 in seq_along(mytable_l7_id$date)){
    d = as.Date(mytable_l7_id$date[indl7])
    inds2 <- which.min(abs(mytable_s2_id$date - d))
    daydiff <- as.numeric(abs(mytable_s2_id$date[inds2] - d))
    
    if (daydiff < 7){
      table_matchups_l7s2 <- rbind(table_matchups_l7s2,
                                   data.frame(lake = id,
                                              date_l7 = d,
                                              blue_l7 = mytable_l7_id$blue[indl7],
                                              green_l7 = mytable_l7_id$green[indl7],
                                              red_l7 = mytable_l7_id$red[indl7],
                                              nir_l7 = mytable_l7_id$nir[indl7],
                                              swir1_l7 = mytable_l7_id$swir1[indl7],
                                              swir2_l7 = mytable_l7_id$swir2[indl7],
                                              dwl_l7 = mytable_l7_id$dwl[indl7],
                                              fui_l7 = mytable_l7_id$fui[indl7],
                                              s_star_l7 = mytable_l7_id$s_star[indl7],
                                              days_difference = daydiff,
                                              date_s2 = d,
                                              blue_s2 = mytable_s2_id$blue[inds2],
                                              green_s2 = mytable_s2_id$green[inds2],
                                              red_s2 = mytable_s2_id$red[inds2],
                                              nir_s2 = mytable_s2_id$nir[inds2],
                                              swir1_s2 = mytable_s2_id$swir1[inds2],
                                              swir2_s2 = mytable_s2_id$swir2[inds2],
                                              dwl_s2 = mytable_s2_id$dwl[inds2],
                                              fui_s2 = mytable_s2_id$fui[inds2] ,
                                              s_star_s2 = mytable_s2_id$s_star[inds2] ))
    }
  }
}
# ggplot(table_matchups_l7s2, aes(days_difference))+geom_density()+theme_article()


p_blue <- ggplot(table_matchups_l7s2, aes(blue_l7, blue_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_green <- ggplot(table_matchups_l7s2, aes(green_l7, green_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_red <- ggplot(table_matchups_l7s2, aes(red_l7,red_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_nir <- ggplot(table_matchups_l7s2, aes(nir_l7,nir_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir1 <- ggplot(table_matchups_l7s2, aes(swir1_l7,swir1_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir2 <- ggplot(table_matchups_l7s2, aes(swir2_l7,swir2_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()

ggarrange(p_blue, p_green, p_red,
          p_nir, p_swir1, p_swir2, ncol = 3)

ggplot(table_matchups_l7s2, aes(dwl_l7, dwl_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)+ggtitle("LANDSAT 7 vs SENTINEL 2")





# -------------------- landsat 8 comparison with sentinel 2 -------------------- 

table_matchups_l8s2 <- NULL
for (id in unique(mytable_l8$ID)){
  mytable_l8_id <- mytable_l8[which(mytable_l8$ID == id),]
  mytable_s2_id <- mytable_s2[which(mytable_s2$ID == id),]
  
  # first we only work on temporal range overlaps
  mytable_l8_id <- mytable_l8_id[which(mytable_l8_id$date >= min(mytable_s2_id$date) & 
                                         mytable_l8_id$date <= max(mytable_s2_id$date)),]
  
  for(indl8 in seq_along(mytable_l8_id$date)){
    d = as.Date(mytable_l8_id$date[indl8])
    inds2 <- which.min(abs(mytable_s2_id$date - d))
    daydiff <- as.numeric(abs(mytable_s2_id$date[inds2] - d))
    
    if (daydiff < 7){
      table_matchups_l8s2 <- rbind(table_matchups_l8s2,
                                   data.frame(lake = id,
                                              date_l8 = d,
                                              blue_l8 = mytable_l8_id$blue[indl8],
                                              green_l8 = mytable_l8_id$green[indl8],
                                              red_l8 = mytable_l8_id$red[indl8],
                                              nir_l8 = mytable_l8_id$nir[indl8],
                                              swir1_l8 = mytable_l8_id$swir1[indl8],
                                              swir2_l8 = mytable_l8_id$swir2[indl8],
                                              dwl_l8 = mytable_l8_id$dwl[indl8],
                                              fui_l8 = mytable_l8_id$fui[indl8],
                                              s_star_l8 = mytable_l8_id$s_star[indl8],
                                              days_difference = daydiff,
                                              date_s2 = d,
                                              blue_s2 = mytable_s2_id$blue[inds2],
                                              green_s2 = mytable_s2_id$green[inds2],
                                              red_s2 = mytable_s2_id$red[inds2],
                                              nir_s2 = mytable_s2_id$nir[inds2],
                                              swir1_s2 = mytable_s2_id$swir1[inds2],
                                              swir2_s2 = mytable_s2_id$swir2[inds2],
                                              dwl_s2 = mytable_s2_id$dwl[inds2],
                                              fui_s2 = mytable_s2_id$fui[inds2] ,
                                              s_star_s2 = mytable_s2_id$s_star[inds2] ))
    }
  }
}
# ggplot(table_matchups_l8s2, aes(days_difference))+geom_histogram()+theme_article()


p_blue <- ggplot(table_matchups_l8s2, aes(blue_l8, blue_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_green <- ggplot(table_matchups_l8s2, aes(green_l8, green_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_red <- ggplot(table_matchups_l8s2, aes(red_l8,red_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_nir <- ggplot(table_matchups_l8s2, aes(nir_l8,nir_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir1 <- ggplot(table_matchups_l8s2, aes(swir1_l8,swir1_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir2 <- ggplot(table_matchups_l8s2, aes(swir2_l8,swir2_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()

ggarrange(p_blue, p_green, p_red,
          p_nir, p_swir1, p_swir2, ncol = 3)

ggplot(table_matchups_l8s2, aes(dwl_l8, dwl_s2))+geom_point()+
  theme_article()+geom_abline(slope=1)+ggtitle("LANDSAT 8 vs SENTINEL 2")




ind_notOK <- which(abs(table_matchups_l8s2$dwl_l8 - table_matchups_l8s2$dwl_s2)/table_matchups_l8s2$dwl_l8 > 0.05)

list_notOK <- unique(table_matchups_l8s2$lake[ind_notOK])

list_OK <- unique(table_matchups_l8s2$lake[!table_matchups_l8s2$lake %in% list_notOK])


table_matchups_l8s2$weird <- "no"
table_matchups_l8s2$weird[table_matchups_l8s2$lake  %in% list_notOK] <- "yes"


p_dwl <- ggplot(table_matchups_l8s2, aes(dwl_l8, dwl_s2, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)

p_b <- ggplot(table_matchups_l8s2, aes(blue_l8, blue_s2, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)
p_g <- ggplot(table_matchups_l8s2, aes(green_l8, green_s2, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)
p_r <- ggplot(table_matchups_l8s2, aes(red_l8, red_s2, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)

ggarrange(p_dwl, p_b, p_g, p_r, ncol = 2)






# -------------------- landsat 9 comparison with landsat 8 -------------------- 

table_matchups_l9l8 <- NULL
for (id in unique(mytable_l9$ID)){
  mytable_l9_id <- mytable_l9[which(mytable_l9$ID == id),]
  mytable_l8_id <- mytable_l8[which(mytable_l8$ID == id),]
  
  # first we only work on temporal range overlaps
  mytable_l9_id <- mytable_l9_id[which(mytable_l9_id$date >= min(mytable_l8_id$date) & 
                                         mytable_l9_id$date <= max(mytable_l8_id$date)),]
  
  for(indl9 in seq_along(mytable_l9_id$date)){
    d = as.Date(mytable_l9_id$date[indl9])
    indl8 <- which.min(abs(mytable_l8_id$date - d))
    daydiff <- as.numeric(abs(mytable_l8_id$date[indl8] - d))
    
    if (daydiff < maxDelay){
      table_matchups_l9l8 <- rbind(table_matchups_l9l8,
                                 data.frame(lake = id,
                                            date_l9 = d,
                                            blue_l9 = mytable_l9_id$blue[indl9],
                                            green_l9 = mytable_l9_id$green[indl9],
                                            red_l9 = mytable_l9_id$red[indl9],
                                            nir_l9 = mytable_l9_id$nir[indl9],
                                            swir1_l9 = mytable_l9_id$swir1[indl9],
                                            swir2_l9 = mytable_l9_id$swir2[indl9],
                                            dwl_l9 = mytable_l9_id$dwl[indl9],
                                            fui_l9 = mytable_l9_id$fui[indl9],
                                            s_star_l9 = mytable_l9_id$s_star[indl9],
                                            days_difference = daydiff,
                                            date_l8 = d,
                                            blue_l8 = mytable_l8_id$blue[indl8],
                                            green_l8 = mytable_l8_id$green[indl8],
                                            red_l8 = mytable_l8_id$red[indl8],
                                            nir_l8 = mytable_l8_id$nir[indl8],
                                            swir1_l8 = mytable_l8_id$swir1[indl8],
                                            swir2_l8 = mytable_l8_id$swir2[indl8],
                                            dwl_l8 = mytable_l8_id$dwl[indl8],
                                            fui_l8 = mytable_l8_id$fui[indl8] ,
                                            s_star_l8 = mytable_l8_id$s_star[indl8] ))
    }
  }
}
# ggplot(table_matchups_l9l8, aes(days_difference))+geom_density()+theme_article()


p_blue <- ggplot(table_matchups_l9l8, aes(blue_l9, blue_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_green <- ggplot(table_matchups_l9l8, aes(green_l9, green_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_red <- ggplot(table_matchups_l9l8, aes(red_l9,red_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_nir <- ggplot(table_matchups_l9l8, aes(nir_l9,nir_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir1 <- ggplot(table_matchups_l9l8, aes(swir1_l9,swir1_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()
p_swir2 <- ggplot(table_matchups_l9l8, aes(swir2_l9,swir2_l8))+geom_point()+
  theme_article()+geom_abline(slope=1)#+scale_x_log10()+scale_y_log10()

ggarrange(p_blue, p_green, p_red,
          p_nir, p_swir1, p_swir2, ncol = 3)

ggplot(table_matchups_l9l8, aes(dwl_l9, dwl_l8, colour = lake))+geom_point()+
  theme_article()+geom_abline(slope=1)+ggtitle("LANDSAT 8 vs LANDSAT 9")


ggplot(table_matchups_l9l8, aes(abs(dwl_l9-dwl_l8)/dwl_l9))+geom_density()+
  theme_article()+ggtitle("LANDSAT 8 vs LANDSAT 9")


ind_notOK <- which(abs(table_matchups_l9l8$dwl_l9 - table_matchups_l9l8$dwl_l8)/table_matchups_l9l8$dwl_l9 > 0.05)

list_notOK <- unique(table_matchups_l9l8$lake[ind_notOK])

list_OK <- unique(table_matchups_l9l8$lake[!table_matchups_l9l8$lake %in% list_notOK])


table_matchups_l9l8$weird <- "no"
table_matchups_l9l8$weird[table_matchups_l9l8$lake  %in% list_notOK] <- "yes"



p_dwl <- ggplot(table_matchups_l9l8, aes(dwl_l9, dwl_l8, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)

p_b <- ggplot(table_matchups_l9l8, aes(blue_l9, blue_l8, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)
p_g <- ggplot(table_matchups_l9l8, aes(green_l9, green_l8, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)
p_r <- ggplot(table_matchups_l9l8, aes(red_l9, red_l8, color = weird))+geom_point()+
  theme_article()+geom_abline(slope=1)+facet_wrap(weird~.)

ggarrange(p_dwl, p_b, p_g, p_r, ncol = 2)







