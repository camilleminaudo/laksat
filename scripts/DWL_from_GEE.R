


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


cie <- cccie31 %>%
  mutate(a = atan2((y - 1/3), (x - 1/3)) * 180/pi) %>%
  dplyr::filter(wlnm <= 700) %>%
  dplyr::filter(wlnm >=380)


get_smax_from_DWL <- function(dwl){
  require(colorscience)
  # make look up table for hue angle to wavelength conversion
  cie <- cccie31 %>%
    mutate(a = atan2((y - 1/3), (x - 1/3)) * 180/pi) %>%
    dplyr::filter(wlnm <= 700) %>%
    dplyr::filter(wlnm >=380)
  
  cie$s_max <- ((cie$x-1/3)^2 + (cie$y-1/3)^2)^(1/2)
  
  # find nearest dominant wavelength to hue angle
  s_max <- cie[as.vector(sapply(dwl,function(x) which.min(abs(x - cie$wlnm)))), 's_max']
  return(s_max)
}


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



# Actual Forel-Ule Colors
fui.colors <- c(
  "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660","#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04")



get_color_metrics <- function(table, satellite){
  #table <- table[table$satellite==satellite,]
  
  table$ANDWI <- (table$blue + table$green + table$red - table$nir - table$swir1 - table$swir2)/
    (table$blue + table$green + table$red + table$nir + table$swir1 + table$swir2)
  
  table <- table[which(table$ANDWI>0),]# https://doi.org/10.1016/j.envsoft.2021.105030
  
  B = table$blue
  G = table$green
  R = table$red
  
  table$BGR <- 0.5*abs(490 * G + 560 * R + 665 * B - 560 * B - 665 * G - 490 * R)
  
  
  
  if(satellite == "LANDSAT_5"){ #https://hess.copernicus.org/articles/26/3517/2022/
    X1 = 1.1302
    X2 = 1.7517
    X3 = 2.7689
    
    Y1 = 0.0601
    Y2 = 4.5907
    Y3 = 1
    
    Z1 = 5.5943
    Z2 = 0.0560
    Z3 = 0
    
    a5 = 25.851
    a4 = -177.4
    a3 = 476.69
    a2 = -653.3
    a1 = 463.33
    a0 = -94.41
    
    X = X1*B + X2*G + X3*R
    Y = Y1*B + Y2*G + Y3*R
    Z = Z1*B + Z2*G + Z3*R
    
  } else if(satellite == "SENTINEL_2"){
    X1 = 12.040
    X2 = 53.696
    X3 = 32.087
    
    Y1 = 23.122
    Y2 = 65.702
    Y3 = 16.830
    
    Z1 = 61.055
    Z2 = 1.778
    Z3 = 0.015
    
    a5 = -164.83
    a4 = 1139.90
    a3 = -3006.04
    a2 = 3677.75
    a1 = -1979.71
    a0 = 371.38
    
    X = X1*B + X2*G + X3*R
    Y = Y1*B + Y2*G + Y3*R
    Z = Z1*B + Z2*G + Z3*R
    
  } else if(satellite == "LANDSAT_7"){
    X1 = 13.104
    X2 = 53.791
    X3 = 31.304
    
    Y1 = 24.097
    Y2 = 65.801
    Y3 = 15.883
    
    Z1 = 63.845
    Z2 = 2.142
    Z3 = 0.013
    
    a5 = -84.94
    a4 = 594.17
    a3 = -1559.86
    a2 = 1852.5
    a1 = -918.11
    a0 = 151.49
    
    
    X = X1*B + X2*G + X3*R
    Y = Y1*B + Y2*G + Y3*R
    Z = Z1*B + Z2*G + Z3*R
    
  } else if(satellite == "LANDSAT_8" | satellite == "LANDSAT_9"){
    UB = table$ultrablue
    X1 = 11.053
    X2 = 6.950
    X3 = 51.135
    X4 = 34.457
    
    Y1 = 1.320
    Y2 = 21.053
    Y3 = 66.023
    Y4 = 18.034
    
    Z1 = 58.038
    Z2 = 34.931
    Z3 = 2.606
    Z4 = 0.016
    
    a5 = -52.16
    a4 = 373.81
    a3 = -981.83
    a2 = 1134.19
    a1 = -533.61
    a0 = 76.72
    
    X = X1*UB + X2*B + X3*G + X4*R
    Y = Y1*UB + Y2*B + Y3*G + Y4*R
    Z = Z1*UB + Z2*B + Z3*G + Z4*R
  }
  
  
  x = X/(X+Y+Z)
  y=Y/(X+Y+Z)
  
  alpha = atan2(y-1/3, x-1/3)
  alpha_deg = alpha*180/pi
  alpha_deg[alpha_deg<0] = 360+alpha_deg[alpha_deg<0]
  a = alpha_deg/100
  
  delta = a5*a^5 +  a4*a^4 +a3*a^3 + a2*a^2 +a1*a + a0
  # plot(a, delta)
  # points(alpha_100, delta, col='red')
  
  alpha_corr <- alpha_deg + delta
  
  
  require(colorscience)
  # make look up table for hue angle to wavelength conversion
  cie <- cccie31 %>%
    mutate(a = atan2((y - 1/3), (x - 1/3)) * 180/pi) %>%
    dplyr::filter(wlnm <= 700) %>%
    dplyr::filter(wlnm >=380)
  
  # calculate positive alpha (in degrees)
  cie$a_positive = cie$a
  cie$a_positive[cie$a_positive<0] = 360+cie$a_positive[cie$a_positive<0]
  
  # find nearest dominant wavelength to hue angle
  dwl <- cie[as.vector(sapply(alpha_corr,function(x) which.min(abs(x - cie$a_positive)))), 'wlnm']
  
  s <- sqrt((x-1/3)^2+(y-1/3)^2)
  delta_s = -0.0099*a^5 + 0.1199*a^4 - 0.4594*a^3 + 0.7515*a^2 - 0.5095*a^1 + 0.1222
  s_corr <- s+delta_s
  smax <- get_smax_from_DWL(dwl)
  s_star <- s_corr/smax
  
  table$coordX = table$x
  table$coordY = table$y
  
  fui <- fui.lookup[as.vector(sapply(dwl,function(x) which.min(abs(x - fui.lookup$dWL)))), 'fui']
  
  df_out <- cbind(table[,-c(which(names(table) == ".geo"), which(names(table) == "x"), which(names(table) == "y"))],
                  data.frame(x=x, 
                             y=y, 
                             alpha_corr = alpha_corr,
                             dwl = dwl,
                             fui = fui,
                             s_star =s_star))
  
  return(df_out)
  
}

# -------------------  Load GEE table ------------------- 


setwd("C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/LAKeSAT/gee_outputs")
mytable_l7 <- as.data.frame(fread("pts2Asset_PNAP_L7.csv"))
# mytable_l7 <- mytable_l7[!duplicated(paste0(mytable_l7$ID,mytable_l7$date)),]
mytable_l8 <- as.data.frame(fread("pts2Asset_PNAP_L8.csv"))
# mytable_l8 <- mytable_l8[!duplicated(paste0(mytable_l8$ID,mytable_l8$date)),]
mytable_s2 <- as.data.frame(fread("pts2Asset_PNAP_S2.csv"))


# -------------------  Extract water colour ------------------- 

df_color_l7 <- get_color_metrics(table = mytable_l7, satellite = "LANDSAT_7")

df_color_l8 <- get_color_metrics(table = mytable_l8, satellite = "LANDSAT_8")

# apply calibration factors to Landsat8
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2023WR036926
mytable_l8corr <- mytable_l8
mytable_l8corr$blue <- 0.8750*(mytable_l8corr$blue)^2+0.6774*mytable_l8corr$blue+0.02072
mytable_l8corr$green <- 0.2960*(mytable_l8corr$green)^2+0.8007*mytable_l8corr$green+0.01571
mytable_l8corr$red <- 0.3832*(mytable_l8corr$red)^2+0.8207*mytable_l8corr$red+0.01300

df_color_l8_corr <- get_color_metrics(table = mytable_l8corr, satellite = "LANDSAT_7")
df_color_l8_corr$satellite <- "LANDSAT_8corr"

df_color_s2 <- get_color_metrics(table = mytable_s2, satellite = "SENTINEL_2")
df_color_s2$satellite <- "SENTINEL_2"


my_cols <- c("ID","LAKECODEME","LAKENAME","LATITUDE", "LONGITUDE",
             "ALTITUDE_M","CATCHMT_HA","MAXDEPTH_M","WRT_MONTHS",
             "satellite",
             "date", "x","y","alpha_corr","dwl","fui","s_star",
             "blue","green","red")

df_color <- rbind(df_color_l7[,my_cols], df_color_l8[,my_cols], df_color_s2[,my_cols])
# df_color <- df_color[which(df_color$s_star<=1),]


# Landsat7 versus Landsat8
ggplot(df_color, aes(fui, fill = satellite))+geom_density(alpha=0.5)+theme_article()

df_color_after2013 <- df_color[year(df_color$date)>=2013,]
tol <- 3
df_compareSats <- NULL
for (id in unique(df_color_after2013$ID)){
  message(id)
  d_s2 <- df_color_after2013$date[which(df_color_after2013$satellite=="SENTINEL_2" & df_color_after2013$ID == id)]
  for(d in d_s2){
    ind_S2 <- which(df_color_after2013$satellite=="SENTINEL_2" & df_color_after2013$ID == id & df_color_after2013$date==d)
    ind_l8 <- which(df_color_after2013$satellite=="LANDSAT_8" & df_color_after2013$ID == id & df_color_after2013$date>=d-tol & df_color_after2013$date<=d+tol)
    ind_l7 <- which(df_color_after2013$satellite=="LANDSAT_7" & df_color_after2013$ID == id & df_color_after2013$date>=d-tol & df_color_after2013$date<=d+tol)
    
    
    if(length(ind_l8)>0 | length(ind_l7)>0){
      tmp <- data.frame(ID = id,
                        d_S2 = as.Date(d),
                        d_L7 = NA,
                        d_L8 = NA,
                        d_diff_L7 = NA,
                        d_diff_L8 = NA,
                        blue_S2 = df_color_after2013$blue[ind_S2],
                        blue_L7 = NA,
                        blue_L8 = NA,
                        green_S2 = df_color_after2013$green[ind_S2],
                        green_L7 = NA,
                        green_L8 = NA,
                        red_S2 = df_color_after2013$red[ind_S2],
                        red_L7 = NA,
                        red_L8 = NA,
                        dwl_S2 = df_color_after2013$dwl[ind_S2],
                        dwl_L7 = NA,
                        dwl_L8 = NA)
      if(length(ind_l8)>0 ){
        ind_l8 <- ind_l8[which.min(abs(as.numeric(df_color_after2013$date[ind_l8])-as.numeric(d)))]
        tmp$d_L8 <- df_color_after2013$date[ind_l8]
        tmp$d_diff_L8 <-  abs(as.numeric(df_color_after2013$date[ind_l8])-as.numeric(d))
        tmp$blue_L8 <-  df_color_after2013$blue[ind_l8]
        tmp$green_L8 <-  df_color_after2013$green[ind_l8]
        tmp$red_L8 <-  df_color_after2013$red[ind_l8]
        tmp$dwl_L8 <-  df_color_after2013$dwl[ind_l8]
      }
      
      if(length(ind_l7)>0 ){
        ind_l7 <- ind_l7[which.min(abs(as.numeric(df_color_after2013$date[ind_l7])-as.numeric(d)))]
        tmp$d_L7 <- df_color_after2013$date[ind_l7]
        tmp$d_diff_L7 <-  abs(as.numeric(df_color_after2013$date[ind_l7])-as.numeric(d))
        tmp$blue_L7 <-  df_color_after2013$blue[ind_l7]
        tmp$green_L7 <-  df_color_after2013$green[ind_l7]
        tmp$red_L7 <-  df_color_after2013$red[ind_l7]
        tmp$dwl_L7 <-  df_color_after2013$dwl[ind_l7]
      }
      df_compareSats <- rbind(df_compareSats, tmp)
    }
  }
}

ggplot(df_compareSats, aes(green_S2, green_L8))+geom_point()+theme_article()+geom_abline(slope = 1, intercept = 0)





df_color$doy <- as.numeric(strftime(df_color$date, format = "%j"))
df_color$month <- month(df_color$date)

head(df_color)

ggplot(df_color[order(df_color$date),], aes(doy, fui))+geom_point(aes(colour = satellite))+#geom_path()+
  theme_article()+facet_wrap(ID~.)

id = 1848
ggplot(df_color[which(df_color$ID==id),], aes(doy, dwl))+geom_point(aes(colour = satellite))+
  theme_article()+facet_wrap(ID~.)


df_color_sel <- df_color[which(df_color$month>=7 & df_color$month <=9),]

ggplot(df_color_sel[order(df_color_sel$date),], aes(date, fui))+geom_point(aes(colour = satellite))+
  theme_article()+facet_wrap(ID~.)

id = 1700
mydf <- df_color_sel[which(df_color_sel$ID==id),]
ggplot(mydf[order(mydf$date),], aes(date, fui, colour = satellite))+geom_point()+geom_path(aes(group_by = satellite))+geom_smooth()+
  theme_article()+facet_wrap(ID~.)


df_color_med <- data.frame(ID = unique(df_color_sel$ID))
i=0
for(id in df_color_med$ID){
  i=i+1
  df_color_med$depth[i] <- df_color_sel$MAXDEPTH_M[which(df_color_sel$ID == id)]
  df_color_med$lat[i] <- df_color_sel$LATITUDE[which(df_color_sel$ID == id)]
  df_color_med$long[i] <- df_color_sel$LONGITUDE[which(df_color_sel$ID == id)]
  df_color_med$altitude[i] <- df_color_sel$ALTITUDE_M[which(df_color_sel$ID == id)]
  df_color_med$catchment_ha[i] <- df_color_sel$CATCHMT_HA[which(df_color_sel$ID == id)]
  df_color_med$volume_m3[i] <- df_color_sel$VOLUME_M3[which(df_color_sel$ID == id)]
  df_color_med$wrt_months[i] <- df_color_sel$WRT_MONTHS[which(df_color_sel$ID == id)]
  
  
  df_color_med$fui_med[i] <- median(df_color_sel$fui[which(df_color_sel$ID == id)])
  df_color_med$fui_sd[i] <- sd(df_color_sel$fui[which(df_color_sel$ID == id)])
  df_color_med$dwl_med[i] <- median(df_color_sel$dwl[which(df_color_sel$ID == id)])
  df_color_med$dwl_sd[i] <- sd(df_color_sel$dwl[which(df_color_sel$ID == id)])
  df_color_med$x_med[i] <- median(df_color_sel$x[which(df_color_sel$ID == id)])
  df_color_med$x_sd[i] <- sd(df_color_sel$x[which(df_color_sel$ID == id)])
  df_color_med$y_med[i] <- median(df_color_sel$y[which(df_color_sel$ID == id)])
  df_color_med$y_sd[i] <- sd(df_color_sel$y[which(df_color_sel$ID == id)])
}


ggplot(df_color_med, aes(long, lat, colour = fui_med))+geom_point()+theme_article()


ggplot(df_color_med, aes(dwl_med, dwl_sd, colour = ID))+geom_point()+theme_article()

ggplot(df_color_med, aes(dwl_med))+geom_density()+theme_article()

ggplot(df_color_med, aes(x_med, y_med, colour = ID))+geom_point()+theme_article()


ggplot(df_color_med, aes(altitude, dwl_med))+geom_point()+theme_article()+geom_smooth(method = "lm")

ggplot(df_color_med, aes(depth, dwl_med))+geom_point()+theme_article()

ggplot(df_color_med, aes(wrt_months, dwl_med))+geom_point()+theme_article()






