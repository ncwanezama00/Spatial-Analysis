install.packages(c("stars","sf","ggplot2","dplyr","raster","sp"))
library(stars) #for spatiotemporal arrays 
library(sf) #for working with vector data 
library(ggplot2)#for data visualization
library(dplyr)#for data manipulation
library(raster)#for working with raster data
library(sp)#for foundational spatial data hanldling

setwd("C:/Users/201807729/Documents/Practical 1")
getwd()
prov<-st_read("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data")
print(prov)
ggplot()+
  geom_sf(data = prov,size= 1.5,color="black",fill="cyan1")+
  ggtitle("Eastern Cape Boundary")+
  coord_sf()
install.packages("ggplot2")
ggplot2()+
  geom_sf(data = prov,size = 1.5, color = "black",fil="cyan1")+
  ggtitle("Eastern Cape Boundary")+
  coord_sf()
library(ggplot2)#the line code was not working needed rectification
ggplot2()+
  geom_sf(data = prov,size = 1.5, color = "black",fill="cyan1")+
  ggtitle("Eastern Cape Boundary")+
  coord_sf()
install.packages("ggplot")
ggplot()+
  geom_sf(data = prov,size = 1.5, color = "black",fill="cyan1")+
  ggtitle("Eastern Cape Boundary")+
  coord_sf()#the installed version for r in ggplot
dem = read_stars("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data/SA_DEM")
dem = read_stars("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data/SA_DEM/SA_DEM1.tif")
install.packages(stars)
library("stars")
install.packages("stars")
("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data/SA_DEM/SA_DEM1.tif")
dem=read_stars("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data/SA_DEM/SA_DEM1.tif")
install.packages("stars")
dem = read_stars("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data/SA_DEM/SA_DEM1.tif")
install.packages("stars")
library(stars)
dem <- read_stars("C:/Users/201807729/Documents/Practical 1/GIS 512 Prac 1 Data/SA_DEM/SA_DEM1.tif")
plot(dem)
