install.packages(c('sf','spdep','spatialreg'))#install packages
library(sf)
library(spdep)
library(spatialreg)
spat.data <- st_read("C:/Users/201807729/Downloads/Practical 5/Data/NCVACO.shp")#spat data
names(spat.data)
summary(spat.data)
spat.data$PCI=as.numeric(levels(spat.data$PCI))[spat.data$PCI]
summary(spat.data)
install.packages("ggplot2")#install packages
library(ggplot2)
ggplot() +
  geom_sf(data = spat.data, aes(fill = SALESPC)) +
  labs (title = "Map of SALESPC",
        fill = "SALESPC") +
  theme_minimal()
queen.nb=poly2nb(spat.data)
rook.nb=poly2nb(spat.data,queen=FALSE)
#summary
summary(queen.nb)
is.symmetric.nb(queen.nb)
queen.listw=nb2listw(queen.nb)
rook.listw=nb2listw(rook.nb)
listw1= queen.listw
#reg
reg.eq1=DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX+ ENTRECP
options(scipen=7)
reg1=lm(reg.eq1,data=spat.data)
lm.morantest(reg1,listw1)
lm.LMtests(reg1,listw1,test= "all")
reg2 =lmSLX(reg.eq1,data=spat.data, listw1)
summary(reg1)
impacts(reg2,listw=listw1)
#summary
summary(impacts(reg2,listw=listw1,R=500),zstats=TRUE)
x1=model.matrix(reg1)
lagx1=create_WX(x1,listw1,prefix="lagx")
spat.data2=cbind(spat.data,lagx1)
reg2b=lm(DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX
         +ENTRECP+ lagx.SALESPC+ lagx.COLLENRP +lagx.BKGRTOABC +lagx.BAPTISTSP
         +lagx.BKGRTOMIX +lagx.ENTRECP, data=spat.data2)
reg2c=lm(DUI1802~SALESPC+COLLENRP+BKGRTOABC+BAPTISTSP+BKGRTOMIX
         +ENTRECP+ lagx.SALESPC+ lagx.COLLENRP +lagx.BKGRTOABC +lagx.BAPTISTSP
         +lagx.BKGRTOMIX +lagx.ENTRECP-1, data=spat.data2)
#summary
summary(reg2b)
summary(reg2c)
rsq.reg2=1-sum(reg2$residuals^2)/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)-1))
rsq.reg2b=1-sum(reg2b$residuals^2)/(var(spat.data$DUI1802)*(length(spat.data$DUI1802)- 1))
rsq.reg2
rsq.reg2b
reg3=lagsarlm(reg.eq1,data= spat.data, listw1)
#summary
summary(reg3)
#impacts
impacts(reg2,listw=listw1)
summary(impacts(reg2,listw=listw1,R=500),zstats=TRUE)
impacts(reg3,listw=listw1)
summary(impacts(reg3,listw=listw1,R=500),zstats=TRUE)
reg4=errorsarlm(reg.eq1,data=spat.data, listw1)
#summary
summary(reg4)
Hausman.test(reg4)
