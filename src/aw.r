### Subset a data frame which contains only necessary variables
df8_12<-subset(as.data.frame(sect8_12), select=c(PrecID.1, District.x, UNM.x, sqkm8, PrecID.2, District.y, UNM, sqkm12, area))

names(df8_12)<-c("PrecID_8", "District_8", "UNM_8", "area_8", "PrecID_12", "District_12", "UNM_12", "area_12", "area_int")


### Conduct areal weighting and estimate 2012 outcomes in the boundaries of 2008
aw<-(df8_12$area_int/df8_12$area_12)*df8_12$UNM_12
names(aw)<-c("arwght")
df8_12<-cbind(df8_12, aw)

res.wgth<-aggregate(.~District_8, df8_12, sum)
res.wgth<-subset(res.wgth, select=c(District_8, aw, UNM_12))

### RMSE
res.wgth$aw.rmse<-sqrt(abs(res.wgth$UNM_12-res.wgth$aw))