### Disaggregating 2008 vote count to the atoms
ZST<-(df8_12$area_int/df8_12$area_8)*df8_12$UNM_8
ZST<-as.data.frame(ZST)

### Aggregating estimated 2008 vote count to 2012 boundaries
slct<-subset(df8_12, select=c(District_8, PrecID_8, PrecID_12))
slct<-cbind(slct, ZST)
ZS<-aggregate(.~PrecID_12, slct, sum)

### Computing the ratio of 2008 in atoms out of 2012 precincts
slct<-merge(slct, ZS, by="PrecID_12", all.x=TRUE)
slct$ratio<-slct$PrecID_8.x/slct$PrecID_8.y
slct<-cbind(slct, df8_12$UNM_12)
names(slct)<-c("PrecID_12", "District", "PrecID_8.x", "ZST.x", "District2",  "PrecID_8.y", "ZST.y", "ratio", "UNM_12")

### Disaggregating 2012 vote counts to atoms according to the ratio
slct$toSum<-slct$UNM_12*slct$ratio

### Aggregating estimated 2012 vote counts to 2008 precinct
res.tdw<-aggregate(.~PrecID_8.x, slct, sum)
res.tdw<-subset(res.tdw, select=c(District, PrecID_8.x, toSum))
names(res.tdw)<-c("District", "PrecID_8", "tdw.interp")
res.tdw.aggr<-aggregate(.~District, slct, sum)
res.tdw.aggr<-subset(res.tdw.aggr, select=c(District, UNM_12, toSum))
res.tdw.aggr$tdw.rmse<-sqrt(abs(res.tdw.aggr$UNM_12-res.tdw.aggr$toSum))
