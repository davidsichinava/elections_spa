### Read shapefiles
shp2008<-readShapePoly("data/parl2008/parl_precinct_2008.shp")
shp2008@data<-subset(shp2008@data, select=c(OBJECTID, PrecID, District_c, Precinct_c, Precinct_i))

shp2012<-readShapePoly("data/parl2012/parliamentary_precinct_2012.shp")
shp2016<-readShapePoly("data/parl2016/parl2016.shp")
shpgeo<-readShapePoly("data/georgia_outline/outline.shp")

shp2008<-as(shp2008, 'SpatialPolygonsDataFrame')
shp2012<-as(shp2012, 'SpatialPolygonsDataFrame')
shp2016<-as(shp2016, 'SpatialPolygonsDataFrame')

### 

georgia<-as(shpgeo, 'SpatialPolygonsDataFrame')


### Read raw vote counts
res2008<-read.csv("data/parl2008/2_parliamentary_2008.csv")
res2012<-read.csv("data/parl2012/4_parliamentary_2012.csv")
res2016<-read.csv("data/parl2016/7_parliamentary_2016.csv")

### Merge vote counts with boundary data
### We will be working on merged data
shp2016@data$PrecID<-shp2016@data$MID*100000+shp2016@data$District*1000+shp2016@data$Precinct

m2008 <- merge(shp2008, res2008, by.x='PrecID', by.y="Prec_ID")
m2012 <- merge(shp2012, res2012, by.x='PrecID', by.y="Prec_ID")
m2016 <- merge(shp2016, res2016, by.x='PrecID', by.y="precinct_id")

m2008@data$year<-2008
m2012@data$year<-2012
m2016@data$year<-2016