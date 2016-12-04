library(GISTools)

munC<-readShapePoly("data/municipalities/municipalities.shp")
munC<-as(munC, 'SpatialPolygonsDataFrame')
ethn<-read.csv("data/ethnicity/Ethnicity_Religion.csv", header=TRUE,
            stringsAsFactors = FALSE, sep=",")

relS<-c("Orthodox", "Muslim", "Armenian_Apostolic", "Catholic", "JW",
          "Yezidi_r", "Protestant", "Judaism", "Other_r", "None_r", "RA_r",
          "NtA")
ethns<-c("Georgian", "Azeri", "Armenian", "Russian", "Ossetian", "Yezidi",
         "Ukrainian", "Kist", "Greek", "Assyrian", "Other", "RA_E", "NA_E")

relEthn<-data.frame(matrix(ncol = 27, nrow = 71))
for(i in (3:14)){
  relEthn[i]<-(ethn[, i]/ethn$Total)^2
}

for(i in (15:27)){
  relEthn[i]<-(ethn[, i]/ethn$Total)^2
}

ethn$rFrac<-(1-as.numeric(rowSums(relEthn[,3:14])))
ethn$eFrac<-(1-as.numeric(rowSums(relEthn[,15:27])))

munC@data$id<-as.numeric(munC$OBJECTID)
df <- fortify(munC, region="id")
df <- merge(df, ethn, by="id", all.x=TRUE)

cnames <- aggregate(cbind(long, lat) ~ id, data=df, 
                    FUN=function(x)mean(range(x)))

cnames<-cbind(cnames, munC$NAME)

relplot<-ggplot()+
  geom_polygon(data=df, aes(long, lat, group=group, fill=rFrac), color="white", size=0.1)+
  coord_equal()+
  scale_fill_continuous(low="grey", high="red", guide = guide_legend(title = NULL))+
  geom_polygon(color = NA)  +
  geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
  labs(title = "Religious Fractionalization")+
# with(cnames, annotate(geom="text", x = long, y=lat, label = id, size = 2.5, 
#                       family= "BPG Excelsior Cond")) +
  theme_void()+
  theme(
    plot.title = element_text(colour = "Black", size=30, family = "Gill Sans MT"),
    legend.position = "bottom",
    legend.title=element_text(family="Garamond", face="bold", size=8),
    legend.text=element_text(family="Garamond", face="bold", size=8)
)

ethnplot<-ggplot()+
  geom_polygon(data=df, aes(long, lat, group=group, fill=eFrac), color="white", size=0.1)+
  coord_equal()+
  scale_fill_continuous(low="grey", high="#f46b42", guide = guide_legend(title = NULL))+
  geom_polygon(color = NA)  +
  geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
  labs(title = "Ethnic Fractionalization")+
  # with(cnames, annotate(geom="text", x = long, y=lat, label = id, size = 2.5, 
  #                       family= "BPG Excelsior Cond")) +
  theme_void()+
  theme(
    plot.title = element_text(colour = "Black", size=30, family = "Gill Sans MT"),
    legend.position = "bottom",
    legend.title=element_text(family="Garamond", face="bold", size=8),
    legend.text=element_text(family="Garamond", face="bold", size=8)
  )

ggsave("img/relplot.pdf", relplot, width = 12, height = 7, device=cairo_pdf)
ggsave("img/ethnplot.pdf", ethnplot, width = 12, height = 7, device=cairo_pdf)
