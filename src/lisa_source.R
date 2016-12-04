### Loop starts from here

IDs<-row.names(m2008)
coords<-coordinates(m2008)
m8w <- poly2nb(m2008)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)
m2008$IDs<-row.names(m2008)
wghts<-nb2listw(m8wg,zero.policy=TRUE)

### Moran's i
morans_i8 <- moran.mc(m2008@data$UNM/m2008@data$Total_vote,
                      listw = wghts, zero.policy = TRUE, nsim = 99999)

### Correlogram with spatial lags 
cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                       var = m2008@data$UNM/m2008@data$Total_vote, 
                       order = 10, method = "I", style = "C", randomisation = TRUE)

### Correlogram with distance classes
corD <- correlog(coordinates(m2008), m2008@data$UNM/m2008@data$Total_vote, method = "Moran")

corL<-as.data.frame(cor8$res)
names(corL)<-c("mor", "err", "pval")
corDc<-as.data.frame(corD)
corDc$dist.class<-corDc$dist.class/100
## Moran's plot using ggplot
### Lags
corL$band<-factor(1:nrow(corL))
corL$gr<-factor(c(1))

theme_mplot <- theme(
  axis.text.y = element_text(colour="black", size = 12, family = "Garamond"),
  axis.text.x = element_text(colour="black", size = 12, family="Garamond"),
  axis.title.x = element_text(size=14, family = "Gill Sans MT", face="bold"),
  axis.title.y = element_text(size=12, family = "Garamond", face="bold"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_line(colour = "grey"),
  plot.title = element_text(colour = "Black", size=14, family = "Gill Sans MT"),
  legend.position = "none"
)

lags <- ggplot(corL, aes(band, mor))+
  geom_line(aes(group=gr, colour=gr), size=1)+ 
  geom_point(aes(group=gr, colour="#070000"), size=3)+
  scale_color_manual(values=c("#070000", "#f44242"))+  
  labs(x="Lags", y = "Moran's I")+
  scale_y_continuous(limits = c(-0.3, 0.5))+
  geom_hline(yintercept = 0, colour="red")+
  theme_mplot
print(lags)  


### Distance
corDc$sig<-1
corDc$sig[corDc$p.val>0.001]<-0
corDc$sig<-as.factor(corDc$sig)
corDc$gr<-factor(c(1))

dist <- ggplot(corDc, aes(dist.class, coef))+
  scale_y_continuous(limits = c(-0.3, 0.5))+
  geom_line(aes(group=gr, colour=gr), size=1)+ 
  geom_point(aes(shape=sig, colour=sig), size=3)+
  scale_color_manual(values=c("#070000", "#f44242"))+  
  labs(x="Distance classes, m")+
  scale_x_continuous(labels=comma)+
  geom_hline(yintercept = 0, colour="red")+
  theme_mplot+
  theme(axis.title.y=element_blank())
print(dist)

mplot<-grid.arrange(lags, dist, ncol=2, 
                    top=textGrob("Moran's Local Autocorrelation, \n2008 Parliamentary Elections", 
                                 gp=gpar(fontsize=18, fontfamily="Gill Sans MT", fontface="bold")))
ggsave("mplot.pdf", mplot, width = 12, height = 6, device=cairo_pdf)

### Local Moran's i
localm <- localmoran(m2008@data$UNM/m2008@data$Total_vote, listw = wghts, zero.policy = TRUE)
m2008$UNM <- scale(m2008@data$UNM/m2008@data$Total_vote)
m2008$lag <- lag.listw(wghts, m2008@data$UNM)

### Moran's plot
x <- m2008$UNM
y <- m2008$lag
xx <- as.data.frame(cbind(x, y))
locmplot<-ggplot(xx, aes(x, y)) + 
  geom_point(alpha = 1/20) + 
  geom_smooth(method = 'lm', se = F, colour="red") + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(title="Moran's Scatterplot, 2008 Parliamentary", x="Scaled UNM Votes",
       y="Spatial Lag")+
  theme_mplot
print(locmplot)

ggsave("locmplot.pdf", locmplot, width = 12, height = 6, device=cairo_pdf)

### Lisa map

m2008$quad_sig <- NA

# high-high quadrant
m2008[(m2008$UNM >= 0 & 
         m2008$lag >= 0) & 
        (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
      , "quad_sig"] <- "high-high"
# low-low quadrant
m2008[(m2008$UNM <= 0 & 
         m2008$lag <= 0) & 
        (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
# high-low quadrant
m2008[(m2008$UNM >= 0 & 
         m2008$lag <= 0) & 
        (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
# low-high quadrant
m2008@data[(m2008$UNM <= 0 
            & m2008$lag >= 0) & 
             (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
# non-significant observations
m2008@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  

m2008$quad_sig <- as.factor(m2008$quad_sig)
m2008@data$id <- rownames(m2008@data)


df <- fortify(m2008, region="id")
df <- left_join(df, m2008@data)

lmap<-ggplot()+
  geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
  geom_polygon(color = NA)  + coord_equal() + 
  geom_polygon(data=georgia, aes(long, lat, group=group), alpha=1/10)+
  scale_fill_brewer(palette = "Set1")+
  labs(title="Vote Share for UNM in 2008 Parliamentary Elections, \nLocal Patterns of Autocorrelation")+
  theme_void()+
  theme(
    plot.title = element_text(colour = "Black", size=14, family = "Gill Sans MT"),
    legend.position = "bottom",
    legend.title=element_text(family="Garamond", face="bold", size=8),
    legend.text=element_text(family="Garamond", face="bold", size=8)
  )
print(lmap)

ggsave("lmap.pdf", lmap, width = 12, height = 7, device=cairo_pdf)
