library(leaflet)
library(htmltools)
library(pgirmess)
library(gridExtra)
library(grid )
library(ggplot2)
library(GeoXp)
library(sp)
library(ggsn)
library(extrafont)
library(scales)

fgeorgia <- fortify(georgia, region="OBJECTID_1")


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

etable<-as.data.frame(setNames(replicate(3,numeric(0), simplify = F), letters[1:3]))

genm<-function(spdf){
  
  IDs<-row.names(spdf)
  coords<-coordinates(spdf)
  m8w <- poly2nb(spdf)
  m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)
  spdf$IDs<-row.names(spdf)
  wghts<-nb2listw(m8wg,zero.policy=TRUE)
  
  ### Moran's i
  morans_i8 <- moran.mc(spdf@data$UNM/spdf@data$Total_vote, 
                        listw = wghts, zero.policy = TRUE, nsim = 99999)
  
  ### Correlogram with spatial lags 
  cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                         var = spdf@data$UNM/spdf@data$Total_vote, 
                         order = 10, method = "I", style = "C", randomisation = TRUE)
  
  ### Correlogram with distance classes
  corD <- correlog(coordinates(spdf), spdf@data$UNM/spdf@data$Total_vote, method = "Moran")
  
  corL<-as.data.frame(cor8$res)
  names(corL)<-c("mor", "err", "pval")
  corDc<-as.data.frame(corD)
  ## Moran's plot using ggplot
  ### Lags
  corL$band<-factor(1:nrow(corL))
  corL$gr<-factor(c(1))
  
  lags <- ggplot(corL, aes(band, mor))+
    geom_line(aes(group=gr, colour=gr), size=1)+ 
    geom_point(aes(group=gr, colour="#070000"), size=3)+
    scale_color_manual(values=c("#070000", "#f44242"))+  
    labs(x="Lags", y = "Moran's I")+
    scale_y_continuous(limits = c(-0.3, 0.7))+
    geom_hline(yintercept = 0, colour="red")+
    theme_mplot
  
  tname<-spdf$year[1]

  ### Distance
  corDc$sig<-1
  corDc$sig[corDc$p.val>0.001]<-0
  corDc$sig<-as.factor(corDc$sig)
  corDc$gr<-factor(c(1))
  
  dist <- ggplot(corDc, aes(dist.class, coef))+
    scale_y_continuous(limits = c(-0.3, 0.7))+
    geom_line(aes(group=gr, colour=gr), size=1)+ 
    geom_point(aes(shape=sig, colour=sig), size=3)+
    scale_color_manual(values=c("#070000", "#f44242"))+  
    labs(x="Distance classes, m")+
    scale_x_continuous(labels=comma)+
    geom_hline(yintercept = 0, colour="red")+
    theme_mplot+
    theme(axis.title.y=element_blank())
  plotTitle<-paste0("Moran's Local Autocorrelation, \n", tname, " Parliamentary Elections")
  mplot<-grid.arrange(lags, dist, ncol=2, 
                      top=textGrob(plotTitle, 
                                   gp=gpar(fontsize=18, fontfamily="Gill Sans MT", fontface="bold")))
  expdir<-paste0("img/", tname, "_mplot.pdf")
  ggsave(expdir, mplot, width = 12, height = 6, device=cairo_pdf)
  
  ### Local Moran's i
  localm <- localmoran(spdf@data$UNM/spdf@data$Total_vote, listw = wghts, zero.policy = TRUE)
  spdf$UNM <- scale(spdf@data$UNM/spdf@data$Total_vote)
  spdf$lag <- lag.listw(wghts, spdf@data$UNM/spdf@data$Total_vote)
  
  ### Moran's scatterplot
  x <- spdf$UNM
  y <- spdf$lag
  xx <- as.data.frame(cbind(x, y))

  plotTitle<-paste0("Moran's Scatterplot, ", tname, " Parliamentary Elections")
  
  locmplot<-ggplot(xx, aes(x, y)) + 
    geom_point(alpha = 1/20) + 
    geom_smooth(method = 'lm', se = F, colour="red") + 
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    geom_vline(xintercept = 0, linetype = 'dashed') +
    labs(title=plotTitle, x="Scaled UNM Votes",
         y="Spatial Lag")+
    theme_mplot
expdir<-paste0("img/", tname, "_locmplot.pdf")
ggsave(expdir, locmplot, width = 12, height = 6, device=cairo_pdf)
  
  ### Lisa map
  
  spdf$quad_sig <- NA
  
  # high-high quadrant
  spdf[(spdf@data$UNM/spdf@data$Total_vote >= 0 & 
          spdf$lag >= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
       , "quad_sig"] <- "high-high"
  # low-low quadrant
  spdf[(spdf@data$UNM/spdf@data$Total_vote <= 0 & 
          spdf$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
  # high-low quadrant
  spdf[(spdf@data$UNM/spdf@data$Total_vote >= 0 & 
          spdf$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
  # low-high quadrant
  spdf@data[(spdf@data$UNM/spdf@data$Total_vote <= 0 
             & spdf$lag >= 0) & 
              (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
  # non-significant observations
  spdf@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  
  
  spdf$quad_sig <- as.factor(spdf$quad_sig)
  spdf@data$id <- rownames(spdf@data)
  
  
  df <- fortify(spdf, region="id")
  df <- left_join(df, spdf@data)
  plotTitle<-paste0("Vote Share for UNM in ", tname, " Parliamentary Elections, \nLocal Patterns of Autocorrelation")
  lmap<-ggplot()+
    geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
    geom_polygon(color = NA)  + coord_equal() + 
    geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
    scale_fill_brewer(palette = "Set1")+
    labs(title = plotTitle)+
    theme_void()+
    theme(
      plot.title = element_text(colour = "Black", size=14, family = "Gill Sans MT"),
      legend.position = "bottom",
      legend.title=element_text(family="Garamond", face="bold", size=8),
      legend.text=element_text(family="Garamond", face="bold", size=8)
    )

  expdir<-paste0("img/", tname, "_lmap.pdf")
  ggsave(expdir, lmap, width = 12, height = 7, device=cairo_pdf)
  dtable<-as.data.frame(cbind(tname, morans_i8$statistic, morans_i8$p.value))
  etable<-rbind(etable, dtable)
}

genm(m2008)
genm(m2012)
genm(m2016)


IDs<-row.names(m2016)
coords<-coordinates(m2016)
m8w <- poly2nb(m2016)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)
m2016$IDs<-row.names(m2016)
wghts<-nb2listw(m8wg,zero.policy=TRUE)

morans_i8 <- moran.mc(m2016@data$UNM/m2016@data$Total_vote, 
                      listw = wghts, zero.policy = TRUE, nsim = 99999)
dtable<-as.data.frame(cbind("m2016", morans_i8$statistic, morans_i8$p.value))
etable<-rbind(etable, dtable)

.