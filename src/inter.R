library(leaflet)
library(geojsonio)
library(colorspace)
geojson_write(georgia, "georgia.json")
govr<- spTransform(georgia, CRS("+proj=longlat +datum=WGS84 +no_defs"))
m8<- spTransform(m2008, CRS("+proj=longlat +datum=WGS84 +no_defs"))
proj4string(m2008) <- CRS("+proj=longlat +datum=WGS84 +no_defs")


pal <- colorFactor(c("navy", "red"), domain = m2008$UNM)
map1<-leaflet() %>%
  setView(lng = 42.01, lat = 42.22, zoom = 7) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=govr,
              color = "#444444",
              fillOpacity = 0.4, 
              weight = 0.3, 
              smoothFactor = 0.2)
map1
  
  %>%
  addLegend(pal = pal, 
            values = df.polygon$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 

download.file(file.path('http://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/50m/cultural',
                        'ne_50m_admin_0_countries.zip'), 
              f <- tempfile())
unzip(f, exdir=tempdir())

world <- readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8')
