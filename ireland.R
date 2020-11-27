library(stplanr)
library(osmdata)
library(osrm)
library(leaflet)
library(htmltools)
library(sp)
library(tmap)
library(readr)
library(janitor)
library(ggplot2)
library(lubridate)
library(ggtext)
library(htmlwidgets)
library(smoothr)


# run in terminal:
# exiftool -common -filename -gpslatitude -gpslongitude -GPSAltitudeRef -csv /Users/xxx/Pictures/Photos\ Library.photoslibrary/originals/ -r > /Users/xxx/metadata.csv


#inspiration from https://twitter.com/lxndrkp/status/1325727846145413122?s=20
#data
photos <- read_csv("metadata.csv") %>% clean_names()
irl <- st_read("Ireland/Ireland.shp") #http://www.arcgis.com/home/item.html?id=ae74a8497a1041669a9d2165a0f450b6

#convert coordinates to decimals
gps_coord<- function(x){
  clean_x<-x %>%
    stringr::str_replace_all( fixed(" deg "), "°") %>%
    stringr::str_replace_all( fixed("' "), "'") %>%
    stringr::str_replace_all( fixed("\" "), "s")

  chd<-"°"
  chm<-"'"
  chs<-"s"
  cd2 = char2dms(clean_x,chd=chd,chm=chm,chs=chs) %>%
    as.numeric()

  return(cd2)
}

#selecting pictures with gps info, taken with iphone
slected_photos<-photos %>%
  filter(!is.na(gps_latitude),
         str_detect(model, 'iPhone')) %>%

  mutate(gps_latitude_dec=gps_coord(gps_latitude),
         gps_longitude_dec=gps_coord(gps_longitude),
         date=ymd_hms(date_time_original)) %>%
  arrange(date_time_original) %>%
  filter(year(date)>2010)

# select bounding box for Ireland
# create sf object
irl_geom_photo<-slected_photos %>%
  filter(
    gps_longitude_dec > -11.315918,
    gps_longitude_dec < -5.295410,
    gps_latitude_dec > 51.371780,
    gps_latitude_dec < 55.441479,
  ) %>%
  st_as_sf(coords = c("gps_longitude_dec", "gps_latitude_dec"), crs = 4326)%>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POINT")


#setting starting and ending points (simply split my collection of pictures in two)
start_points <- irl_geom_photo %>%slice(1:359) %>%  st_as_sf
end_points <- irl_geom_photo %>% slice(360:718) %>% st_as_sf

# computing the routes
routes <- route(from = start_points,
                to = end_points,
                route_fun = osrmRoute,
                returnclass = "sf")

#creating the overline
routes["count"] <- 1
overlapping_segments <- overline(routes, attrib = "count")
# plotting
irl_dark<-leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolylines(data=overlapping_segments,
               weight = overlapping_segments$count / 8,
               color = "#009A49") %>%
  addCircles(data=irl_geom_photo,
             color="#FF7900",
             weight=6) %>%
  addControl(html = paste(tags$h1(HTML("West Atlantic Road Trip")),
                          tags$div(HTML("Mapping the roads with iPhone pictures and stplanr package"))),
             position = "topleft")

#saveWidget(irl_dark, file="irl_dark.html")


#ggplot version with Irish flag colours
overlapping_segments_smooth<-smooth(overlapping_segments, method="chaikin")
irl_light<-ggplot()+
  geom_sf(data=irl, fill="white", color="grey")+
  geom_sf(data=overlapping_segments_smooth ,
          color="#009A49" ,
          alpha=overlapping_segments$count / 5,
          size=overlapping_segments$count / 20)+
  geom_sf(data=irl_geom_photo, color="#FF7900")+
  labs(
    title="West Atlantic Road Trip",
    subtitle="Mapping the roads with iPhone pictures and stplanr package",
    caption="Authors pictures, osm, stplanr"
  )+

  theme_void()+
  theme(panel.background = element_rect(fill = "#cde2f4"),
        plot.title = element_markdown(colour="#009A49",
                                  family = "Futura Condensed Medium",
                                  size = 20),
    plot.subtitle =  element_markdown(colour="#009A49",
                                      family = "Futura Condensed Medium",
                                      size = 15),
    plot.caption=  element_markdown(colour="#009A49",
                                    family = "Futura Condensed Medium",
                                    size = 10)
  )
#ggsave("irl_light.jpg")
