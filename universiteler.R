library(rvest)
library(xml2)
library(dplyr)
library(ggplot2)
library(ggmap)

url <- "https://tr.wikipedia.org/wiki/Türkiye'deki_üniversiteler_listesi"

devlet <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
  html_table()
devlet <- devlet[[1]]

vakif <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[3]') %>%
  html_table()
vakif <- vakif[[1]]

colnames(devlet)  <- c("No", "Ad", "Kurulus", "Il", "Ogrenci", "Tur", "Websitesi")
colnames(vakif) <- c("No", "Ad", "Kurulus", "Il", "Ogrenci", "Tur", "Websitesi")

devlet$Kurulus <- gsub('.*\\/','',devlet$Kurulus)
devlet$Kurulus <- as.integer(devlet$Kurulus)

universiteler <- bind_rows(devlet, vakif)
universiteler <- select(universiteler, Ad:Tur)

universiteler$Ad <- gsub('\\*.*', '', universiteler$Ad)

universiteler$Ogrenci <- gsub('\\*.*','', universiteler$Ogrenci)
universiteler$Ogrenci <- gsub('\\[.*','', universiteler$Ogrenci)
universiteler$Ogrenci <- gsub('\\.', '', universiteler$Ogrenci)
universiteler$Ogrenci <- gsub('\\,', '', universiteler$Ogrenci)
universiteler$Ogrenci <- as.integer(universiteler$Ogrenci)

universiteler2 <- filter(universiteler, !is.na(Ogrenci))

ggplot(universiteler2, aes(x=Kurulus, y=Ogrenci, color=Tur)) +
  geom_point() + 
  xlab("Kurulus Yili") +
  ylab("Ögrenci Sayisi") +
  geom_text(aes(label = Ad))

ggplot(universiteler2, aes (x=Kurulus, y=Ogrenci)) +
  geom_point() +
  facet_wrap(~Tur, scales = "free_y")

universiteler2 %>%
  mutate(lonlat = geocode(Ad))

showonmap <- function(loc, zm = 8, ext = "device") {
  # Map location
  lonlat <- geocode(loc)
  loc_map <- get_map(lonlat, zoom=zm)
  ggmap(loc_map, extent = ext) + geom_point(data=lonlat, aes(lon, lat, col="red")) + 
    theme(legend.position="none")
}

sehir <- "Ankara"
my_map <- showonmap(sehir, 10, 1)
unitoshow <- universiteler2 %>% filter(Il == sehir, Kurulus > 1900)
my_map + geom_point(data = unitoshow, aes(x=lon, y=lat, size = 1, col = "red" ))

save(universiteler, file="unidata.RData")
