if(!require(OpenStreetMap)){install.packages("OpenStreetMap"); library(OpenStreetMap)}
library(ggmap)
map = OpenStreetMap::openmap(upperLeft = c(43, 119), 
                             lowerRight = c(33, 134), type = 'bing')
plot(map)
nm = c("osm", "mapbox", "stamen-toner", 
       "stamen-watercolor", "esri", "esri-topo", 
       "nps", "apple-iphoto", "osm-public-transport")
par(mfrow=c(3,3),  mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0))

for(i in 1:length(nm)){
  map <- openmap(c(43,119),
                 c(33,134),
                 minNumTiles = 3,
                 type = nm[i])
  plot(map, xlab = paste(nm[i]))
}
par(mfrow = c(1, 1))

map1 <- openmap(c(43.46,119.94),
                c(33.22,133.98))
autoplot(map1) # 좌표 값도 보여줌
plot(map1) 
abline(h = 38, col = 'blue') # 아무 일도 일어나지 않는다 openmap은 일반적인 plot과 좌표계가 다르기 때문이다
abline(h = 4500000, col = 'blue') # str(map1)을 통해 범위를 알 수 있다

str(map1)
map1$tiles[[1]]$projection
str(map1$tiles[[1]]$projection)

if(!require(sp)){install.packages("sp"); library(sp)} # Spatial Points
map_p <- openproj(map1, projection = CRS("+proj=longlat")) # 위경도 좌표계로 변했음
                                                 # +는 좌표계를 설정할 옵션 구분자
                                     # CRS(Coordinate Reference System)
str(map_p)
autoplot(map_p)
plot(map_p)
abline(h = 38, col = 'blue')

map_p <- openproj(map1, projection = 
                    CRS("+proj=utm +zone=52N + datum=WGS84"))
plot(map_p)
abline(h = 38, col = 'blue')
str(map_p)
# 이제 map_p에 line을 그을 수 없다. 그렇다면 점을 찍어서 잇자
## 데이터 프레임 만들기
a <- data.frame(lon = seq(100, 140, by=0.1), lat = 38); head(a)
## sp 클래스로 변환하기
sp::coordinates(a) = ~ lon + lat; head(a) # a has coords, bbox
str(a)
a@coords # slot 열 때는 @로 한다. $와 비슷한 역할
## projection을 설정하기
sp::proj4string(a) = "+proj=longlat" # map_p와 같게
str(a)
## 좌표계 변환하기
a_tf = spTransform(a,  CRS("+proj=utm +zone=52N + datum=WGS84"))  # map_p와 같게
str(a_tf)
# 지도위에 매핑하기
plot(map_p)
points(a_tf@coords[,1], a_tf@coords[,2], type='l', col='blue') # lon, lat으로 점 그리기 type = 'l'


# 지도 위에 파이차트 그리기
library(mapplots) # add.pie를 위해
map = openmap(upperLeft = c(43, 119),lowerRight = c(33, 134))
seoul_loc = geocode('Seoul') # 서울의 geocode를 받아옴(lon, lat)
coordinates(seoul_loc) = ~lon + lat # @coords를 lon과 lat 데이터로
proj4string(seoul_loc) = "+proj=longlat +datum=WGS84" 
seoul_loc_Tf = spTransform(seoul_loc,
                           CRS(as.character(map$tiles[[1]]$projection)))
plot(map)
add.pie(z=1:2,labels = c('a','b'),
        x = seoul_loc_Tf@coords[1],
        y = seoul_loc_Tf@coords[2], radius = 100000)


# 범죄 데이터

# only violent crimes
violent_crimes <- subset(crime,
                         offense != "auto theft" &
                           offense != "theft" &
                           offense != "burglary"
)
# rank violent crimes
violent_crimes$offense <-
  factor(violent_crimes$offense,
         levels = c("robbery", "aggravated assault",
                    "rape", "murder")
  )

# restrict to downtown
violent_crimes <- subset(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188 &
                           29.73631 <= lat & lat <= 29.78400
)
# get map and bounding box
theme_set(theme_bw(16))
HoustonMap <- qmap("houston", zoom = 14, color = "bw",
                   extent = "device", legend = "topleft")
HoustonMap <- ggmap(
  get_map("houston", zoom = 14, color = "bw"),
  extent = "device", legend = "topleft"
)
# the bubble chart
HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense, size = offense), data = violent_crimes) +
  scale_colour_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
  scale_size_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder"),
                      range = c(1.75,6)) +
  guides(size = guide_legend(override.aes = list(size = 6))) +
  theme(
    legend.key.size = grid::unit(1.8,"lines"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  labs(colour = "Offense", size = "Offense")

# or, with styling:
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite",
       color = offense, size = offense, legend = "topleft"
) +
  scale_colour_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
  scale_size_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder"),
                      range = c(1.75,6)) +
  guides(size = guide_legend(override.aes = list(size = 6))) +
  theme(
    legend.key.size = grid::unit(1.8,"lines"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  labs(colour = "Offense", size = "Offense")

# a contour plot
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, colour = offense),
                 size = 3, bins = 2, alpha = 3/4, data = violent_crimes) +
  scale_colour_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = grid::unit(1.8,"lines")
  )
# 2d histogram...
HoustonMap +
  stat_bin2d(aes(x = lon, y = lat, colour = offense, fill = offense),
             size = .5, bins = 30, alpha = 2/4, data = violent_crimes) +
  scale_colour_discrete("Offense",
                        labels = c("Robery","Aggravated Assault","Rape","Murder"),
                        guide = FALSE) +
  scale_fill_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = grid::unit(1.8,"lines")
  )
# ... with hexagonal bins
HoustonMap +
  stat_binhex(aes(x = lon, y = lat, colour = offense, fill = offense),
              size = .5, binwidth = c(.00225,.00225), alpha = 2/4, data = violent_crimes) +
  scale_colour_discrete("Offense",
                        labels = c("Robery","Aggravated Assault","Rape","Murder"),
                        guide = FALSE) +
  scale_fill_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = grid::unit(1.8,"lines")
  )



load('data/airport.Rdata')
head(airport_krjp)
map = ggmap(get_googlemap(center = c(lon=134, lat=36),
                          zoom = 5, maptype='roadmap', color='bw'))
map + geom_line(data=link_krjp,aes(x=lon,y=lat,group=group), 
                col='grey10',alpha=0.05) + 
  geom_point(data=airport_krjp[complete.cases(airport_krjp),],
             aes(x=lon,y=lat, size=Freq), colour='black',alpha=0.5) + 
  scale_size(range=c(0,15))



if (!require(sp)) {install.packages('sp'); library(sp)}
if (!require(gstat)) {install.packages('gstat'); library(gstat)}
if (!require(automap)) {install.packages('automap'); library(automap)}
if (!require(rgdal)) {install.packages('rgdal'); library(rgdal)}
if (!require(e1071)) {install.packages('e1071'); library(e1071)}
if (!require(dplyr)) {install.packages('dplyr'); library(dplyr)}
if (!require(lattice)) {install.packages('lattice'); library(lattice)}
if (!require(viridis)) {install.packages('viridis'); library(viridis)}
seoul032823 <- read.csv ("data/seoul032823.csv")
head(seoul032823)
skorea <- raster::getData(name ="GADM", country= "KOR", level=2)  # https://gadm.org/download_country_v3.html
# skorea <- readRDS("data/KOR_adm2.rds")
head(skorea,2)
if (!require(broom)) {install.packages('broom'); library(broom)}
## Loading required package: broom
skorea <- broom::tidy(skorea)
## Regions defined for each Polygons
class(skorea)
## [1] "data.frame"
head(skorea,3)
ggplot() + geom_map(data= skorea, map= skorea,
                    aes(map_id=id,group=group),fill=NA, colour="black") +
  geom_point(data=seoul032823, aes(LON, LAT, col = PM10),alpha=0.7) +
  labs(title= "PM10 Concentration in Seoul Area at South Korea",
       x="Longitude", y= "Latitude", size="PM10(microgm/m3)")
coordinates(seoul032823) <- ~LON+LAT
class(seoul032823)
LON.range <- c(126.5, 127.5)
LAT.range <- c(37, 38)
seoul032823.grid <- expand.grid(
  LON = seq(from = LON.range[1], to = LON.range[2], by = 0.01),
  LAT = seq(from = LAT.range[1], to = LAT.range[2], by = 0.01))
plot(seoul032823.grid)
points(seoul032823, pch= 16,col="red")
coordinates(seoul032823.grid)<- ~LON+LAT ## sp class
gridded(seoul032823.grid)<- T
plot(seoul032823.grid)
points(seoul032823, pch= 16,col="red")
seoul032823_OK <- autoKrige(formula = PM10~1,
                            input_data = seoul032823,
                            new_data = seoul032823.grid )
str(seoul032823_OK)
head(seoul032823_OK$krige_output)
head(seoul032823_OK$krige_output@coords, 2)
head(seoul032823_OK$krige_output@data$var1.pred,2)
myPoints <- data.frame(seoul032823)
myKorea <- data.frame(skorea)
myKrige <- data.frame(seoul032823_OK$krige_output@coords, 
                      pred = seoul032823_OK$krige_output@data$var1.pred)   
if(!require(viridis)){install.packages("viridis"); library(viridis)}

ggplot()+ theme_minimal() +
  geom_tile(data = myKrige, aes(x= LON, y= LAT, fill = pred)) +
  geom_map(data= myKorea, map= myKorea, aes(map_id=id,group=group),
           fill=NA, colour="black") +
  coord_cartesian(xlim= LON.range ,ylim= LAT.range) +
  labs(title= "PM10 Concentration in Seoul Area at South Korea",
       x="Longitude", y= "Latitude")+
  theme(title= element_text(hjust = 0.5,vjust = 1,face= c("bold")))+
  scale_fill_viridis(option="magma")






