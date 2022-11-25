## Eduard Martinez
## Update: 

## llamar pacman (contiene la función p_load)
require(pacman)

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## packages with census data

### **4.1 Acerca de OpenStreetMap**

### **4.2. Geocodificar direcciones**

## Buscar un lugar público por el nombre, la función se conecta a través del open strreet map
geocode_OSM("Casa de Nariño, Bogotá")

## geocode_OSM no reconoce el caracter #, en su lugar se usa %23% 
cbd <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T) #se adicieno el as.sf para codifiarlo como objeto
cbd

## la función addTiles adiciona la capa de OpenStreetMap. para viasualizar
leaflet() %>% addTiles()  %>% addCircles(data=cbd)

### **4.3. Librería `osmdata`**. descargar open street maps
#### **4.3.1. Features disponibles**

available_features() %>% head(20)

available_tags("amenity") %>% head(20)

### **4.4. Descargar features**
  
## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Bogotá Colombia"))

## objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
      add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
ors = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar")
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
ors_sf <- ors %>% osmdata_sf()

## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station
bares <- ors_sf$osm_points %>% select(osm_id, amenity)
bares
##Al seleccionar la columna id, nos dan los puntos sobre los cuales se encuentran los bares en bogota
## Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red")

leaflet() %>% addTiles() %>% addCircleMarkers(data= bares, col="blue")
## **[5.] Operaciones geometricas**

### **5.1 Importar conjuntos de datos**

## my_house
my_house <- geocode_OSM("Calle 26 %23% 4-29, Bogotá", as.sf=T) 
my_house
casa_vicky <- geocode_OSM("Cra 39B %23% 4-45, Bogotá", as.sf=T)
casa_vicky
leaflet() %>% addTiles() %>% addCircles(data= casa_vicky)

## parques
parques <- opq(bbox = getbb("Bogota Colombia")) %>%
           add_osm_feature(key = "leisure", value = "park") %>%
           osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addPolygons(data=parques)

### **5.2 help:** `sf`

## Help
vignette("sf3")
vignette("sf4")

  ### **5.3 Afine transformations**
  
st_crs(my_house) == st_crs(parques) 

  ### **5.4 Filtrar datos**
  
## usando la geometría
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)

## crop puntos con poligono (opcion 2)
parques_chapi <- st_intersection(x = parques , y = chapinero)

leaflet() %>% addTiles() %>% addPolygons(data=chapinero,col="red") %>% addCircles(data=parques_chapi)

## crop puntos con poligono (opcion 3)
parques_chapi <- parques[chapinero,]

leaflet() %>% addTiles() %>% addPolygons(data=chapinero,col="red") %>% addCircles(data=parques_chapi)

  ### **5.5. Distancia a amenities**
  
## Distancia a un punto
my_house$dist_cbd <- st_distance(x=my_house , y=cbd)

my_house$dist_cbd %>% head()

## Distancia a muchos puntos
matrix_dist_bus <- st_distance(x=my_house , y=bus_station)

matrix_dist_bus[1:5,1:5]

min_dist_bus <- apply(matrix_dist_bus , 1 , min)

min_dist_bus %>% head()

my_house$dist_buse = min_dist_bus

## Distancia a muchos polygonos
matrix_dist_parque <- st_distance(x=my_house , y=parques)

matrix_dist_parque[1:5,1:5]

mean_dist_parque <- apply(matrix_dist_parque , 1 , mean)

mean_dist_parque %>% head()

my_house$dist_parque = mean_dist_parque

  ## **[6.] Visualizaciones**
  
## get Bogota-UPZ 
bog <- opq(bbox = getbb("Bogota Colombia")) %>%
      add_osm_feature(key="boundary", value="administrative") %>% 
      osmdata_sf()
bog <- bog$osm_multipolygons %>% subset(admin_level==9)

bog <- import("output/bog_upz.rds")

## basic plot
ggplot() + geom_sf(data=bog)

## plot variable
bog$normal <- rnorm(nrow(bog),100,10)
ggplot() + geom_sf(data=bog , aes(fill=normal))

## plot variable + scale
map <- ggplot() + geom_sf(data=bog , aes(fill=normal)) +
  scale_fill_viridis(option = "A" , name = "Variable")
map 

## add scale_bar
map <- map +
  scalebar(data = bog , dist = 5 , transform = T , dist_unit = "km") +
  north(data = bog , location = "topleft")
map 

## add theme
map <- map + theme_linedraw() + labs(x="" , y="")
map

## add osm layer
osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(bog)), 
                           maptype="toner", source="osm", zoom=13) 

map2 <- ggmap(osm_layer) + 
  geom_sf(data=bog , aes(fill=normal) , alpha=0.3 , inherit.aes=F) +
  scale_fill_viridis(option = "D" , name = "Variable") +
  scalebar(data = bog , dist = 5 , transform = T , dist_unit = "km") +
  north(data = bog , location = "topleft") + theme_linedraw() + labs(x="" , y="")
map2
