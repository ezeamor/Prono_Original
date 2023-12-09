# Prono_Original

#######################################################

# DATOS DE PRONOSTICO ORIGINALES PARA EVALUAR ERRORES

#######################################################

graphics.off() # Elimina configuracion previa de otros graficos.

# Librerias a usar en este programa.

library(metR)
library(ggplot2)
library(maps)
library(mapdata)
library(ggrepel)
library(sf)
library(scales)
library(ncdf4)
library(patchwork)
library(rnaturalearth)

# Agregar los limites de los paises limitrofes (mapa_paises).

mapa_paises <- geom_sf(data=rnaturalearth::ne_countries(country = c("chile","uruguay","paraguay","brazil","bolivia"),returnclass="sf"),
                       inherit.aes=FALSE,fill=NA,color="black")

# Por si no estan cargados el resto de los limites, agregarlos con las siguientes 
# lineas. Recordar que si no estoy parado en las carpetas donde estan los archivos, 
# hay que cambiar el setwd para cargarlos.

mapa_pais          <- read_sf("paises/linea_de_limite_FA004.shp")
mapa_provincias    <- read_sf("provincias/linea_de_limite_070111.shp")
mapa_departamentos <- read_sf("departamentos/linea_de_limite_070110.shp")

####################################################

setwd("/fiona/Prono_Semanal/pre/Data/D20230328") #Cambiar segun la fecha del archivo.

# PRECIPITACION SEGUN LA MEDIA DEL ENSAMBLE.

data_media <- "prate.20230328.week2mean.nc"

pp_media   <- ReadNetCDF(data_media,vars="PRATE_surface", 
                         subset = list(latitude = c(-31,-22.5),longitude = c(296,308.4371))) 

# En caso de querer otra region de datos, modificar las latitudes y longitudes 
# de pp_media.

# Paso las longitudes de 0;360 a -180;180

pp_media$lon <- ConvertLongitude(pp_media$longitude, from = 360)

# Los datos de los archivos son "precipitation rate", con unidades de kg/m**2/s.
# Tengo que convertirlos a milimetros, por lo que tengo que multiplicar a cada 
# dato por 86400 para pasarlos a mm/d y luego por 7, dado que tenemos datos semanales.

PRATE_a_mm <- c()

for(i in 1:length(pp_media$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_media$PRATE_surface[i]*86400*7
}

pp_media$acumulado <- PRATE_a_mm

# Guardo el grafico en una variable.
# "coord_sf" son los limites de la figura. Cambiar en caso de que el area a graficar
# sea mayor o menor. 
# "annotate" es el rectágulo para marcar la región de interés.
# "guide=NULL" no muestra la escala. Si se quiere ver, eliminar este termino.

g_media <- ggplot(pp_media,aes(lon,latitude)) +
    geom_contour_fill(aes(z=acumulado)) +
    geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    mapa_paises +
    coord_sf(ylim=c(-30.3,-23),xlim=c(-63.23,-52.5)) +
    annotate("rect",xmin =-59.45,xmax=-57.55,ymin=-27.95,ymax=-26.05,fill="NA",color="red2",size=0.8) +
    labs(fill="mm",x="Longitud",y="Latitud",
         title= "Media del ensamble") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                           breaks=seq(0,100,10),oob=squish,guide=NULL) +
    scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL)

# Ahora miro los datos observados en la misma region. Cambiar el setwd para hacer
# esto segun donde esten guardado los archivos de precipitacion observada!.
# Si se quiere otra area de datos, cambiar lat y lon.

setwd("/home/ezequiel.amor/Menos_de_100")

pp1 <- ReadNetCDF("2023095.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp2 <- ReadNetCDF("2023096.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp3 <- ReadNetCDF("2023097.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp4 <- ReadNetCDF("2023098.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp5 <- ReadNetCDF("2023099.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp6 <- ReadNetCDF("2023100.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp7 <- ReadNetCDF("2023101.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95)))

suma_pp <- c()

for(i in 1:length(pp1$precipitation)) {
  suma_pp[i] <- pp1$precipitation[i] + pp2$precipitation[i] + pp3$precipitation[i] +
    pp4$precipitation[i] + pp5$precipitation[i] + pp6$precipitation[i] + 
    pp7$precipitation[i]
}

pp_total <- data.frame("lon"=pp1$lon,"lat"=pp1$lat,"precipitation"=suma_pp)

# Guardo el mapa en una variable.

g_obs <- ggplot(pp_total,aes(lon,lat)) +
    geom_contour_fill(aes(z=precipitation)) +
    geom_contour(aes(z=precipitation),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    mapa_paises +
    coord_sf(ylim=c(-30.3,-23),xlim=c(-63.23,-52.5)) +
    annotate("rect",xmin =-59.45,xmax=-57.55,ymin=-27.95,ymax=-26.05,fill="NA",color="red2",size=0.8) +
    labs(fill="mm",x="Longitud",y="Latitud",
         title= "Precipitación observada") +
    theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

####################################################

# PRECIPITACION SEGUN CADA MIEMBRO DEL ENSAMBLE.

setwd("/fiona/Prono_Semanal/pre/Data/D20230328") # Volver a cambiar a donde estan 
                                                 # los datos de pronostico.

# Como la fecha de cada miembro de pronostico es la misma de la media del
# ensamble, se la extrae del nombre de "data_media" para no escribirla a mano.
# Como hay 16 miembros, se repite el programa para cada uno cambiando el 
# numero del miembro.

# Además se guarda en una variable el maximo de cada miembro. Aquellos que no 
# tengan informacion, tendran un maximo igual a cero. Es una buena forma de que
# si hay algun warning al graficar, se pueda identificar facilmente cual es el 
# miembro que no posee datos.

# No hace falta modificar nada, salvo que quiere usarse otra region para la
# figura y allí deberían cambiarse la latitud y la longitud de los datos a leer 
# en pp_miembro y en los graficos.

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m01.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g1 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m01 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m02.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g2 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m02 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m03.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g3 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m03 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m04.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g4 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m04 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m05.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g5 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m05 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m06.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g6 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m06 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m07.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g7 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m07 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m08.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g8 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m08 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m09.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g9 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m09 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m10.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g10 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m10 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m11.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g11 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m11 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m12.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g12 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m12 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m13.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g13 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m13 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m14.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g14 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m14 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m15.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g15 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m15 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m16.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g16 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m16 <- max(pp_miembro$acumulado)

######################################################

# Combino todos los graficos.
# El grafico de precipitacion observada y la media del ensamble quedan uno arriba
# del otro, y los 16 miembros se grafican en paneles más pequeños hacia la derecha
# de los primeros dos graficos mencionados.

(g_obs/g_media)|(g1+g2+g3+g4+g5+g6+g7+g8+g9+g10+g11+g12+g13+g14+g15+g16)
