#31/03/2022
#Trabajar con imagenes landsat
#Dennis Alvarino Cieza Tarrillo

# Instalar paquetes -------------------------------------------------------
#install.packages(c("tidyverse", "raster", "RStoolbox", "sf", "ggspatial"))
#devtools::install_github("strengejacke/strengejacke")

# Cargar librerias --------------------------------------------------------
library(tidyverse)
library(raster)
library(RStoolbox)
library(sf)
library(ggspatial)
library(strengejacke)

# Ruta de trabajo ---------------------------------------------------------
setwd("D:/GITHUB/Landsat/")

#browseURL("https://spatialreference.org/") #Pagina para las proyecciones

# Lectura de datos --------------------------------------------------------
#Shapefile
shp <- read_sf("D:/GITHUB/Landsat/Shapefile/Po_Chota.shp") %>% 
  st_transform(crs="+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

plot(shp[1])
proy <- "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #Borramos south porque landsat esta en norte

#Descomprimiendo datos
zip <- "LC08_L1TP_009065_20200804_20200821_01_T1.tar.gz" #Lectura de imagen comprimida
untar(zip) #Extracción de archivos

# Leer metadatos ----------------------------------------------------------
mtl <- "LC08_L1TP_009065_20200804_20200821_01_T1_MTL.txt" #Reemplazar por el nombre del metadato
(MTL <- readMeta(mtl)) #Ver metadatos

lsat <- stackMeta(mtl) %>% #Compilación de bandas
  crop(shp %>% st_transform(proy)) %>%  mask(shp %>% st_transform(proy)) #Corte de la imagen

# Normalized Difference Vegetation Index (NDVI) ---------------------------
#NDVI (Landsat 8) = (B5 - B4) / (B5 + B4)
red <- lsat$B4_dn
nir <- lsat$B5_dn

#NDVI
ndvi <- ((nir-red)/(nir+red)) %>%
  projectRaster(crs = "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
ndvi[ndvi<0] <- 0

# Ploteo NDVI -------------------------------------------------------------

nd <- ndvi %>% as("SpatialPixelsDataFrame") %>% as.data.frame %>% tibble

g1 <- ggplot()+
  geom_raster(data = nd, aes(x=x, y=y, fill=layer))+
  theme_classic()+
  labs(title = "NDVI DISTRITO DE CHOTA")+
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand=c(0.01,0.01))+
  scale_fill_viridis_c(direction = -1)+
  guides(fill=guide_legend(title = "NDVI", reverse = T))+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))+
  annotation_scale(location = "br",width_hint = 0.2,
                   height = unit(0.2, "cm"),
                   pad_x = unit(1, "in"),
                   pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"), 
                         style = north_arrow_nautical,
                         height = unit(3, "cm"),
                         width = unit(3, "cm"))

# Guardar ploteo ----------------------------------------------------------

sjPlot::save_plot("NDVI.png", fig = g1, width = 20, height = 18, dpi = 300)



