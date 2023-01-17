library(sf)
library(raster)
library(ggplot2)
SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru1  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru2  <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD  =  subset(Peru2 , NAME_1 == "Madre de Dios")

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru1 , fill="gray", color="black", size=0.05)+
  geom_sf(data = MDD, fill="black", color="black", size=0.01)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 12, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
                      label = "Pacific ocean",size = 3, family="serif", color = 
                        "black",  fontface="italic", angle=90)+
                        annotate(geom = "text", x = -60, y = -50, hjust = 0, vjust = 1, 
                                 label = "Atlantic ocean",size = 3, family="serif", color = 
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -70, y = -4, hjust = 0, vjust = 1, 
                                            label = "Peru",size = 3, family="serif", color = 
                                              "black",  fontface="italic")
                                            
SurA

Cuenca = st_read("SHP/Cuenca.shp")  %>% st_as_sf()
Cuenca_MDD  <- st_transform(Cuenca  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_MDD_box = st_as_sfc(st_bbox(Cuenca_MDD))

library(ggspatial)
MDD_map= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru1 , fill="gray", color="black", size=0.05)+
  geom_sf(data = MDD, fill="black", color="black", size=0.01, alpha=0.3)+
  geom_sf_text(data = MDD, aes(label=NAME_2), size=2)+
  geom_sf(data = Cuenca_MDD_box, fill=NA, size=1, color="black")+
  geom_sf(data = Cuenca_MDD, fill="black", size=0.1)+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -71.5, y = -13.2, hjust = 0, vjust = 1, 
           label = "Cusco",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             annotate(geom = "text", x = -69.5, y = -13.5, hjust = 0, vjust = 1, 
                      label = "Puno",size = 3, family="serif", color = 
                        "black",  fontface="italic")+
                        annotate(geom = "text", x = -68.9, y = -11.5, hjust = 0, vjust = 1, angle=300,
                                 label = "Bolivia",size = 3, family="serif", color = 
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -70, y = -10.5, hjust = 0, vjust = 1, 
                                            label = "Brasil",size = 3, family="serif", color = 
                                              "black",  fontface="italic")+
                                              annotate(geom = "text", x = -72, y = -10.5, hjust = 0, vjust = 1, 
                                                       label = "Ucayali",size = 3, family="serif", color = 
                                                         "black",  fontface="italic")+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

Distrito = st_read("SHP/MDD.geojson")  %>% st_as_sf()
Distritos  <- st_transform(Distrito   ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Cuenca_map= ggplot()+

  geom_sf(data = Distritos, fill="gray80", color="black", size=0.01)+
  geom_sf(data = Cuenca_MDD, fill="black", size=0.1, alpha=0.5)+
  coord_sf(xlim = c(-69.89,  -69.15), ylim = c(-13,-12.20))+
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.margin = unit(c(0,0,0,0), "cm"),
      plot.margin = unit(c(0,0,0,0), "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none", 
      
      panel.background = element_rect(fill = "#a2d2ff"),
      panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.9, y = -12.6, hjust = 0, vjust = 1, 
           label = "Laberinto",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             annotate(geom = "text", x = -69.6, y = -12.4, hjust = 0, vjust = 1, 
                      label = "Tambopata",size = 3, family="serif", color = 
                        "black",  fontface="italic")+
                        annotate(geom = "text", x = -69.8, y = -12.96, hjust = 0, vjust = 1, 
                                 label = "Inambari",size = 3, family="serif", color = 
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -69.2, y = -12.4, hjust = 0, vjust = 1, angle=90,
                                            label = "Las Piedras",size = 3, family="serif", color = 
                                              "black",  fontface="italic")+
                                              annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")
                                 
CP = st_read("SHP/CP.SHP")  %>% st_as_sf()
Centro <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Centro_xy <- cbind(Centro, st_coordinates(st_centroid(Centro$geometry)))


Via_Mal <- st_read ("SHP/Red_vial.shp") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

Cover <- st_read ("SHP/Cover.shp") # Caragmos un shp de puerto maldonado
Cover_cuenca  <- st_transform(Cover ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion


library(ggrepel)
library(ggnewscale) 
col=c("#E6A759","#D6842E", "#CD6F31",
               "#DA875B", "#89D79E", "#949956","#74A6A5", "#E0E080", "#689962", 
               "#7EDDD5", "#82E07F","#ACC38B", "#88BF6D", "#759E80", "#A1D9BE", 
               "#6AB9A2", "#B9DB85" ,"#E1E3A1", "#7AB388", "#85E2C1", "#569986",
               "#B8BA71", "#999973", "#C7C595", "#A4DADA", "#92C487", "#8BAD71")
               
Cuenca_mapa= ggplot()+
  
  geom_sf(data = Distritos, aes(fill=distrito_), color="black", size=0.01, show.legend = F)+
  scale_fill_manual(values = grey.colors(13, rev = TRUE))+
  

  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  
  geom_sf(data = Cuenca_MDD, fill=NA, size=1, color="red")+
  new_scale_fill()+
  geom_sf(data=Cover_cuenca, aes(fill = CobVeg2013),  size=0.05, color=NA)+
  scale_fill_manual(values = col, name="Covertura Vegetal de Intercuenca Medio Alto Madre de Dios",)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  
  guides(fill = guide_legend(nrow = 4, ncol=3))+
  coord_sf(xlim = c(-70,  -69.00), ylim = c(-13,-12.20))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

library(ggpubr)
legend <- get_legend(Cuenca_mapa)

Cuenca_Mapa2= Cuenca_mapa + theme(legend.position = "none")+geom_label_repel(data = Centro_xy, aes(x = X, y = Y, label = NOMBRE), 
                 family="serif", box.padding = unit(0.9, "lines"), size =2, face = "bold",color = 'black',
                 point.padding = unit(0.5, "lines"))+
  geom_point(data= Centro_xy, aes(X,Y), show.legend = FALSE) 
  


my_bbox <- c(xmin = min(-69.46793), xmax = max(-69.34982),  ymin = min(-12.69133),  ymax = max(-12.51036))
my_bbox.m <- matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'],  my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']), ncol = 2)

my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

my_bb<-  my_bbox.sf %>% st_transform(crs = 32632) %>%
  st_buffer(dist = 2500) %>% # 2.5 kilometers
  st_transform(crs = 4326)
Zona<- st_transform(my_bb,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
write_sf(Zona, "SHP/Zona.shp")
Zon= st_read("SHP/Zona.shp")  %>% st_as_sf()
Zona <- st_transform(Zon ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

library(elevatr)
elev = get_elev_raster(Zona  , z=12)
plot(elev)
Poligo_alt    <- crop(elev, Zona )                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Zona )
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Zona_map =ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+ scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data=Cover_cuenca, aes(fill = CobVeg2013),  size=0.05, color=NA, show.legend = F,  alpha=0.6)+
  scale_fill_manual(values = col)+
  
  guides(fill = guide_legend(nrow = 4, ncol=3))+
  coord_sf(xlim = c(-69.46793,  -69.34982), ylim = c(-12.69133,-12.51036))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")


library(cowplot)
Mapa= ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  
  
 
  
  
  draw_plot(SurA , width = 6, height = 6,x = -1, y = 14.5)+
  draw_plot(MDD_map , width = 6, height = 6,x = 3.8, y = 14.5)+
  draw_plot(Cuenca_map , width = 6, height = 6,x = 9.4, y = 14.5)+
  
  draw_plot(Cuenca_Mapa2  , width = 15, height = 15,x = 0, y = 0.8)+
  
  draw_plot(legend , width = 5, height = 5,x = 6, y = -1.5)+
  
  draw_plot(Zona_map  , width = 20, height = 20,x = 12, y = 0.5)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))


ggsave(plot=Mapa ,"Mapa de clasificacion.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)

























