library(R.matlab)
library(rgdal)                                                                                                      
library(raster)
library(ggplot2)
library(rgeos)
library(scales)
library(cowplot)
library(ggpubr)
#Limpio el espacio de trabajo
rm(list=ls())

# Cargo el archivo ascii
archivo <- c("norm.daily.aao.index.b790101.current.ascii")
encabezado <- c("Anio", "Mes", "Dia","Valor")
sam <- read.table(archivo, col.names=encabezado)

# Agrego un campo de tipo fecha
sam$Fecha <-
  as.Date(with(sam, paste(sam$Anio, sam$Mes, sam$Dia, sep = "-")), "%Y-%m-%d")

# Filtro las fechas que  voy a procesar
sam <-
  subset(sam, (Fecha >= as.Date(c('1980-05-02')) & Fecha <= as.Date(c('2011-09-29')))
         & (as.integer(format(Fecha, "%m")) >= 5 & as.integer(format(Fecha, "%m")) <= 9) 
         & (ifelse(as.integer(format(Fecha, "%m")) == 5, as.integer(format(Fecha, "%d")) >= 2, Fecha)   
            &  ifelse(as.integer(format(Fecha, "%m")) == 9, as.integer(format(Fecha, "%d")) <= 29, Fecha)),
         select = c(Fecha, Valor, Anio))

# Agrego un campo para numerar los años
sam$nAnio <- (as.integer(sam$Anio) - 1979) 
sam$nDia <- as.integer(strftime(sam$Fecha, format = "%j")) - 
  (as.integer(strftime(as.Date(paste(sam$Anio, c('5'), c('2'), sep = "-"), "%Y-%m-%d"), format = "%j")) - 1)

# Promedio  
m_value <- mean(sam$Valor)
# Desviacion estandar
sd_value <- sd(sam$Valor)

# Funcion para obtener la lista de fechas segun los casos >=
obtenerCasosMayores <- function(nivel, rango) {
  caso <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())
  aux <- data.frame(Fecha = as.Date(character()), nAnio = integer(), nDia = integer())
  i <- 0
  for (item in rownames(sam)) {
    #Verifico cada valor con el nivel
    ds_val <- sam[item, "Valor"]
    if (ds_val >= nivel) {
      aux <- rbind(aux, data.frame(Fecha = sam[item, "Fecha"], nAnio = sam[item, "nAnio"], nDia = sam[item, "nDia"]))
      i <- i + 1
    }
    else {
      # Verificar si se mantiene en el rango del valor estudiado
      if (i > 0) {
        # Verificar si el rango de dias es entre 5 y 10 dias o mayor a 10 dias.
        if (isTRUE(rango)) {
          if (i >= 5 && i <= 10) {
            caso <- rbind(caso, aux)
          }
        }
        else{
          if (i >= 10) {
            caso <- rbind(caso, aux)
          }
        }
        aux <- data.frame(Fecha = as.Date(character()),nAnio = integer(),nDia = integer())
        i <- 0
      }
    }
  }
  return(caso)
}
# Funcion para obtener la lista de fechas segun los casos <=
obtenerCasosMenores <- function(nivel, rango) {
  caso <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())
  aux <- data.frame(Fecha = as.Date(character()), nAnio = integer(), nDia = integer())
  i <- 0
  for (item in rownames(sam)) {
    #Verifico cada valor con el nivel
    ds_val <- sam[item, "Valor"]
    if (ds_val <= nivel) {
      aux <- rbind(aux, data.frame(Fecha = sam[item, "Fecha"], nAnio = sam[item, "nAnio"], nDia = sam[item, "nDia"]))
      i <- i + 1
    }
    else {
      # Verificar si se mantiene en el rango del valor estudiado
      if (i > 0) {
        # Verificar si el rango de dias es entre 5 y 10 dias o mayor a 10 dias.
        if (isTRUE(rango)) {
          if (i >= 5 && i <= 10) {
            caso <- rbind(caso, aux)
          }
        }
        else{
          if (i >= 10) {
            caso <- rbind(caso, aux)
          }
        }
        aux <- data.frame(Fecha = as.Date(character()),nAnio = integer(),nDia = integer())
        i <- 0
      }
    }
  }
  return(caso)
}

#Listas parciales
caso_a <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())
caso_b <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())

caso_c <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())
caso_d <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())

caso_e <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())
caso_f <- data.frame(Fecha=as.Date(character()), nAnio=integer(), nDia=integer())

v_nivel <- (m_value + sd_value)
caso_a <- obtenerCasosMayores(v_nivel, TRUE)
v_nivel <- (m_value - sd_value)
caso_b <- obtenerCasosMenores(v_nivel, TRUE)
v_nivel <- (m_value + (2 * sd_value))
caso_c <- obtenerCasosMayores(v_nivel, TRUE)
v_nivel <- (m_value - (2 * sd_value))
caso_d <- obtenerCasosMenores(v_nivel, TRUE)
v_nivel <- (m_value + sd_value)
caso_e <- obtenerCasosMayores(v_nivel, FALSE)
v_nivel <- (m_value - sd_value)
caso_f <- obtenerCasosMenores(v_nivel, FALSE)

# Cargo el archivo matlab
archivo <- "ahgt250_global_NOAA_INVIERNO.mat"
ahgt250<-readMat(archivo) 

mat_hpa_caso_a <- data.frame(lat=double(), long=double(), valor=double()) 
mat_hpa_caso_b <- data.frame(lat=double(), long=double(), valor=double()) 
mat_hpa_caso_c <- data.frame(lat=double(), long=double(), valor=double()) 
mat_hpa_caso_d <- data.frame(lat=double(), long=double(), valor=double())
mat_hpa_caso_e <- data.frame(lat=double(), long=double(), valor=double()) 
mat_hpa_caso_f <- data.frame(lat=double(), long=double(), valor=double()) 

obtenerDataFrame <- function(caso) {
  mat_hpa_caso <-
    data.frame(lat = double(),
               long = double(),
               valor = double())
  promedio <- data.frame(Valor = double())
  # Recorro cada punto
  for (i in 1:73) {
    for (j in 1:144) {
      # Recorro cada elemento del caso
      for (item in rownames(caso)) {
        ind_inicio <-
          as.integer((caso[item, "nAnio"] -1) * 151 * 73 * 144)  + (as.integer(caso[item, "nDia"] -1) * 73 * 144)
        promedio <-
          rbind(promedio, data.frame(Valor = ahgt250$ahgt[ind_inicio + ((i-1) * 144) + j]))
      }
      v_promedio <- mean(promedio$Valor)
      v_lat <- (-90 + ((i-1) * 2.5))
      v_long <-(-180 + ((j-1) * 2.5))
      mat_hpa_caso = rbind(mat_hpa_caso, data.frame(lat = v_lat,long = v_long,valor = v_promedio))
      promedio <- data.frame(Valor = double())
    }
  }
  return(mat_hpa_caso)
}

# Casos
mat_hpa_caso_a <- obtenerDataFrame(caso_a)
mat_hpa_caso_b <- obtenerDataFrame(caso_b)
mat_hpa_caso_c <- obtenerDataFrame(caso_c)
mat_hpa_caso_d <- obtenerDataFrame(caso_d)
mat_hpa_caso_e <- obtenerDataFrame(caso_e)
mat_hpa_caso_f <- obtenerDataFrame(caso_f)

# Cargo el archivo matlab
archivo <- "datos_aolri_global.mat"
aolri<-readMat(archivo) 
mat_aol_caso_a <- data.frame(lat=double(), long=double(), valor=double()) 
mat_aol_caso_b <- data.frame(lat=double(), long=double(), valor=double())  
mat_aol_caso_c <- data.frame(lat=double(), long=double(), valor=double())  
mat_aol_caso_d <- data.frame(lat=double(), long=double(), valor=double())  
mat_aol_caso_e <- data.frame(lat=double(), long=double(), valor=double())  
mat_aol_caso_f <- data.frame(lat=double(), long=double(), valor=double()) 

# Casos 
mat_aol_caso_a <- obtenerDataFrame(caso_a)
mat_aol_caso_b <- obtenerDataFrame(caso_b)
mat_aol_caso_c <- obtenerDataFrame(caso_c)
mat_aol_caso_d <- obtenerDataFrame(caso_d)
mat_aol_caso_e <- obtenerDataFrame(caso_e)
mat_aol_caso_f <- obtenerDataFrame(caso_f)

#Graficos
grafico_hpa<-function(mat_hpa){
  # Defines the x axes required
  x_lines <- seq(-120,180, by = 60)
  data("wrld_simpl", package = "maptools")                                                                            
  wm <- crop(wrld_simpl, extent(-180, 180, -90, -20))
  v_fill <- scale_fill_distiller(palette='RdBu',limits = c(-50,50),breaks=pretty_breaks(8),labs(fill = "Anomalías"))
  
  g<- ggplot() + 
    geom_tile(data = mat_hpa, aes(x = long, y = lat, fill=valor)) +
    v_fill+
    geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill = "grey", colour = "black", alpha = 0.1) +
    # Convert to polar coordinates
    coord_map("ortho", orientation = c(-90, 0, 0)) +
    scale_y_continuous(breaks = seq(-90, -20, by = 5), labels = NULL) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL) +
    xlab("") + 
    ylab("") +
    
    # Adds labels
    geom_text(aes(x = 180, y = seq(-25, -85, by = -10), hjust = -0.2, label = paste0(seq(25, 85, by = 10), "°S"))) +
    geom_text(aes(x = x_lines, y = -39, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
    
    # Adds axes
    geom_hline(aes(yintercept = -20), size = 1)  +
    geom_segment(aes(y = -90, yend = -20, x = x_lines, xend = x_lines), linetype = "dashed") +
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "black"),axis.ticks=element_blank())
  
  return(g)
}

g1<- grafico_hpa(mat_hpa_caso_a)
g2<- grafico_hpa(mat_hpa_caso_b)

casos_ab <- plot_grid(g1,g2, labels = c("Anomalías Estacionales de hgp250 hPa - Caso A",
                                        "Anomalías Estacionales de hgp250 hPa - Caso B"), 
                      ncol = 2, 
                      nrow = 1)
ggsave(filename = "AnomalíasEstacionales_hgp250_hPa_CasoAB.png", casos_ab)

g3<- grafico_hpa(mat_hpa_caso_c)
g4<- grafico_hpa(mat_hpa_caso_d)

casos_cd <- plot_grid(g3,g4, labels = c("Anomalías Estacionales de hgp250 hPa - Caso C",
                                        "Anomalías Estacionales de hgp250 hPa - Caso D"), 
                      ncol = 2, 
                      nrow = 1)
ggsave(filename = "AnomalíasEstacionales_hgp250_hPa_CasoCD.png", casos_cd)


g5<- grafico_hpa(mat_hpa_caso_e)
g6<- grafico_hpa(mat_hpa_caso_f)

casos_ef <- plot_grid(g5,g6, labels = c("Anomalías Estacionales de hgp250 hPa - Caso E",
                                        "Anomalías Estacionales de hgp250 hPa - Caso F"), 
                      ncol = 2, 
                      nrow = 1)
ggsave(filename = "AnomalíasEstacionales_hgp250_hPa_CasoEF.png", casos_ef)


grafico_aol <- function(mat_aol_caso) {
  
  my_theme <- theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())
  my_cols <- scale_color_distiller(palette='Spectral')
  my_fill <- scale_fill_distiller(palette='Spectral', labs(fill = "Valor Anomalías"))
  
  g <- ggplot(mat_aol_caso, aes(y=lat, x=long, color=valor)) +
    geom_point() +
    borders('world', xlim=range(mat_aol_caso$long), ylim=range(mat_aol_caso$lat), colour='black') + my_theme + my_cols +
    coord_map('lambert', lat0=-50, lat1=-30, xlim=c(-90, -30), ylim=c(-90, 15))
  
  return(g)
}

aol_g1 <- grafico_aol(mat_aol_caso_a)
aol_g2 <- grafico_aol(mat_aol_caso_b)

casos_aol_ab <- plot_grid(aol_g1,aol_g2, labels = c("Anomalías Estacionales de OLR a hgp250 hPa - Caso A",
                                                    "Anomalías Estacionales de OLR a hgp250 hPa - Caso B"), 
                          ncol = 2, 
                          nrow = 1)
ggsave(filename = "AnomalíasEstacionalesOLR_hgp250_hPa_CasoAB.png", casos_aol_ab)


aol_g3 <- grafico_aol(mat_aol_caso_c)
aol_g4 <- grafico_aol(mat_aol_caso_d)

casos_aol_cd <- plot_grid(aol_g3,aol_g4, labels = c("Anomalías Estacionales de OLR a hgp250 hPa - Caso C",
                                                    "Anomalías Estacionales de OLR a hgp250 hPa - Caso D"), 
                          ncol = 2, 
                          nrow = 1)
ggsave(filename = "AnomalíasEstacionalesOLR_hgp250_hPa_CasoCD.png", casos_aol_cd)

aol_g5 <- grafico_aol(mat_aol_caso_e)
aol_g6 <- grafico_aol(mat_aol_caso_f)

casos_aol_ef <- plot_grid(aol_g5,aol_g6, labels = c("Anomalías Estacionales de OLR a hgp250 hPa - Caso E",
                                                    "Anomalías Estacionales de OLR a hgp250 hPa - Caso F"), 
                          ncol = 2, 
                          nrow = 1)
ggsave(filename = "AnomalíasEstacionalesOLR_hgp250_hPa_CasoEF.png", casos_aol_ef)
