library(R.matlab)
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
  
m_value <- mean(sam$Valor)
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

mat_hpa_caso_a <- matrix(nrow = lat_total, ncol = long_total) 
mat_hpa_caso_b <- matrix(nrow = lat_total, ncol = long_total)
mat_hpa_caso_c <- matrix(nrow = lat_total, ncol = long_total) 
mat_hpa_caso_d <- matrix(nrow = lat_total, ncol = long_total) 
mat_hpa_caso_e <- matrix(nrow = lat_total, ncol = long_total) 
mat_hpa_caso_f <- matrix(nrow = lat_total, ncol = long_total) 

obtenerMatriz <- function(caso){
  mat_hpa_caso <- matrix(nrow = lat_total, ncol = long_total) 
  promedio <- data.frame(Valor=double())
  lat_total <- as.integer(73)
  long_total <- as.integer(144)
  v_promedio <- double()
  ind_inicio <- integer()
  # Recorro cada punto
  for (i in 1:lat_total) {
    for (j in 1:long_total) {
      # Recorro cada elemento del caso
      for (item in rownames(caso)){
        ind_inicio <- as.integer(caso[item, "nAnio"]) * as.integer(caso[item, "nDia"])
        #print(ind_inicio)
        promedio <- rbind(promedio, data.frame(Valor=ahgt250$ahgt[ind_inicio * i * j]))
      }
      v_promedio <- mean(promedio$Valor) 
      #print(v_promedio)
      mat_hpa_caso[i,j] = v_promedio
      promedio <- data.frame(Valor=double())
    }
  } 
  return(mat_hpa_caso)
}

# Casos
mat_hpa_caso_a <- obtenerMatriz(caso_a)
mat_hpa_caso_b <- obtenerMatriz(caso_b)
mat_hpa_caso_c <- obtenerMatriz(caso_c)
mat_hpa_caso_d <- obtenerMatriz(caso_d)
mat_hpa_caso_e <- obtenerMatriz(caso_e)
mat_hpa_caso_f <- obtenerMatriz(caso_f)

# Cargo el archivo matlab
archivo <- "datos_aolri_global.mat"

aolri<-readMat(archivo) 
str(aolri)

mat_aol_caso_a <- matrix(nrow = lat_total, ncol = long_total) 
mat_aol_caso_b <- matrix(nrow = lat_total, ncol = long_total)
mat_aol_caso_c <- matrix(nrow = lat_total, ncol = long_total) 
mat_aol_caso_d <- matrix(nrow = lat_total, ncol = long_total) 
mat_aol_caso_e <- matrix(nrow = lat_total, ncol = long_total) 
mat_aol_caso_f <- matrix(nrow = lat_total, ncol = long_total) 

# Casos 
mat_aol_caso_a <- obtenerMatriz(caso_a)
mat_aol_caso_b <- obtenerMatriz(caso_b)
mat_aol_caso_c <- obtenerMatriz(caso_c)
mat_aol_caso_d <- obtenerMatriz(caso_d)
mat_aol_caso_e <- obtenerMatriz(caso_e)
mat_aol_caso_f <- obtenerMatriz(caso_f)


# Graficos

