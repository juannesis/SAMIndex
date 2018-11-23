
#Limpio el espacio de trabajo
rm(list=ls())

# Cargo el archivo ascii
archivo <- c("norm.daily.aao.index.b790101.current.ascii")
encabezado <- c("Anio", "Mes", "Dia","Valor")
sam <- read.table(archivo, col.names=encabezado)

# Convierto las columnas al tipo fecha
sam$Fecha <-
  as.Date(with(sam, paste(sam$Anio, sam$Mes, sam$Dia, sep = "-")), "%Y-%m-%d")

#FFiltro las fechas que  voy a procesar
sam <-
  subset(sam, (Fecha >= as.Date(c('1980-05-02')) & Fecha <= as.Date(c('2011-09-29')))
         & (as.integer(format(Fecha, "%m")) >= 5 & as.integer(format(Fecha, "%m")) <= 9) 
         & (ifelse(as.integer(format(Fecha, "%m")) == 5, as.integer(format(Fecha, "%d")) >= 2, Fecha)   
            &  ifelse(as.integer(format(Fecha, "%m")) == 9, as.integer(format(Fecha, "%d")) <= 29, Fecha)),
         select = c(Fecha, Valor))

m_value <- mean(sam$Valor)

sam$Diferencia <- with(sam, Valor - m_value)
#Run trought the list and add the values to aux lists
stdP1510 <- data.frame(Date=as.Date(character()))
stdM1510 <- data.frame(Date=as.Date(character()))

stdP2510 <- data.frame(Date=as.Date(character()))
stdM2510 <- data.frame(Date=as.Date(character()))

stdP110 <- data.frame(Date=as.Date(character()))
stdM110 <- data.frame(Date=as.Date(character()))

aux <- data.frame(Date=as.Date(character()))
i <- 0
for (item1 in rownames(sam)) {
  #Verify if std value is > 1
  ds_diff <- sam[item1, "Diferencia"]
  if (ds_diff >= 1 && ds_diff < 2) {
    aux <- rbind(aux, data.frame(sam[item1, "Fecha"]))
    i <- i + 1
  }
  else {
    if (i > 0) {
      #Verificar la cantidad de valores que cumplieron la condicion
      if (i >= 10) {
        stdP110 <- rbind(stdP110, aux)
      }
      else if (i >= 5) {
        stdP1510 <- rbind(stdP1510, aux)
      }
      aux <- data.frame(Date=as.Date(character()))
      i = 0
    }
  }
}
aux <- data.frame(Date=as.Date(character()))
i <- 0
for (item2 in rownames(sam)) {
  #Verify if std value is > 1
  ds_diff <- sam[item2, "Diferencia"]
  if (ds_diff >= -1 && ds_diff < 1) {
    aux <- rbind(aux, data.frame(sam[item2, "Fecha"]))
    i <- i + 1
  }
  else {
    if (i > 0) {
      #Verifiy how many items acompliss the condition
      if (i >= 10) {
        stdM110 <- rbind(stdM110, aux)
      }
      else if (i >= 5) {
        stdM1510 <- rbind(stdM1510, aux)
      }
      aux <- data.frame(Date=as.Date(character()))
      i = 0
    }
  }
}

aux <- data.frame(Date=as.Date(character()))
i <- 0
for (item3 in rownames(sam)) {
  #Verify if std value is > 1
  ds_diff <- sam[item3, "Diferencia"]
  if (ds_diff > 2) {
    aux <- rbind(aux, data.frame(sam[item3, "Fecha"]))
    i <- i + 1
  }
  else {
    if (i > 0) {
      #Verifiy how many items acompliss the condition
      if (i >= 5 && i <= 10) {
        stdP2510 <- rbind(stdP2510, aux)
      }
      aux <- list()
      i = 0
    }
  }
}

aux <- data.frame(Date=as.Date(character()))
i <- 0
for (item4 in rownames(sam)) {
  #Verify if std value is > 1
  ds_diff <- sam[item4, "Diferencia"]
  if (ds_diff >= -2 & ds_diff < -1) {
    aux <- rbind(aux, data.frame(sam[item4, "Fecha"]))
    i <- i + 1
  }
  else {
    if (i > 0) {
      #Verifiy how many items acompliss the condition
      if (i >= 5 && i <= 10) {
        stdM2510 <- rbind(stdM2510, aux)
      }
      aux <- list()
      i = 0
    }
  }
}
aux <- data.frame(Date=as.Date(character()))
i <- 0




