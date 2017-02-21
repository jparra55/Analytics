# Limpieza de datos hospitalizacion
# Librerias necesarias
library(lubridate)
library(data.table)

## Tabla dañada
#hospitalizacion.1 = read.table("Universidad/Semillero/Proyecto/Datos Reales/hospitalizacion.txt",
#                             header = TRUE,sep = "|", encoding = "UTF-8",  nrows = 1115720,
#                             stringsAsFactors = F)
#hospitalizacion.2 = read.table("Universidad/Semillero/Proyecto/Datos Reales/hospitalizacion.txt",
#                               header = F,sep = "|", encoding = "UTF-8", nrows = 1, skip = 1115721, 
#                               stringsAsFactors = F)
#hospitalizacion.3 = read.table("Universidad/Semillero/Proyecto/Datos Reales/hospitalizacion.txt",
#                               header = F,sep = "|", encoding = "UTF-8", nrows = -1, skip = 1115722, 
#                               stringsAsFactors = F)

#hospitalizacion.2 = hospitalizacion.2[,-5]
#names(hospitalizacion.2) = names(hospitalizacion.1)
#names(hospitalizacion.3) = names(hospitalizacion.1)

#hospitalizacion = rbind(hospitalizacion.1, hospitalizacion.2, hospitalizacion.3)
#rm(hospitalizacion.1)
#rm(hospitalizacion.2)
#rm(hospitalizacion.3)
#gc()

## Escribir archivo dañado
#write.table(hospitalizacion, "Universidad/Semillero/Proyecto/Datos Reales/hospitalizacion_R.txt",
#            row.names = FALSE, quote = FALSE, sep = "|")

# Cargar tabla hospitalizacion
hospitalizacion = fread("Universidad/Semillero/Proyecto/Datos Reales/hospitalizacion_R.txt",
                         header = TRUE,sep = "|")

# Observar estructura del conjunto de datos
str(hospitalizacion)
summary(hospitalizacion)

# Observar cuales atributos tenian valores perdidos
sapply(hospitalizacion, function(x)any(is.na(x)))

# Reemplazar valores perdidos
# cod_dpto
table(is.na(hospitalizacion$cod_dpto))/nrow(hospitalizacion)
hospitalizacion[is.na(hospitalizacion$cod_dpto), "cod_dpto"] = 0

# cod_mpio
table(is.na(hospitalizacion$cod_mpio))/nrow(hospitalizacion)
hospitalizacion[is.na(hospitalizacion$cod_mpio), "cod_mpio"] = 0

# tipo_usuario
table(is.na(hospitalizacion$tipo_usuario))/nrow(hospitalizacion)
hospitalizacion[is.na(hospitalizacion$tipo_usuario), "tipo_usuario"] = 0

# Identificar las variables de fecha y hora que tienen valores faltantes ("")
# Requerimiento 1 (Variables con nombre de Fecha u Hora)
names.Date.Time = names(hospitalizacion)[c(grep("FECHA",names(hospitalizacion)),
                                           grep("HORA",names(hospitalizacion)))]

# Para identificar que variables tiene datos en blanco
val.faltantes = function(x)any(x=="")

# Inicializar variable var.faltante con ""
var.faltante = ""

# Prueba cada funcion sobre las variables que cumplen el requerimiento 1
for(namesfh in names.Date.Time){
     if(val.faltantes(hospitalizacion[[namesfh]])==TRUE){
          var.faltante = c(var.faltante,namesfh)
     }
}

# Imprime el nombre de la variable que tiene datos en blanco
var.faltante

# Reemplazo de los valores en blanco de la columna hora_ingreso con la hora 00:00
hospitalizacion$HORA_INGRESO[hospitalizacion$HORA_INGRESO == ""] = "00:00"

# Generar variable de fecha con hora
hospitalizacion$FECHA.INGRESO.COMP = ymd_hm(paste(hospitalizacion$FECHA_INGRESO,
                                                  hospitalizacion$HORA_INGRESO, sep = " "))
hospitalizacion$FECHA.EGRESO.COMP = ymd_hm(paste(hospitalizacion$FECHA_EGRE,
                                                  hospitalizacion$HORA_EGRE, sep = " "))                                           

# Conversion de fechas y horas en el formato adecuado
hospitalizacion$FECHA_INGRESO = ymd(hospitalizacion$FECHA_INGRESO)
hospitalizacion$FECHA_EGRE = ymd(hospitalizacion$FECHA_EGRE)
hospitalizacion$HORA_INGRESO = hm(hospitalizacion$HORA_INGRESO)
hospitalizacion$HORA_EGRE = hm(hospitalizacion$HORA_EGRE)

# Reemplazar celdas en blanco ("") con "DESCONOCIDO"
# Identificar las variables tipo string que tienen valores faltantes
char.cols <- names(hospitalizacion)[sapply(hospitalizacion,is.character)]

# Para identificar que variables tiene datos en blanco
val.faltantes = function(x)any(x=="")

# Inicializar variable var.faltante con ""
var.faltante2 = ""

# Prueba cada funcion sobre las variables que cumplen el requerimiento 1
for(name in char.cols){
     if(val.faltantes(hospitalizacion[[name]])==TRUE){
          var.faltante2 = c(var.faltante2,name)
     }
}

# Imprime el nombre de la variable que tiene datos en blanco
var.faltante2

# Reemplazo de los valores en blanco de las columnas encontradas en la variable var.faltante2
hospitalizacion$DIAG_EGRE1[hospitalizacion$DIAG_EGRE1==""] = "DESCONOCIDO"
hospitalizacion$DIAG_EGRE2[hospitalizacion$DIAG_EGRE2==""] = "DESCONOCIDO"
hospitalizacion$DIAG_EGRE3[hospitalizacion$DIAG_EGRE3==""] = "DESCONOCIDO"
hospitalizacion$DIAG_COMPLI[hospitalizacion$DIAG_COMPLI==""] = "DESCONOCIDO"
hospitalizacion$DIAG_MUERTE[hospitalizacion$DIAG_MUERTE==""] = "DESCONOCIDO"
hospitalizacion$sexo[hospitalizacion$sexo==""] = "DESCONOCIDO"
