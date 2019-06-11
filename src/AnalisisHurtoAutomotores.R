setwd("/Users/cesariv/OneDrive/UOC/CicloVidaDato/Practica2/")
library(lubridate)
library(PerformanceAnalytics)

#Importación del archivo csv
hurto2018=read.csv("Hurto_de_automotores_2018.csv")

# Conversión del campo fecha a fechas.
hurto2018$Fecha<-as.Date(hurto2018$Fecha,format="%d/%m/%Y")

# Verificar si existe algun valor duplicado
x=hurto2018[duplicated(hurto2018)]
print(x)

#Eliminar el atributo CodigoDANE ya que es redundante con el municipio.
hurto2018<-hurto2018[,-19]

#verificar cada uno de los atributos por elementos cero, nulos o vacios.
length(hurto2018$Fecha[is.na(hurto2018$Fecha)])
head(hurto2018[is.na(hurto2018$Fecha),])
summary(hurto2018$Fecha)
# Se encuentra un NA en el campo Fecha, revisando los datos originales es una fila que al final dice total y no tiene datos en las demás columnas.
# Por lo tanto se elimina esta fila del dataset 
hurto2018<-hurto2018[!is.na(hurto2018$Fecha),]
#No se detectaron otros valores vacios en el campo Fecha

summary(hurto2018$Departamento)
length(hurto2018$Departamento[is.na(hurto2018$Departamento)])
#El atributo departamento no tiene ninguna dato vacio

summary(hurto2018$Municipio)
length(hurto2018$Municipio[is.na(hurto2018$Municipio)])
#El atributo Municipio no tiene datos nulos o vacios

summary(hurto2018$Hora)
length(hurto2018$Hora[is.na(hurto2018$Hora)])
#El atributo Hora no tiene datos nulos o vacios

summary(hurto2018$Barrio)
length(hurto2018$Barrio[is.na(hurto2018$Barrio)])
nrow(hurto2018[hurto2018$Barrio=="-",])
#El atributo Barrio no tiene datos vacios, pero hay 5 con barrio en "-", lo cual indica que el dato no esta presente
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$Barrio)[levels(hurto2018$Barrio)=="-"]<-"DESCONOCIDO"

summary(hurto2018$Zona)
length(hurto2018$Zona[is.na(hurto2018$Zona)])
#El atributo Zona no tiene datos nulos o vacios

summary(hurto2018$Clase.de.sitio)
length(hurto2018$Clase.de.sitio[is.na(hurto2018$Clase.de.sitio)])
nrow(hurto2018[hurto2018$Clase.de.sitio=="-",])
#El atributo Clase.de.sitio no tiene datos nulos o vacios, pero hay 2 en "-", donde coinciden donde la zona es "OTRAS"
# Se cambio el "-" por el nombre "DESCONOCIDA"
levels(hurto2018$Clase.de.sitio)[levels(hurto2018$Clase.de.sitio)=="-"]<-"DESCONOCIDA"

summary(hurto2018$Arma.empleada)
length(hurto2018$Arma.empleada[is.na(hurto2018$Arma.empleada)])
nrow(hurto2018[hurto2018$Arma.empleada=="-",])
#El atributo Arma.Empleada no tiene datos nulos o vacios, pero hay 2 en "-", son entradas donde tampoco se reporta la profesion ni la escolaridad
# Se cambio el "-" por el nombre "DESCONOCIDA"
levels(hurto2018$Arma.empleada)[levels(hurto2018$Arma.empleada)=="-"]<-"DESCONOCIDA"

summary(hurto2018$Móvil.Agresor)
length(hurto2018$Móvil.Agresor[is.na(hurto2018$Móvil.Agresor)])
nrow(hurto2018[hurto2018$Móvil.Agresor=="-",])
#El atributo Movilagresor no tiene datos nulos o vacios, pero hay 16 en "-", revisandoslas todas son entradas validas por lo tanto se mantienen
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$Móvil.Agresor)[levels(hurto2018$Móvil.Agresor)=="-"]<-"DESCONOCIDO"

summary(hurto2018$Móvil.Victima)
length(hurto2018$Móvil.Victima[is.na(hurto2018$Móvil.Victima)])
nrow(hurto2018[hurto2018$Móvil.Victima=="-",])
#El atributo Móvil.Victima no tiene datos nulos o vacios, pero hay 16 en "-", revisandoslas todas son entradas validas por lo tanto se mantienen
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$Móvil.Victima)[levels(hurto2018$Móvil.Victima)=="-"]<-"DESCONOCIDO"

summary(hurto2018$Edad)
length(hurto2018$Edad[is.na(hurto2018$Edad)])
nrow(hurto2018[hurto2018$Edad<16,])
#El atributo Edad tiene datos 119 datos nulos, y hay 130 menores a 16 la cual es la edad minima para conducir un automotor, por lo tanto el valor no es válido
# Debido a que estos datos pueden afectar los resultados de algunos modelos y afecta un porcentaje bajo de los datos totales
# Por lo tanto se eliminan estas fila del dataset 
hurto2018<-hurto2018[!is.na(hurto2018$Edad),]
hurto2018<-hurto2018[hurto2018$Edad>=16,]
#El dataset queda con 9401 observaciones hasta el momento.

summary(hurto2018$Sexo)
length(hurto2018$Sexo[is.na(hurto2018$Sexo)])
nrow(hurto2018[hurto2018$Sexo=="-",])
#El atributo Sexo no tiene datos nulos o vacios.

summary(hurto2018$Estado.civil)
length(hurto2018$Estado.civil[is.na(hurto2018$Estado.civil)])
nrow(hurto2018[hurto2018$Estado.civil=="-",])
#El atributo Estado.civil no tiene datos nulos o vacios, pero hay 57 en "-", revisandoslas hay datos validao en otros campos.
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$Estado.civil)[levels(hurto2018$Estado.civil)=="-"]<-"DESCONOCIDO"

summary(hurto2018$País.de.nacimiento)
length(hurto2018$País.de.nacimiento[is.na(hurto2018$País.de.nacimiento)])
nrow(hurto2018[hurto2018$País.de.nacimiento=="-",])
#El atributo País.de.nacimiento no tiene datos nulos o vacios, pero hay 880 en "-", revisandoslas hay datos validao en otros campos.
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$País.de.nacimiento)[levels(hurto2018$País.de.nacimiento)=="-"]<-"DESCONOCIDO"

summary(hurto2018$Clase.de.empleado)
length(hurto2018$Clase.de.empleado[is.na(hurto2018$Clase.de.empleado)])
nrow(hurto2018[hurto2018$Clase.de.empleado=="-",])
#El atributo Clase de Empleado no tiene datos nulos o vacios, ni en "-", 

summary(hurto2018$Profesión)
length(hurto2018$Profesión[is.na(hurto2018$Profesión)])
nrow(hurto2018[hurto2018$Profesión=="-",])
#El atributo Profesión no tiene datos nulos o vacios, pero hay 5919 en "-", 
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$Profesión)[levels(hurto2018$Profesión)=="-"]<-"DESCONOCIDO"

summary(hurto2018$Escolaridad)
length(hurto2018$Escolaridad[is.na(hurto2018$Escolaridad)])
nrow(hurto2018[hurto2018$Escolaridad=="-",])
#El atributo Escolaridad no tiene datos nulos o vacios, pero hay 36 en "-", 
# Se cambio el "-" por el nombre "DESCONOCIDA"
levels(hurto2018$Escolaridad)[levels(hurto2018$Escolaridad)=="-"]<-"DESCONOCIDA"

summary(hurto2018$CLASE)
length(hurto2018$CLASE[is.na(hurto2018$CLASE)])
nrow(hurto2018[hurto2018$CLASE=="-",])
#El atributo CLASE no tiene datos nulos o vacios, ni en "-", 

summary(hurto2018$MARCA)
length(hurto2018$MARCA[is.na(hurto2018$MARCA)])
nrow(hurto2018[hurto2018$MARCA=="-",])
#El atributo MARCA no tiene datos nulos o vacios, pero hay uno en "-", 
# Se cambio el "-" por el nombre "DESCONOCIDA"
levels(hurto2018$MARCA)[levels(hurto2018$MARCA)=="-"]<-"DESCONOCIDA"

summary(hurto2018$LINEA)
length(hurto2018$LINEA[is.na(hurto2018$LINEA)])
nrow(hurto2018[hurto2018$LINEA=="-",])
#El atributo LINEA no tiene datos nulos o vacios, pero hay 138 en "-", 
# Se cambio el "-" por el nombre "DESCONOCIDA"
levels(hurto2018$LINEA)[levels(hurto2018$LINEA)=="-"]<-"DESCONOCIDA"

summary(hurto2018$MODELO)
length(hurto2018$MODELO[is.na(hurto2018$MODELO)])
#El atributo MODELO tiene 472 datos nulos
hurto2018<-hurto2018[!is.na(hurto2018$MODELO),]
# Hasta aqui se mantienen 8929 observaciones
nrow(hurto2018[hurto2018$MODELO<1940,])
#El atributo MODELO tiene 2 valores menores a 1940, los cuales revisandolos no son correctos por lo tanto se eliminan dichas observaciones
hurto2018<-hurto2018[hurto2018$MODELO>1940,]
# Hasta aqui 8927 observaciones 

summary(hurto2018$COLOR)
length(hurto2018$COLOR[is.na(hurto2018$COLOR)])
nrow(hurto2018[hurto2018$COLOR=="-",])
#El atributo COLOR no tiene datos nulos o vacios, pero hay 6 en "-", 
# Se cambio el "-" por el nombre "DESCONOCIDO"
levels(hurto2018$COLOR)[levels(hurto2018$COLOR)=="-"]<-"DESCONOCIDO"

summary(hurto2018$Cantidad)
length(hurto2018$Cantidad[is.na(hurto2018$Cantidad)])
nrow(hurto2018[hurto2018$Cantidad>1,])
#En el atributo cantidad solo hay 3 observaciones con 2 en la cantidad, pero no es lógico que hayan hurtado 2 automotores de las mismas características a la misma hora
#Por lo tanto se decide eliminar la columna cantidad por no ofrecer información valiosa para el análisis
hurto2018<-hurto2018[,-24]

# Unas variables numéricas que se pueden derivar y que pueden ser valiosas en el análisis de los datos estan:
# Mes del año
# Dia del mes
# Hora del dia
hurto2018$Mes<-month(hurto2018$Fecha)
hurto2018$Dia.Mes<-day(hurto2018$Fecha)
hurto2018$Hora.dia<-hour(hms(as.character(hurto2018$Hora)))
#Exportar los datos preprocesados
write.csv(hurto2018,file="Hurtos2018_cleaned.csv")


## Analisis de los datos

# Se grafican la matriz de correlación de las variables numericas
chart.Correlation(hurto2018[c("Edad","MODELO","Mes","Dia.Mes","Hora.dia")],histogram = TRUE)

summary(hurto2018[c("Edad","MODELO","Mes","Dia.Mes","Hora.dia")])
hist(hurto2018$Mes,breaks=12)
hist(hurto2018$Dia.Mes,breaks=31)
hist(hurto2018$Hora.dia,breaks=12)




