#########################
# SEMINARIO DE TESIS    #
#########################

getwd()

datos<- read.csv("neumonia_spat.csv")
summary(datos)
datos<-na.omit(datos)
#..............................................................
#install.packages("AER") # instalamos un paquete
library(AER)             # llamado a la libreria
library(dplyr)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Exploracion de datos
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

colnames(datos) # nombres de las columnas

dim(datos)  # dimensiones de filas y columnas
nrow(datos) # numero de filas
ncol(datos) # numero de columnas
str(datos)  # estructura de datos
glimpse(datos) # otra forma de visualizar la estructura

#- registros unicos
nrow(datos %>% distinct()) 
nrow(datos %>% select(gender) %>% distinct())

#- resumen de todas las columnas
summary(datos) 

#- resumen de todas las columnas agrupado por GENDER
(datos %>% group_by(gender) %>% do(the_summary = summary(.)))$the_summary



#- Variables NUMERICAS y CATEGORICAS

#install.packages("funModeling")
library(funModeling)

#para obtener variables numericas
# Numericas
profiling_num(datos) # estadisticas descriptivas
neumonia_num <- profiling_num(datos) %>% as.data.frame()

esta_num
plot_num(datos) # histogramas

# Categoricas
freq(datos)

table(datos$married) # recuento por categoria
prop.table(table(datos$married)) # porcentaje por categoria

summary(datos)
var(datos$Semana)
var(datos$Casos.confirmados)


hist(datos$Semana,
     breaks = 53, # numero de intervalos     main = "Distribución del salario \n (dólares por hora)", # titulo grafico
     freq = FALSE,                                 
     main = "Distribuci?n semana",
     xlab = "INGRESO",                                       
     ylab = "Frecuencia Relativa",         
     #xlim = c(0,50),                                   
     #ylim = c(0,130),                                    
     col = "GREEN",                                 
     border = "green4")   
lines(density(datos$Semana),col="gray",lwd=3)
hist(datos$Semana)

hist(datos$Semana,
     main = "Distribuci?n semana",
     xlab = "N? de Semana",                                       
     ylab = "Frecuencia", 
     col = "GREEN",                                 
     border = "green4")

hist(datos$Casos.confirmados,
     main = "Distribuci?n Casos confirmados",
     xlab = "N? de casos confirmados",                                       
     ylab = "Frecuencia", 
     col = "GREEN",                                 
     border = "green4")

hist(datos$A?.o,
     main = "Distribuci?n A?os",
     xlab = "A?os",                                       
     ylab = "Frecuencia", 
     col = "GREEN",                                 
     border = "green4")

table(datos$Distrito)
freq(datos)
table(datos$A?.o)
table(datos$A?.o,datos$Casos.confirmados)
sum(datos$Casos.confirmados)
plot(datos$A?.o,datos$Casos.confirmados)


#MATRIX DE CORRELACION 
mm<-select(datos,c(Semana,  Casos.confirmados, POB2) )
mm
round(cor(mm),2)  

#::::::::::::::::::::::::::::::::::
#incidencia
datos$incidecia<- datos$Casos.confirmados/datos$POB2*1000
datos$incidecia<-NULL
#:::....

ttt <- datos %>%
  group_by(Departamento,A?.o) %>%
  summarise(count = n())

head(ttt)
tail(ttt)

gg <- ggplot(ttt) +
  geom_line(aes(A?.o, count)) +
  geom_point(aes(A?.o, count), col = 1, size = 0.4) +
  geom_smooth(aes(A?.o, count), col = 2) +
  facet_wrap(~ Departamento, scales = "free_y", ncol = 4) +
  labs(x = NULL, y = "Número de casos confirmados")
# print(gg)
ggsave("datos.pdf", width = 10, height = 10, gg)
#::::::::::::::::::::::::::::::::::::::::::::::::

ppp <- datos %>%
  group_by(Departamento,Casos.confirmados) %>%
  summarise(count = n())

head(ppp)
tail(ppp)

gg <- ggplot(ppp) +
  geom_line(aes(Casos.confirmados, count)) +
  geom_point(aes(Casos.confirmados, count), col = 1, size = 0.4) +
  geom_smooth(aes(Casos.confirmados, count), col = 2) +
  facet_wrap(~Departamento, scales = "free_y", ncol = 4) +
  labs(x = NULL, y = "Número de casos confirmados")
# print(gg)
ggsave("datos.pdf", width = 10, height = 10, gg)

########################
round(prop.table(table(datos$Departamento))*100,2)
round(prop.table(table(datos$Departamento))*100,0)





