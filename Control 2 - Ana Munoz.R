BDwine = read.csv("winequality-white.csv", sep = ";", header = TRUE) 
summary(BDwine)
str(BDwine)
library(ggplot2)
library(dplyr)
library(psych) 
library(corrplot) 
# Informacion General - Calidad de Vino
## Conociendo mi base de datos

### Principales Caracteristicas
#La BD brindada consta de las siguientes caraceristicas:
  
arrange(BDwine)
summary(BDwine)

mat.cor = cor(BDwine[-13])
  corrplot(mat.cor, method = "circle")
# ggsave(filename = "correlacion de todas las variables.png", plot = Correlacion_variables)

#Analizo como estan mis datos de calidad de vino
  ggplot(BDwine, aes(x = BDwine$quality))+
  geom_histogram(binwidth = 0.5) +
  xlab("Calidad de Vino") + ylab ("Frecuencia") +
  ggtitle("Distribucion de datos segun calidad")
  
#se ha encontrado relaciones fuertes entre alcohol y densidad, indirecta, mayor alcohol menos densidad
#RElacion indirecta entre el alcohol, sugar residual y total dioaxigo sulfuro

  
  #VOY A CLASIFICAR MIS VINOS CON UNA NUEVA COLUMNA
BDwine$Classify = ifelse(BDwine$quality >= 8, "Alta", 
  ifelse(BDwine$quality <= 3, "Baja","Media"))

ggplot(BDwine, aes(x = BDwine$density, y = BDwine$alcohol)) +
  xlab("Densidad") + ylab ("Alcohol") +
  ggtitle("Densidad vs Alcohol") +
  geom_point() +  
  facet_wrap(~ BDwine$Classify, scale = "free_y")+
  geom_smooth()

# SEPARO LA INFORMACION EN TABLAS PARA UNA ANALISIS POR CALIDAD DE VINO
#Split table
BDwine_splitted = split(BDwine, BDwine$Classify)

BDwine_Alta = BDwine_splitted$Alta
BDwine_Baja = BDwine_splitted$Baja
BDwine_Media = BDwine_splitted$Media

# AVERIGUO QUE TIPO DE RELACION TIENEN ESTAS DOS VARIABLES MAS FONDO
ggplot(BDwine_Alta, aes(x = BDwine_Alta$density, y = BDwine_Alta$alcohol)) +
  xlab("Densidad") + ylab ("Alcohol") +
  ggtitle("Densidad vs Alcohol Calidad Alta") +
  geom_point() +  
  geom_smooth()

ggplot(BDwine_Baja, aes(x = BDwine_Baja$density, y = BDwine_Baja$alcohol)) +
  xlab("Densidad") + ylab ("Alcohol") +
  ggtitle("Densidad vs Alcohol Calidad Baja") +
  geom_point() +  
  geom_smooth()
  
ggplot(BDwine_Media, aes(x = BDwine_Media$density, y = BDwine_Media$alcohol)) +
  xlab("Densidad") + ylab ("Alcohol") +
  ggtitle("Densidad vs Alcohol Calidad Media") +
  geom_point() 
  #geom_smooth(method = "lm") 


# AVERIGUO QUE TIPO DE RELACION TIENEN ESTAS DOS VARIABLES MAS FONDO

ggplot(BDwine_Media, aes(x = BDwine_Media$density, y = BDwine_Media$residual.sugar)) +
  xlab("Densidad") + ylab ("Azucar Residual") +
  ggtitle("Densidad vs Azucar Residual Calidad Media") +
  geom_point()

ggplot(BDwine_Baja, aes(x = BDwine_Baja$density, y = BDwine_Baja$residual.sugar)) +
  xlab("Densidad") + ylab ("Azucar Residual") +
  ggtitle("Densidad vs Azucar Residual Calidad Baja") +
  geom_point()

ggplot(BDwine_Alta, aes(x = BDwine_Alta$density, y = BDwine_Alta$residual.sugar)) +
  xlab("Densidad") + ylab ("Azucar Residual") +
  ggtitle("Densidad vs Azucar Residual Calidad Alta") +
  geom_point()+
  geom_smooth(method = "lm") 

#La realacion de calidad con las principales variables con boxplot
ggplot(data = BDwine, aes(x = BDwine$quality, y = BDwine$alcohol)) +  
  geom_boxplot(aes(fill = as.character(BDwine$Classify))) + 
  facet_wrap(~ BDwine$Classify, scale = "free_y")+
  ylab("Alcohol") + xlab("Calidad")+
  guides(fill = guide_legend(title="Calidad"))+
  ggtitle("Relacion Calidad vs Alcohol por Clasificacion")

ggplot(data = BDwine, aes(x = BDwine$quality, y = BDwine$total.sulfur.dioxide)) +  
  geom_boxplot(aes(fill = as.character(BDwine$Classify))) +    
  facet_wrap(~ BDwine$Classify, scale = "free_y") +
  ylab("Total Sulfuro Dioxido") + xlab("Calidad") +
  guides(fill = guide_legend(title="Calidad")) +
  ggtitle("Relacion Calidad vs Total Dioxido de Sulfuro por Clasificacion")

ggplot(data = BDwine, aes(x = BDwine$quality, y = BDwine$volatile.acidity)) +  
  geom_boxplot(aes(fill = as.character(BDwine$Classify))) +   
  facet_wrap(~ BDwine$Classify, scale = "free_y") +
  ylab("Valatile Acidity") + xlab("Calidad") +
  guides(fill = guide_legend(title="Calidad")) +
  ggtitle("Relacion Calidad vs Volatile Acidity por Clasificacion")

ggplot(data = BDwine, aes(x = BDwine$quality, y = BDwine$fixed.acidity)) +  
  geom_boxplot(aes(fill = as.character(BDwine$Classify))) +   
  facet_wrap(~ BDwine$Classify, scale = "free_y") +
  ylab("Fixed Acidity") + xlab("Calidad") +
  guides(fill = guide_legend(title="Calidad")) +
  ggtitle("Relacion Calidad vs Fixed Acidity por Clasificacion")

#Analizo mas a fondo la informacion de clasifcacion MEDIA

ggplot(BDwine_Media, aes(x = BDwine_Media$quality))+
  geom_histogram(binwidth = 0.5) +
  xlab("Calidad de Vino") + ylab ("Frecuencia") +
  ggtitle("Distribucion de datos segun calidad")

ggplot(BDwine_Media, aes(x = BDwine_Media$density, y = BDwine_Media$alcohol)) +
  xlab("Densidad") + ylab ("Alcohol") +
  ggtitle("Densidad vs Alcohol") +
  geom_point() +  
  facet_wrap(~ BDwine_Media$quality, scale = "free_y")+
  geom_smooth()

ggplot(BDwine_Media, aes(x = BDwine_Media$quality, y = BDwine_Media$fixed.acidity)) +  
  geom_boxplot(aes(fill = as.character(BDwine_Media$quality))) +   
  facet_wrap(~ BDwine_Media$quality, scale = "free_y") +
  ylab("Fixed Acidity") + xlab("Calidad") +
  guides(fill = guide_legend(title="Calidad")) +
  ggtitle("Relacion Calidad vs Fixed Acidity por Clasificacion")

