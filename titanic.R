#librerias

################## carga de librerias necesarias
if(!require(ggplot2)){
  install.packages('ggplot2', repos='http://cran.us.r-project.org')
  library(ggplot2)
}
## Loading required package: ggplot2
if(!require(grid)){
  install.packages('grid', repos='http://cran.us.r-project.org')
  library(grid)
}
## Loading required package: grid
if(!require(gridExtra)){
  install.packages('gridExtra', repos='http://cran.us.r-project.org')
  library(gridExtra)
}


install.packages("dplyr")
library(dplyr)

data<-read.csv("titanic_train.csv",header=T,sep=",")
#attach(data)

#añadir un nuevo level a los factores de una dataframe https://stackoverflow.com/questions/23316815/add-extra-level-to-factors-in-dataframe
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}



str(data)
summary(data)
head (data , 10)



#3.3. Creación de nuevos atributos/columnas ###############################################################
data$Title<- NA

#añadido atributo Title  con la dedominacion de las personas 

# Don, Major, Master, Capt, Jonkheer, Rev, Col, Mr, Dr --> Mr
# Mrs, Countless, Mne --> Mrs
# Ms, Mlle,Miss --> Miss
# Master --> Master
 
#data$Title<-NULL

Don =grep("Don", data$Name) 
for (i in 1:length(Don)) {  data$Title[Don[i]]="Mr"}
Major =grep("Major", data$Name)   
for (i in 1:length(Major)) {  data$Title[Major[i]]="Mr"}
capt =grep("Capt", data$Name)
for (i in 1:length(capt)) {  data$Title[capt[i]]="Mr"}
Jonkheer =grep("Jonkheer", data$Name)  
for (i in 1:length(Jonkheer)) {  data$Title[Jonkheer[i]]="Mr"}
Rev =grep("Rev", data$Name) 
for (i in 1:length(Rev)) {  data$Title[Rev[i]]="Mr"}
Col =grep("Col", data$Name) 
for (i in 1:length(Col)) {  data$Title[Col[i]]="Mr"}
Mr =grep("Mr.", data$Name) 
for (i in 1:length(Mr)) {  data$Title[Mr[i]]="Mr"}
Dr=grep("Dr.", data$Name) 
for (i in 1:length(Dr)) {  data$Title[Dr[i]]="Mr"}

mrs= grep("Mrs.", data$Name) 
for (i in 1:length(mrs)) {  data$Title[mrs[i]]="Mrs"}
Countess= grep("Countess", data$Name) 
for (i in 1:length(Countess)) {  data$Title[Countess[i]]="Mrs"}
Mme= grep("Mme", data$Name) 
for (i in 1:length(Mme)) {  data$Title[Mme[i]]="Mrs"}

Mlle=grep("Mlle.", data$Name) 
for (i in 1:length(Mlle)) {  data$Title[Mlle[i]]="Miss"}
Ms=grep("Ms.", data$Name) 
for (i in 1:length(Ms)) {  data$Title[Ms[i]]="Miss"}
Miss=grep("Miss.", data$Name) 
for (i in 1:length(Miss)) {  data$Title[Miss[i]]="Miss"}


Master =grep("Master.", data$Name)   
for (i in 1:length(Master)) {  data$Title[Master[i]]="Master"}




# vemos si hay campos sin valor
colSums(is.na(data)) 
# vemos si hay campos con valor ""
colSums(data=="")




#TNULL Tratamiento campos Vacios 

  #TNULL 1- Embarked asisgnamos el valor S a los dos campos vacios
  data$Embarked[data$Embarked == ""] <- "S"

#TNULL 2- Cabin, le asignamos a todos los que no tienen valor el campo Z0 "que hace refererncia a una cabina donde ubicamos a los que no tienen cabina
#añadimos un factor level nuevo  
  data$Cabin <- addLevel(data$Cabin, "Z0")
  data$Cabin[data$Cabin==""]<-"Z0"

  #TNULL 3- Edad
# calculamos la media para cada titulo  
  Masterfilter <-filter(data,data$Title=="Master"  ) 
  z <- Masterfilter$Age
   mean(z, na.rm = TRUE)

  Missfilter <-filter(data,data$Title=="Miss"  ) 
  z <- Missfilter$Age
  mean(z, na.rm = TRUE)

  Mrfilter <-filter(data,data$Title=="Mr"  ) 
  z <- Mrfilter$Age
  mean(z, na.rm = TRUE)
  
  Mrsfilter <-filter(data,data$Title=="Mrs"  ) 
  z <- Mrsfilter$Age
  mean(z, na.rm = TRUE)
  
  
  
  for (i in 1:length(data$Age))
  { 
    if ( is.na(data$Age[i]) && data$Title[i]=="Master") {
      data$Age[i]<-4.57
    }
    if ( is.na(data$Age[i]) && data$Title[i]=="Miss") {
      data$Age[i]<-21.94
    }
    if ( is.na(data$Age[i]) && data$Title[i]=="Mr") {
      data$Age[i]<-32.96
    }
    if ( is.na(data$Age[i]) && data$Title[i]=="Mrs") {
      data$Age[i]<-35.61
    }
      
  
  }
  
  #data$Age[is.na(data$Age) && data$Title=="Master"  ] <- 4.57
  
  
  
# fin Tratamiento campos vacios
  
  
  
  
# si quisieramos quitar alguna fila que cumpla una condicion
#  filtrado <- filter(dataset, columna != " ?")
  
  
  
  
  
  
#3.2 ############### Encontrar valores anómalos              ################################################
  boxplot.stats(data$Pclass)$out
  boxplot.stats(data$Age)$out
  boxplot.stats(data$SibSp)$out
  boxplot.stats(data$Parch)$out
  boxplot.stats(data$Fare)$out
  
  
  
  
  
  
  
  
#3.4 Discretización de los datos  ##########################################################################
  
#realizar alguna discrecion de los datos
#se selecconan las variables que tendria sentido aplicar una proceso de discretizacion
 apply(data,2, function(x) length(unique(x)))
# Discretizamos las variables con pocas clases
#cols<-c("Survived","Pclass","SibSp","Parch","Title")
cols<-c("Survived","Pclass","Title")
for (i in cols){
  data[,i] <- as.factor(data[,i])
}
str(data)

  
#eliminar columnas no relevantes para el estudio
data$PassengerId<-NULL
data$Name<-NULL
data$Ticket<-NULL
#data$Fare<-NULL












# 4 ######### Analisis de los datos ########################################################################

#4.1. Selección del grupo de datos a analizar###############################################################

# Agrupación por clase
Pclass.1 <- data[data$Pclass == "1",]
Pclass.2 <- data[data$Pclass == "2",]
Pclass.3 <- data[data$Pclass == "3",]



#agrupación por sexo
Sex.male <-data[data$Sex=="male",]
Sex.female <-data[data$Sex=="female",]


#agrupación por embarked
Embarked.S <-data[data$Embarked=="S",]
Embarked.C <-data[data$Embarked=="C",]
Embarked.Q <-data[data$Embarked=="Q",]


#agrupacion por titulo
Title.Mr <- data[data$Title=="Mr",]
Title.Mrs <- data[data$Title=="Mrs",]
Title.Miss <- data[data$Title=="Miss",]
Title.Master <- data[data$Title=="Master",]





#4.2. Comprobación de la normalidad y homogeneidad de la varianza #########################################
library(nortest)
alpha = 0.05
col.names = colnames(data)
for (i in 1:ncol(data)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(data[,i]) | is.numeric(data[,i])) {
    p_val = ad.test(data[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(data) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}



#homoeneidad
fligner.test(Fare ~ Pclass, data = data)


data$Survived1<-data$Survived

#4.3  pruebas estadísticas

corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable cuantitativa
# con respecto al campo "precio"
for (i in 1:(ncol(data) - 1)) {
  if (is.integer(data[,i]) | is.numeric(data[,i])) {
    spearman_test = cor.test(data[,i],
                             data[,length(data)],
                             method = "spearman")
    corr_coef = spearman_test$estimate
    p_val = spearman_test$p.value
    # Add row to matrix
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    corr_matrix <- rbind(corr_matrix, pair)
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(data)[i]
  }
}

print(corr_matrix)


#5 graficas ##############################################################


attach(data)





str(data)
grid.newpage()
plotbyClass<-ggplot(data,aes(Pclass,fill=Survived))+geom_bar() +labs(x="Pclass", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Class")
plotbyAge<-ggplot(data,aes(Age,fill=Survived))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Age")
plotbySex<-ggplot(data,aes(Sex,fill=Survived))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Sex")
plotbyTitle<-ggplot(data,aes(Title,fill=Survived))+geom_bar() +labs(x="Title", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Title")
plotbyEmbarked<-ggplot(data,aes(Embarked,fill=Survived))+geom_bar() +labs(x="Embarked", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Embarked")
grid.arrange(plotbyClass,plotbyAge,plotbySex,plotbyTitle, plotbyEmbarked,ncol=2)


# Tablas para la variable Sex
tabla_SST <- table(Sex,Survived)
tabla_SST
prop.table(tabla_SST)
prop.table(tabla_SST, margin = 1)


# Tablas para la variables Class
tabla_SPC <- table(Pclass,Survived)
tabla_SPC
prop.table(tabla_SPC)
prop.table(tabla_SPC,1)

# Tablas para la variables Title
tabla_ST <- table(Title,Survived)
tabla_ST
prop.table(tabla_ST)
prop.table(tabla_ST,1)


# Tablas para la variables Age
tabla_SA <- table(Age,Survived)
tabla_SA
prop.table(tabla_SA)
prop.table(tabla_SA,1)

#eliminamos las columnas que no queremos usar para el analisis y obtenemos tablas cruzando las variables restantes
data$Age<-NULL
data$SibSp<-NULL
data$Parch<-NULL
data$Fare<-NULL
data$Cabin<-NULL
data$Embarked<-NULL
data$Survived1<-NULL

table(data) 


# mostramos graficas en porcentaje

par(mfrow=c(2,2))
plot(tabla_SST, col = c("black","#008000"), main = "SURVIVED vs. SEX")
plot(tabla_SPC, col = c("black","#008000"), main = "SURVIVED vs. Class")
plot(tabla_ST, col = c("black","#008000"), main = "SURVIVED vs. Title")



######### CREACIÓN DEL ÁRBOL DE DECISIÓN    - c5.0
str(data)

set.seed(666)
#cogemos 2/3 de los datos para entrenamiento y 1/3 para validacion

y<-data[,1] #SURVIVED
X <- data[,2:4] #PCLASS, TITLE, SEX

indexes = sample(1:nrow(data), size=floor((2/3)*nrow(data)))
trainx<-X[indexes,]
trainy<-y[indexes]
testx<-X[-indexes,]
testy<-y[-indexes]

str(trainy)

#Creamos el arbol de decision con los datos entrenamiento
model <- C50::C5.0(trainx, trainy,rules=TRUE )
#model <- C50::C5.0(trainx, trainy, control = C5.0Control(noGlobalPruning = TRUE,minCases=1))
summary(model)
model <- C50::C5.0(trainX, trainy)
plot(model)

#verificamos el modelo con los datos de verificacion (test)
predicted_model <- predict( model, testx, type="class" )
print(sprintf("La precisió de l'arbre és: %.4f %%",100*sum(predicted_model == testy) / length(predicted_model)))

#obtenemos la matriz de confusión para obtener más detalle sobre los errores
if(!require(gmodels)){
  install.packages('gmodels', repos='http://cran.us.r-project.org')
  library(gmodels)
}

CrossTable(testy, predicted_model,prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE,dnn = c('Reality', 'Prediction'))





#exportar un fichero csv
write.csv(data, file="salida1.csv")
